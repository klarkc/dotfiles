#!/usr/bin/env python3
"""Fail-closed canonical deployment/rollback on the prevalidated solo098 host.

Requires stopped ZeroClaw and preserved native checkpoint. Does not edit
agent configurations or start/stop daemons. Original canonical root is kept.
"""
from __future__ import annotations
from contextlib import closing
from datetime import datetime, timezone
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import sqlite3
import subprocess
import uuid

import agent_store as a
import production_operator as po
import production_preflight as pf
import snapshot_stage as snap

w=a.writer
HOME=Path.home()
STATE=HOME/".local/state/optmem-production"
CANON=HOME/".optmem/memory"
SIDECAR=STATE/"sidecar"
MARKER=STATE/"ACTIVE.json"
ROLLBACK=STATE/"cutover-rollback.json"
APPROVED=po.APPROVED_SHA


def stopped():
    if subprocess.run(["systemctl","--user","is-active","--quiet",
                       "zeroclaw.service"],check=False).returncode==0:
        raise RuntimeError("Stop ZeroClaw before canonical cutover or rollback")


def write_private(path: Path, obj: dict):
    pending=path.parent/("."+path.name+"-"+uuid.uuid4().hex)
    fd=os.open(pending,os.O_CREAT|os.O_EXCL|os.O_WRONLY|os.O_NOFOLLOW,0o600)
    with os.fdopen(fd,"w") as stream:
        json.dump(obj,stream,sort_keys=True,separators=(",",":"))
        stream.write("\n")
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(pending,path)


def install() -> dict:
    stopped()
    if not pf._source_confirmed(HOME):
        raise RuntimeError("Approved historical evidence did not verify")
    if MARKER.exists() or ROLLBACK.exists() or SIDECAR.exists():
        raise RuntimeError("Existing activation or recovery artifact; refuse overwrite")
    if not CANON.is_dir() or CANON.is_symlink():
        raise RuntimeError("Canonical directory is missing or symbolic")
    if (CANON/"LOG.txt").stat().st_size!=0:
        raise RuntimeError("Cannot overwrite nonempty canonical history")
    # Native checkpoint must be present before entering downtime.
    checkpoints=[p for p in STATE.glob("precutover-recovery-*")
                 if (p/"manifest.json").is_file()]
    if not checkpoints:
        raise RuntimeError("Native pre-cutover checkpoint missing")
    if not (STATE/"legacy-retained/manifest.json").is_file():
        raise RuntimeError("Final quiescent native archive missing")
    po.priv_dir(STATE)
    po.priv_dir(CANON)
    po.priv_dir(po.SOURCE)
    if po.digest(po.SOURCE/"LOG.txt")!=APPROVED:
        raise RuntimeError("Historical digest mismatch")
    parent=HOME/".optmem"
    po.priv_dir(parent)
    incoming=parent/(".memory-incoming-"+uuid.uuid4().hex)
    incoming.mkdir(mode=0o700)
    for name in ("LOG.txt","config"):
        po.copy_file(po.SOURCE/name,incoming/name)
    for src in sorted((po.SOURCE/"TREE").rglob("*")):
        if src.is_symlink():
            raise RuntimeError("Symlinked legacy TREE")
        relative=src.relative_to(po.SOURCE)
        if src.is_dir():
            po.priv_dir(incoming/relative,create=True)
        else:
            po.copy_file(src,incoming/relative)
    (incoming/".lock").touch(mode=0o600)
    new_side=STATE/(".sidecar-incoming-"+uuid.uuid4().hex)
    new_side.mkdir(mode=0o700)
    # Staged provenance ledger with the immutable 26-record historical baseline.
    # No synthetic agent note is copied.
    candidate=a.writer.HERE/"synthetic-cases"
    options=sorted(candidate.glob("g4k-agent-precutover-*"))
    valid=[]
    for item in options:
        try:
            result=a.writer.verify(item/"memory",item/"sidecar")
            if result["memory_entries"]==26 and result["committed_provenance"]==0:
                valid.append(item)
        except (ValueError,RuntimeError,OSError,sqlite3.DatabaseError):
            pass
    if not valid:
        raise RuntimeError("No approved 26-record paired stage available")
    selected=valid[-1]
    for name in ("provenance.sqlite3","agent-facts.sqlite3"):
        po.sqlite_backup(selected/"sidecar"/name,new_side/name)
    if snap._verified_state(incoming,new_side)["records"]!=26:
        raise RuntimeError("Unverified incoming canonical sidecar")
    timestamp=datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    old=parent/(".memory-precutover-"+timestamp)
    if old.exists():
        raise RuntimeError("Rollback path collision")
    # Rollback instruction persisted *before* first filesystem replacement.
    write_private(ROLLBACK,{"schema":"OptMem-cutover-rollback/v1",
        "previous_canonical":str(old),
        "new_sidecar":str(SIDECAR),"source_baseline_sha256":APPROVED})
    CANON.rename(old)
    incoming.rename(CANON)
    new_side.rename(SIDECAR)
    write_private(MARKER,{"schema":"OptMem-production-activation/v1",
                          "enabled":True,
                          "canonical_baseline_sha256":APPROVED,
                          "created_utc":datetime.now(timezone.utc).isoformat()})
    verified=w.verify(CANON,SIDECAR)
    if verified["memory_entries"]!=26:
        raise RuntimeError("Activation verification failed")
    return {"activation":"ENABLED","historical_records":26,
            "writer_verified":True,"rollback_available":True}


def rollback():
    stopped()
    if not ROLLBACK.is_file():
        raise RuntimeError("No cutover rollback instruction")
    doc=json.loads(ROLLBACK.read_text())
    if doc.get("schema")!="OptMem-cutover-rollback/v1":
        raise RuntimeError("Invalid rollback manifest")
    old=Path(doc["previous_canonical"])
    if not old.is_dir() or old.parent!=HOME/".optmem":
        raise RuntimeError("Original canonical directory is unavailable")
    if MARKER.exists():
        MARKER.unlink()
    quarantine=HOME/".optmem"/(".memory-rolled-back-"+uuid.uuid4().hex)
    if CANON.exists():
        CANON.rename(quarantine)
    old.rename(CANON)
    # Preserve the new (possibly written) OptMem store separately for audit.
    if SIDECAR.exists():
        SIDECAR.rename(STATE/(".sidecar-rolled-back-"+uuid.uuid4().hex))
    ROLLBACK.rename(STATE/(".rollback-finished-"+uuid.uuid4().hex+".json"))
    if (CANON/"LOG.txt").stat().st_size!=0:
        raise RuntimeError("Rolled-back canonical does not match original empty root")
    return {"rollback":"COMPLETE","original_canonical_restored":True}


def main():
    cli=argparse.ArgumentParser()
    cli.add_argument("action",choices=["install","rollback"])
    args=cli.parse_args()
    print(json.dumps(install() if args.action=="install" else rollback(),sort_keys=True))


if __name__=="__main__":
    main()

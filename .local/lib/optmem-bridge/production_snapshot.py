#!/usr/bin/env python3
"""Canonical OptMem local coherent snapshot and independent offline restore proof."""
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
import uuid

import agent_store as a
import production_operator as po
import snapshot_stage as snap

HOME=Path.home()
STATE=HOME/".local/state/optmem-production"
ROOT=STATE/"snapshots"
MEMORY=HOME/".optmem/memory"
SIDECAR=STATE/"sidecar"


def seal(snapshot: Path) -> dict:
    if not (STATE/"ACTIVE.json").is_file():
        raise RuntimeError("Production not activated")
    po.priv_dir(STATE)
    po.priv_dir(ROOT,create=True)
    if snapshot.parent!=ROOT or snapshot.exists() or snapshot.is_symlink():
        raise RuntimeError("Destination must be a new direct child under private snapshots")
    stage=ROOT/(".incoming-"+uuid.uuid4().hex)
    stage.mkdir(mode=0o700)
    files={}
    with a.agent_lock(SIDECAR):
        with a.writer.writer_lock(SIDECAR):
            before=snap._verified_state(MEMORY,SIDECAR)
            for src in snap._walk_files(MEMORY):
                rel=(Path("memory")/src.relative_to(MEMORY)).as_posix()
                files[rel]=snap._copy_secure(src,stage/rel)
            for name in ("provenance.sqlite3","agent-facts.sqlite3"):
                rel="sidecar/"+name
                (stage/"sidecar").mkdir(mode=0o700,exist_ok=True)
                (stage/rel).touch(mode=0o600)
                files[rel]=snap._backup_sqlite(SIDECAR/name,stage/rel)
            after=snap._verified_state(MEMORY,SIDECAR)
            if before!=after or before["records"]<26:
                raise RuntimeError("Inconsistent canonical writer snapshot")
            for src in snap._walk_files(MEMORY):
                rel=(Path("memory")/src.relative_to(MEMORY)).as_posix()
                if snap._digest(src)!=files[rel]["sha256"]:
                    raise RuntimeError("Canonical files changed during capture")
    manifest={"schema":"OptMem-production-snapshot/v1","state":before,
              "files":files,"captured_utc":datetime.now(timezone.utc).isoformat()}
    snap._write_json(stage/"manifest.json",manifest)
    for directory,_,_ in os.walk(stage):
        snap._sync_dir(Path(directory))
    stage.rename(snapshot)
    proof=restore_proof(snapshot)
    if proof["records"]!=before["records"]:
        raise RuntimeError("Independent restore did not reconstruct canonical state")
    return {"snapshot":str(snapshot),"records":before["records"],
            "keys":before["keys"],"requests":before["requests"],
            "snapshot_and_restore_verified":True}


def restore_proof(snapshot: Path) -> dict:
    po.priv_dir(snapshot)
    manifest=json.loads((snapshot/"manifest.json").read_text())
    if manifest.get("schema")!="OptMem-production-snapshot/v1":
        raise RuntimeError("Invalid canonical snapshot manifest")
    found={}
    for p in snap._walk_files(snapshot):
        rel=p.relative_to(snapshot).as_posix()
        if rel=="manifest.json":
            continue
        found[rel]={"size":p.stat().st_size,"sha256":snap._digest(p)}
    if found!=manifest["files"]:
        raise RuntimeError("Snapshot file manifest differs")
    with closing(sqlite3.connect(f"file:{snapshot/'sidecar/provenance.sqlite3'}?mode=ro&immutable=1",uri=True)) as c:
        if c.execute("PRAGMA integrity_check").fetchone()[0]!="ok":
            raise RuntimeError("Provenance backup corrupt")
    # Destination is private, fresh and not the live canonical root.
    destination=ROOT/(".restore-proof-"+uuid.uuid4().hex)
    destination.mkdir(mode=0o700)
    try:
        for rel in manifest["files"]:
            snap._copy_secure(snapshot/rel,destination/rel)
        result=snap._verified_state(destination/"memory",destination/"sidecar")
        if result!=manifest["state"]:
            raise RuntimeError("Offline restored log/keys/receipts differ")
        return {"records":result["records"],"keys":result["keys"],
                "requests":result["requests"],"restored":True}
    finally:
        shutil.rmtree(destination)


if __name__=="__main__":
    cli=argparse.ArgumentParser()
    cli.add_argument("action",choices=["seal","verify"])
    cli.add_argument("--snapshot",type=Path,required=True)
    args=cli.parse_args()
    print(json.dumps(seal(args.snapshot) if args.action=="seal"
          else restore_proof(args.snapshot),sort_keys=True))

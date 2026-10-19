#!/usr/bin/env python3
"""Fail-closed native ZeroClaw reconciliation into canonical OptMem.

Lossless SQLite archive is retained independently from short durable facts.
Only eligible, agent-authored core/daily facts are admitted by shared writer.
All raw rows and metadata remain recoverable with one verified private archive.
"""
from __future__ import annotations
from collections import Counter
from contextlib import closing
import hashlib
import json
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import uuid

import agent_store as a
import production_operator as po
import production_preflight as p

HOME=Path.home()
STATE=HOME/".local/state/optmem-production"
ARCHIVE=STATE/"legacy-retained"
MEMORY=HOME/".optmem/memory"
SIDECAR=STATE/"sidecar"


def sha(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def stopped() -> None:
    proc=subprocess.run(["systemctl","--user","is-active","--quiet","zeroclaw.service"],
                        check=False)
    if proc.returncode==0:
        raise RuntimeError("Legacy writer still active; stop the daemon before reconciling")


def reconcile() -> dict:
    stopped()
    if not (STATE/"ACTIVE.json").is_file():
        raise RuntimeError("Canonical writer not explicitly active")
    po.priv_dir(ARCHIVE)
    for filename in ("brain.sqlite3","manifest.json","migration-receipts.jsonl"):
        po.secure_file(ARCHIVE/filename)
    meta=json.loads((ARCHIVE/"manifest.json").read_text())
    db=ARCHIVE/"brain.sqlite3"
    if (meta.get("schema")!="OptMem-native-retained-archive/v1"
        or meta.get("native_db_sha256")!=po.digest(db)
        or meta.get("restoration_verified") is not True):
        raise RuntimeError("Legacy retained archive manifest invalid")
    with closing(sqlite3.connect(f"file:{db}?mode=ro&immutable=1",uri=True)) as con:
        if con.execute("PRAGMA integrity_check").fetchone()[0]!="ok":
            raise RuntimeError("Legacy retained SQLite is corrupt")
        con.row_factory=sqlite3.Row
        rows=con.execute("SELECT * FROM memories ORDER BY id").fetchall()
    if len(rows)!=meta["source_rows"]:
        raise RuntimeError("Legacy archive row inventory inconsistent")
    # Check against the live source before any irreversible native-setting change.
    with closing(sqlite3.connect(f"file:{HOME/'.zeroclaw/data/memory/brain.db'}?mode=ro",uri=True)) as con:
        con.row_factory=sqlite3.Row
        live=con.execute("SELECT * FROM memories ORDER BY id").fetchall()
    if list(map(tuple,live))!=list(map(tuple,rows)):
        raise RuntimeError("Legacy database changed after archive; refuse cutover")
    receipts=[]
    counts=Counter()
    for row in rows:
        source_id=sha(row["id"].encode())
        content=row["content"]
        if not isinstance(content,str):
            raise RuntimeError("Legacy source row not UTF-8 text")
        receipt={"native_id_sha256":source_id,
                 "source_content_sha256":sha(content.encode()),
                 "archive_sha256":meta["native_db_sha256"],
                 "outcome":"retained"}
        # Preserve conversation snapshots, oversized/multiline and sensitive
        # content in the archive; they are not durable atomic memory facts.
        if row["category"] in ("core","daily"):
            key="legacy.zeroclaw."+source_id[:40]
            rid=a.stable_request_id("zeroclaw",key,content)
            try:
                a.validate("zeroclaw",key,content,rid)
            except (ValueError,TypeError):
                counts["ineligible_private_or_long"]+=1
            else:
                outcome=a.request_store(MEMORY,SIDECAR,"zeroclaw",key,content,rid)
                receipt["outcome"]=("imported" if outcome["status"]=="STORED"
                                     else "deduplicated")
                receipt["canonical_index"]=outcome["index"]
        receipts.append(receipt)
        counts[receipt["outcome"]]+=1
    target=STATE/"migration-receipts.jsonl"
    pending=STATE/(".migration-receipts-"+uuid.uuid4().hex)
    fd=os.open(pending,os.O_CREAT|os.O_EXCL|os.O_WRONLY|os.O_NOFOLLOW,0o600)
    try:
        with os.fdopen(fd,"w") as f:
            for rec in receipts:
                f.write(json.dumps(rec,sort_keys=True,separators=(",",":"))+"\n")
            f.flush()
            os.fsync(f.fileno())
        os.replace(pending,target)
    finally:
        if pending.exists():
            pending.unlink()
    source_count,accounted=p._native_counts(HOME)
    if accounted!=len(rows) or source_count!=len(rows):
        raise RuntimeError("Migration receipts do not verify against both stores")
    return {"source_rows":len(rows),"accounted":accounted,"outcomes":dict(counts),
            "archive_verified":True,"canonical_records":a.writer.log_count(MEMORY)}


if __name__=="__main__":
    print(json.dumps(reconcile(),sort_keys=True))

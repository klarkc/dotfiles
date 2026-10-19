#!/usr/bin/env python3
"""Offline full rehearsal with 88 real legacy row shapes and zero live changes."""
from __future__ import annotations
from contextlib import closing
import json
import os
from pathlib import Path
import shutil
import sqlite3
import subprocess
import sys
import tempfile

LIVE=Path.home()
MODULE=Path(__file__).resolve().parent

def main():
    root=LIVE/".local/state/optmem-production"
    root.mkdir(mode=0o700,exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="optmem-rehearsal-",dir=root) as temp:
        fake=Path(temp)/"home"
        fake.mkdir(mode=0o700)
        def d(relative):
            path=fake/relative
            path.mkdir(mode=0o700,parents=True,exist_ok=True)
            return path
        def copy(src,relative):
            dest=fake/relative
            d(dest.relative_to(fake).parent)
            shutil.copy2(src,dest)
            dest.chmod(0o600)
        source=Path(".local/state/optmem-migration-2026-10-09/global-staging-20261014/"
                    "g2b-persistent-20261014/g4-authorized-local-20261015/optmem-test-memory")
        for name in ("LOG.txt","config","PROVENANCE.jsonl","CONSENT.json","BUILD-QA.json"):
            copy(LIVE/source/name,source/name)
        for file in (LIVE/source/"TREE").rglob("*"):
            relative=file.relative_to(LIVE)
            if file.is_dir():
                d(relative)
            else:
                copy(file,relative)
        copy(LIVE/".local/state/optmem-ingest-2026-10-09/events.jsonl",
             ".local/state/optmem-ingest-2026-10-09/events.jsonl")
        copy(LIVE/".local/state/optmem-prototype-2026-10-08/memo",
             ".local/state/optmem-prototype-2026-10-08/memo")
        d(".optmem/memory")
        (fake/".optmem/memory/LOG.txt").touch(mode=0o600)
        d(".local/state/optmem-production/precutover-recovery-test")
        (fake/".local/state/optmem-production/precutover-recovery-test/manifest.json").write_text("{}")
        # Live DB is read through SQLite's online backup, preserving WAL pages.
        archive=d(".zeroclaw/data/memory")/"brain.db"
        with closing(sqlite3.connect(f"file:{LIVE/'.zeroclaw/data/memory/brain.db'}?mode=ro",uri=True)) as origin:
            with closing(sqlite3.connect(archive)) as target:
                origin.backup(target)
        archive.chmod(0o600)
        child=r"""
import json
from pathlib import Path
import production_operator as po
import production_activate as act
import native_migrate as migration
import production_snapshot as snap
import production_preflight as gate
po.stage_prepare()
archive=po.ARCHIVE_ROOT/'legacy-retained'
po.native_archive(destination=archive,require_daemon_stopped=False)
act.stopped=lambda:None
migration.stopped=lambda:None
start=act.install()
summary=migration.reconcile()
sealed=snap.seal(snap.ROOT/'rehearsal-backup')
assert sealed['snapshot_and_restore_verified']
assert summary['accounted']==summary['source_rows']
assert gate._native_counts(Path.home())==(summary['source_rows'],summary['source_rows'])
# Same restored root, keys and upstream memory bytes, not only a checksum.
proof=snap.restore_proof(snap.ROOT/'rehearsal-backup')
assert proof['restored']
done=act.rollback()
assert done['original_canonical_restored']
assert (Path.home()/'.optmem/memory/LOG.txt').stat().st_size==0
print(json.dumps({'cutover':'PASS','native_accounted':summary['accounted'],
                  'outcomes':summary['outcomes'],'sealed_records':sealed['records'],
                  'restore':'PASS','rollback':'PASS'}))
"""
        env={**os.environ,"HOME":str(fake),"PYTHONDONTWRITEBYTECODE":"1"}
        run=subprocess.run([sys.executable,"-B","-c",child],env=env,
                 cwd=MODULE,capture_output=True,text=True,timeout=120)
        if run.returncode:
            # Omit native text, keys and credentials from failure output.
            print("REHEARSAL_FAILED",run.stderr[-1200:].replace(str(fake),"[TEMP]"))
            raise SystemExit(run.returncode)
        print(run.stdout.strip())

if __name__=="__main__":
    main()

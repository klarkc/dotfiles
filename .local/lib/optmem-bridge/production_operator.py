#!/usr/bin/env python3
"""Private OptMem pre-cutover preparation. No service/config changes or remote sync.

The existing 26-record approved stage is the *only* historical source. The
native ZeroClaw archive remains a separate, immutable recovery artifact, not
an automatically admitted memory index. Never promote smoke-test staging.
"""
from __future__ import annotations
import argparse
from contextlib import closing
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import shutil
import sqlite3
import stat
import uuid

import agent_mcp
import agent_store as a
import production_preflight as preflight

w = a.writer
HOME = Path.home()
SOURCE = (HOME / ".local/state/optmem-migration-2026-10-09/global-staging-20261014"
          / "g2b-persistent-20261014/g4-authorized-local-20261015/optmem-test-memory")
ARCHIVE_ROOT = HOME / ".local/state/optmem-production"
APPROVED_SHA = "78eafd77c8c0c4eb67fcd1468bd758500c090b0d979cc0b4eccd2f6e34a2f565"
DB_SOURCE = HOME / ".zeroclaw/data"
DB_RELATIVE = ("memory/brain.db", "sessions/sessions.db", "sessions/acp-sessions.db",
               "control_plane.db", "devices.db", "cron/jobs.db", "memory.db")


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def priv_dir(path: Path, *, create: bool = False) -> Path:
    if create:
        path.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = path.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise RuntimeError("Unsafe private directory")
    return path


def secure_file(path: Path) -> None:
    info = path.lstat()
    if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077 or info.st_nlink != 1:
        raise RuntimeError("Unsafe source regular file")


def copy_file(source: Path, target: Path) -> None:
    secure_file(source)
    priv_dir(target.parent, create=True)
    fd = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "wb") as dest, source.open("rb") as src:
        shutil.copyfileobj(src, dest)
        dest.flush()
        os.fsync(dest.fileno())
    if digest(source) != digest(target):
        raise RuntimeError("Copy mismatch")


def stage_prepare() -> dict:
    if not preflight._source_confirmed(HOME):
        raise RuntimeError("Historical source consent, journal or provenance changed")
    priv_dir(SOURCE)
    if digest(SOURCE / "LOG.txt") != APPROVED_SHA:
        raise RuntimeError("Approved 26-record LOG digest differs")
    stage_base = w.HERE / "synthetic-cases"
    priv_dir(stage_base, create=True)
    candidate = stage_base / ("g4k-agent-precutover-" + uuid.uuid4().hex[:16])
    candidate.mkdir(mode=0o700)
    memory = candidate / "memory"
    sidecar = candidate / "sidecar"
    priv_dir(memory, create=True)
    priv_dir(sidecar, create=True)
    for name in ("LOG.txt", "config"):
        copy_file(SOURCE / name, memory / name)
    for source in sorted((SOURCE / "TREE").rglob("*")):
        relative = source.relative_to(SOURCE)
        if source.is_symlink():
            raise RuntimeError("Unsafe TREE symlink")
        if source.is_dir():
            priv_dir(memory / relative, create=True)
        elif source.is_file():
            copy_file(source, memory / relative)
        else:
            raise RuntimeError("Unexpected TREE node")
    (memory / ".lock").touch(mode=0o600)
    result = w.enroll_historical_baseline(memory, sidecar, APPROVED_SHA, 26)
    with closing(a.agent_connection(sidecar)):
        pass
    agent_mcp.StageAgentMCP(memory, sidecar, "opencode")
    verify = w.verify(memory, sidecar)
    if verify["memory_entries"] != 26 or verify["committed_provenance"] != 0:
        raise RuntimeError("Unexpected candidate provenance")
    return {"candidate": str(candidate), "verified": result,
            "candidate_records": verify["memory_entries"]}


def sqlite_backup(source: Path, destination: Path) -> dict:
    if source.is_symlink() or not source.is_file():
        raise RuntimeError("Native source missing or symlinked")
    destination.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    if source.stat().st_size == 0:
        fd = os.open(destination, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        os.close(fd)
        return {"sha256": digest(destination), "bytes": 0}
    with closing(sqlite3.connect(f"file:{source}?mode=ro", uri=True)) as src:
        with closing(sqlite3.connect(destination)) as dst:
            src.backup(dst, pages=64, sleep=0.02)
        destination.chmod(0o600)
        with closing(sqlite3.connect(f"file:{destination}?mode=ro", uri=True)) as snapshot:
            if snapshot.execute("PRAGMA integrity_check").fetchone()[0] != "ok":
                raise RuntimeError("SQLite backup integrity check failed")
            if snapshot.execute("PRAGMA foreign_key_check").fetchall():
                raise RuntimeError("SQLite backup foreign key validation failed")
    return {"sha256": digest(destination), "bytes": destination.stat().st_size}


def native_checkpoint() -> dict:
    priv_dir(ARCHIVE_ROOT, create=True)
    path = ARCHIVE_ROOT / ("native-checkpoint-" + datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
                           + "-" + uuid.uuid4().hex[:6])
    path.mkdir(mode=0o700)
    restored = priv_dir(path / "restore-proof", create=True)
    files = {}
    for rel in DB_RELATIVE:
        source = DB_SOURCE / rel
        result = sqlite_backup(source, path / rel)
        proof = sqlite_backup(path / rel, restored / rel)
        if result != proof:
            raise RuntimeError("Independent restored native checkpoint differs")
        files[rel] = result
    with closing(sqlite3.connect(f"file:{path / 'memory/brain.db'}?mode=ro", uri=True)) as con:
        row_count = con.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
    manifest = {"schema": "OptMem-native-checkpoint/v1", "files": files,
                "records": row_count, "created_utc": datetime.now(timezone.utc).isoformat()}
    target = path / "manifest.json"
    target.write_text(json.dumps(manifest, sort_keys=True, separators=(",", ":")) + "\n")
    target.chmod(0o600)
    secure_file(target)
    return {"checkpoint": str(path), "restored_db_count": len(files),
            "native_memory_rows": row_count, "verified": True}


def native_archive(*, destination: Path, require_daemon_stopped: bool) -> dict:
    """Retain every original row/metadata without copying private text into Git.

    Conversation, oversized and sensitive entries must not be blindly coerced
    into short OptMem facts. All rows remain byte-for-byte recoverable.
    """
    if require_daemon_stopped:
        import subprocess
        active = subprocess.run(["systemctl", "--user", "is-active", "--quiet",
                                 "zeroclaw.service"], check=False)
        if active.returncode == 0:
            raise RuntimeError("ZeroClaw is still writing native memory; cannot freeze cutover")
    priv_dir(ARCHIVE_ROOT, create=True)
    if destination.exists() or destination.is_symlink():
        raise FileExistsError("Refusing to replace an existing legacy archive")
    work = ARCHIVE_ROOT / ("." + destination.name + ".preparing-" + uuid.uuid4().hex)
    work.mkdir(mode=0o700)
    backup_file = work / "brain.sqlite3"
    backup = sqlite_backup(DB_SOURCE / "memory/brain.db", backup_file)
    restored = work / "restored" / "brain.sqlite3"
    restore = sqlite_backup(backup_file, restored)
    if restore != backup:
        raise RuntimeError("Legacy SQLite restoration mismatch")
    source_uri = f"file:{DB_SOURCE / 'memory/brain.db'}?mode=ro"
    archive_uri = f"file:{backup_file}?mode=ro"
    with closing(sqlite3.connect(source_uri, uri=True)) as source:
        with closing(sqlite3.connect(archive_uri, uri=True)) as saved:
            columns = [r[1] for r in source.execute("PRAGMA table_info(memories)")]
            if not {"id", "content", "created_at", "updated_at", "superseded_by", "namespace", "pinned"} <= set(columns):
                raise RuntimeError("Unrecognized native memory schema")
            original = source.execute("SELECT * FROM memories ORDER BY id").fetchall()
            archived = saved.execute("SELECT * FROM memories ORDER BY id").fetchall()
            if not original or original != archived:
                raise RuntimeError("Native memory changed during backup; retry after quiescence")
            metadata = {
                "schema": "OptMem-native-retained-archive/v1",
                "source_rows": len(original),
                "native_db_sha256": backup["sha256"],
                "native_db_bytes": backup["bytes"],
                "fields": columns,
                "created_utc": datetime.now(timezone.utc).isoformat(),
                "restoration_verified": True,
            }
            receipts = []
            id_pos = columns.index("id")
            content_pos = columns.index("content")
            for row in original:
                native_id = row[id_pos]
                value = row[content_pos]
                if not isinstance(native_id, str) or not isinstance(value, str):
                    raise RuntimeError("Unexpected native row data type")
                receipts.append({
                    "outcome": "retained",
                    "native_id_sha256": hashlib.sha256(native_id.encode()).hexdigest(),
                    "source_content_sha256": hashlib.sha256(value.encode()).hexdigest(),
                    "archive_sha256": backup["sha256"],
                })
    def json_secure(path: Path, value) -> None:
        fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
        with os.fdopen(fd, "w") as file:
            if isinstance(value, list):
                for item in value:
                    file.write(json.dumps(item, sort_keys=True) + "\n")
            else:
                file.write(json.dumps(value, sort_keys=True) + "\n")
            file.flush()
            os.fsync(file.fileno())
    json_secure(work / "manifest.json", metadata)
    json_secure(work / "migration-receipts.jsonl", receipts)
    if destination.exists():
        raise RuntimeError("Legacy archive target appeared concurrently")
    work.rename(destination)
    return {"archive": str(destination), "rows": len(receipts),
            "sqlite_restored": True, "all_columns_compared": True}


def main() -> int:
    cli = argparse.ArgumentParser(description="Private, offline OptMem cutover preparation")
    cli.add_argument("operation", choices=("prepare-stage", "native-checkpoint", "native-archive"))
    cli.add_argument("--archive", type=Path, help="Fresh private destination for lossless retained legacy archive")
    args = cli.parse_args()
    if args.operation == "prepare-stage":
        result = stage_prepare()
    elif args.operation == "native-checkpoint":
        result = native_checkpoint()
    else:
        if not args.archive or args.archive.parent != ARCHIVE_ROOT:
            raise ValueError("Only a new direct child of private OptMem production state is permitted")
        result = native_archive(destination=args.archive, require_daemon_stopped=True)
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

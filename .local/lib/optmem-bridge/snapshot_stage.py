#!/usr/bin/env python3
"""Local, stage-only consistent snapshot of OptMem LOG and both SQLite ledgers.

Never snapshots native ZeroClaw or canonical ~/.optmem/memory, and never uploads.
Locks serialize all cooperating OptMem stage writers; live SQLite databases
are copied with sqlite3.backup(), not copied as uncheckpointed raw files.
"""
from __future__ import annotations
from contextlib import closing
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import sqlite3
import stat
import uuid

import agent_mcp
import agent_store as a

w = a.writer
ROOT = w.HERE / "synthetic-cases"
DB_NAMES = ("provenance.sqlite3", "agent-facts.sqlite3")
MAX_FILES = 512
MAX_BYTES = 64 * 1024 * 1024
SCHEMA = "OptMem-G4N-stage-snapshot/v1"


def _digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _private_root(path: Path, prefix: str) -> Path:
    if (not path.is_absolute() or path.is_symlink() or path.parent.is_symlink()
            or path.parent.resolve() != ROOT.resolve()
            or not path.name.startswith(prefix)
            or not re.fullmatch(r"[a-z0-9][a-z0-9._-]{8,110}", path.name)):
        raise ValueError("Only private synthetic staging roots permitted")
    if not ROOT.is_dir() or ROOT.stat().st_uid != os.getuid() or ROOT.stat().st_mode & 0o077:
        raise ValueError("Unsafe synthetic staging parent")
    return path


def _safe_regular(path: Path) -> None:
    info = path.lstat()
    if (not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid()
            or info.st_mode & 0o077 or info.st_nlink != 1):
        raise ValueError("Unsafe file type, ownership, link count or mode")


def _walk_files(root: Path) -> list[Path]:
    files = []
    total = 0
    for current, dirs, names in os.walk(root, followlinks=False):
        here = Path(current)
        info = here.lstat()
        if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
            raise ValueError("Unsafe stage directory")
        for name in dirs:
            p = here / name
            info = p.lstat()
            if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
                raise ValueError("Unsafe descendant directory")
        for name in names:
            path = here / name
            _safe_regular(path)
            if path.relative_to(root).as_posix() == ".lock":
                continue  # OptMem advisory lock is recreated, never restored as evidence.
            files.append(path)
            total += path.stat().st_size
            if len(files) > MAX_FILES or total > MAX_BYTES:
                raise ValueError("Snapshot fixture exceeds bounded local budget")
    return sorted(files)


def _open_readonly(db: Path, *, sealed: bool = False):
    _safe_regular(db)
    # Live SQLite must read WAL pages. A SEALED backup is fully checkpointed
    # by sqlite3.backup and must be opened immutable to avoid creating -shm
    # companion files that would invalidate its exact file manifest.
    uri = f"file:{db}?mode=ro" + ("&immutable=1" if sealed else "")
    connection = sqlite3.connect(uri, uri=True, timeout=15)
    connection.row_factory = sqlite3.Row
    if connection.execute("PRAGMA integrity_check").fetchone()[0] != "ok":
        connection.close()
        raise RuntimeError("SQLite integrity check failed")
    return connection


def _verified_state(memory: Path, sidecar: Path) -> dict:
    if not (memory / "LOG.txt").is_file() or not all((sidecar / n).is_file() for n in DB_NAMES):
        raise RuntimeError("Incomplete OptMem/sidecar pair")
    with closing(_open_readonly(sidecar / DB_NAMES[0])) as ledger:
        bindings = w.verify_bindings(memory, ledger)
        if ledger.execute("SELECT 1 FROM bindings WHERE state='PREPARED' LIMIT 1").fetchone():
            raise RuntimeError("Unresolved writer receipt: refuse snapshot")
        base = ledger.execute("SELECT initial_log_count FROM baseline WHERE id=1").fetchone()
        if base is None:
            raise RuntimeError("Missing authenticated historical baseline")
        baseline = base[0]
    with closing(_open_readonly(sidecar / DB_NAMES[1])) as con:
        if con.execute("SELECT 1 FROM agent_requests WHERE state!='COMMITTED' LIMIT 1").fetchone():
            raise RuntimeError("Unresolved agent receipt: refuse snapshot")
        keys = list(con.execute("SELECT normalized_sha256, memory_index FROM agent_keys"))
        requests = list(con.execute("SELECT normalized_sha256,memory_index FROM agent_requests"))
        for row in [*keys, *requests]:
            a.verified_index(memory, row["memory_index"], row["normalized_sha256"])
        key_count, request_count = len(keys), len(requests)
    return {"records": w.log_count(memory), "baseline": baseline, "bindings": bindings,
            "keys": key_count, "requests": request_count}


def _copy_secure(source: Path, target: Path) -> dict:
    _safe_regular(source)
    target.parent.mkdir(parents=True, mode=0o700, exist_ok=True)
    fd = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_CLOEXEC, 0o600)
    with source.open("rb") as inp, os.fdopen(fd, "wb") as out:
        out_fd = out.fileno()
        while chunk := inp.read(65536):
            out.write(chunk)
        out.flush()
        os.fsync(out_fd)
    return {"sha256": _digest(target), "size": target.stat().st_size}


def _backup_sqlite(source: Path, target: Path) -> dict:
    with closing(_open_readonly(source)) as read:
        with closing(sqlite3.connect(target)) as write:
            read.backup(write, pages=16, sleep=0.01)
            if write.execute("PRAGMA integrity_check").fetchone()[0] != "ok":
                raise RuntimeError("SQLite backup integrity failed")
    target.chmod(0o600)
    return {"sha256": _digest(target), "size": target.stat().st_size}


def _sync_dir(directory: Path) -> None:
    fd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def _create_private(path: Path) -> None:
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_CLOEXEC, 0o600)
    os.close(fd)


def _write_json(path: Path, data: dict) -> None:
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_CLOEXEC, 0o600)
    with os.fdopen(fd, "w", encoding="utf-8") as file:
        json.dump(data, file, sort_keys=True, separators=(",", ":"))
        file.write("\n")
        file.flush()
        os.fsync(file.fileno())


def create_snapshot(memory: Path, sidecar: Path, backup: Path) -> dict:
    """Make one atomic local stage snapshot, refusing pending or inconsistent state.

    Always acquire agent lock before writer lock (the request-store lock order).
    A backup consists of the entire staged upstream memory, not LOG alone,
    and independent SQLite online backups of both sidecar databases.
    """
    memory, sidecar = agent_mcp.StageAgentMCP.checked_stage(memory, sidecar)
    backup = _private_root(backup, "g4n-snapshot-")
    if backup.exists() or backup.is_symlink():
        raise FileExistsError("Never overwrite an existing OptMem snapshot")
    staging = backup.with_name("." + backup.name + ".incomplete-" + uuid.uuid4().hex[:10])
    staging.mkdir(mode=0o700)
    try:
        with a.agent_lock(sidecar):
            with w.writer_lock(sidecar):
                initial = _verified_state(memory, sidecar)
                paths = _walk_files(memory)
                if not any(p.name == "LOG.txt" and p.parent == memory for p in paths):
                    raise RuntimeError("Missing staged upstream LOG")
                blobs: dict[str, dict] = {}
                for file in paths:
                    rel = Path("memory") / file.relative_to(memory)
                    blobs[rel.as_posix()] = _copy_secure(file, staging / rel)
                (staging / "sidecar").mkdir(mode=0o700)
                for name in DB_NAMES:
                    source = sidecar / name
                    dest = staging / "sidecar" / name
                    _create_private(dest)  # SQLite never creates a public-mode file.
                    blobs[(Path("sidecar") / name).as_posix()] = _backup_sqlite(source, dest)
                if initial != _verified_state(memory, sidecar):
                    raise RuntimeError("OptMem state moved during stage snapshot")
                for file in paths:
                    rel = (Path("memory") / file.relative_to(memory)).as_posix()
                    if _digest(file) != blobs[rel]["sha256"]:
                        raise RuntimeError("Concurrent out-of-band memory edit detected")
                manifest = {"schema": SCHEMA,
                            "created_utc": datetime.now(timezone.utc).isoformat(),
                            "state": initial, "files": blobs,
                            "log_sha256": blobs["memory/LOG.txt"]["sha256"]}
                _write_json(staging / "manifest.json", manifest)
                for root, directories, _ in os.walk(staging):
                    _sync_dir(Path(root))
                _sync_dir(staging)
        if backup.exists() or backup.is_symlink():
            raise FileExistsError("Snapshot name became occupied")
        staging.rename(backup)
        _sync_dir(ROOT)
        verify_snapshot(backup)
        return {"records": initial["records"], "keys": initial["keys"],
                "requests": initial["requests"], "snapshot_manifest_sha256": _digest(backup / "manifest.json")}
    except Exception:
        if staging.exists():
            shutil.rmtree(staging)
        raise


def verify_snapshot(backup: Path) -> dict:
    """Verify private snapshot file inventory and digests without changing it.

    SHA-256 protects against corruption only. Authentication against a
    malicious rewriter requires a separately trusted secret/signature later.
    """
    backup = _private_root(backup, "g4n-snapshot-")
    if not backup.is_dir() or backup.stat().st_uid != os.getuid() or backup.stat().st_mode & 0o077:
        raise ValueError("Unsafe or missing snapshot root")
    manifest_path = backup / "manifest.json"
    _safe_regular(manifest_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if not isinstance(manifest, dict) or set(manifest) != {
            "schema", "created_utc", "state", "files", "log_sha256"} or manifest["schema"] != SCHEMA:
        raise RuntimeError("Snapshot manifest schema mismatch")
    files = manifest["files"]
    if not isinstance(files, dict) or not {"memory/LOG.txt", "sidecar/provenance.sqlite3",
            "sidecar/agent-facts.sqlite3"}.issubset(files):
        raise RuntimeError("Snapshot missing required records")
    found = set()
    for file in _walk_files(backup):
        rel = file.relative_to(backup).as_posix()
        if rel == "manifest.json":
            continue
        found.add(rel)
        if rel not in files or files[rel] != {"sha256": _digest(file), "size": file.stat().st_size}:
            raise RuntimeError("Snapshot file missing, changed or unlisted")
    if found != set(files) or len(found) > MAX_FILES:
        raise RuntimeError("Snapshot inventory mismatch")
    if manifest["log_sha256"] != files["memory/LOG.txt"]["sha256"]:
        raise RuntimeError("Snapshot log hash mismatch")
    for name in DB_NAMES:
        with closing(_open_readonly(backup / "sidecar" / name, sealed=True)):
            pass
    return manifest


def restore_snapshot(backup: Path, destination: Path) -> dict:
    """Validate offline backup, then atomically publish a new synthetic stage.

    Never overwrite a stage or production store. Integrity and alias binding
    checks occur BEFORE the destination becomes visible to MCP clients.
    """
    manifest = verify_snapshot(backup)
    destination = _private_root(destination, "g4k-agent-")
    if destination.exists() or destination.is_symlink():
        raise FileExistsError("Never overwrite an OptMem stage")
    staging = destination.with_name("." + destination.name + ".incomplete-" + uuid.uuid4().hex[:10])
    staging.mkdir(mode=0o700)
    try:
        for name in ("memory", "sidecar"):
            (staging / name).mkdir(mode=0o700)
        for rel in sorted(manifest["files"]):
            source = backup / rel
            copied = _copy_secure(source, staging / rel)
            if copied != manifest["files"][rel]:
                raise RuntimeError("Snapshot changed during restore copy")
        _create_private(staging / "memory" / ".lock")
        restored = _verified_state(staging / "memory", staging / "sidecar")
        if restored != manifest["state"]:
            raise RuntimeError("Restored ledger/key state differs from sealed snapshot")
        if _digest(staging / "memory" / "LOG.txt") != manifest["log_sha256"]:
            raise RuntimeError("Restored upstream log differs from backup")
        for root, _, _ in os.walk(staging):
            _sync_dir(Path(root))
        if destination.exists() or destination.is_symlink():
            raise FileExistsError("Destination stage name became occupied")
        staging.rename(destination)
        _sync_dir(ROOT)
        return {"records": restored["records"], "keys": restored["keys"],
                "requests": restored["requests"], "restored": True}
    except Exception:
        if staging.exists():
            shutil.rmtree(staging)
        raise


def main() -> int:
    import argparse
    parser = argparse.ArgumentParser(description="OptMem consistent LOCAL synthetic stage snapshots only")
    sub = parser.add_subparsers(dest="operation", required=True)
    create = sub.add_parser("create")
    create.add_argument("--memory", type=Path, required=True)
    create.add_argument("--sidecar", type=Path, required=True)
    create.add_argument("--backup", type=Path, required=True)
    check = sub.add_parser("verify")
    check.add_argument("--backup", type=Path, required=True)
    restore = sub.add_parser("restore")
    restore.add_argument("--backup", type=Path, required=True)
    restore.add_argument("--destination", type=Path, required=True)
    args = parser.parse_args()
    if args.operation == "create":
        result = create_snapshot(args.memory, args.sidecar, args.backup)
    elif args.operation == "verify":
        result = verify_snapshot(args.backup)["state"]
    else:
        result = restore_snapshot(args.backup, args.destination)
    # No memory text, keys or private paths printed to standard output.
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""G4N private, synthetic, stage-only hot snapshot/restore regressions."""
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import json
import os
import shutil
import sqlite3
import subprocess
import sys
import threading
import unittest
from unittest import mock
import uuid

import agent_mcp
import agent_store as a
import snapshot_stage as s

w = a.writer
ROOT = s.ROOT


class SnapshotStageTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        ROOT.mkdir(mode=0o700, exist_ok=True)

    def setUp(self):
        self.tag = uuid.uuid4().hex[:12]
        self.base = ROOT / ("g4k-agent-g4n-fixture-" + self.tag)
        self.base.mkdir(mode=0o700)
        self.memory = self.base / "memory"
        self.sidecar = self.base / "sidecar"
        self.memory.mkdir(mode=0o700)
        env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1", "MEMORY_DIR": str(self.memory)}
        result = subprocess.run([sys.executable, "-B", str(w.MEMO), "init"],
                                env=env, capture_output=True, text=True, timeout=16,
                                umask=0o077)
        self.assertEqual(result.returncode, 0, result.stderr)
        (self.memory / "LOG.txt").chmod(0o600)
        self.backup = ROOT / ("g4n-snapshot-fixture-" + self.tag)
        self.restored = ROOT / ("g4k-agent-restored-fixture-" + self.tag)
        self.store()

    def tearDown(self):
        for path in (self.base, self.backup, self.restored):
            if path.exists():
                shutil.rmtree(path)

    def store(self, key="test.worktrees", text="Prefer isolated Git worktrees for verified changes",
              request="request-00000100", harness="opencode", **kwargs):
        return a.request_store(self.memory, self.sidecar, harness, key, text, request, **kwargs)

    def test_roundtrip_preserves_aliases_receipts_and_tree(self):
        self.store(key="alias.git.worktrees", request="request-00000101", harness="zeroclaw")
        tree = self.memory / "TREE"
        tree.mkdir(mode=0o700, exist_ok=True)
        (tree / "fixture").write_text("synthetic supporting tree fixture\n")
        (tree / "fixture").chmod(0o600)
        first = s.create_snapshot(self.memory, self.sidecar, self.backup)
        self.assertEqual((first["records"], first["keys"], first["requests"]), (1, 2, 2))
        self.assertEqual(s.verify_snapshot(self.backup)["state"]["records"], 1)
        restored = s.restore_snapshot(self.backup, self.restored)
        self.assertEqual((restored["records"], restored["keys"], restored["requests"]), (1, 2, 2))
        self.assertEqual((self.restored / "memory/TREE/fixture").read_text(), "synthetic supporting tree fixture\n")
        self.assertEqual((self.restored / "memory/LOG.txt").read_bytes(),
                         (self.memory / "LOG.txt").read_bytes())
        self.assertEqual(a.lookup_key(self.restored / "memory", self.restored / "sidecar",
                                      "alias.git.worktrees")["index"], 0)
        replay = a.request_store(self.restored / "memory", self.restored / "sidecar",
                                 "zeroclaw", "alias.git.worktrees",
                                 "Prefer isolated Git worktrees for verified changes", "request-00000101")
        self.assertEqual(replay["status"], "ALREADY_PRESENT")
        self.assertEqual(w.log_count(self.restored / "memory"), 1)
        self.assertEqual(w.log_count(self.memory), 1)
        # A newly admitted fact on the copy never alters the original source.
        extra = a.request_store(self.restored / "memory", self.restored / "sidecar",
                                "opencode", "test.incremental",
                                "Verify durable snapshots before migrating agents", "request-00000102")
        self.assertEqual(extra["status"], "STORED")
        self.assertEqual(w.log_count(self.restored / "memory"), 2)
        self.assertEqual(w.log_count(self.memory), 1)

    def test_incomplete_backup_and_partial_restore_are_refused(self):
        s.create_snapshot(self.memory, self.sidecar, self.backup)
        (self.backup / "sidecar/agent-facts.sqlite3").unlink()
        with self.assertRaisesRegex(RuntimeError, "inventory mismatch"):
            s.restore_snapshot(self.backup, self.restored)
        self.assertFalse(self.restored.exists())
        self.assertEqual(w.log_count(self.memory), 1)

    def test_corrupt_log_and_sqlite_are_detected(self):
        s.create_snapshot(self.memory, self.sidecar, self.backup)
        for rel in ("memory/LOG.txt", "sidecar/provenance.sqlite3"):
            target = self.backup / rel
            original = target.read_bytes()
            with target.open("r+b") as file:
                file.seek(0)
                file.write(b"Q")
            with self.assertRaises((RuntimeError, ValueError, sqlite3.DatabaseError)):
                s.verify_snapshot(self.backup)
            target.write_bytes(original)
            target.chmod(0o600)
        self.assertEqual(s.verify_snapshot(self.backup)["state"]["records"], 1)

    def test_prepared_agent_or_writer_transactions_block_backup(self):
        with self.assertRaises(InterruptedError):
            self.store(key="test.pending.agent", request="request-00000200",
                       text="Pending requests must block incomplete backups",
                       failpoint="after_request_prepare")
        with self.assertRaisesRegex(RuntimeError, "Unresolved agent receipt"):
            s.create_snapshot(self.memory, self.sidecar, self.backup)
        self.assertFalse(self.backup.exists())

    def test_pending_upstream_writer_blocks_backup(self):
        with self.assertRaises(InterruptedError):
            self.store(key="test.pending.writer", request="request-00000201",
                       text="Upstream pending transactions must not be snapshotted",
                       failpoint="after_prepare")
        with self.assertRaisesRegex(RuntimeError, "Unresolved writer receipt"):
            s.create_snapshot(self.memory, self.sidecar, self.backup)
        self.assertFalse(self.backup.exists())

    def test_backup_mutation_during_restore_is_rejected_before_publish(self):
        s.create_snapshot(self.memory, self.sidecar, self.backup)
        original = s._copy_secure
        mutated = {"done": False}

        def copy_with_mutation(source, target):
            if source.name == "config" and not mutated["done"]:
                with source.open("ab") as output:
                    output.write(b"unexpected concurrent edit")
                mutated["done"] = True
            return original(source, target)

        with mock.patch.object(s, "_copy_secure", side_effect=copy_with_mutation):
            with self.assertRaisesRegex(RuntimeError, "changed during restore copy"):
                s.restore_snapshot(self.backup, self.restored)
        self.assertTrue(mutated["done"])
        self.assertFalse(self.restored.exists())
        self.assertEqual(w.log_count(self.memory), 1)

    def test_symlinks_and_existing_targets_are_refused(self):
        (self.memory / "insecure-link").symlink_to(self.memory / "LOG.txt")
        with self.assertRaises(ValueError):
            s.create_snapshot(self.memory, self.sidecar, self.backup)
        (self.memory / "insecure-link").unlink()
        s.create_snapshot(self.memory, self.sidecar, self.backup)
        with self.assertRaises(FileExistsError):
            s.create_snapshot(self.memory, self.sidecar, self.backup)
        s.restore_snapshot(self.backup, self.restored)
        with self.assertRaises(FileExistsError):
            s.restore_snapshot(self.backup, self.restored)
        with self.assertRaises(ValueError):
            s.restore_snapshot(self.backup, Path.home() / ".optmem/memory")

    def test_concurrent_writer_cannot_interleave_snapshot_databases(self):
        entered = threading.Event()
        release = threading.Event()
        original = s._backup_sqlite
        counters = {"calls": 0}

        def slow_backup(*args, **kwargs):
            if counters["calls"] == 0:
                entered.set()
                if not release.wait(timeout=8):
                    raise RuntimeError("Concurrent test did not release backup gate")
            counters["calls"] += 1
            return original(*args, **kwargs)

        with mock.patch.object(s, "_backup_sqlite", side_effect=slow_backup):
            with ThreadPoolExecutor(max_workers=2) as executor:
                snap = executor.submit(s.create_snapshot, self.memory, self.sidecar, self.backup)
                self.assertTrue(entered.wait(timeout=8), "Snapshot never reached SQLite copy")
                later = executor.submit(self.store, key="test.after.snapshot",
                                        request="request-00000222",
                                        text="The later fact must never appear in an earlier snapshot")
                self.assertFalse(later.done(), "Writer bypassed shared OptMem snapshot lock")
                release.set()
                self.assertEqual(snap.result(timeout=15)["records"], 1)
                self.assertEqual(later.result(timeout=15)["status"], "STORED")
        self.assertEqual(w.log_count(self.memory), 2)
        self.assertEqual(s.restore_snapshot(self.backup, self.restored)["records"], 1)
        self.assertEqual(a.lookup_key(self.restored / "memory", self.restored / "sidecar",
                                      "test.after.snapshot")["status"], "NOT_FOUND")

    def test_online_sqlite_backup_captures_uncheckpointed_wal(self):
        db = self.sidecar / "agent-facts.sqlite3"
        with sqlite3.connect(db, timeout=10) as source:
            mode = source.execute("PRAGMA journal_mode=WAL").fetchone()[0]
            self.assertEqual(mode, "wal")
            source.execute("PRAGMA wal_autocheckpoint=0")
            source.execute("PRAGMA user_version=13")
            source.commit()
            s.create_snapshot(self.memory, self.sidecar, self.backup)
        s.restore_snapshot(self.backup, self.restored)
        with sqlite3.connect(self.restored / "sidecar/agent-facts.sqlite3") as restored:
            self.assertEqual(restored.execute("PRAGMA user_version").fetchone()[0], 13)
            self.assertEqual(restored.execute("PRAGMA integrity_check").fetchone()[0], "ok")

    def test_memory_only_restore_cannot_be_accepted_as_valid_pair(self):
        s.create_snapshot(self.memory, self.sidecar, self.backup)
        s.restore_snapshot(self.backup, self.restored)
        (self.restored / "sidecar/agent-facts.sqlite3").unlink()
        with self.assertRaises(ValueError):
            agent_mcp.StageAgentMCP(self.restored / "memory", self.restored / "sidecar", "opencode")
        self.assertEqual(w.log_count(self.memory), 1)


if __name__ == "__main__":
    unittest.main(verbosity=2)

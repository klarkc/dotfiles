#!/usr/bin/env python3
"""Offline Git origin, recovery, conflict and privacy regressions for OptMem."""
from __future__ import annotations
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

import git_sync as g

HOST = Path.home()
TEST_PARENT = HOST / ".local/state/optmem-production"


class GitMemoryTests(unittest.TestCase):
    def setUp(self):
        if not (TEST_PARENT / "ACTIVE.json").is_file():
            self.skipTest("Live OptMem not active on this host; private Git fixtures unavailable")
        self.old_umask = os.umask(0o077)
        self.temp = tempfile.TemporaryDirectory(prefix="git-sync-e2e-", dir=TEST_PARENT)
        self.addCleanup(self.temp.cleanup)
        self.addCleanup(os.umask, self.old_umask)
        self.root = Path(self.temp.name)
        self.remote = self.root / "remote.git"
        self.cmd("init", "--bare", "-b", "main", str(self.remote), root=True)
        self.store = g.GitMemory(home=HOST, remote=str(self.remote),
                                 verify_private=lambda: None)
        self.store.state = self.root / "state"
        self.store.repo = self.store.state / "repo"
        self.store.recovery = self.store.state / "recovery"
        self.store.scan_private_secrets = lambda root: None
        self.live_log = HOST / ".optmem/memory/LOG.txt"
        self.original_digest = g.digest(self.live_log)
        self.addCleanup(self.verify_live_unchanged)

    def cmd(self, *args, root=False, cwd=None):
        command = ["/usr/bin/git", *args] if root else [
            "/usr/bin/git", "-C", str(self.store.repo), *args]
        result = subprocess.run(command, cwd=cwd,
                                env=g.github_environment(HOST, str(self.remote)),
                                capture_output=True, text=True,
                                timeout=30, check=False)
        if result.returncode:
            raise AssertionError("Local test Git command failed")
        return result.stdout.strip()

    def verify_live_unchanged(self):
        self.assertEqual(g.digest(self.live_log), self.original_digest)
        git = subprocess.run(["/usr/bin/git", "--git-dir="+str(HOST/".git"),
                              "config", "--get", "core.bare"],
                             env=g.github_environment(HOST, str(self.remote)),
                             capture_output=True, text=True)
        self.assertEqual(git.stdout.strip(), "false",
                         "Nested Git test accidentally mutated dotfiles core.bare")

    def test_bootstrap_push_restore_and_unchanged_dedup(self):
        first = self.store.sync()
        self.assertEqual(first["status"], "PUSHED_AND_RESTORED")
        self.assertEqual(first["native_rows"], 88)
        self.assertGreaterEqual(first["records"], 43)
        self.assertEqual(len(self.store.local_generations()), 1)
        again = self.store.sync()
        self.assertEqual(again["status"], "UNCHANGED")
        self.assertEqual(self.cmd("rev-list", "--count", "HEAD"), "1")
        generation = first["generation"]
        result = self.store.restore(generation)
        self.assertEqual(result["status"], "RECOVERED_ISOLATED")
        recovered = Path(result["destination"])
        self.assertEqual(g.digest(recovered / "canonical/memory/LOG.txt"),
                         self.original_digest)
        self.assertTrue(self.store.verify_generation(recovered)["restored"])
        with self.assertRaises(FileExistsError):
            self.store.restore(generation)
        self.assertEqual(g.digest(self.live_log), self.original_digest)

    def test_corrupt_legacy_file_is_rejected(self):
        first = self.store.sync()
        origin = self.store.repo / "snapshots" / first["generation"]
        p = origin / "legacy/brain.sqlite3"
        with p.open("r+b") as stream:
            stream.seek(32)
            stream.write(b"X")
        with self.assertRaises(RuntimeError):
            self.store.verify_generation(origin)
        with self.assertRaises(RuntimeError):
            self.store.sync()

    def test_path_traversal_and_unlisted_files_rejected(self):
        first = self.store.sync()
        directory = self.store.repo / "snapshots" / first["generation"]
        (directory / "injected.txt").write_text("unlisted")
        (directory / "injected.txt").chmod(0o600)
        with self.assertRaises(RuntimeError):
            self.store.verify_generation(directory)
        for value in ("../secret", "/etc/passwd", "a//b", "a/./b", ".git/config"):
            with self.subTest(value=value), self.assertRaises(RuntimeError):
                g.safe_relative(value)

    def test_never_resolve_conflicting_git_content_by_force(self):
        self.store.sync()
        other = self.root / "other"
        self.cmd("clone", str(self.remote), str(other), root=True)
        local_readme = self.store.repo / "README.md"
        remote_readme = other / "README.md"
        local_readme.write_text("Host-A local edit\n")
        self.cmd("add", "README.md")
        self.cmd("-c", "user.name=Test", "-c", "user.email=test@localhost",
                 "commit", "--quiet", "-m", "local edit")
        remote_readme.write_text("Host-B conflicting edit\n")
        self.cmd("-C", str(other), "add", "README.md", root=True)
        self.cmd("-C", str(other), "-c", "user.name=Test",
                 "-c", "user.email=test@localhost",
                 "commit", "--quiet", "-m", "concurrent change", root=True)
        self.cmd("-C", str(other), "push", "origin", "HEAD:main", root=True)
        with self.assertRaisesRegex(RuntimeError, "conflicted"):
            self.store.update_git()
        self.assertEqual(local_readme.read_text(), "Host-A local edit\n")
        self.assertEqual(self.cmd("status", "--porcelain"), "")

    def test_nested_git_hook_environment_cannot_select_dotfiles_git(self):
        from unittest.mock import patch
        with patch.dict(os.environ, {"GIT_DIR": "/tmp/foreign-parent",
                                     "GIT_WORK_TREE": "/tmp/foreign-parent",
                                     "GIT_INDEX_FILE": "/tmp/foreign-index",
                                     "GIT_CONFIG_COUNT": "1",
                                     "GIT_CONFIG_KEY_0": "remote.origin.url",
                                     "GIT_CONFIG_VALUE_0": "/tmp/foreign-repo"}):
            env = g.github_environment(HOST, str(self.remote))
            self.assertFalse(any(name.startswith("GIT_") and name not in (
                "GIT_TERMINAL_PROMPT", "GIT_ASKPASS", "GIT_CONFIG_NOSYSTEM")
                for name in env))
            self.store.env = env
            result = self.store.sync()
            self.assertEqual(result["status"], "PUSHED_AND_RESTORED")

    def test_remote_url_change_is_denied(self):
        self.store.sync()
        self.cmd("remote", "set-url", "origin", str(self.root / "unapproved.git"))
        with self.assertRaisesRegex(RuntimeError, "remote URL changed"):
            self.store.update_git()

    def test_private_visibility_guard_is_fail_closed(self):
        self.store.authorize = lambda: (_ for _ in ()).throw(
            RuntimeError("Remote is no longer private"))
        with self.assertRaisesRegex(RuntimeError, "no longer private"):
            self.store.update_git()
        self.assertFalse(self.store.repo.exists())

    def test_symlink_and_world_readable_content_denied(self):
        self.store.sync()
        root = self.store.repo / "snapshots" / self.store.local_generations()[0]
        file = root / "legacy/manifest.json"
        file.chmod(0o644)
        with self.assertRaises((RuntimeError, ValueError)):
            self.store.verify_generation(root)
        file.chmod(0o600)
        bad = root / "legacy/manifest-link"
        bad.symlink_to(file)
        with self.assertRaises((RuntimeError, ValueError)):
            self.store.verify_generation(root)


if __name__ == "__main__":
    unittest.main(verbosity=2)

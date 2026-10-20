#!/usr/bin/env python3
"""Offline real Git history tests for the single canonical memory tree."""
from __future__ import annotations
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
import uuid
from unittest.mock import patch

import git_sync as g

HOST=Path.home()
TEST_PARENT=HOST/".local/state/optmem-production"


class GitMemoryTests(unittest.TestCase):
    def setUp(self):
        if not (TEST_PARENT/"ACTIVE.json").is_file():
            self.skipTest("OptMem inactive; live immutable fixture unavailable")
        self.old_umask=os.umask(0o077)
        self.temp=tempfile.TemporaryDirectory(prefix="git-flat-e2e-",dir=TEST_PARENT)
        self.addCleanup(self.temp.cleanup)
        self.addCleanup(os.umask,self.old_umask)
        self.root=Path(self.temp.name)
        self.remote=self.root/"remote.git"
        self.cmd("init","--bare","-b","main",str(self.remote),root=True)
        self.store=g.GitMemory(home=HOST,remote=str(self.remote),verify_private=lambda:None)
        self.store.state=self.root/"state"
        self.store.repo=self.store.state/"repo"
        self.store.recovery=self.store.state/"recovery"
        self.store.scan_private_secrets=lambda root:None
        self.live=HOST/".optmem/memory/LOG.txt"
        self.live_hash=g.digest(self.live)
        self.addCleanup(self.check_live)

    def cmd(self,*args,root=False):
        command=["/usr/bin/git",*args] if root else [
            "/usr/bin/git","-C",str(self.store.repo),*args]
        result=subprocess.run(command,env=g.github_environment(HOST,str(self.remote)),
                              capture_output=True,text=True,timeout=30)
        if result.returncode:
            raise AssertionError("Local Git fixture command failed")
        return result.stdout.strip()

    def check_live(self):
        self.assertEqual(g.digest(self.live),self.live_hash)
        result=subprocess.run(["/usr/bin/git","--git-dir="+str(HOST/".git"),
                               "config","--get","core.bare"],
                              env=g.github_environment(HOST,str(self.remote)),
                              capture_output=True,text=True)
        self.assertEqual(result.stdout.strip(),"false")

    def test_one_tree_git_versions_and_no_redundant_commits(self):
        first=self.store.sync()
        self.assertEqual(first["status"],"PUSHED_AND_RESTORED")
        self.assertEqual(first["native_rows"],88)
        self.assertGreaterEqual(first["records"],43)
        self.assertFalse((self.store.repo/"snapshots").exists())
        self.assertEqual(self.cmd("rev-list","--count","HEAD"),"1")
        self.assertTrue((self.store.repo/"canonical/memory/LOG.txt").is_file())
        self.assertEqual(self.store.verify_tree(self.store.repo)["status"],"VERIFIED")
        second=self.store.sync()
        self.assertEqual(second["status"],"UNCHANGED")
        self.assertEqual(self.cmd("rev-list","--count","HEAD"),"1")
        prior=self.cmd("rev-parse","--short=12","HEAD")
        restored=self.store.restore(prior)
        self.assertEqual(restored["status"],"RECOVERED_ISOLATED")
        path=Path(restored["destination"])
        self.assertEqual(g.digest(path/"canonical/memory/LOG.txt"),self.live_hash)
        with self.assertRaises(FileExistsError):
            self.store.restore(prior)

    def test_modified_single_file_uses_second_commit_and_old_commit_restores(self):
        self.store.sync()
        before=self.cmd("rev-parse","--short=12","HEAD")
        original=self.store.ancillary
        alternate=self.root/"activation-v2.json"
        alternate.write_text('{"schema":"synthetic-activation-test","value":2}\n')
        alternate.chmod(0o600)
        def replacement():
            files=original()
            files["metadata/activation.json"]=alternate
            return files
        def fake_verified():
            return {target:{"sha256":g.digest(source),"size":source.stat().st_size}
                    for target,source in replacement().items()}
        with patch.object(self.store,"ancillary",replacement),patch.object(
                self.store,"verified_sources",fake_verified):
            newer=self.store.sync()
        self.assertEqual(newer["status"],"PUSHED_AND_RESTORED")
        self.assertEqual(self.cmd("rev-list","--count","HEAD"),"2")
        self.assertFalse((self.store.repo/"snapshots").exists())
        self.assertEqual(self.store.restore(before)["records"],newer["records"])
        self.assertEqual(self.store.verify_tree(self.store.repo)["status"],"VERIFIED")
        self.assertNotEqual(self.cmd("rev-parse",before),self.cmd("rev-parse","HEAD"))

    def test_existing_legacy_snapshot_history_migrates_without_rewrite(self):
        self.store.update_git()
        old_dir=self.store.repo/"snapshots"/("solo098-20261020T123502Z-52036f55b8f6")
        g.private_dir(old_dir/"canonical",create=True)
        tmp=TEST_PARENT/"snapshots"/("git-old-format-test-"+uuid.uuid4().hex[:12])
        try:
            g.prod.seal(tmp)
            files={}
            for source in g.snap._walk_files(tmp):
                rel="canonical/"+source.relative_to(tmp).as_posix()
                files[rel]=g.snap._copy_secure(source,old_dir/rel)
            for rel,source in self.store.ancillary().items():
                files[rel]=g.snap._copy_secure(source,old_dir/rel)
            meta=json.loads((tmp/"manifest.json").read_text())
            aux={x:files[x] for x in g.AUX}
            fp=hashlib.sha256(g.canonical_json(
                {"canonical":meta["files"],"ancillary":aux})).hexdigest()
            bundle={"schema":g.LEGACY_SCHEMA,"generation":old_dir.name,
                    "host":"solo098","created_utc":"2026-10-20T12:35:02Z",
                    "fingerprint":fp,"files":files,
                    "canonical_state":meta["state"],"native_rows":88}
            g.write_once(old_dir/"bundle.json",g.canonical_json(bundle)+b"\n")
            g.write_once(self.store.repo/"README.md",b"Legacy snapshots layout\n")
            self.store.verify_generation(old_dir)
            self.cmd("add","-A")
            self.cmd("-c","user.name=Test","-c","user.email=test@localhost",
                     "commit","--quiet","-m","old layout")
            oldsha=self.cmd("rev-parse","--short=12","HEAD")
            self.cmd("push","origin","HEAD:main")
            migrated=self.store.sync()
            self.assertEqual(migrated["status"],"PUSHED_AND_RESTORED")
            self.assertEqual(self.cmd("rev-list","--count","HEAD"),"2")
            self.assertFalse((self.store.repo/"snapshots").exists())
            self.assertEqual(self.store.verify_tree(self.store.repo)["status"],"VERIFIED")
            previous=self.store.restore(oldsha)
            self.assertEqual(previous["native_rows"],88)
            self.assertTrue((Path(previous["destination"])/"snapshots"/old_dir.name/"bundle.json").is_file())
        finally:
            if tmp.exists():
                shutil.rmtree(tmp)

    def test_corruption_missing_file_and_unexpected_object_fail(self):
        self.store.sync()
        folder=self.store.repo
        target=folder/"legacy/brain.sqlite3"
        with target.open("r+b") as file:
            file.seek(32)
            file.write(b"X")
        with self.assertRaises(RuntimeError):
            self.store.verify_tree(folder)
        with self.assertRaises(RuntimeError):
            self.store.sync()

    def test_unexpected_files_or_traversal_denied(self):
        self.store.sync()
        folder=self.store.repo
        (folder/"unauthorized").write_text("bad")
        (folder/"unauthorized").chmod(0o600)
        with self.assertRaisesRegex(RuntimeError,"top-level"):
            self.store.verify_tree(folder)
        for value in ("../secret","/etc/passwd","a//b","a/./b",".git/config"):
            with self.subTest(value=value),self.assertRaises(RuntimeError):
                g.safe_relative(value)

    def test_concurrent_histories_fail_closed_without_force(self):
        self.store.sync()
        other=self.root/"other"
        self.cmd("clone",str(self.remote),str(other),root=True)
        local=self.store.repo/"README.md"
        local.write_text("Host A local update\n")
        self.cmd("add","README.md")
        self.cmd("-c","user.name=Test","-c","user.email=test@localhost",
                 "commit","--quiet","-m","Host A")
        theirs=other/"README.md"
        theirs.write_text("Host B other update\n")
        self.cmd("-C",str(other),"add","README.md",root=True)
        self.cmd("-C",str(other),"-c","user.name=Test",
                 "-c","user.email=test@localhost",
                 "commit","--quiet","-m","Host B",root=True)
        self.cmd("-C",str(other),"push","origin","HEAD:main",root=True)
        with self.assertRaisesRegex(RuntimeError,"conflicted"):
            self.store.update_git()
        self.assertEqual(local.read_text(),"Host A local update\n")
        self.assertEqual(self.cmd("status","--porcelain"),"")

    def test_unapproved_remote_and_private_check_fail(self):
        self.store.sync()
        self.cmd("remote","set-url","origin",str(self.root/"bad.git"))
        with self.assertRaisesRegex(RuntimeError,"remote URL changed"):
            self.store.update_git()
        fresh=g.GitMemory(home=HOST,remote=str(self.remote),
                          verify_private=lambda:(_ for _ in ()).throw(
                              RuntimeError("Remote no longer private")))
        fresh.state=self.root/"denied-state"
        fresh.repo=fresh.state/"repo"
        with self.assertRaisesRegex(RuntimeError,"private"):
            fresh.update_git()
        self.assertFalse(fresh.repo.exists())

    def test_symlink_world_readable_and_git_env_are_denied(self):
        self.store.sync()
        target=self.store.repo/"legacy/manifest.json"
        target.chmod(0o644)
        with self.assertRaises((RuntimeError,ValueError)):
            self.store.verify_tree(self.store.repo)
        target.chmod(0o600)
        (self.store.repo/"legacy/injected-link").symlink_to(target)
        with self.assertRaises((RuntimeError,ValueError)):
            self.store.verify_tree(self.store.repo)
        env=g.github_environment(HOST,str(self.remote))
        for bad in ("GIT_DIR","GIT_WORK_TREE","GIT_INDEX_FILE","GIT_CONFIG_COUNT"):
            self.assertNotIn(bad,env)
        with patch.dict(os.environ,{"GIT_DIR":"/tmp/foreign",
                                    "GIT_INDEX_FILE":"/tmp/foreign_index"}):
            env=g.github_environment(HOST,str(self.remote))
            self.assertNotIn("GIT_DIR",env)
            self.assertNotIn("GIT_INDEX_FILE",env)


if __name__=="__main__":
    unittest.main(verbosity=2)

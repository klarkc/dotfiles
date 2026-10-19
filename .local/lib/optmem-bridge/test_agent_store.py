#!/usr/bin/env python3
"""Real upstream OptMem append with one writer authority; isolated fixtures."""
from __future__ import annotations
import concurrent.futures
from datetime import date
from pathlib import Path
import json
import os
import shutil
import sqlite3
import subprocess
import sys
import threading
import unittest
import uuid

import agent_store as a
import optmem_writer_isolated as w

SUITE_ROOT=w.HERE/"synthetic-cases"


class AgentStoreTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        SUITE_ROOT.mkdir(mode=0o700,exist_ok=True)

    def setUp(self):
        self.base=SUITE_ROOT/("g4k-agent-store-"+uuid.uuid4().hex[:12])
        self.base.mkdir(mode=0o700)
        self.memory=self.base/"memory"
        self.sidecar=self.base/"sidecar"
        self.memory.mkdir(mode=0o700)
        env={**os.environ,"MEMORY_DIR":str(self.memory),"PYTHONDONTWRITEBYTECODE":"1"}
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"init"],
                         capture_output=True,text=True,env=env,timeout=15)
        self.assertEqual(p.returncode,0,p.stderr)
        # Keep the synthetic initial LOG private regardless of shell umask.
        (self.memory/"LOG.txt").chmod(0o600)

    def tearDown(self):
        shutil.rmtree(self.base)

    def count(self):
        return w.log_count(self.memory)

    def store(self,harness="opencode",key="tools.git.policy",text="Git worktrees are required for changes",
              rid="request-00000001",failpoint=None):
        return a.request_store(self.memory,self.sidecar,harness,key,text,rid,failpoint=failpoint)

    def test_search_shared_agent_key_without_mutating_log(self):
        self.store(key="tools.git.policy",text="Git worktrees are required for changes")
        before=self.count()
        result=a.search_memories(self.memory,self.sidecar,"GIT worktree")
        self.assertEqual(result["status"],"FOUND")
        self.assertEqual(result["results"][0]["memory_key"],"tools.git.policy")
        self.assertEqual(result["results"][0]["index"],0)
        self.assertFalse(result["truncated"])
        self.assertEqual(a.search_memories(self.memory,self.sidecar,"no such fact")["status"],"NOT_FOUND")
        self.assertEqual(self.count(),before)

    def test_search_alias_group_only_once(self):
        self.store()
        self.store(harness="zeroclaw",key="team.worktrees",rid="request-00000002")
        found=a.search_memories(self.memory,self.sidecar,"worktree")
        self.assertEqual(len(found["results"]),1)
        self.assertEqual(found["results"][0]["alias_count"],2)
        self.assertEqual(set(found["results"][0]["aliases"]),{"tools.git.policy","team.worktrees"})
        self.assertEqual(self.count(),1)

    def test_search_unkeyed_upstream_baseline(self):
        env={**os.environ,"MEMORY_DIR":str(self.memory),"PYTHONDONTWRITEBYTECODE":"1"}
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"note",
                 "Semantic context belongs to the canonical memory"],env=env,text=True,
                 capture_output=True,timeout=15)
        self.assertEqual(p.returncode,0,p.stderr)
        content=(self.memory/"LOG.txt").read_bytes()
        w.enroll_historical_baseline(self.memory,self.sidecar,w.checksum(content),1)
        found=a.search_memories(self.memory,self.sidecar,"canonical memory")
        self.assertEqual(found["status"],"FOUND")
        self.assertIsNone(found["results"][0]["memory_key"])
        self.assertEqual(found["results"][0]["index"],0)
        self.assertEqual(self.count(),1)

    def test_search_rejects_unbounded_and_control_queries(self):
        for query in ("*", ".*", "../", "hi\nthere", "x"*65, " "):
            with self.subTest(query=query):
                with self.assertRaises(ValueError):
                    a.search_memories(self.memory,self.sidecar,query)
        self.assertEqual(self.count(),0)

    def test_search_is_bounded_and_reports_truncation(self):
        for i in range(10):
            result=self.store(key=f"tools.shared.{i:02d}",
                      text=f"Shared worktree rule for case {i:02d} is durable",
                      rid=f"request-{i + 100:08d}")
            self.assertEqual(result["status"],"STORED")
        found=a.search_memories(self.memory,self.sidecar,"worktree rule")
        self.assertEqual(found["status"],"FOUND")
        self.assertEqual(len(found["results"]),a.MAX_SEARCH_RESULTS)
        self.assertTrue(found["truncated"])
        self.assertEqual(self.count(),10)

    def test_search_refuses_out_of_band_corruption(self):
        self.store()
        with (self.memory/"LOG.txt").open("r+b") as file:
            file.seek(8)
            file.write(b"!")
        with self.assertRaises(RuntimeError):
            a.search_memories(self.memory,self.sidecar,"worktree")

    def test_lookup_shared_key_and_missing_key_without_appending(self):
        self.assertEqual(a.lookup_key(self.memory,self.sidecar,"tools.git.policy")["status"],"NOT_FOUND")
        self.assertEqual(self.count(),0)
        self.store()
        found=a.lookup_key(self.memory,self.sidecar,"tools.git.policy")
        self.assertEqual((found["status"],found["index"]),("FOUND",0))
        self.assertEqual(found["text"],"Git worktrees are required for changes")
        with self.assertRaises(ValueError):
            a.lookup_key(self.memory,self.sidecar,"../other")
        self.assertEqual(self.count(),1)

    def test_one_normalized_log_entry_across_harnesses_and_keys(self):
        first=self.store()
        self.assertEqual(first,{"status":"STORED","index":0})
        self.assertEqual(self.store("zeroclaw",rid="request-00000002")["status"],"ALREADY_PRESENT")
        self.assertEqual(self.store("zeroclaw",key="team.git.worktree",rid="request-00000003")["index"],0)
        self.assertEqual(self.store("opencode",key="tools.git.alt",text="  git   WORKTREES are required for changes.  ",rid="request-00000004")["index"],0)
        self.assertEqual(self.count(),1)
        self.assertEqual(w.verify(self.memory,self.sidecar)["committed_provenance"],1)
        con=sqlite3.connect(self.sidecar/"agent-facts.sqlite3")
        self.assertEqual(con.execute("SELECT count(DISTINCT memory_index) FROM agent_keys").fetchone()[0],1)
        self.assertEqual(con.execute("SELECT count(*) FROM agent_requests").fetchone()[0],4)
        con.close()

    def test_idempotent_replay_and_conflicting_id(self):
        self.store()
        same=self.store()
        self.assertEqual(same["status"],"ALREADY_PRESENT")
        with self.assertRaisesRegex(a.Conflict,"Idempotency"):
            self.store(key="other.key",text="Distinct permanent retention behavior")
        self.assertEqual(self.count(),1)

    def test_same_key_new_content_cannot_duplicate(self):
        self.store()
        with self.assertRaisesRegex(a.Conflict,"Logical"):
            self.store("zeroclaw",text="Completely different retained memory",rid="request-00000002")
        self.assertEqual(self.count(),1)
        # A conflicting attempt must not poison a valid key's future receipts.
        retried=self.store("opencode",rid="request-00000003")
        self.assertEqual(retried["status"],"ALREADY_PRESENT")
        self.assertEqual(self.count(),1)

    def test_deduplicate_existing_import_baseline(self):
        env={**os.environ,"MEMORY_DIR":str(self.memory),"PYTHONDONTWRITEBYTECODE":"1"}
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"note",
                          "Git worktrees are required for changes"],env=env,text=True,
                         capture_output=True,timeout=15)
        self.assertEqual(p.returncode,0,p.stderr)
        self.assertEqual(self.count(),1)
        w.enroll_historical_baseline(self.memory,self.sidecar,
                w.checksum((self.memory/"LOG.txt").read_bytes()),1)
        result=self.store()
        self.assertEqual(result["status"],"ALREADY_PRESENT")
        self.assertEqual(result["index"],0)
        self.assertEqual(self.count(),1)

    def test_shared_ledger_other_writer_dedup(self):
        self.store()
        proof={"source":"bash-sanitized-feature","feature":"git worktrees",
          "model_id":"synthetic-model","evidence_hmac":"a"*64,
          "observations":[{"command_hmac":chr(97+i-10)*64,"byte_offset":i,
                            "observed_day":f"2026-10-{i:02d}"} for i in range(10,16)]}
        result=w.commit(self.memory,self.sidecar,"bash-observation:git worktrees",
                        "Git worktrees are required for changes",proof)
        self.assertEqual(result,{"outcome":"ALREADY_PRESENT","index":0})
        self.assertEqual(self.count(),1)

    def test_request_reservation_survives_crash_and_rejects_conflict(self):
        with self.assertRaises(InterruptedError):
            self.store(failpoint="after_request_prepare")
        self.assertEqual(self.count(),0)
        with self.assertRaisesRegex(a.Conflict,"Idempotency"):
            self.store(text="Different meaningful project decision")
        result=self.store()
        self.assertEqual(result["status"],"STORED")
        self.assertEqual(self.count(),1)

    def test_crash_after_prepare_recovers_only_one(self):
        with self.assertRaises(InterruptedError):
            self.store(failpoint="after_prepare")
        self.assertEqual(self.count(),0)
        self.assertEqual(self.store()["index"],0)
        self.assertEqual(self.count(),1)

    def test_crash_after_note_recovers_only_one(self):
        with self.assertRaises(InterruptedError):
            self.store(failpoint="after_note")
        self.assertEqual(self.count(),1)
        self.assertEqual(self.store()["status"],"ALREADY_PRESENT")
        self.assertEqual(self.count(),1)

    def test_crash_after_upstream_before_receipt(self):
        with self.assertRaises(InterruptedError):
            self.store(failpoint="after_writer_before_index")
        self.assertEqual(self.count(),1)
        self.assertEqual(self.store("zeroclaw",rid="request-00000002")["index"],0)
        self.assertEqual(self.count(),1)

    def test_concurrent_proposals_one_physical_entry(self):
        def task(i):
            return self.store("opencode" if i%2 else "zeroclaw",
                              key="cache.policy" if i%3 else "cache.alias",
                              text="Always reuse deterministic cache keys" if i%3 else "  ALWAYS  reuse deterministic cache keys. ",
                              rid="request-"+str(i).zfill(8))
        with concurrent.futures.ThreadPoolExecutor(max_workers=12) as pool:
            result=list(pool.map(task,range(36)))
        self.assertEqual(sum(x["status"]=="STORED" for x in result),1)
        self.assertEqual(self.count(),1)
        self.assertTrue(all(x["index"]==0 for x in result))

    def test_unsafe_origin_and_real_canonical_forbidden(self):
        with self.assertRaises(ValueError):
            self.store(harness="gptweb")
        with self.assertRaises(ValueError):
            self.store(key="../../other")
        with self.assertRaises(ValueError):
            self.store(text="API token should be shared as memory")
        with self.assertRaises(ValueError):
            a.request_store(Path.home()/".optmem/memory",self.sidecar,
                            "opencode","tools.git.policy","Git worktrees are required for changes",
                            "request-00000001")
        self.assertEqual(self.count(),0)

    def test_manual_out_of_band_log_change_fails_closed(self):
        self.store()
        with (self.memory/"LOG.txt").open("r+b") as f:
            f.seek(8)
            f.write(b"!")
        with self.assertRaises(RuntimeError):
            self.store("zeroclaw",rid="request-00000002")


if __name__=="__main__":
    unittest.main()

#!/usr/bin/env python3
"""Isolated regression tests. All filesystem effects stay below this prototype."""
from pathlib import Path
import hashlib
import http.server
import json
import os
import shutil
import sqlite3
import subprocess
import sys
import threading
import unittest
from unittest import mock

import bash_optmem_agent as app

ROOT=Path(__file__).resolve().parent
SYNTH=ROOT/"test-fixtures-20261015"/("run-"+str(os.getpid()))
MEMO=Path.home()/".local/state/optmem-prototype-2026-10-08/memo"
class Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if SYNTH.exists():raise RuntimeError("Refuse to reuse existing synthetic fixtures")
        SYNTH.mkdir(mode=0o700,parents=True)
    def setUp(self):
        self.loc=SYNTH/self.id().replace(".","-")
        self.loc.mkdir(mode=0o700)
        self.history=self.loc/"history"
        self.state=self.loc/"state"
        self.mem=self.loc/"memory"
        self.history.write_bytes(b"")
        self.mem.mkdir(mode=0o700)
        self._init_memory()
    def _init_memory(self):
        env=os.environ.copy();env["MEMORY_DIR"]=str(self.mem)
        proc=subprocess.run([sys.executable,"-B",str(MEMO),"init"],
                            env=env,text=True,capture_output=True,timeout=8)
        self.assertEqual(proc.returncode,0,proc.stderr)
    def _memory_note(self,s):
        env=os.environ.copy();env["MEMORY_DIR"]=str(self.mem)
        proc=subprocess.run([sys.executable,"-B",str(MEMO),"note",s],
                            env=env,text=True,capture_output=True,timeout=8)
        self.assertEqual(proc.returncode,0,proc.stderr)
    def _append(self,command,count=1):
        with self.history.open("ab") as f:
            f.write((command+"\n").encode()*count)
    def test_busy_local_backend_preserves_eligible_backlog(self):
        self._append("git status",8)
        app.capture(self.history,self.state,observed_at="2026-10-11T00:00:00+00:00")
        for day in (12,13,14):
            self._append("git status",2)
            app.capture(self.history,self.state,
                        observed_at=f"2026-10-{day}T00:00:00+00:00")
        result=app.auto_select(self.state,self.mem,MEMO,
             endpoint_resolver=lambda:("http://127.0.0.1:9999/v1","synthetic-local"),
             load_probe=lambda _:{"num_requests_running":2,"num_requests_waiting":2})
        self.assertFalse(result["model_called"])
        self.assertEqual(result["deferred_reason"],"local_vllm_busy")
        self.assertEqual(app.safe_status(self.state)["private_outbox_ready_not_activated"],0)

    def test_llm_config_full_gpt_still_resolves_local_vllm_only(self):
        folder=self.loc/"isolated-vllm-config"
        folder.mkdir(mode=0o700)
        (folder/"local.env").write_text(
            "HOST=127.0.0.1\nPORT=8000\nSERVED_MODEL_NAME=qwen3.8-27b\n")
        seen=[]
        def fake_run(args,*pos,**kw):
            seen.append(tuple(args))
            if str(args[0]).endswith("/llm-config"):
                return type("Run",(),{"returncode":0,
                    "stdout":"primary-model=openai/gpt-6 backend=Full GPT mode=full-gpt\n"
                    "vllm-current=vllm/qwen3.8-27b vllm-context=43008\n"})()
            if args[:4]==["systemctl","--user","is-enabled","--quiet"]:
                return type("Run",(),{"returncode":0,"stdout":""})()
            raise AssertionError("Unexpected process call")
        with mock.patch.object(app.subprocess,"run",side_effect=fake_run):
            endpoint,model=app.local_vllm_endpoint(Path.home(),config=folder)
        self.assertEqual(endpoint,"http://127.0.0.1:8000/v1")
        self.assertEqual(model,"qwen3.8-27b")
        self.assertEqual(len(seen),2)
        self.assertFalse(any("openai/gpt-6" in " ".join(x) for x in seen))

    def test_local_vllm_metrics_parser_and_denial(self):
        self.assertRaises(RuntimeError,app.local_vllm_load,"https://api.example/v1")
        class Handler(http.server.BaseHTTPRequestHandler):
            def do_GET(self):
                body=b"vllm:num_requests_running{model_name=\"local\"} 2\nvllm:num_requests_waiting{model_name=\"local\"} 1\n"
                self.send_response(200)
                self.send_header("Content-Length",str(len(body)))
                self.end_headers()
                self.wfile.write(body)
            def log_message(self,*args):pass
        srv=http.server.ThreadingHTTPServer(("127.0.0.1",0),Handler)
        worker=threading.Thread(target=srv.serve_forever,daemon=True)
        worker.start()
        old=os.environ.get("VLLM_API_KEY")
        os.environ["VLLM_API_KEY"]="synthetic-placeholder-not-a-secret"
        try:
            result=app.local_vllm_load(f"http://127.0.0.1:{srv.server_port}/v1")
            self.assertEqual(result["num_requests_running"],2)
            self.assertEqual(result["num_requests_waiting"],1)
        finally:
            if old is None:os.environ.pop("VLLM_API_KEY",None)
            else:os.environ["VLLM_API_KEY"]=old
            srv.shutdown()
            srv.server_close()
            worker.join(timeout=3)

    def test_private_sanitizer(self):
        safe={"git status":"git status","git -C /home/x status":"git status",
              "systemctl --user status zeroclaw.service":"systemctl status",
              "nix develop .":"nix develop",
              "ssh internal.private.example":"ssh usage"}
        for raw,expected in safe.items():
            with self.subTest(raw=raw):
                self.assertEqual(app.parse_feature(raw.encode())[0],expected)
        risky=["export TOKEN=abc","curl -H 'Authorization: Bearer sk-test-token' https://localhost",
               "git commit -m 'password=verySecret'", "git status && rm -rf ~/.ssh",
               "echo $(cat ~/.config/token)","ssh user@example.com | bash","sudo systemctl restart x",
               "gh auth login", "nix build --api-key=whatever","git -C . status; echo injected"]
        for value in risky:
            with self.subTest(risky=value):
                self.assertIsNone(app.parse_feature(value.encode())[0])
        self.assertIsNone(app.parse_feature(b"git status\xff")[0])
        self.assertIsNone(app.parse_feature(b"a"*5000)[0])
    def test_backfill_is_not_live_usage_and_no_raw_secret_retention(self):
        self._append("git status",12)
        self._append("export SECRET_TOKEN=NeverPersistThisSensitiveValue")
        first=app.capture(self.history,self.state,observed_at="2026-10-15T11:00:00+00:00")
        self.assertEqual(first["source_quality"],"backfill")
        self.assertEqual(first["privacy_rejected_lines"],1)
        self.assertEqual(first["features_admitted_to_private_aggregate"],12)
        again=app.capture(self.history,self.state,observed_at="2026-10-15T11:05:00+00:00")
        self.assertEqual(again["features_admitted_to_private_aggregate"],0)
        con=app.open_database(self.state)
        rows=app.feature_summaries(con)
        self.assertEqual(rows[0]["live_days"],0)
        con.close()
        for p in self.state.glob("*"):
            if p.is_file():
                self.assertNotIn(b"NeverPersistThisSensitiveValue",p.read_bytes())
        result=app.auto_select(self.state,self.mem,MEMO,choice_fn=lambda *_:self.fail("model called"))
        self.assertFalse(result["model_called"])
    def test_partial_line_and_same_size_replacement(self):
        self._append("git status")
        app.capture(self.history,self.state,observed_at="2026-10-15T00:00:00+00:00")
        with self.history.open("ab") as f:f.write(b"git lo")
        pending=app.capture(self.history,self.state,observed_at="2026-10-15T00:01:00+00:00")
        self.assertEqual(pending["incomplete_tail_bytes_deferred"],6)
        with self.history.open("ab") as f:f.write(b"g\n")
        done=app.capture(self.history,self.state,observed_at="2026-10-16T00:00:00+00:00")
        self.assertEqual(done["features_admitted_to_private_aggregate"],1)
        self.assertEqual(done["source_quality"],"live_append")
        previous=self.history.read_bytes()
        self.history.write_bytes(b"git branch\n"+b" "*(len(previous)-len(b"git branch\n")))
        rotate=app.capture(self.history,self.state,observed_at="2026-10-17T00:00:00+00:00")
        self.assertTrue(rotate["loss_or_rotation_detected"])
        self.assertEqual(rotate["source_quality"],"uncertain_reload")
        self.assertEqual(app.safe_status(self.state)["loss_or_rotation_events"],1)
    def test_automatic_candidate_uses_real_optmem_and_never_writes(self):
        self._append("git status",8)
        app.capture(self.history,self.state,observed_at="2026-10-15T00:00:00+00:00")
        for day in (16,17,18):
            self._append("git status",2)
            app.capture(self.history,self.state,observed_at=f"2026-10-{day}T00:00:00+00:00")
        con=app.open_database(self.state)
        self.assertEqual(app.feature_summaries(con)[0]["live_days"],3)
        con.close()
        result=app.auto_select(self.state,self.mem,MEMO,choice_fn=lambda f,c:["git status"])
        self.assertEqual(result["newly_approved_to_private_outbox"],1)
        same=app.auto_select(self.state,self.mem,MEMO,choice_fn=lambda f,c:["git status"])
        self.assertEqual(same["newly_approved_to_private_outbox"],0)
        con=app.open_database(self.state)
        entry=con.execute("SELECT statement,provenance_json,model_id FROM decisions").fetchone()
        self.assertIn("git status",entry[0])
        refs=json.loads(entry[1])
        self.assertGreaterEqual(len(refs),6)
        self.assertTrue(all(len(x["command_hmac"])==64 and "byte_offset" in x for x in refs))
        self.assertEqual(entry[2],"test-double")
        con.close()
        self.assertEqual((self.mem/"LOG.txt").stat().st_size,0)
    def test_duplicate_found_through_real_optmem_read(self):
        self._memory_note("Bash usage observed repeatedly: git status in previous session")
        self._append("git status",8)
        app.capture(self.history,self.state,observed_at="2026-10-15T00:00:00+00:00")
        for day in (16,17,18):
            self._append("git status",2)
            app.capture(self.history,self.state,observed_at=f"2026-10-{day}T00:00:00+00:00")
        self.assertTrue(app.lookup_optmem(self.mem,MEMO,"git status"))
        context=app.read_optmem_context(self.mem,MEMO)
        self.assertEqual(len(context),1)
        decision=app.auto_select(self.state,self.mem,MEMO,choice_fn=lambda f,c:["git status"])
        self.assertEqual(decision["duplicate_memory_matches"],1)
        self.assertEqual(decision["newly_approved_to_private_outbox"],0)

    def test_deep_same_size_history_rewrite_detected(self):
        self._append("git status",140)
        app.capture(self.history,self.state,observed_at="2026-10-11T00:00:00+00:00")
        old=self.history.stat().st_size
        with self.history.open("r+b") as f:
            f.seek(0)
            f.write(b"git branch")  # Same byte count, >1KB ahead of the tail witness.
        self.assertEqual(self.history.stat().st_size,old)
        result=app.capture(self.history,self.state,observed_at="2026-10-12T00:00:00+00:00")
        self.assertTrue(result["loss_or_rotation_detected"])
        self.assertEqual(result["source_quality"],"uncertain_reload")
        self.assertEqual(app.safe_status(self.state)["loss_or_rotation_events"],1)
    def test_missing_legacy_prefix_hash_is_not_treated_as_live_append(self):
        self._append("git status",10)
        app.capture(self.history,self.state,observed_at="2026-10-11T00:00:00+00:00")
        con=app.open_database(self.state)
        with con:
            con.execute("UPDATE cursor SET prefix_hmac='' WHERE id=1")
        con.close()
        self._append("git status",2)
        outcome=app.capture(self.history,self.state,observed_at="2026-10-12T00:00:00+00:00")
        self.assertEqual(outcome["source_quality"],"uncertain_reload")
        self.assertTrue(outcome["loss_or_rotation_detected"])
        con=app.open_database(self.state)
        rows=app.feature_summaries(con)
        self.assertEqual(rows[0]["live_count"],0)
        con.close()

    def test_fail_closed_on_missing_optmem(self):
        self._append("nix develop",9)
        app.capture(self.history,self.state,observed_at="2026-10-15T00:00:00+00:00")
        for day in (16,17,18):
            self._append("nix develop",2)
            app.capture(self.history,self.state,observed_at=f"2026-10-{day}T00:00:00+00:00")
        with self.assertRaises(RuntimeError):
            app.auto_select(self.state,self.loc/"nonexistent",MEMO,choice_fn=lambda f,c:["nix develop"])
        self.assertEqual(app.safe_status(self.state)["private_outbox_ready_not_activated"],0)
    def test_no_remote_llm_and_synthetic_loopback_json(self):
        with self.assertRaises(RuntimeError):
            app.call_local_model("https://some-cloud.example/v1","model",[],[])
        requests=[]
        class Handler(http.server.BaseHTTPRequestHandler):
            def do_POST(self):
                length=int(self.headers["Content-Length"])
                obj=json.loads(self.rfile.read(length))
                requests.append(obj)
                blob=json.dumps({"choices":[{"message":{
                    "content":json.dumps({"selected_features":["git status"]})}}]}).encode()
                self.send_response(200)
                self.send_header("Content-Type","application/json")
                self.send_header("Content-Length",str(len(blob)))
                self.end_headers()
                self.wfile.write(blob)
            def log_message(self,*args):
                pass
        srv=http.server.ThreadingHTTPServer(("127.0.0.1",0),Handler)
        thread=threading.Thread(target=srv.serve_forever,daemon=True)
        thread.start()
        try:
            selections=app.call_local_model(
                f"http://127.0.0.1:{srv.server_port}/v1","synthetic",
                [{"feature":"git status","observed_count":12,"live_count":8,"live_days":4}],[])
            self.assertEqual(selections,["git status"])
            self.assertEqual(len(requests),1)
            self.assertEqual(requests[0]["model"],"synthetic")
            self.assertNotIn("chat_template_kwargs",requests[0])
            self.assertIn("git status",requests[0]["messages"][1]["content"])
            second=app.call_local_model(
                f"http://127.0.0.1:{srv.server_port}/v1","qwen3.8-27b",
                [{"feature":"git status","observed_count":12,"live_count":8,"live_days":4}],[])
            self.assertEqual(second,["git status"])
            self.assertEqual(len(requests),2)
            self.assertEqual(requests[1]["chat_template_kwargs"],{"enable_thinking":False})
            self.assertNotIn("NeverPersistThisSensitiveValue",
                             json.dumps(requests))
        finally:
            srv.shutdown()
            srv.server_close()
            thread.join(timeout=3)
    def test_unavailable_local_vllm_fails_closed(self):
        wrong=self.loc/"isolated.env"
        wrong.mkdir(mode=0o700)
        (wrong/"remote.env").write_text("HOST=external.example\nPORT=443\nSERVED_MODEL_NAME=remote\n")
        # The production resolver requires a genuine local llm-config status.
        with self.assertRaises(RuntimeError):
            app.call_local_model("http://example.com:443/v1","remote",[],[])
        self.assertEqual(app.parse_public_model_config(wrong/"remote.env")["HOST"],
                         "external.example")
    def test_optmem_zoom_read_only(self):
        self._memory_note("Important versioned project rule in isolated memory")
        self._memory_note("Another stable observation for zoom test")
        before=(self.mem/"LOG.txt").read_bytes()
        nodes=app.read_optmem_zoom(self.mem,MEMO,"0-1")
        self.assertEqual(len(nodes),2)
        self.assertIn("Important",nodes[0])
        self.assertEqual((self.mem/"LOG.txt").read_bytes(),before)
        with self.assertRaises(ValueError):
            app.read_optmem_zoom(self.mem,MEMO,"1-2")

    def test_concurrent_capture_same_file_is_idempotent(self):
        import concurrent.futures
        self._append("git status",40)
        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
            results=list(pool.map(lambda _:app.capture(
                self.history,self.state,observed_at="2026-10-15T01:00:00+00:00"),range(3)))
        self.assertEqual(sum(x["features_admitted_to_private_aggregate"] for x in results),40)
        con=app.open_database(self.state)
        self.assertEqual(con.execute("SELECT COUNT(*) FROM observations").fetchone()[0],40)
        self.assertEqual(app.feature_summaries(con)[0]["observed_count"],40)
        con.close()

    @classmethod
    def tearDownClass(cls):
        # All test data is synthetic; leave the private fixtures for auditing.
        assert SYNTH.is_dir()
        for path in SYNTH.rglob("*"):
            if path.is_file():
                assert str(path.resolve()).startswith(str(SYNTH.resolve()))
        print("Synthetic fixtures retained in private staging for audit")

if __name__=="__main__":
    unittest.main(verbosity=2)

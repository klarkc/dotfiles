#!/usr/bin/env python3
"""Synthetic-only isolated provenance-writer acceptance tests."""
from pathlib import Path
import concurrent.futures
from datetime import date,timedelta
import json
import os
import shutil
import sqlite3
import socket
import time
import subprocess
import sys
import unittest
import uuid
import hashlib

import optmem_writer_isolated as w
sys.path.insert(0,str(w.HERE.parent))
import bash_optmem_agent as a

SUITE_ROOT=w.HERE/"synthetic-cases"
class TestWriter(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        SUITE_ROOT.mkdir(mode=0o700,exist_ok=True)
    def setUp(self):
        self.path=SUITE_ROOT/(self._testMethodName+"-"+uuid.uuid4().hex[:10])
        if self.path.exists():
            raise RuntimeError("Refusing to overwrite existing synthetic writer fixture")
        self.path.mkdir(mode=0o700)
        self.memory=self.path/"memory"
        self.sidecar=self.path/"provenance"
        self.source=self.path/"collector"
        self.history=self.path/"history.txt"
        self.memory.mkdir(mode=0o700)
        self.history.write_text("")
        env=os.environ.copy()
        env["MEMORY_DIR"]=str(self.memory)
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"init"],env=env,text=True,
                         capture_output=True,timeout=12)
        self.assertEqual(p.returncode,0,p.stderr)
        # Upstream memo inherits process umask; Nix hook shells may use 0022.
        # Model the intended private writer runtime, not a public test LOG.
        (self.memory/"LOG.txt").chmod(0o600)
        self.assertEqual((self.memory/"LOG.txt").stat().st_size,0)
    def populate(self,feature="git status"):
        self.history.write_bytes((feature+"\n").encode()*8)
        today=date.today()
        a.capture(self.history,self.source,observed_at=(today-timedelta(days=4)).isoformat()+"T12:00:00+00:00")
        for gap in (3,2,1):
            with self.history.open("ab") as f:f.write(((feature+"\n")*2).encode())
            a.capture(self.history,self.source,observed_at=(today-timedelta(days=gap)).isoformat()+"T12:00:00+00:00")
        answer=a.auto_select(self.source,self.memory,w.MEMO,choice_fn=lambda facts,context:[feature])
        self.assertEqual(answer["newly_approved_to_private_outbox"],1)
        return feature
    def log_count(self):
        return (self.memory/"LOG.txt").stat().st_size//320
    def start_ipc(self):
        socket_dir=Path(f"/run/user/{os.getuid()}")/f"optmem-stage-writer-{uuid.uuid4().hex[:12]}"
        socket_dir.mkdir(mode=0o700)
        path=socket_dir/"writer.sock"
        server=w.HERE/"optmem_writer_ipc.py"
        proc=subprocess.Popen([sys.executable,"-B",str(server),
            "--socket",str(path),"--memory-dir",str(self.memory),
            "--sidecar",str(self.sidecar),"--source-state",str(self.source)],
            stdout=subprocess.DEVNULL,stderr=subprocess.PIPE,text=True,
            env={**os.environ,"PYTHONDONTWRITEBYTECODE":"1"})
        for _ in range(60):
            if path.exists():break
            if proc.poll() is not None:break
            time.sleep(0.05)
        if not path.exists():
            proc.terminate()
            try:proc.wait(timeout=2)
            except subprocess.TimeoutExpired:proc.kill()
            raise RuntimeError("Stage-only Unix socket server did not start")
        self.assertEqual(path.stat().st_mode & 0o077,0)
        return proc,path

    def stop_ipc(self,proc,path):
        if proc.poll() is None:
            proc.terminate()
            try:proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                proc.kill();proc.wait(timeout=5)
        if proc.stderr:proc.stderr.close()
        self.assertFalse(path.exists())
        path.parent.rmdir()

    def ipc_request(self,path,data):
        with socket.socket(socket.AF_UNIX,socket.SOCK_STREAM) as sock:
            sock.settimeout(6)
            sock.connect(str(path))
            sock.sendall(data)
            response=b""
            while b"\n" not in response and len(response)<1024:
                response+=sock.recv(1024)
            return json.loads(response.split(b"\n",1)[0])

    def test_commit_and_duplicate_replay(self):
        feature=self.populate()
        one=w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(one,{"outcome":"COMMITTED","index":0})
        self.assertEqual(self.log_count(),1)
        second=w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(second["outcome"],"ALREADY_COMMITTED")
        self.assertEqual(self.log_count(),1)
        status=w.verify(self.memory,self.sidecar)
        self.assertEqual(status["committed_provenance"],1)
        self.assertEqual(status["unresolved_prepared"],0)
        self.assertEqual(status["memory_entries"],1)
    def test_crash_after_prepare_replays_once(self):
        feature=self.populate()
        with self.assertRaises(InterruptedError):
            w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature,failpoint="after_prepare")
        self.assertEqual(self.log_count(),0)
        self.assertEqual(w.verify(self.memory,self.sidecar)["unresolved_prepared"],1)
        result=w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(result["outcome"],"COMMITTED")
        self.assertEqual(self.log_count(),1)
    def test_crash_after_note_reconciles_without_duplicate(self):
        feature=self.populate()
        with self.assertRaises(InterruptedError):
            w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature,failpoint="after_note")
        self.assertEqual(self.log_count(),1)
        result=w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(result["outcome"],"ALREADY_COMMITTED")
        self.assertEqual(self.log_count(),1)
        self.assertEqual(w.verify(self.memory,self.sidecar)["unresolved_prepared"],0)
    def test_interleaved_external_writer_fails_closed(self):
        feature=self.populate()
        with self.assertRaises(InterruptedError):
            w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature,failpoint="after_prepare")
        env=os.environ.copy()
        env["MEMORY_DIR"]=str(self.memory)
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"note","Unrelated user test memory"],
                         env=env,capture_output=True,text=True,timeout=12)
        self.assertEqual(p.returncode,0,p.stderr)
        with self.assertRaises(RuntimeError):
            w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(self.log_count(),1)
        self.assertNotIn("Bash usage", (self.memory/"LOG.txt").read_text())
    def test_foreign_append_after_commit_detected(self):
        feature=self.populate()
        w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        env=os.environ.copy()
        env["MEMORY_DIR"]=str(self.memory)
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"note","Foreign memory after committed entry"],
                        env=env,capture_output=True,text=True,timeout=12)
        self.assertEqual(p.returncode,0,p.stderr)
        with self.assertRaises(RuntimeError):
            w.verify(self.memory,self.sidecar)
        self.assertEqual(self.log_count(),2)

    def test_existing_snapshot_remains_sealed_after_first_append(self):
        env=os.environ.copy()
        env["MEMORY_DIR"]=str(self.memory)
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"note","Already imported historical memory"],
                        env=env,capture_output=True,text=True,timeout=12)
        self.assertEqual(p.returncode,0,p.stderr)
        digest=hashlib.sha256((self.memory/"LOG.txt").read_bytes()).hexdigest()
        with self.assertRaisesRegex(RuntimeError,"explicit baseline enrollment"):
            w.verify(self.memory,self.sidecar)
        receipt=w.enroll_historical_baseline(self.memory,self.sidecar,digest,1)
        self.assertEqual(receipt["baseline_records"],1)
        feature=self.populate()
        out=w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(out["index"],1)
        self.assertEqual(self.log_count(),2)
        con=sqlite3.connect(self.sidecar/"provenance.sqlite3")
        row=con.execute("SELECT initial_log_count,initial_log_sha256 FROM baseline").fetchone()
        self.assertEqual(row[0],1)
        self.assertEqual(len(row[1]),64)
        con.close()
        self.assertEqual(w.verify(self.memory,self.sidecar)["committed_provenance"],1)
        # Deliberate corruption of only the disposable fixture prefix.
        with (self.memory/"LOG.txt").open("r+b") as f:
            f.seek(5)
            f.write(b"X")
        with self.assertRaises(RuntimeError):
            w.verify(self.memory,self.sidecar)

    def test_baseline_enrollment_denies_world_readable_log(self):
        env={**os.environ,"MEMORY_DIR":str(self.memory)}
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"note",
                         "Synthetic legacy provenance requires privacy"],
                         env=env,capture_output=True,text=True,timeout=12)
        self.assertEqual(p.returncode,0,p.stderr)
        log=self.memory/"LOG.txt"
        digest=hashlib.sha256(log.read_bytes()).hexdigest()
        log.chmod(0o644)
        with self.assertRaisesRegex(ValueError,"ownership or permissions"):
            w.enroll_historical_baseline(self.memory,self.sidecar,digest,1)
        log.chmod(0o600)
        self.assertEqual(w.enroll_historical_baseline(self.memory,self.sidecar,digest,1)["baseline_records"],1)

    def test_nonempty_restore_cannot_reenroll_without_manifest(self):
        feature=self.populate()
        w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        duplicate=self.path/"memory-only-copy"
        shutil.copytree(self.memory,duplicate)
        incomplete=self.path/"unverified-sidecar"
        with self.assertRaisesRegex(RuntimeError,"explicit baseline enrollment"):
            w.verify(duplicate,incomplete)
        with self.assertRaisesRegex(RuntimeError,"hash/count differs"):
            w.enroll_historical_baseline(duplicate,incomplete,"0"*64,1)
        with self.assertRaisesRegex(RuntimeError,"hash/count differs"):
            w.enroll_historical_baseline(duplicate,incomplete,
                    hashlib.sha256((duplicate/"LOG.txt").read_bytes()).hexdigest(),2)
        self.assertEqual(w.verify(self.memory,self.sidecar)["committed_provenance"],1)

    def test_sha_tampering_rejected(self):
        feature=self.populate()
        con=sqlite3.connect(self.source/"capture.sqlite3")
        with con:
            con.execute("UPDATE decisions SET evidence_hmac=? WHERE feature=?",
                        ("0"*64,feature))
        con.close()
        with self.assertRaises(RuntimeError):
            w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(self.log_count(),0)
    def test_direct_real_canonical_path_is_forbidden(self):
        with self.assertRaises(ValueError):
            w.commit(Path.home()/".optmem/memory",self.sidecar,
               "bash-observation:git status","Bash usage observed repeatedly: git status",
               {"source":"bash-sanitized-feature","observations":[]})
    def test_concurrent_writer_single_committed_index(self):
        feature=self.populate()
        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
            out=list(pool.map(lambda _:w.commit_ready_bash_observation(
                self.memory,self.sidecar,self.source,feature),range(3)))
        self.assertEqual([x["outcome"] for x in out].count("COMMITTED"),1)
        self.assertEqual(self.log_count(),1)
        self.assertEqual(w.verify(self.memory,self.sidecar)["committed_provenance"],1)

    def test_acknowledged_receipt_is_durable_idempotent(self):
        feature=self.populate()
        first=w.commit_and_acknowledge(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(first["outcome"],"COMMITTED_AND_ACKNOWLEDGED")
        self.assertEqual(first["index"],0)
        self.assertEqual(len(first["receipt_sha256"]),64)
        src=sqlite3.connect(self.source/"capture.sqlite3")
        receipt=src.execute("SELECT log_index,memory_record_sha256 FROM writer_receipts").fetchone()
        self.assertEqual(receipt,(0,first["receipt_sha256"]))
        self.assertEqual(src.execute("SELECT status FROM decisions").fetchone()[0],
                         "RECEIPT_ACKNOWLEDGED_TO_ISOLATED_OPT_MEM")
        src.close()
        retry=w.commit_and_acknowledge(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(retry["outcome"],"ALREADY_ACKNOWLEDGED")
        self.assertEqual(self.log_count(),1)
        self.assertEqual(a.safe_status(self.source)["private_outbox_ready_not_activated"],0)

    def test_crash_between_writer_and_ack_receives_exactly_once(self):
        feature=self.populate()
        with self.assertRaises(InterruptedError):
            w.commit_and_acknowledge(self.memory,self.sidecar,self.source,feature,
                                     failpoint="after_writer_before_receipt")
        self.assertEqual(self.log_count(),1)
        src=sqlite3.connect(self.source/"capture.sqlite3")
        self.assertEqual(src.execute("SELECT status FROM decisions").fetchone()[0],
                         "READY_FOR_WRITER_NOT_ACTIVATED")
        src.close()
        repaired=w.commit_and_acknowledge(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(repaired["outcome"],"ALREADY_ACKNOWLEDGED")
        self.assertEqual(self.log_count(),1)
        self.assertEqual(a.safe_status(self.source)["private_outbox_ready_not_activated"],0)

    def test_new_activity_cannot_invalidate_approved_source_snapshot(self):
        feature=self.populate()
        # The model's candidate contains an immutable 6+ HMAC snapshot.
        # Additional observations change the newest-ten list, not the proof.
        with self.history.open("ab") as f:
            f.write(((feature+"\n")*25).encode())
        a.capture(self.history,self.source,observed_at=date.today().isoformat()+"T13:00:00+00:00")
        out=w.commit_and_acknowledge(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(out["outcome"],"COMMITTED_AND_ACKNOWLEDGED")
        self.assertEqual(self.log_count(),1)

    def test_receipt_blocks_second_memory_before_any_append(self):
        feature=self.populate()
        w.commit_and_acknowledge(self.memory,self.sidecar,self.source,feature)
        secondary=self.path/"other-memory"
        secondary.mkdir(mode=0o700)
        env=os.environ.copy();env["MEMORY_DIR"]=str(secondary)
        p=subprocess.run([sys.executable,"-B",str(w.MEMO),"init"],env=env,
            capture_output=True,text=True,timeout=12)
        self.assertEqual(p.returncode,0,p.stderr)
        with self.assertRaises(RuntimeError):
            w.commit_and_acknowledge(secondary,self.path/"other-sidecar",self.source,feature)
        self.assertEqual((secondary/"LOG.txt").stat().st_size,0)
        self.assertEqual(self.log_count(),1)

    def test_expired_frozen_source_observation_fails_closed(self):
        feature=self.populate()
        con=sqlite3.connect(self.source/"capture.sqlite3")
        row=con.execute("SELECT provenance_json FROM decisions WHERE feature=?",(feature,)).fetchone()
        proof=json.loads(row[0])
        ref=proof[0]
        with con:
            con.execute("DELETE FROM observations WHERE epoch=? AND byte_offset=?",
                        (ref["epoch"],ref["byte_offset"]))
        con.close()
        with self.assertRaises(RuntimeError):
            w.commit_and_acknowledge(self.memory,self.sidecar,self.source,feature)
        self.assertEqual(self.log_count(),0)

    def test_unix_ipc_serializes_duplicate_requests(self):
        feature=self.populate()
        proc,path=self.start_ipc()
        try:
            payload=json.dumps({"operation":"admit","feature":feature}).encode()+b"\n"
            with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
                results=list(pool.map(lambda _:self.ipc_request(path,payload),range(5)))
            self.assertEqual(sum(x["result"]=="COMMITTED_AND_ACKNOWLEDGED" for x in results),1)
            self.assertEqual(sum(x["result"]=="ALREADY_ACKNOWLEDGED" for x in results),4)
            self.assertTrue(all(x["status"]=="ACCEPTED" and x["index"]==0 for x in results))
            self.assertEqual(self.log_count(),1)
            self.assertEqual(a.safe_status(self.source)["private_outbox_ready_not_activated"],0)
        finally:self.stop_ipc(proc,path)

    def test_unix_ipc_denies_arbitrary_operations_and_raw_commands(self):
        feature=self.populate()
        proc,path=self.start_ipc()
        try:
            bad=[
              {"operation":"exec","feature":"git status"},
              {"operation":"admit","feature":"git status; rm -rf ."},
              {"operation":"admit","feature":"git status","raw_command":"secret"},
              {"operation":"admit","feature":"git status","memory_dir":"/home/user/.optmem/memory"}]
            for msg in bad:
                response=self.ipc_request(path,json.dumps(msg).encode()+b"\n")
                self.assertEqual(response["status"],"REFUSED")
            self.assertEqual(self.log_count(),0)
        finally:self.stop_ipc(proc,path)

    def test_unix_ipc_restart_preserves_exact_receipt(self):
        feature=self.populate()
        payload=json.dumps({"operation":"admit","feature":feature}).encode()+b"\n"
        proc,path=self.start_ipc()
        try:
            first=self.ipc_request(path,payload)
            self.assertEqual(first["result"],"COMMITTED_AND_ACKNOWLEDGED")
        finally:self.stop_ipc(proc,path)
        proc,path=self.start_ipc()
        try:
            again=self.ipc_request(path,payload)
            self.assertEqual(again["result"],"ALREADY_ACKNOWLEDGED")
            self.assertEqual(self.log_count(),1)
        finally:self.stop_ipc(proc,path)

    def test_private_snapshot_restoration_and_corruption_detection(self):
        feature=self.populate()
        w.commit_ready_bash_observation(self.memory,self.sidecar,self.source,feature)
        backup=self.path/"backup"
        shutil.copytree(self.memory,backup)
        backup_sidecar=self.path/"backup-sidecar"
        shutil.copytree(self.sidecar,backup_sidecar)
        original=hashlib.sha256((backup/"LOG.txt").read_bytes()).hexdigest()
        with (self.memory/"LOG.txt").open("r+b") as f:
            f.seek(40)
            f.write(b"X")
        with self.assertRaises(RuntimeError):
            w.verify(self.memory,self.sidecar)
        shutil.copy2(backup/"LOG.txt",self.memory/"LOG.txt")
        self.assertEqual(hashlib.sha256((self.memory/"LOG.txt").read_bytes()).hexdigest(),original)
        self.assertEqual(w.verify(self.memory,self.sidecar)["committed_provenance"],1)
        for path in backup.rglob("*"):
            if path.is_file():
                restored=self.memory/path.relative_to(backup)
                self.assertEqual(hashlib.sha256(path.read_bytes()).hexdigest(),
                                 hashlib.sha256(restored.read_bytes()).hexdigest())

if __name__=="__main__":
    unittest.main(verbosity=2)

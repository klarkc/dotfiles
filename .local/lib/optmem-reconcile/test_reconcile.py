#!/usr/bin/env python3
"""Offline regression tests: metadata-only ingestion, drift and privacy."""
import hashlib
import json
from pathlib import Path
import sqlite3
import tempfile
import unittest
import reconcile as r

def sha(s):
    return hashlib.sha256(s.encode()).hexdigest()

class ReconciliationTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.db = Path(self.tmp.name) / "sample.sqlite"
        con = sqlite3.connect(self.db)
        con.executescript("""CREATE TABLE message (id TEXT, session_id TEXT, time_created INTEGER, data TEXT);
        CREATE TABLE part (id TEXT, message_id TEXT, data TEXT);""")
        records = [
            ("a", "Continue if you have next steps, or stop and ask for clarification if you are unsure how to proceed."),
            ("b", "senha=FictitiousSensitive123"),
            ("c", "From Build: result from agent"),
            ("d", "prefiro que o agente use somente staging"),
            ("e", "From me: prefiro staging no writer"),
            ("f", ""),
        ]
        for i,(mid,text) in enumerate(records):
            con.execute("INSERT INTO message VALUES (?,?,?,?)",
                        (mid,"ses1",r.CUTOFF_MS+1000+i,json.dumps({"role":"user"})))
            con.execute("INSERT INTO part VALUES (?,?,?)",
                        (f"part{i}",mid,json.dumps({"type":"text","text":text})
                         if text else json.dumps({"type":"compaction"})))
        con.execute("INSERT INTO message VALUES ('agent','ses1',?,'{\"role\":\"assistant\"}')",
                    (r.CUTOFF_MS+9000,))
        con.commit()
        con.close()

    def test_classification_and_no_raw_text(self):
        rows=list(r.source_rows(self.db))
        self.assertEqual(len(rows),6)
        classes={x["message_id"]:x["classification"] for x in rows}
        self.assertEqual(classes["a"],"EPHEMERAL_REPLAY")
        self.assertEqual(classes["b"],"PRIVACY_HOLD")
        self.assertEqual(classes["c"],"DELEGATED_HANDOFF_NO_USER_AUTHORITY")
        self.assertEqual(classes["d"],"STANDING_DECISION_MANUAL_REVIEW")
        self.assertEqual(classes["e"],"STANDING_DECISION_MANUAL_REVIEW")
        self.assertEqual(classes["f"],"NON_TEXT_UNREVIEWED")
        self.assertNotIn("senha",json.dumps(rows))
        self.assertEqual(next(x for x in rows if x["message_id"]=="b")["text_sha256"],sha("senha=FictitiousSensitive123"))

    def test_reconcile_drift_and_missing_fail_closed(self):
        rows=list(r.source_rows(self.db))
        prior=[{"message_id":"a","text_sha256":sha("Continue if you have next steps, or stop and ask for clarification if you are unsure how to proceed.")}]
        self.assertEqual(r.reconcile(rows,prior)["new_after_prior_index"],5)
        changed=[{"message_id":"a","text_sha256":"0"*64}]
        with self.assertRaisesRegex(RuntimeError,"drift"):
            r.reconcile(rows,changed)
        with self.assertRaisesRegex(RuntimeError,"missing"):
            r.reconcile(rows,[{"message_id":"notfound","text_sha256":"1"*64}])

    def test_private_output_and_refuse_overwrite(self):
        root=Path(self.tmp.name)
        root.chmod(0o700)
        out=root/"generation"
        r.private_output(out,[{"message_id":"a"}],{"safe":True},[])
        self.assertEqual(out.stat().st_mode&0o777,0o700)
        self.assertTrue(all(x.stat().st_mode&0o777==0o600 for x in out.iterdir()))
        with self.assertRaises(FileExistsError):
            r.private_output(out,[],{},[])
        self.assertNotIn("senha", (out/"status.json").read_text())

if __name__=="__main__":
    unittest.main()

#!/usr/bin/env python3
"""Protocol and read-only provenance regressions, no external LLMs or services."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
import readonly_mcp as m

H=Path.home()
BASE=H/".local/state/optmem-migration-2026-10-09/global-staging-20261014/g2b-persistent-20261014"
MEM=BASE/"g4-authorized-local-20261015/optmem-test-memory"
MEMO=H/".local/state/optmem-prototype-2026-10-08/memo"
LOG_SHA="78eafd77c8c0c4eb67fcd1468bd758500c090b0d979cc0b4eccd2f6e34a2f565"

def req(method, ident=1, params=None):
    r={"jsonrpc":"2.0","id":ident,"method":method}
    if params is not None:r["params"]=params
    return r

class MCPReadTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.before=m.sha(MEM/"LOG.txt")
        cls.reader=m.MCPReader(MEM,MEMO,LOG_SHA)
    @classmethod
    def tearDownClass(cls):
        assert cls.before == m.sha(MEM/"LOG.txt"), "G4P staging source was modified"

    def test_init_and_tool_names(self):
        i=self.reader.dispatch(req("initialize"))
        self.assertEqual(i["result"]["serverInfo"]["name"],"optmem-stage-readonly")
        tools=self.reader.dispatch(req("tools/list"))["result"]["tools"]
        self.assertEqual({x["name"] for x in tools},
                         {"optmem_wake","optmem_recall","optmem_zoom"})
        self.assertIsNone(self.reader.dispatch({"jsonrpc":"2.0","method":"notifications/initialized"}))

    def test_wake_complete(self):
        self.assertIn("You are awake.",self.reader.read("optmem_wake",{"part":1}))

    def test_recall_literally_escaped(self):
        self.assertIn("U01",self.reader.read("optmem_recall",{"query":"U01"}))
        self.assertNotIn("No match",self.reader.read("optmem_recall",{"query":"S22"}))

    def test_zoom(self):
        self.assertTrue(self.reader.read("optmem_zoom",{"block":"0-1"}).strip())

    def test_reject_writes_and_injection(self):
        for name,arg in [
            ("optmem_note",{"text":"x"}),
            ("optmem_nap",{"summary":"x"}),
            ("optmem_wake",{"part":True}),
            ("optmem_wake",{"part":0}),
            ("optmem_recall",{"query":".*"}),
            ("optmem_recall",{"query":"U01\nnote"}),
            ("optmem_zoom",{"block":"../../../etc"}),
            ("optmem_zoom",{"block":"1-1"}),
        ]:
            with self.subTest(name=name,arg=arg):
                result=self.reader.dispatch(req("tools/call",params={"name":name,"arguments":arg}))
                self.assertTrue(result["result"]["isError"])
        self.assertEqual(m.sha(MEM/"LOG.txt"),LOG_SHA)

    def test_refuses_canonical_and_drift(self):
        with self.assertRaises(RuntimeError):
            m.MCPReader(H/".optmem/memory",MEMO,LOG_SHA)
        with self.assertRaises(RuntimeError):
            m.MCPReader(MEM,MEMO,"0"*64)
        with self.assertRaises(ValueError):
            self.reader.read("optmem_recall",{"query":"\u0000"})

    def test_transport_real_subprocess(self):
        cmd=[sys.executable,"-B",str(Path(m.__file__)),"--memory-dir",str(MEM),
             "--memo",str(MEMO),"--expected-log-sha256",LOG_SHA]
        env={**os.environ,"PYTHONDONTWRITEBYTECODE":"1"}
        p=subprocess.Popen(cmd,stdin=subprocess.PIPE,stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE,text=True,env=env)
        assert p.stdin and p.stdout and p.stderr
        try:
            for i,method in enumerate(("initialize","tools/list","tools/call"),1):
                params=({"name":"optmem_recall","arguments":{"query":"U01"}}
                        if method=="tools/call" else None)
                p.stdin.write(json.dumps(req(method,i,params))+"\n")
                p.stdin.flush()
                answer=json.loads(p.stdout.readline())
                self.assertEqual(answer["id"],i)
                if method=="tools/call":
                    self.assertIn("U01",answer["result"]["content"][0]["text"])
            self.assertIsNone(p.poll(), "Server exited between MCP requests")
        finally:
            p.stdin.close()
            p.wait(timeout=10)
            p.stdout.close()
            p.stderr.close()
        self.assertEqual(p.returncode,0)

if __name__=="__main__":
    unittest.main()

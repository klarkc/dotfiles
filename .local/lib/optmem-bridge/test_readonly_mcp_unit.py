#!/usr/bin/env python3
"""Hermetic MCP regressions; no host memory, daemon or model."""
import unittest
import readonly_mcp as m
class FakeReader(m.MCPReader):
    def __init__(self):self.calls=[]
    def upstream(self,op,*args):
        self.calls.append((op,*args))
        return {"wake":"#0 synthetic\nYou are awake.\n",
                "recall":"#0 synthetic\n1 match.\n",
                "zoom":"#0 left\n#1 right\n"}[op]
def msg(method,params=None,id=1):
    q={"jsonrpc":"2.0","id":id,"method":method}
    if params is not None:q["params"]=params
    return q
class Protocol(unittest.TestCase):
    def setUp(self):self.r=FakeReader()
    def test_handshake(self):
        x=self.r.dispatch(msg("initialize"))
        self.assertEqual(x["result"]["serverInfo"]["name"],"optmem-stage-readonly")
        self.assertIsNone(self.r.dispatch({"jsonrpc":"2.0","method":"notifications/initialized"}))
        self.assertEqual(self.r.dispatch(msg("ping"))["result"],{})
    def test_three_read_tools(self):
        tools=self.r.dispatch(msg("tools/list"))["result"]["tools"]
        self.assertEqual({x["name"] for x in tools},{"optmem_wake","optmem_recall","optmem_zoom"})
        self.assertTrue(all(x["inputSchema"]["additionalProperties"] is False for x in tools))
    def test_wake(self):
        r=self.r.dispatch(msg("tools/call",{"name":"optmem_wake","arguments":{"part":1}}))
        self.assertFalse(r["result"]["isError"])
        self.assertIn("You are awake.",r["result"]["content"][0]["text"])
        self.assertEqual(self.r.calls,[("wake","1")])
    def test_recall_literal_and_zoom(self):
        self.r.read("optmem_recall",{"query":"safe.test"})
        self.r.read("optmem_zoom",{"block":"0-1"})
        self.assertEqual(self.r.calls,[("recall","safe\\.test"),("zoom","0-1")])
    def test_reject_every_unsafe_operation(self):
        for name,params in [
            ("optmem_note",{"text":"test"}),("optmem_nap",{"text":"summary"}),
            ("optmem_wake",{"part":True}),("optmem_wake",{"part":0}),
            ("optmem_recall",{"query":".*"}),("optmem_recall",{"query":"token\nmore"}),
            ("optmem_zoom",{"block":"../../etc"}),("optmem_zoom",{"block":"2-2"}),
            ("optmem_zoom",{"block":"0-1048576"}),
            ("optmem_recall",{"query":"U01","path":"/etc/passwd"})
        ]:
            with self.subTest(name=name,params=params):
                r=self.r.dispatch(msg("tools/call",{"name":name,"arguments":params}))
                self.assertTrue(r["result"]["isError"])
        self.assertEqual(self.r.calls,[])
    def test_rpc_unsupported(self):
        self.assertEqual(self.r.dispatch(msg("resources/list"))["error"]["code"],-32601)
        self.assertEqual(self.r.dispatch({"jsonrpc":"1.0","id":1,"method":"tools/list"})["error"]["code"],-32600)
        self.assertEqual(self.r.dispatch({"jsonrpc":"2.0","id":[],"method":"tools/list"})["error"]["code"],-32600)
if __name__=="__main__":unittest.main()

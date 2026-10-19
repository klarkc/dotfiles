#!/usr/bin/env python3
"""Actual stdio MCP and writer integration, without an external LLM."""
from __future__ import annotations
import concurrent.futures
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import unittest
import uuid

import agent_mcp as m

w=m.w
ROOT=w.HERE/"synthetic-cases"

def rpc(name,args,identifier=1):
    return {"jsonrpc":"2.0","id":identifier,"method":"tools/call",
            "params":{"name":name,"arguments":args}}

class AgentMCPTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        ROOT.mkdir(mode=0o700,exist_ok=True)

    def setUp(self):
        self.root=ROOT/("g4k-agent-mcp-"+uuid.uuid4().hex[:12])
        self.root.mkdir(mode=0o700)
        self.memory=self.root/"memory"
        self.sidecar=self.root/"sidecar"
        self.memory.mkdir(mode=0o700)
        env={**os.environ,"MEMORY_DIR":str(self.memory),"PYTHONDONTWRITEBYTECODE":"1"}
        proc=subprocess.run([sys.executable,"-B",str(w.MEMO),"init"],env=env,
                            capture_output=True,text=True,timeout=15)
        self.assertEqual(proc.returncode,0,proc.stderr)

    def tearDown(self):
        shutil.rmtree(self.root)

    def store(self,api,key="workflow.worktree",text="Always use a dedicated worktree for changes"):
        return api.dispatch(rpc("optmem_store",{"memory_key":key,"text":text}))

    def test_auto_admission_cross_agent_repeat(self):
        a=m.StageAgentMCP(self.memory,self.sidecar,"opencode")
        b=m.StageAgentMCP(self.memory,self.sidecar,"zeroclaw")
        self.assertEqual(len(a.dispatch({"jsonrpc":"2.0","id":1,
                       "method":"tools/list"})["result"]["tools"]),4)
        one=self.store(a)
        self.assertFalse(one["result"]["isError"])
        self.assertIn("STORED #0",one["result"]["content"][0]["text"])
        again=self.store(b)
        self.assertIn("ALREADY_PRESENT #0",again["result"]["content"][0]["text"])
        self.assertEqual(w.log_count(self.memory),1)
        found=b.dispatch(rpc("optmem_recall",{"query":"worktree"}))
        self.assertIn("worktree",found["result"]["content"][0]["text"].lower())
        wake=a.dispatch(rpc("optmem_wake",{"part":1}))
        self.assertFalse(wake["result"]["isError"])
        self.assertIn("You are awake.",wake["result"]["content"][0]["text"])

    def test_reused_key_contradiction_refused(self):
        a=m.StageAgentMCP(self.memory,self.sidecar,"opencode")
        self.store(a)
        bad=self.store(a,text="Always modify the main home tree directly")
        self.assertTrue(bad["result"]["isError"])
        self.assertEqual(w.log_count(self.memory),1)

    def test_alias_and_normalized_text(self):
        a=m.StageAgentMCP(self.memory,self.sidecar,"opencode")
        b=m.StageAgentMCP(self.memory,self.sidecar,"zeroclaw")
        self.store(a)
        out=self.store(b,"team.worktree","  ALWAYS  use a dedicated worktree for changes. ")
        self.assertIn("ALREADY_PRESENT #0",out["result"]["content"][0]["text"])
        self.assertEqual(w.log_count(self.memory),1)

    def test_no_origin_or_path_override(self):
        a=m.StageAgentMCP(self.memory,self.sidecar,"opencode")
        for data in [
            {"memory_key":"workflow.worktree","text":"Always use a dedicated worktree for changes",
             "harness":"zeroclaw"},
            {"memory_key":"workflow.worktree","text":"Always use a dedicated worktree for changes",
             "path":"/etc/passwd"},
            {"memory_key":"workflow.worktree","text":"API token is private"},
        ]:
            with self.subTest(data=list(data)):
                out=a.dispatch(rpc("optmem_store",data))
                self.assertTrue(out["result"]["isError"])
        self.assertEqual(w.log_count(self.memory),0)

    def test_cross_process_stdio_competition(self):
        def runner(harness):
            cmd=[sys.executable,"-B",str(Path(m.__file__)),"--harness",harness,
                 "--memory-dir",str(self.memory),"--sidecar",str(self.sidecar)]
            msgs=[
               {"jsonrpc":"2.0","id":1,"method":"initialize","params":{}},
               rpc("optmem_store",{"memory_key":"tools.shared-policy",
                   "text":"Always preserve the durable source of truth"},2),
            ]
            payload="".join(json.dumps(x)+"\n" for x in msgs)
            p=subprocess.run(cmd,input=payload,text=True,capture_output=True,
                             timeout=25,env={**os.environ,"PYTHONDONTWRITEBYTECODE":"1"})
            self.assertEqual(p.returncode,0,p.stderr)
            lines=[json.loads(x) for x in p.stdout.splitlines()]
            self.assertEqual(len(lines),2)
            self.assertFalse(lines[1]["result"]["isError"])
            return lines[1]["result"]["content"][0]["text"]
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
            results=list(pool.map(runner,("opencode","zeroclaw")))
        self.assertEqual(sum(x.startswith("STORED") for x in results),1)
        self.assertEqual(sum(x.startswith("ALREADY_PRESENT") for x in results),1)
        self.assertEqual(w.log_count(self.memory),1)

    def test_only_staging_paths(self):
        with self.assertRaises(ValueError):
            m.StageAgentMCP(Path.home()/".optmem/memory",self.sidecar,"opencode")
        with self.assertRaises(ValueError):
            m.StageAgentMCP(self.memory,self.sidecar,"gptweb")

if __name__=="__main__":
    unittest.main()

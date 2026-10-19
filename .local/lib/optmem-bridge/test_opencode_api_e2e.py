#!/usr/bin/env python3
"""Offline real OpenCode server MCP tool dispatch through documented HTTP API."""
from __future__ import annotations
from http.server import ThreadingHTTPServer
from pathlib import Path
import argparse
import json
import os
import shutil
import socket
import subprocess
import threading
import time
import urllib.request

from test_zeroclaw_store_e2e import AgentMock


def api(uri, data=None):
    req=urllib.request.Request(uri,
         data=None if data is None else json.dumps(data).encode(),
         headers={"Content-Type":"application/json"})
    with urllib.request.urlopen(req,timeout=6) as response:
        raw=response.read(2000000)
    return json.loads(raw) if raw else None


def main():
    cli=argparse.ArgumentParser()
    for key in ("opencode","server","memory","sidecar","output"):
        cli.add_argument("--"+key,required=True,type=Path)
    cli.add_argument("--production",action="store_true")
    o=cli.parse_args()
    root=o.output
    if root.exists():raise FileExistsError("Existing E2E output")
    root.mkdir(mode=0o700)
    for sub in ("config","data","state","cache","workspace"):
        (root/sub).mkdir(mode=0o700)
    public=Path.home()/".cache/opencode/models.json"
    if public.is_file():
        (root/"cache/opencode").mkdir(mode=0o700)
        shutil.copyfile(public,root/"cache/opencode/models.json")
    AgentMock.events=[]
    AgentMock.search_first=True
    AgentMock.memory_key=("optmem.production.integration" if o.production else "workflow.remote")
    AgentMock.memory_text=("OptMem production on solo098 uses one shared canonical writer"
                           if o.production else "Prefer isolated local agent staging")
    AgentMock.search_term=("OptMem production" if o.production else "isolated local agent")
    model=ThreadingHTTPServer(("127.0.0.1",0),AgentMock)
    t=threading.Thread(target=model.serve_forever,daemon=True)
    t.start()
    process=None
    try:
        config={
         "$schema":"https://opencode.ai/config.json",
         "model":"vllm/g4i-local-mock",
         "provider":{"vllm":{"npm":"@ai-sdk/openai-compatible",
            "name":"OptMem Private Mock",
            "options":{"baseURL":f"http://127.0.0.1:{model.server_address[1]}/v1",
                       "apiKey":AgentMock.expected_key},
            "models":{"g4i-local-mock":{"name":"Mock","limit":{"context":8192,"output":512}}}}},
         "mcp":{"optmem":{"type":"local","enabled":True,"timeout":30000,
             "command":["/usr/bin/python3","-B",str(o.server),"--harness","opencode",
                        "--memory-dir",str(o.memory),"--sidecar",str(o.sidecar)]
                       +(["--production"] if o.production else [])}}}
        cfg=root/"config/opencode.json"
        cfg.write_text(json.dumps(config)+"\n")
        cfg.chmod(0o600)
        env={**os.environ,
         "OPENCODE_CONFIG":str(cfg),"OPENCODE_CONFIG_DIR":str(root/"config"),
         "XDG_CONFIG_HOME":str(root/"config"),"XDG_DATA_HOME":str(root/"data"),
         "XDG_STATE_HOME":str(root/"state"),"XDG_CACHE_HOME":str(root/"cache"),
         "NO_PROXY":"localhost,127.0.0.1"}
        with socket.socket() as sock:
            sock.bind(("127.0.0.1",0))
            port=sock.getsockname()[1]
        logfile=(root/"server.log").open("w")
        process=subprocess.Popen([str(o.opencode),"serve","--pure","--hostname",
                  "127.0.0.1","--port",str(port)],
                  env=env,cwd=root/"workspace",stdout=logfile,stderr=subprocess.STDOUT)
        base=f"http://127.0.0.1:{port}"
        for _ in range(90):
            if process.poll() is not None:
                raise RuntimeError("OpenCode server exit")
            try:
                if api(base+"/global/health")["healthy"]:break
            except (OSError,KeyError):
                time.sleep(0.15)
        else:raise RuntimeError("OpenCode server unhealthy")
        sid=api(base+"/session",{"title":"OptMem offline tool E2E"})["id"]
        prompt=("Use optmem_search, then optmem_store with memory_key "
                +AgentMock.memory_key+" and text "+AgentMock.memory_text
                +". Return STORE_VERIFIED only after tool confirmation.")
        api(base+f"/session/{sid}/prompt_async",{
          "model":{"providerID":"vllm","modelID":"g4i-local-mock"},
          "parts":[{"type":"text","text":prompt}]})
        deadline=time.monotonic()+38
        while time.monotonic()<deadline:
            if any(e.get("matched_STORE") is True for e in AgentMock.events):break
            time.sleep(0.2)
        events=AgentMock.events
        result={"server_healthy":True,"session_created":sid.startswith("ses"),
            "provider_request_seen":any(e["phase"]=="provider_request" for e in events),
            "model_requested_search":any(e["phase"]=="model_requested_search" for e in events),
            "model_requested_store":any(e["phase"]=="model_requested_tool" for e in events),
            "tool_result_seen":any(e.get("matched_STORE") is True for e in events),
            "mock_only":True,"production":o.production}
        proof=root/"result.json"
        proof.write_text(json.dumps(result,sort_keys=True)+"\n")
        proof.chmod(0o600)
        print(json.dumps(result,sort_keys=True))
        if not all((result["server_healthy"],result["session_created"],
                    result["provider_request_seen"],result["model_requested_store"],
                    result["tool_result_seen"])):
            raise RuntimeError("OpenCode live MCP E2E incomplete")
    finally:
        if process and process.poll() is None:
            process.terminate()
            try:process.wait(timeout=4)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=4)
        model.shutdown()
        model.server_close()
        t.join(timeout=3)


if __name__=="__main__":
    main()

#!/usr/bin/env python3
"""Offline real OpenCode binary and OptMem MCP tool-call smoke."""
from __future__ import annotations
from http.server import ThreadingHTTPServer
from pathlib import Path
import argparse
import json
import os
import secrets
import shutil
import subprocess
import threading

from test_zeroclaw_store_e2e import AgentMock

def main():
    cli=argparse.ArgumentParser()
    for key in ("opencode","server","memory","sidecar","output"):
        cli.add_argument("--"+key,required=True,type=Path)
    cli.add_argument("--production",action="store_true")
    options=cli.parse_args()
    if options.output.exists():
        raise FileExistsError("Never overwrite earlier E2E")
    output=options.output
    output.mkdir(mode=0o700)
    for folder in ("config","state","data","cache","workspace"):
        (output/folder).mkdir(mode=0o700)
    # Use the public cached model registry to avoid an unrelated network fetch
    # at OpenCode session initialization. No credentials are copied.
    models=Path.home()/".cache/opencode/models.json"
    if models.is_file():
        dest=output/"cache/opencode/models.json"
        dest.parent.mkdir(mode=0o700,parents=True,exist_ok=True)
        shutil.copyfile(models,dest)
        dest.chmod(0o600)
    AgentMock.events=[]
    AgentMock.search_first=True
    if options.production:
        AgentMock.memory_key="optmem.production.integration"
        AgentMock.memory_text="OptMem production on solo098 uses one shared canonical writer"
        AgentMock.search_term="OptMem production"
    model=ThreadingHTTPServer(("127.0.0.1",0),AgentMock)
    thread=threading.Thread(target=model.serve_forever,daemon=True)
    thread.start()
    try:
        uri=f"http://127.0.0.1:{model.server_address[1]}/v1"
        config={
            "$schema":"https://opencode.ai/config.json",
            "model":"vllm/g4i-local-mock",
            "provider":{"vllm":{"npm":"@ai-sdk/openai-compatible",
                "name":"Private OptMem E2E",
                "options":{"baseURL":uri,"apiKey":AgentMock.expected_key},
                "models":{"g4i-local-mock":{"name":"Smoke","limit":{"context":8192,"output":512}}}}},
            "mcp":{"optmem":{"type":"local","enabled":True,"timeout":30000,
                "command":["/usr/bin/python3","-B",str(options.server),
                   "--harness","opencode","--memory-dir",str(options.memory),
                   "--sidecar",str(options.sidecar)]
                   + (["--production"] if options.production else [])}},
        }
        config_file=output/"config/opencode.json"
        config_file.write_text(json.dumps(config,sort_keys=True)+"\n")
        config_file.chmod(0o600)
        env=os.environ.copy()
        env.update({
            "OPENCODE_CONFIG":str(config_file),
            "OPENCODE_CONFIG_DIR":str(output/"config"),
            "XDG_CONFIG_HOME":str(output/"config"),
            "XDG_DATA_HOME":str(output/"data"),
            "XDG_STATE_HOME":str(output/"state"),
            "XDG_CACHE_HOME":str(output/"cache"),
            "PYTHONDONTWRITEBYTECODE":"1",
            "NO_COLOR":"1",
            "NO_PROXY":"localhost,127.0.0.1",
        })
        cmd=[str(options.opencode),"run","--pure","--model","vllm/g4i-local-mock",
             "--format","json","--title","OptMem store shared MCP test",
             "--dir",str(output/"workspace"),
             f"Search for OptMem memory using optmem_search, then call optmem_store with memory_key {AgentMock.memory_key} and text {AgentMock.memory_text}. Say STORE_VERIFIED only if the tool confirmed success."]
        try:
            proc=subprocess.run(cmd,cwd=output,env=env,text=True,
                 capture_output=True,timeout=30)
        except subprocess.TimeoutExpired:
            print(json.dumps({"status":"TIMEOUT","seen_events":len(AgentMock.events)}))
            raise
    finally:
        model.shutdown()
        model.server_close()
        thread.join(timeout=3)
    events=AgentMock.events
    result={"exit":proc.returncode,
            "provider_seen":any(e["phase"]=="provider_request" for e in events),
            "model_requested_search":any(e["phase"]=="model_requested_search" for e in events),
            "model_requested_store":any(e["phase"]=="model_requested_tool" for e in events),
            "tool_result_received":any(e.get("matched_STORE") is True for e in events),
            "answer_verified":"STORE_VERIFIED" in proc.stdout,
            "mock_only":True,"production":options.production}
    (output/"result.json").write_text(json.dumps(result,sort_keys=True)+"\n")
    (output/"result.json").chmod(0o600)
    print(json.dumps(result,sort_keys=True))
    if not all((result["exit"]==0,result["provider_seen"],
                result["model_requested_store"],result["tool_result_received"],
                result["answer_verified"])):
        print("stderr_tail_class:",proc.stderr[-180:].replace(str(output),"[E2E]"))
        raise SystemExit(1)

if __name__=="__main__":
    main()

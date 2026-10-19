#!/usr/bin/env python3
"""Real OpenCode MCP dispatch through a loopback mock model; no cloud model."""
from __future__ import annotations
import argparse
from http.server import ThreadingHTTPServer
import json
import os
from pathlib import Path
import subprocess
import secrets
import threading
from test_zeroclaw_e2e import AgentMock, HASH

def main():
    args=argparse.ArgumentParser()
    for name in ("opencode","server","memo","memory","output"):
        args.add_argument("--"+name,required=True,type=Path)
    opt=args.parse_args()
    if opt.output.exists():raise FileExistsError("No overwriting test evidence")
    opt.output.mkdir(mode=0o700)
    config=opt.output/"config"
    config.mkdir(mode=0o700)
    for sub in ("data","state","cache","workspace"):
        (opt.output/sub).mkdir(mode=0o700)
    AgentMock.events=[]
    model=ThreadingHTTPServer(("127.0.0.1",0),AgentMock)
    thread=threading.Thread(target=model.serve_forever,daemon=True)
    thread.start()
    try:
        base=f"http://127.0.0.1:{model.server_address[1]}/v1"
        config_file=config/"opencode.json"
        test_api_key=secrets.token_urlsafe(24)
        cfg={"$schema":"https://opencode.ai/config.json",
             "model":"localmock/g4i-local-mock",
             "provider":{"localmock":{
                "npm":"@ai-sdk/openai-compatible",
                "name":"OptMem Local Synthetic Model",
                "options":{"baseURL":base,"apiKey":test_api_key},
                "models":{"g4i-local-mock":{"name":"OptMem Probe","limit":{"context":8192,"output":512}}}}},
             "mcp":{"optmem_stage":{"type":"local","enabled":True,
               "command":["/usr/bin/python3","-B",str(opt.server),
                   "--memory-dir",str(opt.memory),"--memo",str(opt.memo),
                   "--expected-log-sha256",HASH]}}}
        config_file.write_text(json.dumps(cfg,indent=2))
        config_file.chmod(0o600)
        env=os.environ.copy()
        env.update({
            "OPENCODE_CONFIG_DIR":str(config),
            "OPENCODE_CONFIG":str(config_file),
            "XDG_CONFIG_HOME":str(config),
            "XDG_DATA_HOME":str(opt.output/"data"),
            "XDG_STATE_HOME":str(opt.output/"state"),
            "XDG_CACHE_HOME":str(opt.output/"cache"),
            "PYTHONDONTWRITEBYTECODE":"1",
            "NO_COLOR":"1",
            "HTTP_PROXY":"http://127.0.0.1:9",
            "HTTPS_PROXY":"http://127.0.0.1:9",
            "NO_PROXY":"localhost,127.0.0.1",
        })
        cmd=[str(opt.opencode),"run","--model","localmock/g4i-local-mock",
             "--format","json","--title","G4I local mock read acceptance",
             "--dir",str(opt.output/"workspace"),
             "Call the tool optmem_stage_optmem_recall with query U01. Return FOUND_U01 only when the tool output includes U01."]
        try:p=subprocess.run(cmd,capture_output=True,text=True,env=env,timeout=40,cwd=opt.output)
        except subprocess.TimeoutExpired:
            status={"opencode_e2e":"TIMEOUT","seconds":40,
                    "model_requests_seen":len(AgentMock.events),
                    "canonical_or_native_memory_modified":False}
            out=opt.output/"result.json"
            out.write_text(json.dumps(status,indent=2)+"\n")
            out.chmod(0o600)
            print(json.dumps(status))
            return 3
    finally:
        model.shutdown()
        model.server_close()
        thread.join(timeout=3)
    events=AgentMock.events
    model_requests=[x for x in events if x["phase"]=="provider_request"]
    relevant=[x for x in events if x["phase"] in ("model_requested_tool","tool_result_seen_by_model")]
    report={
        "opencode_exit_code":p.returncode,
        "provider_requests":len(model_requests),
        "optmem_in_model_tools":any(any("optmem" in str(t) for t in x["tool_names"]) for x in model_requests),
        "tool_call_generated":any(x["phase"]=="model_requested_tool" for x in relevant),
        "tool_response_received":any(x.get("matched_U01") is True for x in relevant),
        "opencode_answer_found":"FOUND_U01" in p.stdout,
        "events":events,
        "mock_only":True,"external_cloud_model_called":False,
        "native_or_canonical_memory_modified":False
    }
    target=opt.output/"result.json";target.write_text(json.dumps(report,indent=2)+"\n");target.chmod(0o600)
    print(json.dumps({k:v for k,v in report.items() if k!="events"}))
    if p.returncode or not report["tool_response_received"]:
        # Do not print raw tool/memory content or tokens.
        print("ERROR_CLASS",p.stderr[-400:].replace(str(opt.output),"[staging]"))
        return 4
    return 0

if __name__=="__main__":
    raise SystemExit(main())

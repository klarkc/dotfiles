#!/usr/bin/env python3
"""Real ZeroClaw executable + real OptMem MCP; local deterministic model mock.

Offline test of MCP dispatch. Does not validate real model inference or production
daemon configuration. All config/state stays in a private test directory.
"""
from __future__ import annotations
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import argparse
import json
import os
import re
import secrets
import subprocess
import threading

HASH="78eafd77c8c0c4eb67fcd1468bd758500c090b0d979cc0b4eccd2f6e34a2f565"

class AgentMock(BaseHTTPRequestHandler):
    events=[]
    expected_key=secrets.token_urlsafe(24)
    def log_message(self,*args):pass

    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type","application/json")
        self.end_headers()
        self.wfile.write(json.dumps({"object":"list","data":[{"id":"g4i-local-mock","object":"model"}]}).encode())

    def do_POST(self):
        n=int(self.headers.get("Content-Length","0"))
        if n>262144 or n<2:
            self.send_error(413)
            return
        try:payload=json.loads(self.rfile.read(n))
        except ValueError:
            self.send_error(400)
            return
        header=self.headers.get("Authorization","")
        AgentMock.events.append({"phase":"authorization",
            "bearer_prefix_present":header.startswith("Bearer "),
            "expected_bearer_value":header == "Bearer "+AgentMock.expected_key})
        messages=payload.get("messages") or []
        tools=[(x.get("function") or {}).get("name") for x in payload.get("tools",[]) if isinstance(x,dict)]
        # A local model mock must prove the MCP reader is present in the actual
        # agent's model-tool schema, not pretend a tool succeeded.
        AgentMock.events.append({"phase":"provider_request","path":self.path,
              "tool_names":tools,"message_roles":[x.get("role") for x in messages]})
        target=next((t for t in tools if t and "optmem" in t and "recall" in t),None)
        already=any(x.get("role")=="tool" for x in messages)
        if not already and target:
            message={"role":"assistant","content":None,"tool_calls":[
                {"id":"call_optmem_stage_1","type":"function",
                 "function":{"name":target,"arguments":json.dumps({"query":"U01"})}}]}
            reason="tool_calls"
            AgentMock.events.append({"phase":"model_requested_tool","name":target})
        else:
            matches=any(x.get("role")=="tool" and "U01" in str(x.get("content",""))
                        for x in messages)
            AgentMock.events.append({"phase":"tool_result_seen_by_model","matched_U01":matches})
            message={"role":"assistant","content":"FOUND_U01" if matches else "NOT_FOUND"}
            reason="stop"
        if payload.get("stream"):
            self.send_response(200)
            self.send_header("Content-Type","text/event-stream")
            self.end_headers()
            delta={"role":"assistant"}
            if "tool_calls" in message:
                tool=message["tool_calls"][0]
                delta["tool_calls"]=[{"index":0,**tool}]
            else:delta["content"]=message["content"]
            for choice in ({"index":0,"delta":delta,"finish_reason":None},
                           {"index":0,"delta":{},"finish_reason":reason}):
                chunk={"id":"chatcmpl-g4i","object":"chat.completion.chunk","model":"g4i-local-mock",
                       "choices":[choice]}
                self.wfile.write(("data: "+json.dumps(chunk)+"\n\n").encode())
            self.wfile.write(b"data: [DONE]\n\n")
        else:
            self.send_response(200)
            self.send_header("Content-Type","application/json")
            self.end_headers()
            answer={"id":"chatcmpl-g4i","object":"chat.completion",
                    "model":"g4i-local-mock","choices":[{"index":0,"message":message,
                    "finish_reason":reason}],"usage":{"prompt_tokens":12,"completion_tokens":9,"total_tokens":21}}
            self.wfile.write(json.dumps(answer).encode())

def main():
    cli=argparse.ArgumentParser()
    cli.add_argument("--negative-no-bundle",action="store_true")
    cli.add_argument("--zeroclaw",required=True,type=Path)
    cli.add_argument("--server",required=True,type=Path)
    cli.add_argument("--memo",required=True,type=Path)
    cli.add_argument("--memory",required=True,type=Path)
    cli.add_argument("--output",required=True,type=Path)
    args=cli.parse_args()
    if args.output.exists():
        raise FileExistsError("Never overwrite previous harness evidence")
    args.output.mkdir(mode=0o700)
    server=ThreadingHTTPServer(("127.0.0.1",0),AgentMock)
    thread=threading.Thread(target=server.serve_forever,daemon=True)
    thread.start()
    try:
        config=args.output/"config"
        config.mkdir(mode=0o700)
        uri=f"http://127.0.0.1:{server.server_address[1]}/v1"
        command=json.dumps(["-B",str(args.server),"--memory-dir",str(args.memory),
                            "--memo",str(args.memo),"--expected-log-sha256",HASH])
        conf=f'''schema_version=3

[providers.models.vllm.default]
base_url="{uri}"
uri="{uri}"
model="g4i-local-mock"
name="vllm"
timeout_secs=15
requires_openai_auth=false

[agents.default]
model_provider="vllm.default"
risk_profile="default"
runtime_profile="default"
mcp_bundles={json.dumps([] if args.negative_no_bundle else ["optmem_only"])}

[mcp_bundles.optmem_only]
servers=["optmem_stage"]

[risk_profiles.default]
allowed_commands=[]
auto_approve=["optmem_stage__optmem_wake","optmem_stage__optmem_recall","optmem_stage__optmem_zoom"]
excluded_tools=["shell","file_write","file_edit"]

[runtime_profiles.default]
max_tool_iterations=3
max_context_tokens=8192

[memory]
backend="none"
auto_save=false

[skills]
open_skills_enabled=false

[mcp]
enabled=true
deferred_loading=false

[[mcp.servers]]
name="optmem_stage"
transport="stdio"
command="/usr/bin/python3"
args={command}
tool_timeout_secs=15
'''
        dest=config/"config.toml"
        dest.write_text(conf)
        dest.chmod(0o600)
        env=os.environ.copy()
        env["VLLM_API_KEY"]=AgentMock.expected_key
        env["ZEROCLAW_providers__models__vllm__default__api_key"]=AgentMock.expected_key
        env["ZEROCLAW_API_KEY"]=AgentMock.expected_key
        env["API_KEY"]=AgentMock.expected_key
        env["PYTHONDONTWRITEBYTECODE"]="1"
        p=subprocess.run([str(args.zeroclaw),"--verbose","--log-level","debug","--config-dir",str(config),
             "agent","--agent","default","--message",
             "Use the optmem_stage__optmem_recall tool with query U01. Return FOUND_U01 if it was present."],
             cwd=args.output,env=env,capture_output=True,text=True,timeout=35)
    finally:
        server.shutdown()
        server.server_close()
        thread.join(timeout=4)
    events=AgentMock.events
    output={"zero_claw_exit":p.returncode,
            "saw_provider_request":any(x["phase"]=="provider_request" for x in events),
            "provider_tool_schema_seen":any(any("optmem" in str(n) for n in x.get("tool_names",[]))
                 for x in events if x["phase"]=="provider_request"),
            "model_requested_tool":any(x["phase"]=="model_requested_tool" for x in events),
            "actual_tool_result_returned":any(x.get("matched_U01") is True for x in events),
            "last_stdout_contains_found": "FOUND_U01" in p.stdout,
            "used_local_mock_only":True,
            "negative_no_bundle":args.negative_no_bundle,
            "expected_bearer_sent":all(v.get("expected_bearer_value") for v in events if v["phase"]=="authorization"),
            "events":events,
            "stderr_class": ("none" if p.returncode==0 else "AGENT_ERROR"),
            "no_production_services_or_native_memories_touched":True}
    out=args.output/"result.json"
    out.write_text(json.dumps(output,indent=2)+"\n")
    out.chmod(0o600)
    print(json.dumps({k:v for k,v in output.items() if k not in ("events","stderr_class")}))
    if args.negative_no_bundle:
        success=output["zero_claw_exit"]==0 and not output["provider_tool_schema_seen"] and not output["model_requested_tool"]
    else:
        success=all((output["zero_claw_exit"]==0,output["provider_tool_schema_seen"],
                     output["model_requested_tool"],output["actual_tool_result_returned"],
                     output["last_stdout_contains_found"],output["expected_bearer_sent"]))
    if not success:
        raise AssertionError("Real ZeroClaw MCP integration not yet proved")

if __name__=="__main__":
    main()

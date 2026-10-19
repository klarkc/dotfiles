#!/usr/bin/env python3
"""OptMem stage-only MCP adapter for deliberate agent memories.

Harness identity is fixed at startup by a trusted MCP configuration, never by
model-supplied tool arguments. It writes via the *same* provenance-checked
upstream OptMem writer as Bash, sharing its serialization lock and ledger.
"""
from __future__ import annotations
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys

import agent_store
import readonly_mcp

w=agent_store.writer

STORE_SCHEMA={
    "name":"optmem_store",
    "description":(
        "Remember one durable fact automatically. First use optmem_search "
        "to discover existing memory facts and keys, then reuse an existing "
        "stable memory_key when semantically equivalent. The writer refuses "
        "conflicting updates and deduplicates normalized content globally."
    ),
    "inputSchema":{
        "type":"object",
        "properties":{
            "memory_key":{"type":"string","minLength":3,"maxLength":90,
                          "pattern":"^[a-z0-9][a-z0-9._/-]{2,89}$",
                          "description":"Stable topic identity shared with other harnesses, e.g. workflow.worktrees"},
            "text":{"type":"string","minLength":12,"maxLength":280,
                    "description":"One fact, short single line; omit secrets."},
        },
        "required":["memory_key","text"],
        "additionalProperties":False,
    },
}

LOOKUP_SCHEMA={
    "name":"optmem_lookup_key",
    "description":"Find a globally shared stable memory_key before storing. A match returns the existing upstream OptMem text and index; NOT_FOUND never creates a memory.",
    "inputSchema":{
        "type":"object",
        "properties":{"memory_key":{"type":"string","minLength":3,"maxLength":90,
                    "pattern":"^[a-z0-9][a-z0-9._/-]{2,89}$"}},
        "required":["memory_key"],"additionalProperties":False,
    },
}


SEARCH_SCHEMA={
    "name":"optmem_search",
    "description":(
        "Before choosing a NEW memory_key, search existing global OptMem facts "
        "and aliases using short literal terms. Results are suggestions: compare "
        "meaning and reuse the key when appropriate. Unkeyed baseline entries "
        "can be bound using the same original text. No search creates a memory."
    ),
    "inputSchema":{
        "type":"object",
        "properties":{"query":{"type":"string","minLength":2,"maxLength":64}},
        "required":["query"],"additionalProperties":False,
    },
}

class StageReader:
    read=readonly_mcp.MCPReader.read

    def __init__(self,memory:Path,sidecar:Path):
        self.memory=memory
        self.sidecar=sidecar
        w.verify(memory,sidecar)

    def upstream(self,operation:str,*args:str) -> str:
        w.verify(self.memory,self.sidecar)
        env=os.environ.copy()
        env["MEMORY_DIR"]=str(self.memory)
        env["PYTHONDONTWRITEBYTECODE"]="1"
        proc=subprocess.run([sys.executable,"-B",str(w.MEMO),operation,*args],
                            env=env,text=True,capture_output=True,timeout=18)
        if proc.returncode!=0 or len(proc.stdout.encode())>readonly_mcp.MAX_OUTPUT:
            raise RuntimeError("Stage memory read incomplete or exceeds transport budget")
        w.verify(self.memory,self.sidecar)
        return proc.stdout


class StageAgentMCP:
    def __init__(self,memory:Path,sidecar:Path,harness:str):
        if harness not in agent_store.ALLOWED:
            raise ValueError("Only known agent clients supported")
        self.memory,self.sidecar=self.checked_stage(memory,sidecar)
        self.harness=harness
        self.reader=StageReader(self.memory,self.sidecar)

    @staticmethod
    def checked_stage(memory:Path,sidecar:Path) -> tuple[Path,Path]:
        root=w.HERE/"synthetic-cases"
        if (memory.parent.is_symlink() or sidecar.parent.is_symlink()
            or not memory.is_absolute() or not sidecar.is_absolute()
            or memory.is_symlink() or sidecar.is_symlink()
            or memory.name!="memory" or sidecar.name!="sidecar"
            or memory.parent!=sidecar.parent or
            not memory.parent.name.startswith("g4k-agent-")
            or memory.parent.resolve().parent!=root.resolve()):
            raise ValueError("Only paired synthetic G4K stage roots allowed")
        # A nonempty LOG without either sidecar DB is NOT a fresh stage: it may
        # be an incomplete restore. Do not silently enroll controlled records
        # as unaudited historical baseline or lose aliases/request receipts.
        log=memory/"LOG.txt"
        if log.is_file() and log.stat().st_size:
            for name in ("provenance.sqlite3","agent-facts.sqlite3"):
                file=sidecar/name
                if (file.is_symlink() or not file.is_file()
                    or file.stat().st_uid!=os.getuid()
                    or file.stat().st_mode&0o077):
                    raise ValueError("Nonempty stage requires both sealed writer and agent indices")
        return w.private_dir(memory),w.private_dir(sidecar)

    def dispatch(self,request:object) -> dict|None:
        if not isinstance(request,dict) or request.get("jsonrpc")!="2.0":
            return {"jsonrpc":"2.0","id":None,
                    "error":{"code":-32600,"message":"Invalid JSON-RPC request"}}
        ident=request.get("id")
        if isinstance(ident,(list,dict,bool)):
            return {"jsonrpc":"2.0","id":None,
                    "error":{"code":-32600,"message":"Invalid request ID"}}
        if ident is None:return None
        method=request.get("method")
        if method=="initialize":
            return {"jsonrpc":"2.0","id":ident,"result":{
                "protocolVersion":"2025-03-26",
                "capabilities":{"tools":{"listChanged":False}},
                "serverInfo":{"name":"optmem-agent-stage","version":"0.1.0"}}}
        if method=="ping":
            return {"jsonrpc":"2.0","id":ident,"result":{}}
        if method=="tools/list":
            return {"jsonrpc":"2.0","id":ident,
                    "result":{"tools":[*readonly_mcp.SCHEMAS,SEARCH_SCHEMA,LOOKUP_SCHEMA,STORE_SCHEMA]}}
        if method=="tools/call":
            params=request.get("params")
            try:
                # MCP clients may attach protocol-owned _meta to tools/call.
                # It is never forwarded to the writer or treated as authority.
                if (not isinstance(params,dict)
                    or not {"name","arguments"}<=set(params)
                    or set(params)-{"name","arguments","_meta"}
                    or ("_meta" in params and not isinstance(params["_meta"],dict))):
                    raise ValueError("Invalid MCP tool call")
                name=params["name"]
                args=params["arguments"]
                if not isinstance(args,dict):
                    raise ValueError("Invalid tool arguments")
                if name=="optmem_search":
                    if set(args)!={"query"}:
                        raise ValueError("Only bounded literal memory search permitted")
                    outcome=agent_store.search_memories(self.memory,self.sidecar,args["query"])
                    content=json.dumps(outcome,sort_keys=True,ensure_ascii=False)
                elif name=="optmem_lookup_key":
                    if set(args)!={"memory_key"}:
                        raise ValueError("Only fixed memory_key lookup permitted")
                    outcome=agent_store.lookup_key(self.memory,self.sidecar,args["memory_key"])
                    content=json.dumps(outcome,sort_keys=True,ensure_ascii=False)
                elif name=="optmem_store":
                    if set(args)!={"memory_key","text"}:
                        raise ValueError("Only fixed memory_key and text permitted")
                    key=args["memory_key"]
                    text=args["text"]
                    rid=agent_store.stable_request_id(self.harness,key,text)
                    outcome=agent_store.request_store(
                        self.memory,self.sidecar,self.harness,key,text,rid)
                    content=outcome["status"]+" #"+str(outcome["index"])
                else:
                    content=self.reader.read(name,args)
            except (OSError,ValueError,RuntimeError,subprocess.TimeoutExpired,TypeError):
                return {"jsonrpc":"2.0","id":ident,"result":{
                    "content":[{"type":"text","text":"OptMem operation refused by stage safety gate"}],
                    "isError":True}}
            return {"jsonrpc":"2.0","id":ident,"result":{
                "content":[{"type":"text","text":content}],"isError":False}}
        return {"jsonrpc":"2.0","id":ident,
                "error":{"code":-32601,"message":"Method not found"}}


def main() -> int:
    cli=argparse.ArgumentParser()
    cli.add_argument("--harness",choices=sorted(agent_store.ALLOWED),required=True)
    cli.add_argument("--memory-dir",type=Path,required=True)
    cli.add_argument("--sidecar",type=Path,required=True)
    args=cli.parse_args()
    server=StageAgentMCP(args.memory_dir,args.sidecar,args.harness)
    for line in sys.stdin.buffer:
        if len(line)>readonly_mcp.MAX_INBOUND:
            continue
        try:
            request=json.loads(line.decode())
            response=server.dispatch(request)
            if response is not None:
                sys.stdout.write(json.dumps(response,separators=(",",":"))+"\n")
                sys.stdout.flush()
        except (ValueError,TypeError,UnicodeDecodeError):
            sys.stdout.write(json.dumps({
                "jsonrpc":"2.0","id":None,
                "error":{"code":-32700,"message":"Parse error"}})+"\n")
            sys.stdout.flush()
    return 0

if __name__=="__main__":
    raise SystemExit(main())

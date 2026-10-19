#!/usr/bin/env python3
"""Read-only, private OptMem MCP stdio bridge. Strictly stage-only until cutover.

Authoritative memory is the audited upstream OptMem LOG/TREE, never an MCP replica.
This process cannot expose note/nap/forget/config/import or select paths per request.
"""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

UPSTREAM_MEMO_SHA256 = "3dc120d01be3115ef6267eab4103e7909fc830d6227b549f20991ba999ee9ffb"
MAX_INBOUND = 12288
MAX_OUTPUT = 16000
MEMO_RECORD_SIZE = 320
VALID_TOOL_ARGS = {
    "optmem_wake": {"part"},
    "optmem_recall": {"query"},
    "optmem_zoom": {"block"},
}
SCHEMAS = [
    {
        "name": "optmem_wake",
        "description": "Read current shared OptMem context (staging only). No writes; call at session start.",
        "inputSchema": {"type": "object", "properties": {
            "part": {"type": "integer", "minimum": 1, "maximum": 16, "default": 1}
        }, "additionalProperties": False},
    },
    {
        "name": "optmem_recall",
        "description": "Retrieve exact historical memory matches (staging only). Literal terms, not regex.",
        "inputSchema": {"type": "object", "properties": {
            "query": {"type": "string", "minLength": 2, "maxLength": 64}
        }, "required": ["query"], "additionalProperties": False},
    },
    {
        "name": "optmem_zoom",
        "description": "Expand an OptMem tree block to its two halves (staging only).",
        "inputSchema": {"type": "object", "properties": {
            "block": {"type": "string", "pattern": "^[0-9]+-[0-9]+$"}
        }, "required": ["block"], "additionalProperties": False},
    },
]

def sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()

def check_safe_file(path: Path) -> None:
    if path.is_symlink() or not path.is_file() or path.stat().st_uid != os.getuid():
        raise RuntimeError("Missing or unsafe OptMem source")
    if path.stat().st_mode & 0o022:
        raise RuntimeError("OptMem source is writable by another account")

def check_memory(memory: Path, expected_sha: str) -> Path:
    # The only approved source during pre-cutover: the G4P test memory.
    if not memory.is_absolute() or memory.is_symlink():
        raise RuntimeError("Absolute non-symlink staging path required")
    resolved = memory.resolve(strict=True)
    if (resolved.name != "optmem-test-memory"
            or resolved.parent.name != "g4-authorized-local-20261015"
            or resolved.parent.parent.name != "g2b-persistent-20261014"):
        raise RuntimeError("Canonical or unapproved memory root refused")
    for directory in (resolved, resolved.parent):
        if directory.stat().st_uid != os.getuid() or directory.stat().st_mode & 0o077:
            raise RuntimeError("Unsafe private memory directory")
    log = resolved / "LOG.txt"
    check_safe_file(log)
    if log.stat().st_size % MEMO_RECORD_SIZE != 0:
        raise RuntimeError("Partial OptMem record refused")
    if sha(log) != expected_sha:
        raise RuntimeError("OptMem source hash changed; refuse stale reader")
    return resolved

class MCPReader:
    def __init__(self, memory: Path, memo: Path, expected_sha: str):
        if not re.fullmatch(r"[0-9a-f]{64}", expected_sha):
            raise RuntimeError("Expected SHA256 is mandatory")
        check_safe_file(memo)
        if sha(memo) != UPSTREAM_MEMO_SHA256:
            raise RuntimeError("Audited upstream memo source hash mismatch")
        self.memory = check_memory(memory, expected_sha)
        self.memo = memo
        self.expected_sha = expected_sha

    def upstream(self, operation: str, *arguments: str) -> str:
        check_memory(self.memory, self.expected_sha)
        env = os.environ.copy()
        env["MEMORY_DIR"] = str(self.memory)
        env["PYTHONDONTWRITEBYTECODE"] = "1"
        cmd = [sys.executable, "-B", str(self.memo), operation, *arguments]
        result = subprocess.run(cmd, env=env, capture_output=True, text=True, timeout=15)
        if result.returncode != 0:
            raise RuntimeError("OptMem read unavailable; refusing incomplete context")
        if len(result.stdout.encode("utf-8")) > MAX_OUTPUT:
            raise RuntimeError("OptMem read over transport budget; narrow query or page")
        return result.stdout

    def read(self, name: str, args: object) -> str:
        if name not in VALID_TOOL_ARGS or not isinstance(args, dict):
            raise ValueError("Unsupported read tool or arguments")
        if set(args) - VALID_TOOL_ARGS[name]:
            raise ValueError("Unknown parameters refused")
        if name == "optmem_wake":
            part = args.get("part", 1)
            if type(part) is not int or not 1 <= part <= 16:
                raise ValueError("Invalid wake part")
            output = self.upstream("wake", str(part))
            if "Cannot wake:" in output or "Not awake yet." in output:
                # Preserve failure rather than presenting truncated content as a full wake.
                raise RuntimeError("OptMem wake incomplete; summaries or paging required")
            if "You are awake." not in output:
                raise RuntimeError("OptMem did not acknowledge complete wake")
            return output
        if name == "optmem_recall":
            query = args.get("query")
            if not isinstance(query, str) or not re.fullmatch(r"[a-zA-Z0-9 _./-]{2,64}", query):
                raise ValueError("Literal query refused")
            return self.upstream("recall", re.escape(query))
        block = args.get("block")
        if not isinstance(block, str) or not re.fullmatch(r"[0-9]{1,6}-[0-9]{1,6}", block):
            raise ValueError("Invalid block")
        lo, hi = map(int, block.split("-"))
        if lo >= hi or hi - lo + 1 > 1048576:
            raise ValueError("Invalid tree range")
        return self.upstream("zoom", block)

    def dispatch(self, request: object) -> dict | None:
        if not isinstance(request, dict) or request.get("jsonrpc") != "2.0":
            return {"jsonrpc": "2.0", "id": None,
                    "error": {"code": -32600, "message": "Invalid JSON-RPC request"}}
        ident = request.get("id")
        method = request.get("method")
        if isinstance(ident, (dict, list, bool)):
            return {"jsonrpc": "2.0", "id": None,
                    "error": {"code": -32600, "message": "Invalid request ID"}}
        if ident is None:
            return None  # Notifications must not get responses.
        if method == "initialize":
            return {"jsonrpc": "2.0", "id": ident, "result": {
                "protocolVersion": "2025-03-26",
                "capabilities": {"tools": {"listChanged": False}},
                "serverInfo": {"name": "optmem-stage-readonly", "version": "0.1.0"}
            }}
        if method == "ping":
            return {"jsonrpc": "2.0", "id": ident, "result": {}}
        if method == "tools/list":
            return {"jsonrpc": "2.0", "id": ident, "result": {"tools": SCHEMAS}}
        if method == "tools/call":
            params = request.get("params")
            if not isinstance(params, dict) or set(params) - {"name", "arguments"}:
                err = "Invalid tool parameters"
            else:
                try:
                    output = self.read(params.get("name"), params.get("arguments", {}))
                except (ValueError, RuntimeError, subprocess.TimeoutExpired, OSError):
                    err = "OptMem read refused: invalid scope, incomplete source or unsafe query"
                else:
                    return {"jsonrpc": "2.0", "id": ident, "result": {
                        "content": [{"type": "text", "text": output}], "isError": False
                    }}
            return {"jsonrpc": "2.0", "id": ident, "result": {
                "content": [{"type": "text", "text": err}], "isError": True
            }}
        return {"jsonrpc": "2.0", "id": ident,
                "error": {"code": -32601, "message": "Method not found"}}

def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--memory-dir", type=Path, required=True)
    parser.add_argument("--memo", type=Path, required=True)
    parser.add_argument("--expected-log-sha256", required=True)
    options = parser.parse_args()
    reader = MCPReader(options.memory_dir, options.memo, options.expected_log_sha256)
    for line in sys.stdin.buffer:
        if len(line) > MAX_INBOUND:
            continue  # Failed request never alters the store.
        try:
            request = json.loads(line.decode("utf-8"))
            response = reader.dispatch(request)
            if response is not None:
                sys.stdout.write(json.dumps(response, separators=(",", ":")) + "\n")
                sys.stdout.flush()
        except (ValueError, UnicodeError, TypeError):
            sys.stdout.write(json.dumps({"jsonrpc": "2.0", "id": None,
                "error": {"code": -32700, "message": "Parse error"}}) + "\n")
            sys.stdout.flush()
    return 0

if __name__ == "__main__":
    raise SystemExit(main())

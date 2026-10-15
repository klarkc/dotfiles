#!/usr/bin/env python3
"""MCP protocol smoke: no real vault or secrets are touched."""
import json
import os
import pathlib
import selectors
import subprocess
import sys
import tempfile
import time

exe = sys.argv[1]
with tempfile.TemporaryDirectory(prefix="enpass-readonly-mcp-") as home:
    env = dict(os.environ)
    env["HOME"] = home
    env["XDG_CONFIG_HOME"] = home + "/config"
    env["ENPASS_MCP_CONFIG_DIR"] = home + "/enpass"
    env["ENPASS_MCP_ALLOW_WRITES"] = "1"  # hostile inherited env: wrapper must override
    env.pop("ENPASS_MASTER_PASSWORD", None)
    with subprocess.Popen(
        [exe],
        env=env,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        bufsize=0,
    ) as p:
        selector = selectors.DefaultSelector()
        selector.register(p.stdout, selectors.EVENT_READ)
        buffer = b""

        def send(method, params=None, request_id=None):
            obj = {"jsonrpc": "2.0", "method": method}
            if params is not None:
                obj["params"] = params
            if request_id is not None:
                obj["id"] = request_id
            p.stdin.write((json.dumps(obj) + "\n").encode())
            p.stdin.flush()

        def reply(request_id):
            nonlocal_dummy = None
            global buffer
            deadline = time.monotonic() + 18
            while time.monotonic() < deadline:
                if b"\n" in buffer:
                    line, buffer = buffer.split(b"\n", 1)
                    obj = json.loads(line)
                    if obj.get("id") == request_id:
                        return obj
                    continue
                if p.poll() is not None:
                    raise RuntimeError("MCP exited prematurely")
                if selector.select(timeout=1):
                    chunk = os.read(p.stdout.fileno(), 65536)
                    if not chunk:
                        raise RuntimeError("MCP stdout closed prematurely")
                    buffer += chunk
            raise RuntimeError("MCP response timeout")

        send("initialize", {
            "protocolVersion": "2025-03-26",
            "capabilities": {},
            "clientInfo": {"name": "enpass-readonly-contract", "version": "1.0.0"},
        }, 1)
        response = reply(1)
        assert "result" in response, response.get("error")
        send("notifications/initialized")
        send("tools/list", {}, 2)
        response = reply(2)
        names = {t["name"] for t in response["result"]["tools"]}
        expected = {
            "list_vaults", "unlock_vault", "lock_vault", "list_items",
            "get_item", "get_password", "get_otp",
            "list_attachments", "export_attachment", "sync_status"
        }
        assert expected <= names, (expected - names)
        forbidden = {"create_item", "delete_item", "sync_pull"}
        assert not (names & forbidden), names & forbidden
        # No configured vault: only metadata, no keyring/master password.
        send("tools/call", {"name": "list_vaults", "arguments": {}}, 3)
        response = reply(3)
        assert "result" in response
        payload = json.loads(response["result"]["content"][0]["text"])
        assert payload["vaults"] == []
        print("enpass-mcp-protocol: PASS MCP initialize/list/call, writes absent even with hostile env")
        p.stdin.close()
        p.terminate()
        p.wait(timeout=10)

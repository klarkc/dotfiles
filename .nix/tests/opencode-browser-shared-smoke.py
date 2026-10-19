#!/usr/bin/env python3
"""Two-client Playwright HTTP smoke. Run ONLY with a disposable browser profile."""
import argparse
import json
from urllib.parse import urlsplit
from urllib.request import Request, urlopen

def parse_response(data):
    text = data.decode()
    events = []
    for line in text.splitlines():
        if line.startswith("data:"):
            try:
                events.append(json.loads(line[5:].strip()))
            except json.JSONDecodeError:
                pass
    if not events:
        events = [json.loads(text)]
    return events

def post(url, method, params, request_id, session=None):
    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json, text/event-stream",
        "MCP-Protocol-Version": "2025-03-26",
    }
    if session:
        headers["Mcp-Session-Id"] = session
    data = json.dumps({
        "jsonrpc": "2.0", "id": request_id, "method": method, "params": params,
    }).encode()
    with urlopen(Request(url, data=data, headers=headers), timeout=40) as response:
        assert response.status == 200, method
        return response.headers.get("Mcp-Session-Id"), parse_response(response.read(1000000))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", help="HTTP MCP URL for a disposable-profile test server")
    parser.add_argument("--selftest", action="store_true")
    args = parser.parse_args()
    if args.selftest:
        fixture = b'event: message\ndata: {"jsonrpc":"2.0","id":1,"result":{"ok":true}}\n\n'
        assert parse_response(fixture)[0]["result"]["ok"]
        print("opencode-browser-shared-smoke: SSE parser selftest PASS")
        return
    if not args.url:
        parser.error("--url is required for integration tests")
    endpoint = urlsplit(args.url)
    if endpoint.scheme != "http" or endpoint.hostname not in ("localhost", "127.0.0.1"):
        parser.error("refusing anything other than a loopback HTTP MCP endpoint")
    if endpoint.port == 18927:
        parser.error("refusing the production paired Enpass profile; use disposable test port")
    sessions = []
    for client in (1, 2):
        sid, res = post(args.url, "initialize", {
            "protocolVersion": "2025-03-26",
            "capabilities": {},
            "clientInfo": {"name": "isolated-multi-client-test", "version": str(client)},
        }, client)
        assert sid and res[0].get("result"), "initialization failed"
        _, tools = post(args.url, "tools/list", {}, client + 10, sid)
        assert any(
            t["name"] == "browser_navigate"
            for event in tools
            for t in event.get("result", {}).get("tools", [])
        ), "browser_navigate tool missing"
        sessions.append(sid)
    _, navigated = post(args.url, "tools/call", {
        "name": "browser_navigate",
        "arguments": {"url": "about:blank#opencode-browser-multi-client-regression"},
    }, 25, sessions[0])
    assert navigated[0].get("result") and not navigated[0]["result"].get("isError")
    _, tabs = post(args.url, "tools/call", {
        "name": "browser_tabs", "arguments": {"action": "list"},
    }, 26, sessions[1])
    assert "opencode-browser-multi-client-regression" in str(tabs)
    print("opencode-browser-shared-smoke: two HTTP clients share Brave context PASS")

if __name__ == "__main__":
    main()

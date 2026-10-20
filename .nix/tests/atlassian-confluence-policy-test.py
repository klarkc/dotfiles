#!/usr/bin/env python3
"""Offline regression for Atlassian Confluence scope and OpenCode write gates."""

import importlib.machinery
import importlib.util
import io
import json
import sys
import unittest
from contextlib import redirect_stdout
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CONFIG = Path(sys.argv[1]) if len(sys.argv) == 4 else ROOT / ".config/opencode/opencode.json"
SKILL = Path(sys.argv[2]) if len(sys.argv) == 4 else ROOT / ".agents/skills/atlassian-write-human-guidance/SKILL.md"
PROBE = Path(sys.argv[3]) if len(sys.argv) == 4 else ROOT / ".local/bin/atlassian-confluence-preflight"
loader = importlib.machinery.SourceFileLoader("atlassian_preflight", str(PROBE))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)

DENIED_TOOLS = {
    "atlassian_createConfluenceContent",
    "atlassian_updateConfluenceContent",
    "atlassian_executeWrite",
    "atlassian_executeDestructive",
}


def check_config(data):
    mcp = data["mcp"]
    assert "atlassian_token" not in mcp, "duplicate MCP integration"
    atlas = mcp["atlassian"]
    assert atlas == {
        "type": "remote",
        "url": "https://mcp.atlassian.com/v2/mcp",
        "oauth": False,
        "headers": {"Authorization": "Basic {env:ATLASSIAN_MCP_BASIC_AUTH}"},
        "enabled": True,
    }, "unexpected auth integration or committed secret"
    policy = data["permission"]
    assert all(policy.get(k) == "deny" for k in DENIED_TOOLS), "write must remain denied while auth fails"
    assert policy.get("atlassian_*") not in ("allow", "ask"), "broad grant bypasses gate"
    assert policy.get("atlassian_getConfluenceContent") != "deny", "read probe must remain available"


class MockMcp:
    def __init__(self, read_authorized):
        self.read_authorized = read_authorized
        self.calls = []

    def initialize(self):
        self.calls.append("initialize")

    def rpc(self, method, params):
        assert method == "tools/list"
        return {"result": {"tools": [{"name": n} for n in [
            "atlassianUserInfo", "getAccessibleAtlassianResources",
            "getConfluenceContent", "createConfluenceContent", "updateConfluenceContent",
        ]]}}

    def tool(self, name, arguments):
        self.calls.append(name)
        if name == "atlassianUserInfo":
            return {"data": {"accountId": "fake-account"}}, False, 0
        if name == "getAccessibleAtlassianResources":
            return {"data": {"resources": [
                {"url": "https://example.atlassian.net", "cloudId": "synthetic-cloud"}
            ]}}, False, 0
        if name == "getConfluenceContent":
            assert arguments["cloudId"] == "synthetic-cloud"
            if self.read_authorized:
                return {"data": {"id": arguments["content_id"]}}, False, 0
            return {"error": True, "statusCode": 403,
                    "message": "Required scope not granted"}, True, 403
        raise AssertionError("unexpected tool")


class PolicyTests(unittest.TestCase):
    def test_canonical_skill_and_selection(self):
        text = SKILL.read_text()
        self.assertTrue(text.startswith("---\nname: atlassian-write-human-guidance\n"))
        self.assertIn("description: Use for requests to create, update", text)
        for phrase in (
            "OpenCode", "read:confluence:agent-interface",
            "write:confluence:agent-interface", "write_confluence",
            "snapshotToken", "explicit authorization", "HTTP 403",
            "atlassian_executeDestructive", "manual", "Connectors",
            "getContentFormatGuide", "dryRun=true",
        ):
            self.assertIn(phrase, text)

    def test_current_write_gate(self):
        data = json.loads(CONFIG.read_text())
        check_config(data)
        for target in DENIED_TOOLS:
            copy = json.loads(json.dumps(data))
            copy["permission"][target] = "ask"
            with self.assertRaises(AssertionError):
                check_config(copy)
            del copy["permission"][target]
            with self.assertRaises(AssertionError):
                check_config(copy)
        copy = json.loads(json.dumps(data))
        copy["mcp"]["atlassian_token"] = copy["mcp"]["atlassian"]
        with self.assertRaises(AssertionError):
            check_config(copy)

    def test_tools_list_never_grants_write_or_bypasses_403(self):
        client = MockMcp(read_authorized=False)
        output = io.StringIO()
        with redirect_stdout(output):
            code = module.preflight("https://example.atlassian.net", "123", client)
        self.assertEqual(code, 3)
        self.assertIn("write_tools_advertised=true", output.getvalue())
        self.assertIn("scope_or_token_rejected_status_403", output.getvalue())
        self.assertTrue(set(client.calls) <= {
            "initialize", "atlassianUserInfo",
            "getAccessibleAtlassianResources", "getConfluenceContent",
        })

    def test_authorized_read_still_does_not_grant_write(self):
        client = MockMcp(read_authorized=True)
        output = io.StringIO()
        with redirect_stdout(output):
            code = module.preflight("https://example.atlassian.net", "123", client)
        self.assertEqual(code, 0)
        self.assertIn("write_permission=not_proven", output.getvalue())

    def test_probe_rejects_mutating_tool_calls(self):
        with self.assertRaises(module.PreflightError):
            module.OfficialMcp("dummy").tool("updateConfluenceContent", {})
        with self.assertRaises(module.PreflightError):
            module.OfficialMcp("dummy").tool("executeWrite", {})
        self.assertEqual(module.classify(409, {}), "version_conflict")
        self.assertEqual(module.classify(403, {"message": "Missing scope"}), "scope_or_token_rejected")


if __name__ == "__main__":
    unittest.main(argv=[sys.argv[0]], verbosity=2)

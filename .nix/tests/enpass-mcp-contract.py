#!/usr/bin/env python3
"""Offline policy check for personal Enpass MCP in dotfiles."""
import json
import pathlib
import sys

config, runtime, skill, flake = map(pathlib.Path, sys.argv[1:5])
data = json.loads(config.read_text())
mcp = data["mcp"]["enpass"]
assert mcp["type"] == "local"
assert mcp["enabled"] is True
assert mcp["command"] == [
    "sh", "-c", 'exec "$HOME/.nix-profile/bin/enpass-mcp-readonly"'
]
assert mcp["timeout"] >= 30000
assert not mcp.get("environment"), "secrets must never be in MCP config"
perm = data["permission"]
assert perm["enpass_*"] == "deny"
for tool in (
    "list_vaults", "sync_status", "lock_vault"
):
    assert perm[f"enpass_{tool}"] == "allow"
for tool in (
    "unlock_vault", "list_items", "get_item",
    "get_password", "get_otp", "list_attachments"
):
    assert perm[f"enpass_{tool}"] == "ask", tool
for tool in (
    "create_item", "delete_item", "sync_pull",
    "export_attachment"
):
    assert perm.get(f"enpass_{tool}", "deny") == "deny", tool
text = runtime.read_text()
assert "ENPASS_MCP_ALLOW_WRITES=0" in text
assert "enpass-mcp/fe805b729de1f0e4196b2e9cdd5df18a8747db26" in flake.read_text()
assert "ENPASS_MASTER_PASSWORD" in text
instructions = skill.read_text()
assert "Never enable ENPASS_MCP_ALLOW_WRITES" in instructions
assert "not through an agent" in instructions
assert "read-only" in instructions
assert "Never perform automatic Enpass writes." in instructions
print("enpass-mcp-contract: PASS readonly launcher, MCP permissions, manual-write skill")

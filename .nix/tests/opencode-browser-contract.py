#!/usr/bin/env python3
"""Read-only/static safety contract for the isolated OpenCode browser setup."""
import json
import pathlib
import sys

config, launcher, display, unit, pairing, mcp_unit = map(pathlib.Path, sys.argv[1:7])
data = json.loads(config.read_text())
assert "enpass" not in data["mcp"]
assert not any(key.startswith("enpass_") for key in data["permission"])
mcp = data["mcp"]["playwright"]
assert mcp["type"] == "remote" and mcp["enabled"] is True
assert mcp["url"] == "http://localhost:18927/mcp"
assert "command" not in mcp
assert mcp["timeout"] >= 30000
assert "environment" not in mcp
for name in ("playwright_browser_evaluate", "playwright_browser_run_code"):
    assert data["permission"][name] == "deny"
assert data["permission"]["playwright_browser_file_upload"] == "ask"
assert data["permission"]["playwright_browser_snapshot"] == "ask"

script = launcher.read_text()
for required in (
    'export DISPLAY=:92', 'export PLAYWRIGHT_MCP_USER_DATA_DIR=', 'export XAUTHORITY=', 'flock -n 9',
    'opencode-browser-display.service', '/usr/bin/xset -display :92 q', 'kmcfomidfpdkfieipokbalgegidffkal',
    '"headless": False', '"isolated": False', '--load-extension=', 'playwright-mcp',
    '"userDataDir": str(profile)', '"ignoreDefaultArgs": ["--disable-extensions"]',
):
    assert required in script, required
for forbidden in (".enpassdb", "ENPASS_MASTER_PASSWORD", "enpass-mcp"):
    assert forbidden not in script, forbidden
screen = display.read_text()
assert "-nolisten tcp" in screen and '-auth "$auth"' in screen
assert "MIT-MAGIC-COOKIE-1" in screen
service = unit.read_text()
assert "ExecStart=%h/.local/bin/opencode-browser-display" in service
assert "NoNewPrivileges=yes" in service
assert "PrivateTmp=yes" not in service
shared = mcp_unit.read_text()
assert "Requires=opencode-browser-display.service" in shared
assert "After=opencode-browser-display.service" in shared
assert "ExecStart=%h/.local/bin/opencode-browser-mcp" in shared
assert "NoNewPrivileges=yes" in shared
assert "ProtectSystem=full" in shared
pair = pairing.read_text()
assert "Xephyr :92" in pair and "flock -n 9" in pair
assert "/usr/bin/xset q" in pair and "/tmp/.X11-unix/X92" not in pair
assert "systemctl --user stop opencode-browser-display.service" in pair
assert "systemctl --user start opencode-browser-display.service" in pair
assert "--user-data-dir=" in pair and "ENPASS_MASTER_PASSWORD" not in pair
assert "systemctl --user stop opencode-browser-mcp.service" in pair
assert "systemctl --user start opencode-browser-mcp.service" in pair
assert "exec 9>&-" in pair
assert "--host 127.0.0.1 --port 18927 --shared-browser-context" in script
assert "--snapshot-mode none" in script
assert "0.0.0.0" not in script
assert "http://localhost:18927/mcp" in str(mcp)
print("opencode-browser-contract: PASS separated profile, auth display, Enpass MCP absent")

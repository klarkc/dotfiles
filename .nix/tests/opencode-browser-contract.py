#!/usr/bin/env python3
"""Read-only/static safety contract for the isolated OpenCode browser setup."""
import json
import pathlib
import sys

config, launcher, display, unit, pairing = map(pathlib.Path, sys.argv[1:6])
data = json.loads(config.read_text())
assert "enpass" not in data["mcp"]
assert not any(key.startswith("enpass_") for key in data["permission"])
mcp = data["mcp"]["playwright"]
assert mcp["type"] == "local" and mcp["enabled"] is True
assert mcp["command"] == ["sh", "-c", 'exec "$HOME/.local/bin/opencode-browser-mcp"']
assert mcp["timeout"] >= 30000
assert "environment" not in mcp
for name in ("playwright_browser_evaluate", "playwright_browser_run_code"):
    assert data["permission"][name] == "deny"
assert data["permission"]["playwright_browser_file_upload"] == "ask"

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
pair = pairing.read_text()
assert "Xephyr :92" in pair and "flock -n 9" in pair
assert "/usr/bin/xset q" in pair and "/tmp/.X11-unix/X92" not in pair
assert "systemctl --user stop opencode-browser-display.service" in pair
assert "systemctl --user start opencode-browser-display.service" in pair
assert "--user-data-dir=" in pair and "ENPASS_MASTER_PASSWORD" not in pair
print("opencode-browser-contract: PASS separated profile, auth display, Enpass MCP absent")

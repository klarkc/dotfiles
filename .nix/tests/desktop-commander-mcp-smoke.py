#!/usr/bin/env python3
"""Out-of-band protocol smoke for the local DesktopCommander MCP bridge."""

import asyncio
from contextlib import asynccontextmanager
import json
import os
import re
import signal
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path

from mcp import ClientSession
from mcp.client.streamable_http import streamablehttp_client


RUNTIME = sys.argv[1]
PORT = None
URL = None


def require(condition, message):
    if not condition:
        raise AssertionError(message)


async def wait_ready(child):
    deadline = time.monotonic() + 20
    last_error = None
    while time.monotonic() < deadline:
        if child.poll() is not None:
            raise AssertionError("MCP server exited before becoming ready")
        try:
            async with streamablehttp_client(URL) as (read_stream, write_stream, _):
                async with ClientSession(read_stream, write_stream) as session:
                    await session.initialize()
                    if child.poll() is not None:
                        raise AssertionError("MCP server exited during readiness check")
                    return
        except Exception as exc:
            last_error = exc
            await asyncio.sleep(0.2)
    raise AssertionError("MCP server did not become ready") from last_error


@asynccontextmanager
async def connected():
    transport = streamablehttp_client(URL)
    async with transport as (read_stream, write_stream, _):
        async with ClientSession(read_stream, write_stream) as session:
            await session.initialize()
            yield session


async def call(session, name, arguments):
    return await session.call_tool(name, arguments)


async def run_smoke(workspace, outside):
    process = None
    env = os.environ.copy()
    env["HOME"] = str(workspace.parent / "isolated-home")
    env["XDG_CONFIG_HOME"] = str(Path(env["HOME"]) / ".config")
    home = Path(env["HOME"])
    home.mkdir(parents=True)
    config_dir = home / ".claude-server-commander"
    config_dir.mkdir(parents=True)
    config = {
        "allowedDirectories": [str(workspace)],
        "blockedCommands": [
            "mkfs", "format", "mount", "umount", "fdisk", "dd", "parted", "diskpart",
            "sudo", "su", "passwd", "adduser", "useradd", "usermod", "groupadd",
            "chsh", "visudo", "shutdown", "reboot", "halt", "poweroff", "init",
            "iptables", "firewall", "netsh", "sfc", "bcdedit", "reg", "net", "sc",
            "runas", "cipher", "takeown",
        ],
        "telemetryEnabled": False,
        "pendingWelcomeOnboarding": False,
        "welcomeOnboardingEligible": False,
    }
    (config_dir / "config.json").write_text(json.dumps(config), encoding="utf-8")
    (workspace / "read-me.txt").write_text("initial contents\n", encoding="utf-8")
    (outside / "private.txt").write_text("outside allowed directory\n", encoding="utf-8")

    def start_server(port):
        return subprocess.Popen(
            [RUNTIME, "serve", "--port", str(port)],
            env=env,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )

    def stop_server(child):
        if child.poll() is not None:
            return
        try:
            os.killpg(child.pid, signal.SIGTERM)
        except ProcessLookupError:
            return
        deadline = time.monotonic() + 5
        while child.poll() is None and time.monotonic() < deadline:
            time.sleep(0.05)
        if child.poll() is None:
            try:
                os.killpg(child.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
        try:
            child.wait(timeout=5)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(child.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            child.wait(timeout=5)

    try:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as reservation:
            reservation.bind(("127.0.0.1", 0))
            port = reservation.getsockname()[1]
        global PORT, URL
        PORT = port
        URL = f"http://127.0.0.1:{PORT}/mcp"

        process = start_server(PORT)
        await wait_ready(process)
        async with connected() as session:
            tools = await session.list_tools()
            names = {tool.name for tool in tools.tools}
            essential = {
                "read_file", "write_file", "edit_block", "list_directory",
                "start_process", "read_process_output", "force_terminate",
            }
            require(essential <= names, "essential DesktopCommander tools are missing")

            read = await call(session, "read_file", {"path": str(workspace / "read-me.txt")})
            require(not read.isError and "initial contents" in str(read.content), "read_file failed")
            written = await call(session, "write_file", {
                "path": str(workspace / "written.txt"), "content": "written contents\n",
            })
            require(not written.isError, "write_file failed")
            edited = await call(session, "edit_block", {
                "file_path": str(workspace / "written.txt"),
                "old_string": "written contents", "new_string": "edited contents",
            })
            require(not edited.isError, "edit_block failed")
            edited_read = await call(session, "read_file", {"path": str(workspace / "written.txt")})
            require(not edited_read.isError and "edited contents" in str(edited_read.content),
                    "edited contents were not readable via MCP")
            require((workspace / "written.txt").read_text(encoding="utf-8") == "edited contents\n",
                    "edited contents were not persisted to disk")
            listing = await call(session, "list_directory", {"path": str(workspace)})
            require(not listing.isError and "written.txt" in str(listing.content), "list_directory failed")

            denied = await call(session, "read_file", {"path": str(outside / "private.txt")})
            require(denied.isError, "read outside allowedDirectories was not denied")

            blocked = await call(session, "start_process", {
                "command": "sudo -n true", "timeout_ms": 1000,
            })
            require(blocked.isError, "default blocked command policy did not reject sudo")

            started = await call(session, "start_process", {
                "command": "python3 -u -c 'import sys,time; print(\"stdout-initial\", flush=True); print(\"stderr-initial\", file=sys.stderr, flush=True); time.sleep(3); print(\"stdout-final\", flush=True); print(\"stderr-final\", file=sys.stderr, flush=True); sys.exit(7)'",
                "timeout_ms": 1000,
            })
            started_text = "\n".join(getattr(item, "text", "") for item in started.content)
            pid_match = re.search(r"Process started with PID (\d+)", started_text)
            require(not started.isError and pid_match is not None, "start_process did not return a process PID")
            pid = int(pid_match.group(1))

        # Reconnect and accumulate output; each offset=0 poll consumes new lines.
        collected = started_text
        deadline = time.monotonic() + 12
        while time.monotonic() < deadline:
            async with connected() as session:
                output = await call(session, "read_process_output", {
                    "pid": pid, "timeout_ms": 1000, "offset": 0,
                })
            require(not output.isError, "read_process_output failed")
            new_text = "\n".join(getattr(item, "text", "") for item in output.content)
            collected += "\n" + new_text
            if "exit code 7" in collected:
                break
        require("stdout-initial" in collected and "stderr-initial" in collected,
                "initial process output was not preserved across reconnect")
        require("stdout-final" in collected and "stderr-final" in collected,
                "final process output was not observed across reconnect")
        require("exit code 7" in collected, "process did not finish with expected exit code")

        stop_server(process)
        process = start_server(PORT)
        await wait_ready(process)
        async with connected() as session:
            after_restart = await call(session, "read_file", {"path": str(workspace / "read-me.txt")})
            require(not after_restart.isError and "initial contents" in str(after_restart.content),
                    "read_file failed after server restart")
    finally:
        if process is not None:
            stop_server(process)


def main():
    with tempfile.TemporaryDirectory(prefix="desktop-commander-mcp-") as temp:
        root = Path(temp)
        # HOME and the allowed workspace are siblings, keeping HOME outside the allowlist.
        workspace = root / "workspace"
        workspace.mkdir()
        outside = root / "outside"
        outside.mkdir()
        asyncio.run(run_smoke(workspace, outside))
    print("desktop-commander-mcp-smoke: PASS local loopback MCP protocol and tool lifecycle")
    print("This verifies the local MCP bridge only; it does not verify Cloudflare OAuth or ChatGPT.")


if __name__ == "__main__":
    main()

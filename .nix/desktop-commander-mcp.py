import argparse
from contextlib import contextmanager
import json
import os
from pathlib import Path
import re
import runpy
import secrets
import stat
import sys
import tempfile
import uuid


PORT = 18879
STATE = ".profile_override.desktop-commander-mcp"
PRIVATE = runpy.run_path(str(Path(__file__).with_name("private-environment.py")))
RUNTIME_KEYS = (
    "DESKTOP_COMMANDER_MCP_HOSTNAME", "CLOUDFLARE_TUNNEL_ID", "CLOUDFLARE_ACCESS_TEAM",
    "CLOUDFLARE_ACCESS_AUDIENCE", "CLOUDFLARE_TUNNEL_SECRET", "CLOUDFLARE_ACCOUNT_ID",
)


def private_file(path):
    path = Path(path).expanduser().absolute()
    info = path.lstat()
    if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid():
        raise ValueError("Credential/config must be a regular file owned by this user")
    if info.st_mode & 0o077:
        raise ValueError("Credential/config permissions must be 0600 or stricter")
    return path


def validate(settings):
    if set(settings) != {"hostname", "tunnel_id", "team_name", "audience", "credentials_file"}:
        raise ValueError("Unexpected or missing configuration fields")
    hostname = settings["hostname"]
    if not isinstance(hostname, str) or not re.fullmatch(
        r"(?=.{1,253}$)(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z]{2,63}", hostname
    ):
        raise ValueError("Expected a public DNS hostname, without scheme, wildcard or path")
    if not re.fullmatch(r"[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?", settings["team_name"]):
        raise ValueError("Expected the Zero Trust team name, not its URL")
    if not re.fullmatch(r"[a-fA-F0-9]{64}", settings["audience"]):
        raise ValueError("Expected the Access application's 64-character AUD tag")
    tunnel = str(uuid.UUID(settings["tunnel_id"]))
    credential = private_file(settings["credentials_file"])
    data = json.loads(credential.read_text())
    if str(uuid.UUID(data.get("TunnelID", ""))) != tunnel or not data.get("TunnelSecret"):
        raise ValueError("Tunnel credential does not match the configured tunnel")
    return {
        "tunnel": tunnel,
        "credentials-file": str(credential),
        "no-autoupdate": True,
        "loglevel": "warn",
        "metrics": "127.0.0.1:18880",
        "ingress": [
            {
                "hostname": hostname,
                "service": f"http://127.0.0.1:{PORT}",
                "originRequest": {
                    "httpHostHeader": "127.0.0.1",
                    "access": {
                        "required": True,
                        "teamName": settings["team_name"],
                        "audTag": [settings["audience"]],
                    },
                },
            },
            {"service": "http_status:404"},
        ],
    }


@contextmanager
def runtime_configuration(values):
    runtime = os.environ.get("RUNTIME_DIRECTORY") or os.environ.get("XDG_RUNTIME_DIR")
    if runtime and (not Path(runtime).is_absolute() or not Path(runtime).is_dir()):
        raise ValueError("Invalid private runtime directory")
    with tempfile.TemporaryDirectory(prefix="desktop-commander-mcp-", dir=runtime) as directory:
        base = Path(directory)
        credential = base / "credentials.json"
        credential.write_text(json.dumps({
            "AccountTag": values["CLOUDFLARE_ACCOUNT_ID"],
            "TunnelID": values["CLOUDFLARE_TUNNEL_ID"],
            "TunnelSecret": values["CLOUDFLARE_TUNNEL_SECRET"],
        }))
        credential.chmod(0o600)
        config = validate({
            "hostname": values["DESKTOP_COMMANDER_MCP_HOSTNAME"],
            "tunnel_id": values["CLOUDFLARE_TUNNEL_ID"],
            "team_name": values["CLOUDFLARE_ACCESS_TEAM"],
            "audience": values["CLOUDFLARE_ACCESS_AUDIENCE"],
            "credentials_file": str(credential),
        })
        path = base / "tunnel.json"
        path.write_text(json.dumps(config))
        path.chmod(0o600)
        yield path, config


def tunnel():
    values = PRIVATE["load_profile"](required=RUNTIME_KEYS)
    if not os.environ.get("RUNTIME_DIRECTORY") and not os.environ.get("XDG_RUNTIME_DIR"):
        raise ValueError("Run the tunnel through its systemd user service (runtime directory required)")
    with runtime_configuration(values) as (path, config):
        import subprocess

        result = subprocess.run([
            "cloudflared", "tunnel", "--config", str(path),
            "--no-autoupdate", "run", config["tunnel"],
        ], check=False, env={key: os.environ[key] for key in (
            "HOME", "PATH", "LANG", "LC_ALL", "XDG_RUNTIME_DIR"
        ) if key in os.environ})
        return result.returncode


def serve(args):
    runtime = Path(os.environ.get("RUNTIME_DIRECTORY") or tempfile.mkdtemp(prefix="desktop-commander-mcp-"))
    runtime.mkdir(mode=0o700, parents=True, exist_ok=True)
    probe = runtime / "optmem-probe"
    with probe.open("w") as stream:
        stream.write(secrets.token_hex(16))
    command = [
        "mcp-proxy", "--host", "127.0.0.1", "--port", str(args.port),
        "--stateless", "--log-level", "WARNING",
    ]
    environment = {
        "HOME": str(Path.home()),
        "PATH": os.environ["PATH"],
        "SHELL": os.environ.get("SHELL", "/bin/bash"),
        "DESKTOP_COMMANDER_DISABLE_TELEMETRY": "1",
        "DESKTOP_COMMANDER_OPTMEM_PROBE_FILE": str(probe),
    }
    for name in ("LANG", "LC_ALL", "TMPDIR", "XDG_RUNTIME_DIR", "DBUS_SESSION_BUS_ADDRESS", "DISPLAY", "WAYLAND_DISPLAY"):
        if name in os.environ:
            environment[name] = os.environ[name]
    for key, value in environment.items():
        command.extend(["-e", key, value])
    command.extend(["--", "desktop-commander", "--no-onboarding"])
    print(f"Desktop Commander MCP: loopback 127.0.0.1:{args.port}/mcp; external auth is enforced by cloudflared", flush=True)
    os.execvp(command[0], command)


def challenge(action):
    workspace = Path.home() / STATE / "smoke"
    if action == "prepare":
        workspace.mkdir(mode=0o700, parents=True, exist_ok=True)
        fixture = workspace / "challenge.txt"
        if fixture.exists():
            raise ValueError("Challenge already exists; do not overwrite evidence")
        with fixture.open("x") as stream:
            stream.write(f"host={os.uname().nodename}\nnonce={secrets.token_hex(32)}\n")
        print(f"Challenge ready at {fixture}; nonce intentionally not displayed")
    else:
        fixture = private_file(workspace / "challenge.txt").read_text()
        proof = private_file(workspace / "proof.txt").read_text()
        nonce = next((line for line in fixture.splitlines() if line.startswith("nonce=")), None)
        if not nonce or nonce not in proof.splitlines() or "verified-from-chatgpt" not in proof.splitlines():
            raise ValueError("Proof does not match challenge")
        print("PASS: proof file matches the undisclosed host nonce and edit marker")
        print("ChatGPT origin must additionally be confirmed in the user's ChatGPT tool history")


def main():
    os.umask(0o077)
    parser = argparse.ArgumentParser(description="Self-hosted Desktop Commander MCP; no vendor Remote relay")
    sub = parser.add_subparsers(dest="action", required=True)
    server = sub.add_parser("serve", help="Loopback-only MCP HTTP bridge")
    server.add_argument("--port", type=int, default=PORT)
    sub.add_parser("tunnel", help="Run cloudflared with mandatory Access JWT validation")
    sub.add_parser("check", help="Validate private configuration without displaying credentials")
    sub.add_parser("smoke", help="Isolated local MCP integration test, not a ChatGPT/OAuth test")
    sub.add_parser("probe", help="Verify public TLS, unauthenticated rejection and OAuth discovery")
    sub.add_parser("prepare", help="Create a host nonce fixture for the manual ChatGPT end-to-end test")
    sub.add_parser("verify", help="Verify the proof file written by the manual ChatGPT test")
    args = parser.parse_args()
    if args.action == "serve":
        if not 1024 <= args.port <= 65535:
            parser.error("port must be between 1024 and 65535")
        serve(args)
    elif args.action == "tunnel":
        return tunnel()
    elif args.action == "check":
        values = PRIVATE["load_profile"](required=RUNTIME_KEYS)
        with runtime_configuration(values):
            pass
        print("Private config valid; exact hostname, Access JWT required, all other hosts denied")
    elif args.action == "smoke":
        sys.argv = ["desktop-commander-mcp-smoke", os.environ["DESKTOP_COMMANDER_MCP_EXECUTABLE"]]
        runpy.run_path("@smoke@", run_name="__main__")
    elif args.action == "probe":
        values = PRIVATE["load_profile"](required=("DESKTOP_COMMANDER_MCP_HOSTNAME",))
        sys.argv = ["desktop-commander-mcp-public-probe", values["DESKTOP_COMMANDER_MCP_HOSTNAME"]]
        runpy.run_path("@publicProbe@", run_name="__main__")
    elif args.action in ("prepare", "verify"):
        challenge(args.action)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError, TypeError, KeyError):
        sys.exit("Desktop Commander MCP: configuration/startup failed; check paths, fields and private-file permissions (values hidden)")

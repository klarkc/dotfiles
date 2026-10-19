#!/usr/bin/env python3
"""Render ZeroClaw's private runtime TOML without persisting host identifiers."""

import json
import os
from pathlib import Path
import re
import runpy
import secrets
import stat
import sys
import tomllib


ROOT = Path(__file__).resolve().parents[1]
TEMPLATE = ROOT / ".zeroclaw" / "config.toml"
RUNTIME_DIR_NAME = "zeroclaw-config"
DEFAULT_READ_ONLY_ROOTS = [
    "${HOME}/Sources",
    "${HOME}/.config/git/ignore",
    "${HOME}/.gitconfig",
    "/nix/var/nix/daemon-socket",
    "/etc/nix/sentry-endpoint",
    "/proc/meminfo",
    "/proc/stat",
]
ID_RE = re.compile(r"[0-9]+\Z")


def _ids(value, name):
    if not value:
        return []
    try:
        result = json.loads(value)
    except json.JSONDecodeError as exc:
        raise ValueError(f"{name} must be a JSON array of digit strings") from exc
    if not isinstance(result, list) or any(
        not isinstance(item, str) or not ID_RE.fullmatch(item) for item in result
    ):
        raise ValueError(f"{name} must be a JSON array of digit strings")
    if len(set(result)) != len(result):
        raise ValueError(f"{name} must not contain duplicate IDs")
    return result


def _json_env(values, name, default):
    value = values.get(name)
    if not value:
        return default
    try:
        parsed = json.loads(value)
    except json.JSONDecodeError as exc:
        raise ValueError(f"{name} must be a JSON array of strings") from exc
    if not isinstance(parsed, list) or any(not isinstance(item, str) for item in parsed):
        raise ValueError(f"{name} must be a JSON array of strings")
    if any("\x00" in item or "\n" in item or "\r" in item for item in parsed):
        raise ValueError(f"{name} contains an invalid path")
    return parsed


def _toml_value(value):
    # JSON string and array syntax is valid TOML for these scalar/list values.
    return json.dumps(value, ensure_ascii=True, separators=(",", ":"))


def render(template_path=TEMPLATE, runtime_base=None):
    helper = runpy.run_path(str(Path(__file__).with_name("private-environment.py")))
    values = helper["load_profile"]()
    home = Path.home()
    runtime_base = Path(runtime_base or os.environ.get("XDG_RUNTIME_DIR", ""))
    if not str(runtime_base) or not runtime_base.is_absolute():
        raise ValueError("XDG_RUNTIME_DIR must be an absolute runtime directory")
    info = runtime_base.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError("XDG_RUNTIME_DIR must be owned by this user and mode 0700")

    source = Path(template_path).read_text(encoding="utf-8")
    tomllib.loads(source)
    guilds = _ids(values.get("ZEROCLAW_DISCORD_GUILD_IDS_JSON", ""), "ZEROCLAW_DISCORD_GUILD_IDS_JSON")
    peers = _ids(values.get("ZEROCLAW_DISCORD_PEER_IDS_JSON", ""), "ZEROCLAW_DISCORD_PEER_IDS_JSON")
    token = values.get("DISCORD_BOT", "")
    discord_enabled = bool(token and guilds and peers)
    read_only_roots = _json_env(
        values, "ZEROCLAW_READ_ONLY_ROOTS_JSON", DEFAULT_READ_ONLY_ROOTS
    )
    allowed_roots = _json_env(values, "ZEROCLAW_ALLOWED_ROOTS_JSON", ["/nix/store", "${HOME}/.cache/nix"])
    read_only_roots = [item.replace("${HOME}", str(home)) for item in read_only_roots]
    allowed_roots = [item.replace("${HOME}", str(home)) for item in allowed_roots]

    # Replace only complete assignments within the existing schema sections.
    replacements = {
        "enabled": discord_enabled,
        "guild_ids": guilds if discord_enabled else [],
        "external_peers": peers if discord_enabled else [],
        "allowed_roots": allowed_roots,
        "allowed_roots_read_only": read_only_roots,
    }
    sections = {
        "[channels.discord.default]": {"enabled", "guild_ids"},
        "[peer_groups.discord_default]": {"external_peers"},
        "[risk_profiles.default]": {"allowed_roots", "allowed_roots_read_only"},
    }
    section_pattern = re.compile(r"(?ms)^\[([^\]\n]+)\]\s*\n(.*?)(?=^\[|\Z)")
    seen = set()

    def rewrite_section(match):
        section_name, body = match.group(1), match.group(2)
        section_key = f"[{section_name}]"
        keys = sections.get(section_key)
        if not keys:
            return match.group(0)
        for key in keys:
            assignment = re.compile(
                rf"(?m)^(\s*{re.escape(key)}\s*=\s*)(\[[^\]]*\]|[^\n]+)(?:\n)?"
            )
            found = assignment.search(body)
            if found is None:
                raise ValueError("ZeroClaw template is missing required private-config fields")
            body = assignment.sub(lambda item: item.group(1) + _toml_value(replacements[key]) + "\n", body, count=1)
            seen.add(key)
        return f"[{section_name}]\n{body}"

    rendered = section_pattern.sub(rewrite_section, source)
    required = set().union(*sections.values())
    if seen != required:
        raise ValueError("ZeroClaw template is missing required private-config fields")
    if any(char in str(home) for char in (chr(10), chr(13), chr(34), chr(92))):
        raise ValueError("Invalid home path for MCP configuration")
    rendered = rendered.replace("${OPTMEM_HOME}", str(home))
    result = tomllib.loads(rendered)
    discord = result["channels"]["discord"]["default"]
    peer_group = result["peer_groups"]["discord_default"]
    if discord["enabled"] and (not discord["guild_ids"] or not peer_group["external_peers"]):
        raise ValueError("Discord requires explicit guild and peer allowlists")
    if not discord["enabled"] and (discord["guild_ids"] or peer_group["external_peers"]):
        raise ValueError("Disabled Discord must not retain private allowlists")

    runtime_dir = runtime_base / RUNTIME_DIR_NAME
    runtime_dir.mkdir(mode=0o700, exist_ok=True)
    info = runtime_dir.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError("ZeroClaw runtime config directory permissions are unsafe")
    target = runtime_dir / "config.toml"
    temporary = runtime_dir / (".config-" + secrets.token_hex(12) + ".tmp")
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            stream.write(rendered)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, target)
        os.chmod(target, 0o600)
    finally:
        try:
            temporary.unlink()
        except FileNotFoundError:
            pass
    return runtime_dir


if __name__ == "__main__":
    try:
        print(render())
    except (OSError, ValueError, KeyError):
        print("zeroclaw-private-config: configuration failed; check .profile_override and private runtime permissions (values hidden)", file=sys.stderr)
        raise SystemExit(1)

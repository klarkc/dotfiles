#!/usr/bin/env python3
"""Offline contract test for ZeroClaw's private runtime configuration renderer."""

import importlib.util
import json
import os
from pathlib import Path
import stat
import tempfile
import tomllib


ROOT = Path(__file__).resolve().parents[2]
RENDERER = ROOT / ".nix" / "zeroclaw-private-config.py"
TEMPLATE = ROOT / ".zeroclaw" / "config.toml"
spec = importlib.util.spec_from_file_location("zeroclaw_private_config", RENDERER)
renderer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(renderer)


def write_profile(home, values):
    lines = [f"export {key}={json.dumps(value)}" for key, value in values.items()]
    path = home / ".profile_override"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    path.chmod(0o600)
    return path


def check(enabled):
    with tempfile.TemporaryDirectory(prefix="zeroclaw-private-config-") as temporary:
        base = Path(temporary)
        home = base / "synthetic-home"
        runtime = base / "runtime"
        home.mkdir(mode=0o700)
        runtime.mkdir(mode=0o700)
        values = {}
        if enabled:
            values.update(
                DISCORD_BOT="example-not-secret",
                ZEROCLAW_DISCORD_GUILD_IDS_JSON='["123456789012345678"]',
                ZEROCLAW_DISCORD_PEER_IDS_JSON='["234567890123456789"]',
            )
        write_profile(home, values)
        original_home = os.environ.get("HOME")
        os.environ["HOME"] = str(home)
        try:
            runtime_dir = renderer.render(TEMPLATE, runtime)
        finally:
            if original_home is None:
                os.environ.pop("HOME", None)
            else:
                os.environ["HOME"] = original_home
        config_path = runtime_dir / "config.toml"
        assert stat.S_IMODE(runtime_dir.stat().st_mode) == 0o700
        assert stat.S_IMODE(config_path.stat().st_mode) == 0o600
        config_text = config_path.read_text(encoding="utf-8")
        config = tomllib.loads(config_text)
        discord = config["channels"]["discord"]["default"]
        peers = config["peer_groups"]["discord_default"]["external_peers"]
        assert discord["enabled"] is enabled
        if enabled:
            assert discord["guild_ids"] == ["123456789012345678"]
            assert peers == ["234567890123456789"]
        else:
            assert discord["guild_ids"] == []
            assert peers == []
        assert config["risk_profiles"]["default"]["allowed_roots"] == ["/nix/store", str(home / ".cache/nix")]
        roots = config["risk_profiles"]["default"]["allowed_roots_read_only"]
        assert roots == [
            str(home / "Sources"),
            str(home / ".config/git/ignore"),
            str(home / ".gitconfig"),
            "/nix/var/nix/daemon-socket",
            "/etc/nix/sentry-endpoint",
            "/proc/meminfo",
            "/proc/stat",
        ]
        assert str(Path.home()) not in config_text
        assert "synthetic-token-not-a-credential" not in config_text


def check_invalid_ids():
    with tempfile.TemporaryDirectory(prefix="zeroclaw-invalid-json-") as temporary:
        base = Path(temporary)
        home = base / "home"
        runtime = base / "runtime"
        home.mkdir(mode=0o700)
        runtime.mkdir(mode=0o700)
        write_profile(home, {"ZEROCLAW_DISCORD_GUILD_IDS_JSON": '["*"]'})
        previous = os.environ.get("HOME")
        os.environ["HOME"] = str(home)
        try:
            try:
                renderer.render(TEMPLATE, runtime)
            except ValueError as exc:
                assert "digit strings" in str(exc)
            else:
                raise AssertionError("invalid Discord IDs were accepted")
        finally:
            if previous is None:
                os.environ.pop("HOME", None)
            else:
                os.environ["HOME"] = previous


template_text = TEMPLATE.read_text(encoding="utf-8")
template = tomllib.loads(template_text)
assert not template["channels"]["discord"]["default"]["enabled"]
assert template["channels"]["discord"]["default"]["guild_ids"] == []
assert template["peer_groups"]["discord_default"]["external_peers"] == []
assert str(Path.home()) not in template_text
check(False)
check(True)
check_invalid_ids()
print("zeroclaw-private-config: passed")

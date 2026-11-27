import copy
import json
import os
from pathlib import Path
import runpy
import sys
import tempfile
import unittest
from unittest.mock import patch


source = Path(sys.argv.pop(1))
root = Path(sys.argv.pop(1))
controller = runpy.run_path(str(source))
validate = controller["validate"]


class ConfigContract(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.credential = Path(self.temp.name) / "credential.json"
        self.tunnel = "12345678-1234-4234-8234-123456789abc"
        self.credential.write_text(json.dumps({"TunnelID": self.tunnel, "TunnelSecret": "test-only"}))
        self.credential.chmod(0o600)
        self.settings = {
            "hostname": "mcp.example.com",
            "tunnel_id": self.tunnel,
            "team_name": "test-team",
            "audience": "a" * 64,
            "credentials_file": str(self.credential),
        }

    def test_exact_host_authenticated_origin_and_deny_fallback(self):
        config = validate(self.settings)
        ingress = config["ingress"]
        self.assertEqual(len(ingress), 2)
        self.assertEqual(ingress[0]["hostname"], self.settings["hostname"])
        self.assertEqual(ingress[0]["service"], "http://127.0.0.1:18879")
        self.assertEqual(ingress[1], {"service": "http_status:404"})
        self.assertEqual(ingress[0]["originRequest"]["access"], {
            "required": True, "teamName": "test-team", "audTag": ["a" * 64]
        })
        self.assertNotIn("TunnelSecret", json.dumps(config))
        self.assertEqual(config["metrics"], "127.0.0.1:18880")
        self.assertTrue(config["no-autoupdate"])

    def test_invalid_fields_are_rejected(self):
        for key, values in {
            "hostname": ["*", "*.example.com", "https://example.com", "example.com/mcp", "localhost"],
            "team_name": ["", "https://test.cloudflareaccess.com", "../test"],
            "audience": ["", "any", "a" * 63],
            "tunnel_id": ["", "another-tunnel"],
        }.items():
            for value in values:
                with self.subTest(key=key, value=value), self.assertRaises((ValueError, TypeError)):
                    validate({**self.settings, key: value})
        for extra in ("token", "originRequest", "service", "access", "ingress"):
            with self.assertRaises(ValueError):
                validate({**self.settings, extra: False})
        missing = copy.copy(self.settings)
        del missing["audience"]
        with self.assertRaises(ValueError):
            validate(missing)

    def test_public_symlink_and_mismatched_credentials_rejected(self):
        self.credential.chmod(0o644)
        with self.assertRaises(ValueError):
            validate(self.settings)
        self.credential.chmod(0o600)
        link = self.credential.with_name("symlink.json")
        link.symlink_to(self.credential)
        with self.assertRaises(ValueError):
            validate({**self.settings, "credentials_file": str(link)})
        with self.assertRaises(ValueError):
            validate({**self.settings, "tunnel_id": "00000000-0000-4000-8000-000000000000"})

    def test_systemd_and_runtime_contract(self):
        render = runpy.run_path(str(root / ".nix/desktop-commander-systemd.py"))["render_units"]
        units = render("example", 1234, 2345, "/home/example")
        server = units["desktop-commander-mcp-runtime.service"]
        tunnel = units["desktop-commander-mcp-tunnel.service"]
        for unit in (server, tunnel):
            self.assertIn("/home/example/.nix-profile/bin/desktop-commander-mcp-runtime", unit)
            self.assertIn("UMask=0077", unit)
            self.assertIn("NoNewPrivileges=yes", unit)
            self.assertIn("KillMode=control-group", unit)
            self.assertNotIn("nix build", unit)
            self.assertNotIn("npx", unit)
        self.assertIn("ProtectHome=read-only", tunnel)
        self.assertIn("PartOf=desktop-commander-mcp-runtime.service", tunnel)
        self.assertIn("ConditionPathExists=/home/example/.profile_override\n", tunnel)
        text = source.read_text()
        self.assertIn('"--host", "127.0.0.1"', text)
        self.assertIn('"--stateless"', text)
        self.assertIn('"--no-onboarding"', text)
        self.assertIn('"DESKTOP_COMMANDER_DISABLE_TELEMETRY": "1"', text)
        self.assertNotIn('"--pass-environment"', text)
        self.assertNotIn('"desktop-commander", "remote"', text)
        self.assertEqual(controller["STATE"], ".profile_override.desktop-commander-mcp")

    def test_profile_is_authoritative_and_append_does_not_overwrite(self):
        with patch.dict(os.environ, {"HOME": self.temp.name, "PRIVATE_SAMPLE": "inherited-not-authoritative"}):
            profile = Path(self.temp.name) / ".profile_override"
            profile.write_text("printf 'PRIVATE_OUTPUT_MUST_NOT_ESCAPE'\nexport PRIVATE_SAMPLE='sample-value'\n")
            profile.chmod(0o600)
            helper = controller["PRIVATE"]
            self.assertEqual(helper["load_profile"](required=("PRIVATE_SAMPLE",))["PRIVATE_SAMPLE"], "sample-value")
            helper["append_profile"]({"PRIVATE_SECOND": "a value with ' quotes"})
            self.assertEqual(helper["load_profile"]()["PRIVATE_SECOND"], "a value with ' quotes")
            before = profile.read_bytes()
            helper["append_profile"]({"PRIVATE_SECOND": "a value with ' quotes"})
            self.assertEqual(profile.read_bytes(), before)
            with self.assertRaises(ValueError):
                helper["append_profile"]({"PRIVATE_SECOND": "different"})
            self.assertEqual(profile.read_bytes(), before)
            profile.chmod(0o644)
            with self.assertRaises(ValueError):
                helper["load_profile"]()

    def test_runtime_json_is_generated_temporarily_not_saved_as_configuration(self):
        values = {
            "DESKTOP_COMMANDER_MCP_HOSTNAME": "mcp.example.com", "CLOUDFLARE_TUNNEL_ID": self.tunnel,
            "CLOUDFLARE_ACCESS_TEAM": "team-example", "CLOUDFLARE_ACCESS_AUDIENCE": "a" * 64,
            "CLOUDFLARE_TUNNEL_SECRET": "synthetic-test-value", "CLOUDFLARE_ACCOUNT_ID": "account-example",
        }
        with patch.dict(os.environ, {"RUNTIME_DIRECTORY": self.temp.name}):
            with controller["runtime_configuration"](values) as (path, config):
                self.assertEqual(path.stat().st_mode & 0o777, 0o600)
                self.assertEqual(path.parent.stat().st_mode & 0o777, 0o700)
                credential = Path(config["credentials-file"])
                self.assertEqual(credential.stat().st_mode & 0o777, 0o600)
                self.assertEqual(json.loads(credential.read_text())["TunnelSecret"], "synthetic-test-value")
                self.assertTrue(config["ingress"][0]["originRequest"]["access"]["required"])
            self.assertFalse(path.exists())
            self.assertFalse(credential.exists())

    def test_inherited_private_setting_is_not_a_profile_fallback(self):
        with patch.dict(os.environ, {"HOME": self.temp.name, "PRIVATE_MISSING": "not-in-profile"}):
            profile = Path(self.temp.name) / ".profile_override"
            profile.write_text("export ANOTHER_SETTING='example'\n")
            profile.chmod(0o600)
            with self.assertRaises(ValueError):
                controller["PRIVATE"]["load_profile"](required=("PRIVATE_MISSING",))

    def test_manual_proof_requires_unseen_nonce_and_marker(self):
        with patch.dict(os.environ, {"HOME": self.temp.name}):
            controller["challenge"]("prepare")
            workspace = Path(self.temp.name) / controller["STATE"] / "smoke"
            fixture = (workspace / "challenge.txt").read_text()
            self.assertIn("nonce=", fixture)
            with self.assertRaises(ValueError):
                controller["challenge"]("prepare")
            proof = workspace / "proof.txt"
            proof.write_text("verified-from-chatgpt\nnonce=wrong\n")
            proof.chmod(0o600)
            with self.assertRaises(ValueError):
                controller["challenge"]("verify")
            proof.write_text(fixture + "verified-from-chatgpt\n")
            controller["challenge"]("verify")


class SystemServiceContract(unittest.TestCase):
    def setUp(self):
        self.module = runpy.run_path(str(root / ".nix/desktop-commander-systemd.py"))
        self.render = self.module["render_units"]

    def test_unprivileged_system_units_preserve_namespace_and_lifecycle(self):
        units = self.render("example", 1234, 2345, "/home/example")
        self.assertEqual(set(units), {
            "desktop-commander-mcp-runtime.service", "desktop-commander-mcp-tunnel.service"
        })
        server = units["desktop-commander-mcp-runtime.service"]
        tunnel = units["desktop-commander-mcp-tunnel.service"]
        for unit in units.values():
            for setting in ("User=example", "Group=2345", "PrivateUsers=yes",
                            "CapabilityBoundingSet=", "AmbientCapabilities=",
                            "NoNewPrivileges=yes", "PrivateTmp=yes", "ProtectSystem=strict",
                            "UMask=0077", "RuntimeDirectoryMode=0700",
                            "Environment=XDG_RUNTIME_DIR=/run/user/1234",
                            "Requires=user@1234.service", "WantedBy=multi-user.target"):
                self.assertIn(setting + "\n", unit)
            self.assertIn("Environment=PATH=/home/example/.nix-profile/bin:/home/example/.local/bin:/usr/local/bin:/usr/bin:/bin\n", unit)
            self.assertNotIn("desktop-commander-ssh", unit)
            self.assertNotIn("GIT_SSH", unit)
            self.assertNotIn("ExecStart=+", unit)
            self.assertNotIn("ExecStartPre=", unit)
            self.assertNotIn("EnvironmentFile=", unit)
        self.assertIn("ReadWritePaths=/home/example /run/user/1234\n", server)
        mount = "BindReadOnlyPaths=/etc/desktop-commander-mcp/ssh-client-policy.conf:/etc/ssh/ssh_config.d/99-desktop-commander-mcp.conf"
        self.assertIn(mount + "\n", server)
        self.assertNotIn("BindReadOnlyPaths=", tunnel)
        self.assertIn("Environment=DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1234/bus\n", server)
        self.assertIn("ExecStart=/home/example/.nix-profile/bin/desktop-commander-mcp-runtime serve\n", server)
        self.assertIn("ExecStart=/home/example/.nix-profile/bin/desktop-commander-mcp-runtime tunnel\n", tunnel)
        self.assertIn("ProtectHome=read-only\n", tunnel)
        self.assertNotIn("ReadWritePaths=/home/example", tunnel)
        self.assertIn("PartOf=desktop-commander-mcp-runtime.service\n", tunnel)
        self.assertIn("ConditionPathExists=/home/example/.profile_override\n", tunnel)

    def test_fresh_bootstrap_installs_only_new_reviewed_artifacts(self):
        from types import SimpleNamespace
        account = SimpleNamespace(pw_name="example", pw_uid=1234, pw_gid=2345, pw_dir="/home/example")
        install = self.module["install_fresh_artifacts"]
        with tempfile.TemporaryDirectory() as tmp:
            etc = Path(tmp)
            (etc / "systemd/system").mkdir(parents=True)
            include = etc / "ssh/ssh_config.d"
            include.mkdir(parents=True)
            stock = include / "20-systemd-ssh-proxy.conf"
            stock.write_text("Host .host\n    User root\n")
            before = stock.read_bytes(), stock.stat().st_mode
            install(etc, account)
            for name, content in self.render("example", 1234, 2345, "/home/example").items():
                path = etc / "systemd/system" / name
                self.assertEqual(path.read_text(), content)
                self.assertEqual(path.stat().st_mode & 0o777, 0o644)
            policy = etc / "desktop-commander-mcp/ssh-client-policy.conf"
            self.assertEqual(policy.read_text(), self.module["SSH_CLIENT_POLICY"])
            self.assertEqual(policy.stat().st_mode & 0o777, 0o644)
            self.assertEqual(policy.parent.stat().st_mode & 0o777, 0o755)
            anchor = include / "99-desktop-commander-mcp.conf"
            self.assertEqual(anchor.read_bytes(), b"")
            self.assertEqual(anchor.stat().st_mode & 0o777, 0o644)
            self.assertEqual((stock.read_bytes(), stock.stat().st_mode), before)
            with self.assertRaises((OSError, ValueError)):
                install(etc, account)
            self.assertEqual(policy.read_text(), self.module["SSH_CLIENT_POLICY"])

    def test_fresh_bootstrap_rejects_conflicts_before_any_write(self):
        from types import SimpleNamespace
        account = SimpleNamespace(pw_name="example", pw_uid=1234, pw_gid=2345, pw_dir="/home/example")
        install = self.module["install_fresh_artifacts"]
        for conflict in ("unit", "anchor", "directory", "symlink", "parent"):
            with self.subTest(conflict=conflict), tempfile.TemporaryDirectory() as tmp:
                etc = Path(tmp)
                unit_dir = etc / "systemd/system"
                unit_dir.mkdir(parents=True)
                include = etc / "ssh/ssh_config.d"
                include.mkdir(parents=True)
                target = include / "99-desktop-commander-mcp.conf"
                if conflict == "unit":
                    target = unit_dir / "desktop-commander-mcp-tunnel.service"
                    target.write_text("existing")
                elif conflict == "anchor":
                    target.write_text("existing")
                elif conflict == "directory":
                    (etc / "desktop-commander-mcp").mkdir()
                elif conflict == "symlink":
                    target.symlink_to(etc / "missing")
                else:
                    include.chmod(0o777)
                before = sorted(str(p.relative_to(etc)) for p in etc.rglob("*"))
                with self.assertRaises((OSError, ValueError)):
                    install(etc, account)
                self.assertEqual(sorted(str(p.relative_to(etc)) for p in etc.rglob("*")), before)

    def test_fresh_bootstrap_cleans_up_partial_creation(self):
        from types import SimpleNamespace
        account = SimpleNamespace(pw_name="example", pw_uid=1234, pw_gid=2345, pw_dir="/home/example")
        install = self.module["install_fresh_artifacts"]
        original = Path.open
        with tempfile.TemporaryDirectory() as tmp:
            etc = Path(tmp)
            (etc / "systemd/system").mkdir(parents=True)
            (etc / "ssh/ssh_config.d").mkdir(parents=True)
            stock = etc / "ssh/ssh_config.d/20-systemd-ssh-proxy.conf"
            stock.write_text("stock")
            before = sorted(str(p.relative_to(etc)) for p in etc.rglob("*"))
            def fail_late(path, *args, **kwargs):
                if path.name == "ssh-client-policy.conf":
                    raise OSError("synthetic write failure")
                return original(path, *args, **kwargs)
            with patch.object(Path, "open", fail_late), self.assertRaises(OSError):
                install(etc, account)
            self.assertEqual(sorted(str(p.relative_to(etc)) for p in etc.rglob("*")), before)
            self.assertEqual(stock.read_text(), "stock")

    def test_bootstrap_selects_pinned_system_units_not_legacy_user_services(self):
        bootstrap = (root / ".nix/servidor-hj-mesmo/guest-bootstrap.sh").read_text()
        self.assertIn('cat-file blob "$expected_repo_sha:.nix/desktop-commander-systemd.py"', bootstrap)
        self.assertIn('install-fresh klarkc', bootstrap)
        self.assertIn('systemctl enable --now desktop-commander-mcp-runtime.service desktop-commander-mcp-tunnel.service', bootstrap)
        self.assertNotIn('default.target.wants/desktop-commander-mcp-', bootstrap)
        self.assertFalse((root / ".nix/desktop-commander-ssh/bin/ssh").exists())
        for name in ("runtime", "tunnel"):
            self.assertFalse((root / f".config/systemd/user/desktop-commander-mcp-{name}.service").exists())

    def test_root_and_unsafe_account_substitutions_are_rejected(self):
        for args in (("root", 0, 0, "/root"), ("example", 0, 2345, "/home/example"),
                     ("example", 1234, 0, "/home/example"), ("example\nUser=root", 1234, 2345, "/home/example"),
                     ("example", 1234, 2345, "/home/other"), ("example", 1234, 2345, "/home/example%h")):
            with self.subTest(args=args), self.assertRaises(ValueError):
                self.render(*args)

    def test_minimal_namespace_mapping_not_full_or_overflow(self):
        valid = self.module["minimal_identity_map"]
        self.assertTrue(valid("0 0 1\n1234 1234 1\n", 1234))
        for mapping in ("1234 1234 1\n", "0 0 65536\n", "0 65534 1\n1234 1234 1\n",
                        "0 0 1\n1234 1234 1\n9999 9999 1\n", "invalid"):
            self.assertFalse(valid(mapping, 1234))
        self.assertFalse(valid("0 0 1\n", 0))

    def test_admin_probe_is_transient_and_uses_exact_candidate_security_settings(self):
        from types import SimpleNamespace
        account = SimpleNamespace(pw_name="example", pw_uid=1234, pw_gid=2345, pw_dir="/home/example")
        argv = self.module["admin_probe_command"](account, [Path("/example/repo")], Path("/example/probe.py"))
        self.assertEqual(argv[:5], ["sudo", "systemd-run", "--wait", "--pipe", "--collect"])
        for item in ("User=example", "Group=2345", "PrivateUsers=yes",
                     "CapabilityBoundingSet=", "AmbientCapabilities=", "NoNewPrivileges=yes",
                     "ProtectSystem=strict", "PrivateTmp=yes", "ReadWritePaths=/home/example /run/user/1234"):
            self.assertIn("--property=" + item, argv)
        self.assertIn("--setenv=XDG_RUNTIME_DIR=/run/user/1234", argv)
        self.assertIn("--property=BindReadOnlyPaths=/etc/desktop-commander-mcp/ssh-client-policy.conf:/etc/ssh/ssh_config.d/99-desktop-commander-mcp.conf", argv)
        self.assertEqual(argv[-4:], ["/usr/bin/python3", "/example/probe.py", "probe", "/example/repo"])
        self.assertFalse(any("RuntimeDirectory=" in item or "ExecStart=" in item or "GIT_SSH" in item for item in argv))

    def test_service_ssh_defaults_and_safe_endpoint_parser(self):
        self.assertEqual(self.module["SSH_CLIENT_POLICY"].splitlines()[1:], [
            "Host *", "    BatchMode yes", "    StrictHostKeyChecking yes",
            "    UpdateHostKeys no", "    ForwardAgent no", "    ForwardX11 no",
            "    ClearAllForwardings yes",
        ])
        parse = self.module["ssh_config_query"]
        self.assertEqual(parse("git@example.com:example/repo.git"),
                         ["ssh", "-G", "git@example.com"])
        self.assertEqual(parse("ssh://git@example.com:2222/example/repo.git"),
                         ["ssh", "-G", "-p", "2222", "git@example.com"])
        self.assertEqual(parse("ssh://git@[2001:db8::1]:2222/example/repo.git"),
                         ["ssh", "-G", "-p", "2222", "git@2001:db8::1"])
        for url in ("https://example.com/repo", "ssh://git:secret@example.com/repo",
                    "ssh://-o@example.com/repo", "ssh://git@-example.com/repo",
                    "ssh://git@example.com:0/repo", "ssh://git@example.com:65536/repo",
                    "ssh://git@example.com/repo?x=1", "git@example.com:",
                    "git@-example.com:repo"):
            with self.subTest(url=url):
                self.assertIsNone(parse(url))

    def test_effective_ssh_policy_fails_closed(self):
        check = self.module["strict_ssh_policy"]
        good = ("batchmode yes\nstricthostkeychecking true\nupdatehostkeys false\n"
                "forwardagent no\nforwardx11 no\nclearallforwardings yes\n")
        self.assertTrue(check(good))
        for changed in (good.replace("stricthostkeychecking true", "stricthostkeychecking ask"),
                        good.replace("batchmode yes", "batchmode no"),
                        good.replace("updatehostkeys false", "updatehostkeys true"),
                        good.replace("forwardagent no", "forwardagent yes"),
                        good.replace("clearallforwardings yes", "clearallforwardings no"),
                        good.replace("forwardx11 no\n", ""),
                        good + "stricthostkeychecking ask\n"):
            self.assertFalse(check(changed))

    def test_git_probe_uses_unmodified_normal_transport_and_sanitizes_failures(self):
        probe = self.module["probe_git"]
        strict = ("batchmode yes\nstricthostkeychecking true\nupdatehostkeys false\n"
                  "forwardagent no\nforwardx11 no\nclearallforwardings yes\n")
        with patch.dict(os.environ, {}, clear=True), patch.object(self.module["subprocess"], "run") as execute:
            execute.side_effect = [
                __import__("subprocess").CompletedProcess([], 1, "", ""),
                __import__("subprocess").CompletedProcess([], 0, "ssh://git@example.com/example/repo.git\n", ""),
                __import__("subprocess").CompletedProcess([], 0, strict, ""),
                __import__("subprocess").CompletedProcess([], 0, "a" * 40 + "\tHEAD\n", ""),
            ]
            self.assertEqual(probe(Path("/example/repo")), "PASS")
            self.assertEqual(execute.call_args.args[0], ["git", "-C", "/example/repo", "ls-remote", "origin", "HEAD"])
            self.assertNotIn("env", execute.call_args.kwargs)
        with patch.dict(os.environ, {"GIT_SSH_COMMAND": "unexpected"}, clear=True):
            self.assertEqual(probe(Path("/example/repo")), "SSH_OVERRIDE_PRESENT")
        with patch.dict(os.environ, {}, clear=True), patch.object(self.module["subprocess"], "run") as execute:
            execute.side_effect = [
                __import__("subprocess").CompletedProcess([], 1, "", ""),
                __import__("subprocess").CompletedProcess([], 0, "git@example.com:example/repo.git\n", ""),
                __import__("subprocess").CompletedProcess([], 0, strict, ""),
                __import__("subprocess").CompletedProcess([], 128, "", "Bad owner or permissions on private path"),
            ]
            self.assertEqual(probe(Path("/example/repo")), "SSH_OWNERSHIP_ERROR")
        with patch.dict(os.environ, {}, clear=True), patch.object(self.module["subprocess"], "run") as execute:
            execute.side_effect = [
                __import__("subprocess").CompletedProcess([], 1, "", ""),
                __import__("subprocess").CompletedProcess([], 0, "git@example.com:example/repo.git\n", ""),
                __import__("subprocess").CompletedProcess([], 0, strict.replace("stricthostkeychecking true", "stricthostkeychecking ask"), ""),
            ]
            self.assertEqual(probe(Path("/example/repo")), "SSH_POLICY_UNVERIFIED")
            self.assertEqual(execute.call_count, 3)  # no connection after weaker policy



if __name__ == "__main__":
    os.umask(0o077)
    unittest.main()

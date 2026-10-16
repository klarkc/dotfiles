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
            "hostname": "host.example.invalid",
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
        server = (root / ".config/systemd/user/desktop-commander-mcp-runtime.service").read_text()
        tunnel = (root / ".config/systemd/user/desktop-commander-mcp-tunnel.service").read_text()
        for unit in (server, tunnel):
            self.assertIn("%h/.nix-profile/bin/desktop-commander-mcp-runtime", unit)
            self.assertIn("UMask=0077", unit)
            self.assertIn("NoNewPrivileges=yes", unit)
            self.assertIn("KillMode=control-group", unit)
            self.assertNotIn("nix build", unit)
            self.assertNotIn("npx", unit)
        self.assertIn("ProtectHome=read-only", tunnel)
        self.assertIn("PartOf=desktop-commander-mcp-runtime.service", tunnel)
        text = source.read_text()
        self.assertIn('"--host", "127.0.0.1"', text)
        self.assertIn('"--stateless"', text)
        self.assertIn('"--no-onboarding"', text)
        self.assertIn('"DESKTOP_COMMANDER_DISABLE_TELEMETRY": "1"', text)
        self.assertNotIn('"--pass-environment"', text)
        self.assertNotIn('"desktop-commander", "remote"', text)
        self.assertEqual(controller["STATE"], ".profile_override.desktop-commander-mcp")

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


if __name__ == "__main__":
    os.umask(0o077)
    unittest.main()

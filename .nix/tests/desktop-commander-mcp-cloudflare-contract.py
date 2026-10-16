import json
import os
from pathlib import Path
import runpy
import sys
import tempfile
import unittest
from unittest.mock import patch


source = Path(sys.argv.pop(1))
module = runpy.run_path(str(source))


class FakeAPI:
    def __init__(self):
        self.calls = []
        self.apps = []
        self.tunnels = []
        self.dns = []
        self.writes = []
        self.email = "owner@example.com"
        self.app = None

    def request(self, values, method, path, payload=None):
        self.calls.append((method, path, payload))
        if path.startswith("/zones?"):
            return [{"id": "zone1", "name": "solosig.com.br", "account": {"id": "acct"}}]
        if path == "/zones/zone1":
            return {"name": "solosig.com.br", "account": {"id": "acct"}, "status": "active"}
        if path == "/zones/zone1/ssl/certificate_packs":
            return {"certs": [{"status": "active", "hosts": ["*.solosig.com.br"], "type": "universal"}]}
        if path == "/accounts/acct/access/organizations":
            return {"auth_domain": "solotopografia.cloudflareaccess.com"}
        if path == "/accounts/acct/access/apps":
            if method == "GET":
                return self.apps
            self.writes.append((method, path, payload))
            self.app = {"id": "app1", "aud": "a" * 64, "domain": payload["domain"],
                        "type": "self_hosted", **payload}
            self.app["policies"] = payload.get("policies", [])
            self.apps.append(self.app)
            return self.app
        if path == "/accounts/acct/cfd_tunnel?is_deleted=false":
            return self.tunnels
        if path == "/accounts/acct/cfd_tunnel":
            self.writes.append((method, path, payload))
            self.tunnels.append({"id": "12345678-1234-4234-8234-123456789abc", "name": payload["name"], "config_src": "local"})
            return self.tunnels[-1]
        if path == "/accounts/acct/access/apps/app1":
            return self.app
        if path == "/accounts/acct/access/apps/app1/policies":
            return self.app["policies"]
        if path.startswith("/zones/zone1/dns_records?"):
            return self.dns
        if path == "/zones/zone1/dns_records":
            self.writes.append((method, path, payload))
            record = {"id": "dns1", **payload}
            self.dns.append(record)
            return record
        raise AssertionError((method, path))


class CloudflareContract(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name)
        self.values = {"CLOUDFLARE_API_TOKEN": "not-a-real-token", "CLOUDFLARE_ACCOUNT_ID": "acct",
                       "CLOUDFLARE_ACCESS_EMAIL": "owner@example.com"}
        self.api = FakeAPI()

    def provision(self):
        with patch.dict(os.environ, {"HOME": str(self.home)}), patch.object(Path, "home", return_value=self.home), \
             patch.dict(module["provision"].__globals__, {"cloudflare_client_request": self.api.request, "validate": lambda settings: settings}):
            module["provision"](self.values, "host.example.invalid")

    def test_owner_policy_exact_dns_last_and_private_secret(self):
        self.provision()
        app_call = next(call for call in self.api.writes if call[1] == "/accounts/acct/access/apps")
        app = app_call[2]
        self.assertEqual(app["policies"][0]["decision"], "allow")
        self.assertEqual(app["policies"][0]["include"], [{"email": {"email": self.values["CLOUDFLARE_ACCESS_EMAIL"]}}])
        self.assertEqual(app["oauth_configuration"]["grant"], {"access_token_lifetime": "15m", "session_duration": "168h"})
        dcr = app["oauth_configuration"]["dynamic_client_registration"]
        self.assertEqual(dcr["allowed_uris"], module["REDIRECT_URIS"])
        self.assertTrue(dcr["enabled"])
        self.assertFalse(dcr["allow_any_on_localhost"])
        self.assertFalse(dcr["allow_any_on_loopback"])
        self.assertEqual(self.api.writes[-1][1], "/zones/zone1/dns_records")
        state = self.home / module["STATE_DIR"]
        self.assertEqual((state / "credentials.json").stat().st_mode & 0o777, 0o600)
        credential = json.loads((state / "credentials.json").read_text())
        self.assertEqual(credential["TunnelID"], "12345678-1234-4234-8234-123456789abc")
        self.assertNotIn(self.values["CLOUDFLARE_API_TOKEN"], (state / "credentials.json").read_text())
        self.assertEqual((state / "cloudflare-state.json").stat().st_mode & 0o777, 0o600)
        self.assertEqual((state / "config.json").stat().st_mode & 0o777, 0o600)
        self.assertNotIn(self.values["CLOUDFLARE_API_TOKEN"], (state / "cloudflare-state.json").read_text())

    def test_authentication_is_verified_before_dns_and_re_run_refuses_existing_dns(self):
        self.provision()
        calls = [(method, path) for method, path, _ in self.api.calls]
        self.assertLess(calls.index(("GET", "/accounts/acct/access/apps/app1/policies")),
                        calls.index(("POST", "/zones/zone1/dns_records")))
        self.provision()
        self.assertEqual(sum(path == "/accounts/acct/cfd_tunnel" for _, path, _ in self.api.writes), 1)
        self.assertEqual(sum(path == "/accounts/acct/access/apps" for _, path, _ in self.api.writes), 1)
        self.provision()
        self.assertEqual(sum(path == "/accounts/acct/cfd_tunnel" for _, path, _ in self.api.writes), 1)
        self.assertEqual(sum(path == "/accounts/acct/access/apps" for _, path, _ in self.api.writes), 1)

    def test_conflicting_app_and_preexisting_dns_are_never_overwritten(self):
        self.api.apps = [{"id": "unrelated", "name": "other", "domain": "*.solosig.com.br"}]
        with self.assertRaises(ValueError):
            self.provision()
        self.assertEqual(self.api.writes, [])
        self.api.apps = []
        self.api.dns = [{"id": "existing", "name": "host.example.invalid", "type": "A"}]
        with self.assertRaises(ValueError):
            self.provision()
        self.assertFalse(any(path == "/zones/zone1/dns_records" for _, path, _ in self.api.writes))
        self.assertEqual(self.api.writes, [])

    def test_policy_or_oauth_drift_prevents_reuse(self):
        self.provision()
        self.api.app["policies"].append({"decision": "bypass", "include": [{"everyone": {}}]})
        with self.assertRaises(ValueError):
            self.provision()
        self.api.app["policies"].pop()
        self.api.app["oauth_configuration"]["dynamic_client_registration"]["allowed_uris"].append("https://evil.example/*")
        with self.assertRaises(ValueError):
            self.provision()

    def test_certificate_wildcard_is_one_level_only(self):
        covers = module["certificate_covers"]
        self.assertTrue(covers("*.solosig.com.br", "host.example.invalid"))
        self.assertFalse(covers("*.solosig.com.br", "host.example.invalid"))

    def test_access_path_conflict_is_not_adopted(self):
        self.api.apps = [{"id": "other", "domain": "host.example.invalid/private"}]
        with self.assertRaises(ValueError):
            self.provision()
        self.assertEqual(self.api.writes, [])

    def test_only_fixed_hostname_is_accepted(self):
        with self.assertRaises(ValueError):
            module["provision"](self.values, "host.example.invalid")

    def test_controller_settings_validate_uuid_audience_and_private_credential(self):
        cred = self.home / "credential.json"
        cred.write_text(json.dumps({"TunnelID": "12345678-1234-4234-8234-123456789abc", "TunnelSecret": "private"}))
        cred.chmod(0o600)
        settings = module["controller_settings"]("host.example.invalid", "12345678-1234-4234-8234-123456789abc",
                                                 "solotopografia", "a" * 64, cred)
        self.assertEqual(settings["audience"], "a" * 64)
        self.assertNotIn("private", json.dumps(settings))
        with self.assertRaises(ValueError):
            module["controller_settings"]("host.example.invalid", "bad-id", "team", "a" * 64, cred)


if __name__ == "__main__":
    os.umask(0o077)
    unittest.main()

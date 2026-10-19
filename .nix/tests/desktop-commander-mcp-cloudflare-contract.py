import contextlib
import io
import os
from pathlib import Path
import runpy
import sys
import unittest
from unittest.mock import patch

source = Path(sys.argv.pop(1))
module = runpy.run_path(str(source))


class FakeAPI:
    def __init__(self):
        self.calls = []
        self.writes = []
        self.apps, self.tunnels, self.dns = [], [], []
        self.app = None

    def request(self, values, method, path, payload=None):
        self.calls.append((method, path, payload))
        if path.startswith("/zones?name="):
            return [{"id": "zone-id", "name": "example.com", "account": {"id": "account-id"}}]
        if path == "/zones/zone-id":
            return {"name": "example.com", "account": {"id": "account-id"}, "status": "active"}
        if path.endswith("/ssl/certificate_packs"):
            return {"certs": [{"status": "active", "hosts": ["*.example.com"]}]}
        if path.endswith("/access/organizations"):
            return {"auth_domain": "team-example.cloudflareaccess.com"}
        if path.endswith("/access/apps"):
            if method == "GET": return self.apps
            self.writes.append((method, path, payload))
            self.app = {"id": "app-id", "aud": "a" * 64, "domain": payload["domain"],
                        "type": "self_hosted", "oauth_configuration": payload["oauth_configuration"]}
            self.app["policies"] = payload["policies"]
            self.apps.append({**self.app, "name": payload["name"]})
            return self.app
        if path == "/accounts/account-id/cfd_tunnel?is_deleted=false": return self.tunnels
        if path == "/accounts/account-id/cfd_tunnel":
            self.writes.append((method, path, payload))
            tunnel = {"id": "tunnel-id", "name": payload["name"]}
            self.tunnels.append(tunnel)
            return tunnel
        if path == "/accounts/account-id/access/apps/app-id": return self.app
        if path == "/accounts/account-id/access/apps/app-id/policies": return self.app["policies"]
        if path.startswith("/zones/zone-id/dns_records?"): return self.dns
        if path == "/zones/zone-id/dns_records":
            self.writes.append((method, path, payload))
            self.dns.append({"id": "dns-id", **payload})
            return self.dns[-1]
        raise AssertionError("Unexpected fake API request")


class Contract(unittest.TestCase):
    def setUp(self):
        self.values = {"CLOUDFLARE_API_TOKEN": "synthetic-token", "CLOUDFLARE_ACCOUNT_ID": "account-id",
            "CLOUDFLARE_ACCESS_EMAIL": "owner@example.com", "DESKTOP_COMMANDER_MCP_HOSTNAME": "mcp.example.com",
            "CLOUDFLARE_ZONE_NAME": "example.com", "DESKTOP_COMMANDER_MCP_RESOURCE_NAME": "team-example-resource"}
        self.profile = {}
        self.api = FakeAPI()

    def persist(self, updates):
        for key, value in updates.items():
            if key in self.profile and self.profile[key] != value:
                raise ValueError("Profile checkpoint conflicts")
        self.profile.update(updates)
        return dict(updates)

    def run_provision(self):
        with patch.dict(module["provision"].__globals__, {
            "api": self.api.request, "load_profile": lambda required=(): dict(self.values),
            "append_profile": self.persist, "private_environment": lambda: (lambda required=(): dict(self.values), self.persist)}):
            module["provision"](self.values)

    def test_provisions_profile_checkpoints_only_and_is_idempotent(self):
        self.run_provision()
        self.assertEqual(self.profile["CLOUDFLARE_TUNNEL_ID"], "tunnel-id")
        self.assertEqual(len(__import__("base64").b64decode(self.profile["CLOUDFLARE_TUNNEL_SECRET"])), 32)
        self.assertEqual(self.profile["CLOUDFLARE_ACCESS_APP_ID"], "app-id")
        self.assertEqual(self.profile["CLOUDFLARE_MCP_DNS_RECORD_ID"], "dns-id")
        self.assertEqual(self.api.writes[-1][1], "/zones/zone-id/dns_records")
        self.run_provision()
        self.assertEqual(len(self.api.writes), 3)

    def test_missing_required_environment_fails_closed(self):
        with patch.dict(module["environment"].__globals__, {"private_environment": lambda:
             (lambda required=(): (_ for _ in ()).throw(ValueError("Missing required profile settings")), self.persist)}):
            with self.assertRaises(ValueError): module["environment"]()

    def test_conflicting_dns_prevents_every_write(self):
        self.api.dns = [{"id": "unowned", "type": "A"}]
        with self.assertRaises(ValueError): self.run_provision()
        self.assertEqual(self.api.writes, [])

    def test_owner_and_oauth_are_verified_before_dns(self):
        self.run_provision()
        calls = [(method, path) for method, path, _ in self.api.calls]
        self.assertLess(calls.index(("GET", "/accounts/account-id/access/apps/app-id/policies")),
                        calls.index(("POST", "/zones/zone-id/dns_records")))
        app_payload = next(payload for _, path, payload in self.api.writes if path.endswith("/access/apps"))
        self.assertEqual(app_payload["policies"][0]["include"], [{"email": {"email": "owner@example.com"}}])
        self.assertEqual(set(app_payload["oauth_configuration"]["dynamic_client_registration"]["allowed_uris"]),
                          set(module["REDIRECT_URIS"]))

    def test_unowned_named_resources_are_not_duplicated(self):
        self.api.tunnels = [{"id": "unowned", "name": self.values["DESKTOP_COMMANDER_MCP_RESOURCE_NAME"]}]
        with self.assertRaises(ValueError):
            self.run_provision()
        self.assertEqual(self.api.writes, [])

    def test_invalid_hostname_is_rejected_before_requests(self):
        self.values["DESKTOP_COMMANDER_MCP_HOSTNAME"] = "*.example.com"
        with self.assertRaises(ValueError):
            self.run_provision()
        self.assertEqual(self.api.calls, [])

    def test_inspect_does_not_print_domain_team_or_resource_names(self):
        def inspect_api(values, method, path, payload=None):
            if path.startswith("/zones?name="): return [{"id": "zone-id", "name": "example.com", "account": {"id": "account-id"}}]
            if path == "/user/tokens/verify": return {"status": "active"}
            if path == "/zones/zone-id": return {"name": "example.com", "status": "active", "account": {"id": "account-id"}}
            if path.endswith("certificate_packs"): return {"certs": []}
            if path.endswith("organizations"): return {"auth_domain": "team-example.cloudflareaccess.com"}
            return []
        output = io.StringIO()
        with patch.dict(module["inspect"].__globals__, {"api": inspect_api}), contextlib.redirect_stdout(output):
            module["inspect"](self.values)
        for private_detail in ("example.com", "team-example", "team-example-resource", "owner@example.com"):
            self.assertNotIn(private_detail, output.getvalue())


if __name__ == "__main__":
    os.umask(0o077)
    unittest.main()

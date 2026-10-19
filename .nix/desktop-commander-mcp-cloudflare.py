"""Inspect or provision the configured Desktop Commander Cloudflare resources."""

import argparse
import base64
import json
from pathlib import Path
import re
import runpy
import secrets
import sys
import urllib.error
import urllib.parse
import urllib.request

REQUIRED = ("CLOUDFLARE_API_TOKEN", "CLOUDFLARE_ACCOUNT_ID", "CLOUDFLARE_ACCESS_EMAIL",
            "DESKTOP_COMMANDER_MCP_HOSTNAME", "CLOUDFLARE_ZONE_NAME",
            "DESKTOP_COMMANDER_MCP_RESOURCE_NAME")
CHECKPOINTS = ("CLOUDFLARE_TUNNEL_ID", "CLOUDFLARE_TUNNEL_SECRET",
               "CLOUDFLARE_ACCESS_APP_ID", "CLOUDFLARE_ACCESS_AUDIENCE",
               "CLOUDFLARE_ACCESS_TEAM", "CLOUDFLARE_MCP_DNS_RECORD_ID")
REDIRECT_URIS = ["https://chatgpt.com/connector_platform_oauth_redirect",
                 "https://chatgpt.com/connector/oauth/*"]


def private_environment():
    helper = runpy.run_path(str(Path(__file__).with_name("private-environment.py")))
    return helper["load_profile"], helper["append_profile"]


def environment():
    load_profile, _ = private_environment()
    return load_profile(required=REQUIRED)


def api(values, method, path, payload=None):
    body = None if payload is None else json.dumps(payload).encode()
    req = urllib.request.Request("https://api.cloudflare.com/client/v4" + path, data=body,
        method=method, headers={"Authorization": "Bearer " + values["CLOUDFLARE_API_TOKEN"],
        "Accept": "application/json", "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=25) as response:
            data = json.load(response)
    except urllib.error.HTTPError:
        raise ValueError("Cloudflare request failed (response hidden)") from None
    if not isinstance(data, dict) or not data.get("success"):
        raise ValueError("Cloudflare rejected request (response hidden)")
    return data.get("result")


def paginated(values, path):
    result = api(values, "GET", path)
    return result if isinstance(result, list) else []


def locate_zone(values):
    name = values["CLOUDFLARE_ZONE_NAME"]
    account = values["CLOUDFLARE_ACCOUNT_ID"]
    configured = values.get("CLOUDFLARE_ZONE_ID")
    zones = paginated(values, "/zones?name=" + urllib.parse.quote(name))
    matches = [z for z in zones if z.get("name") == name and z.get("account", {}).get("id") == account]
    if configured:
        if not any(z.get("id") == configured for z in matches):
            raise ValueError("Configured zone does not match the configured account and zone name")
        return configured
    if len(matches) != 1:
        raise ValueError("Expected exactly one matching configured zone")
    return matches[0]["id"]


def certificate_covers(pattern, hostname):
    return pattern == hostname or (pattern.startswith("*.") and hostname.split(".", 1)[-1] == pattern[2:])


def inspect(values):
    zone = locate_zone(values)
    account, hostname = values["CLOUDFLARE_ACCOUNT_ID"], values["DESKTOP_COMMANDER_MCP_HOSTNAME"]
    checks = [("token", "/user/tokens/verify"), ("zone", f"/zones/{zone}"),
              ("certificates", f"/zones/{zone}/ssl/certificate_packs"),
              ("organization", f"/accounts/{account}/access/organizations"),
              ("applications", f"/accounts/{account}/access/apps"),
              ("tunnels", f"/accounts/{account}/cfd_tunnel?is_deleted=false"),
              ("dns", f"/zones/{zone}/dns_records?name={urllib.parse.quote(hostname)}")]
    output = {}
    for label, path in checks:
        result = api(values, "GET", path)
        if label == "token":
            output[label] = {"status": result.get("status")}
        elif label == "zone":
            output[label] = {"active": result.get("status") == "active",
                             "account_matches": result.get("account", {}).get("id") == account}
        elif label == "certificates":
            certs = result.get("certs", []) if isinstance(result, dict) else result
            output[label] = {"covers_hostname": any(c.get("status") == "active" and any(
                certificate_covers(h, hostname) for h in c.get("hosts", [])) for c in certs)}
        elif label == "organization":
            output[label] = {"exists": bool(isinstance(result, dict) and result.get("auth_domain"))}
        else:
            output[label] = {"count": len(result) if isinstance(result, list) else int(bool(result))}
    for label, result in output.items():
        print(label + ": " + json.dumps(result))


def hostname_conflicts(hostname, apps):
    for app in apps:
        domain = app.get("domain", "").lower().split("/", 1)[0].rstrip(".")
        if domain and (domain == hostname or domain.startswith("*.") and hostname.endswith(domain[1:])):
            return True
    return False


def verify_app(values, account_path, app_id, hostname):
    app = api(values, "GET", account_path + f"/access/apps/{app_id}")
    if not app.get("aud") or app.get("domain") != hostname or app.get("type") != "self_hosted":
        raise ValueError("Dedicated Access application has drifted")
    policies = paginated(values, account_path + f"/access/apps/{app_id}/policies")
    if len(policies) != 1 or policies[0].get("decision") != "allow" or policies[0].get("include") != [
        {"email": {"email": values["CLOUDFLARE_ACCESS_EMAIL"]}}]:
        raise ValueError("Access policy must allow only the configured owner")
    oauth = app.get("oauth_configuration", {})
    dcr = oauth.get("dynamic_client_registration", {})
    if oauth.get("enabled") is not True or dcr.get("enabled") is not True or set(dcr.get("allowed_uris", [])) != set(REDIRECT_URIS):
        raise ValueError("Managed OAuth or restricted client registration is not configured")
    if dcr.get("allow_any_on_localhost") or dcr.get("allow_any_on_loopback"):
        raise ValueError("Unexpected OAuth loopback client permission")
    return app


def provision(values, hostname=None):
    hostname = hostname or values["DESKTOP_COMMANDER_MCP_HOSTNAME"]
    if hostname != values["DESKTOP_COMMANDER_MCP_HOSTNAME"]:
        raise ValueError("Hostname must come from the private profile")
    if not re.fullmatch(r"(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z]{2,63}", hostname):
        raise ValueError("Configured hostname must be an exact DNS name")
    zone_name = values["CLOUDFLARE_ZONE_NAME"]
    if hostname != zone_name and not hostname.endswith("." + zone_name):
        raise ValueError("Configured hostname must belong to the configured zone")
    account, resource = values["CLOUDFLARE_ACCOUNT_ID"], values["DESKTOP_COMMANDER_MCP_RESOURCE_NAME"]
    zone = locate_zone(values)
    zone_obj = api(values, "GET", f"/zones/{zone}")
    if zone_obj.get("name") != values["CLOUDFLARE_ZONE_NAME"] or zone_obj.get("account", {}).get("id") != account:
        raise ValueError("Zone/account verification failed")
    cert_data = api(values, "GET", f"/zones/{zone}/ssl/certificate_packs")
    certs = cert_data.get("certs", []) if isinstance(cert_data, dict) else cert_data
    if not any(c.get("status") == "active" and any(certificate_covers(h, hostname) for h in c.get("hosts", [])) for c in certs):
        raise ValueError("An active certificate must already cover the hostname")
    path = f"/accounts/{account}"
    organization = api(values, "GET", path + "/access/organizations")
    team = values.get("CLOUDFLARE_ACCESS_TEAM")
    if not team:
        auth_domain = organization.get("auth_domain", "") if isinstance(organization, dict) else ""
        team = auth_domain.split(".")[0]
        if not team or "." in team:
            raise ValueError("Cloudflare Access organization is unavailable")
        _, append = private_environment()
        values.update(append({"CLOUDFLARE_ACCESS_TEAM": team}))
    dns_path = f"/zones/{zone}/dns_records?name={urllib.parse.quote(hostname)}"
    dns = paginated(values, dns_path)
    if dns and (len(dns) != 1 or not values.get("CLOUDFLARE_MCP_DNS_RECORD_ID") or
                dns[0].get("id") != values["CLOUDFLARE_MCP_DNS_RECORD_ID"] or
                dns[0].get("type") != "CNAME" or
                dns[0].get("content") != values.get("CLOUDFLARE_TUNNEL_ID", "") + ".cfargotunnel.com" or
                dns[0].get("proxied") is not True):
        raise ValueError("DNS hostname exists but is not the exact profile-owned record")
    apps = paginated(values, path + "/access/apps")
    app_id = values.get("CLOUDFLARE_ACCESS_APP_ID")
    if any(a.get("name") == resource and a.get("id") != app_id for a in apps):
        raise ValueError("Resource name belongs to an unowned Access application")
    named_apps = [a for a in apps if a.get("id") == app_id] if app_id else []
    conflicts = [a for a in apps if hostname_conflicts(hostname, [a])]
    if conflicts and (len(named_apps) != 1 or conflicts != named_apps or named_apps[0].get("name") != resource or named_apps[0].get("domain") != hostname):
        raise ValueError("Conflicting Access application exists; refusing to modify it")
    tunnel_id, secret = values.get("CLOUDFLARE_TUNNEL_ID"), values.get("CLOUDFLARE_TUNNEL_SECRET")
    tunnels = paginated(values, path + "/cfd_tunnel?is_deleted=false")
    if any(t.get("name") == resource and t.get("id") != tunnel_id for t in tunnels):
        raise ValueError("Resource name belongs to an unowned tunnel")
    tunnel = next((t for t in tunnels if t.get("id") == tunnel_id), None) if tunnel_id else None
    if tunnel and tunnel.get("name") != resource:
        raise ValueError("Configured tunnel ownership does not match resource name")
    if not tunnel:
        if tunnel_id or secret:
            raise ValueError("Configured tunnel checkpoint is missing; refusing to recreate")
        secret = base64.b64encode(secrets.token_bytes(32)).decode("ascii")
        tunnel = api(values, "POST", path + "/cfd_tunnel", {"name": resource, "config_src": "local", "tunnel_secret": secret})
        if not tunnel or not tunnel.get("id"):
            raise ValueError("Tunnel creation response was incomplete")
        _, append = private_environment()
        values.update(append({"CLOUDFLARE_TUNNEL_ID": tunnel["id"], "CLOUDFLARE_TUNNEL_SECRET": secret}))
    elif not secret:
        raise ValueError("Configured tunnel secret is missing")
    if app_id:
        if not named_apps:
            raise ValueError("Configured Access application is missing; refusing to recreate")
        app = verify_app(values, path, app_id, hostname)
    else:
        if conflicts:
            raise ValueError("Access application is not profile-owned")
        app = api(values, "POST", path + "/access/apps", {"name": resource, "domain": hostname, "type": "self_hosted",
            "session_duration": "24h", "app_launcher_visible": False,
            "oauth_configuration": {"enabled": True, "dynamic_client_registration": {"enabled": True,
                "allowed_uris": list(REDIRECT_URIS), "allow_any_on_localhost": False, "allow_any_on_loopback": False},
                "grant": {"access_token_lifetime": "15m", "session_duration": "168h"}},
            "policies": [{"name": resource + " owner", "decision": "allow",
                "include": [{"email": {"email": values["CLOUDFLARE_ACCESS_EMAIL"]}}], "exclude": [], "require": []}]})
        if not app or not app.get("id") or not (app.get("aud") or app.get("audience")):
            raise ValueError("Access application response incomplete")
        _, append = private_environment()
        values.update(append({"CLOUDFLARE_ACCESS_APP_ID": app["id"], "CLOUDFLARE_ACCESS_AUDIENCE": app.get("aud") or app.get("audience")}))
    app = verify_app(values, path, values["CLOUDFLARE_ACCESS_APP_ID"], hostname)
    aud = app.get("aud") or app.get("audience")
    if values.get("CLOUDFLARE_ACCESS_AUDIENCE") != aud:
        raise ValueError("Configured Access audience does not match owned application")
    if dns:
        record_id = values.get("CLOUDFLARE_MCP_DNS_RECORD_ID")
        if len(dns) != 1 or not record_id or dns[0].get("id") != record_id or dns[0].get("type") != "CNAME" or dns[0].get("content") != tunnel["id"] + ".cfargotunnel.com" or dns[0].get("proxied") is not True:
            raise ValueError("DNS hostname exists but is not the exact profile-owned record")
        print("Provisioning already verified; resources unchanged")
        return
    record = api(values, "POST", f"/zones/{zone}/dns_records", {"type": "CNAME", "name": hostname,
        "content": tunnel["id"] + ".cfargotunnel.com", "proxied": True})
    if not record or not record.get("id"):
        raise ValueError("DNS creation response was incomplete")
    _, append = private_environment()
    values.update(append({"CLOUDFLARE_MCP_DNS_RECORD_ID": record["id"]}))
    print("Provisioning complete; dedicated resources verified")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command")
    sub.add_parser("inspect", help="Read-only Cloudflare preflight")
    sub.add_parser("provision", help="Create configured dedicated resources")
    args = parser.parse_args(argv)
    values = environment()
    (provision if args.command == "provision" else inspect)(values)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, TypeError, KeyError, IndexError):
        sys.exit("Cloudflare operation failed; details and response contents hidden")

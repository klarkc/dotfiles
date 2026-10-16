"""Inspect Cloudflare state or provision dedicated Desktop Commander resources."""

import argparse
import base64
import json
import os
from pathlib import Path
import re
import runpy
import secrets
import stat
import subprocess
import sys
import uuid
import urllib.error
import urllib.parse
import urllib.request


DEFAULT_HOSTNAME = "host.example.invalid"
ZONE_NAME = "solosig.com.br"
RESOURCE_NAME = "host-example-desktop-commander-mcp"
STATE_DIR = ".profile_override.desktop-commander-mcp"
REDIRECT_URIS = ["https://chatgpt.com/connector_platform_oauth_redirect",
                 "https://chatgpt.com/connector/oauth/*"]


def environment():
    profile = Path.home() / ".profile_override"
    info = profile.stat()
    if info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError("Private .profile_override permission check failed")
    loaded = subprocess.run(
        ["bash", "--noprofile", "--norc", "-c",
         'set +x; set -a; source "$HOME/.profile_override" >/dev/null 2>&1 || exit 2; exec env -0'],
        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, check=False,
    )
    if loaded.returncode:
        raise ValueError("Private environment could not be loaded")
    values = {}
    for entry in loaded.stdout.split(b"\0"):
        key, separator, value = entry.partition(b"=")
        if separator:
            values[key.decode()] = value.decode()
    if not all(values.get(k) for k in ("CLOUDFLARE_API_TOKEN", "CLOUDFLARE_ACCOUNT_ID", "CLOUDFLARE_ACCESS_EMAIL")):
        raise ValueError("Required Cloudflare variables are missing")
    return values


def api(values, method, path, payload=None):
    return cloudflare_client_request(values, method, path, payload)


def cloudflare_client_request(values, method, path, payload=None):
    body = None if payload is None else json.dumps(payload).encode()
    req = urllib.request.Request("https://api.cloudflare.com/client/v4" + path, data=body,
        method=method, headers={"Authorization": "Bearer " + values["CLOUDFLARE_API_TOKEN"],
        "Accept": "application/json", "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=25) as response:
            data = json.load(response)
    except urllib.error.HTTPError as exc:
        exc.close()
        raise ValueError("Cloudflare request failed (response hidden)") from None
    if not isinstance(data, dict):
        return data
    if not data.get("success"):
        raise ValueError("Cloudflare rejected request (response hidden)")
    return data.get("result")


def safe_dir():
    path = Path.home() / STATE_DIR
    path.mkdir(mode=0o700, exist_ok=True)
    info = path.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
        raise ValueError("Private state directory ownership check failed")
    path.chmod(0o700)
    return path


def read_checkpoint(path):
    if not path.exists():
        return {}
    info = path.lstat()
    if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError("Private checkpoint permission check failed")
    value = json.loads(path.read_text())
    if not isinstance(value, dict):
        raise ValueError("Invalid private checkpoint")
    return value


def save_private(path, value):
    tmp = path.with_name(path.name + "." + secrets.token_hex(8) + ".tmp")
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(fd, "w") as stream:
        json.dump(value, stream)
        stream.write("\n")
    os.replace(tmp, path)
    path.chmod(0o600)


def controller_settings(hostname, tunnel_id, team, audience, credentials_path):
    tunnel_id = str(uuid.UUID(tunnel_id))
    if not re.fullmatch(r"[a-fA-F0-9]{64}", audience):
        raise ValueError("Invalid Access application audience")
    credential_info = credentials_path.lstat()
    if not stat.S_ISREG(credential_info.st_mode) or credential_info.st_uid != os.getuid() or credential_info.st_mode & 0o077:
        raise ValueError("Private tunnel credential permission check failed")
    credential = json.loads(credentials_path.read_text())
    if credential.get("TunnelID") != tunnel_id or not credential.get("TunnelSecret"):
        raise ValueError("Private tunnel credential does not match tunnel")
    return {"hostname": hostname, "tunnel_id": tunnel_id, "team_name": team,
            "audience": audience, "credentials_file": str(credentials_path)}


def paginated(values, path):
    result = api(values, "GET", path)
    return result if isinstance(result, list) else []


def locate_zone(values, account):
    configured = values.get("CLOUDFLARE_ZONE_ID")
    zones = paginated(values, "/zones?name=" + urllib.parse.quote(ZONE_NAME))
    matches = [z for z in zones if z.get("name") == ZONE_NAME and z.get("account", {}).get("id") == account]
    if configured:
        if not any(z.get("id") == configured for z in matches):
            raise ValueError("Configured zone does not match the account/domain")
        return configured
    if len(matches) != 1:
        raise ValueError("Expected exactly one matching zone")
    return matches[0]["id"]


def certificate_covers(pattern, hostname):
    return pattern == hostname or (
        pattern.startswith("*.") and hostname.split(".", 1)[-1] == pattern[2:]
    )


def inspect(values):
    account = values["CLOUDFLARE_ACCOUNT_ID"]
    zone = locate_zone(values, account)
    checks = [
        ("token", "/user/tokens/verify"),
        ("zone", f"/zones/{zone}"),
        ("certificates", f"/zones/{zone}/ssl/certificate_packs"),
        ("organization", f"/accounts/{account}/access/organizations"),
        ("applications", f"/accounts/{account}/access/apps"),
        ("tunnels", f"/accounts/{account}/cfd_tunnel?is_deleted=false"),
        ("dns", f"/zones/{zone}/dns_records?name={urllib.parse.quote(DEFAULT_HOSTNAME)}"),
    ]
    outputs = {}
    for name, path in checks:
        result = api(values, "GET", path)
        if name == "token":
            outputs[name] = {"status": result.get("status")}
        elif name == "zone":
            outputs[name] = {"domain": result.get("name"), "active": result.get("status") == "active",
                             "account_matches": result.get("account", {}).get("id") == account}
        elif name == "certificates":
            certs = result.get("certs", []) if isinstance(result, dict) else result
            outputs[name] = [{"status": c.get("status"), "covers_hostname": any(
                certificate_covers(h, DEFAULT_HOSTNAME)
                for h in c.get("hosts", [])), "type": c.get("type")} for c in certs]
        elif name == "organization":
            outputs[name] = {"exists": bool(result), "domain": result.get("auth_domain") if isinstance(result, dict) else None}
        elif name == "applications":
            outputs[name] = [{"name": a.get("name"), "domain": a.get("domain"), "type": a.get("type")} for a in result]
        elif name == "tunnels":
            outputs[name] = [{"name": t.get("name"), "status": t.get("status")} for t in result]
        elif name == "dns":
            outputs[name] = [{"name": r.get("name"), "type": r.get("type"), "proxied": r.get("proxied")} for r in result]
    for name, result in outputs.items():
        print(name + ": " + json.dumps(result))


def hostname_conflicts(hostname, apps):
    for app in apps:
        domain = app.get("domain", "").lower().split("/", 1)[0].rstrip(".")
        if not domain:
            continue
        if domain == hostname or domain.startswith("*.") and hostname.endswith(domain[1:]):
            return True
    return False


def verify_app(values, account_path, app_id, hostname):
    app = api(values, "GET", account_path + f"/access/apps/{app_id}")
    if not app.get("aud") or app.get("domain") != hostname or app.get("type") != "self_hosted":
        raise ValueError("Dedicated Access application has drifted")
    policies = paginated(values, account_path + f"/access/apps/{app_id}/policies")
    if len(policies) != 1 or policies[0].get("decision") != "allow" or policies[0].get("include") != [
        {"email": {"email": values["CLOUDFLARE_ACCESS_EMAIL"]}}
    ]:
        raise ValueError("Access policy must allow only the configured owner, with no bypass")
    oauth = app.get("oauth_configuration", {})
    dcr = oauth.get("dynamic_client_registration", {})
    if oauth.get("enabled") is not True or dcr.get("enabled") is not True or set(dcr.get("allowed_uris", [])) != set(REDIRECT_URIS):
        raise ValueError("Access Managed OAuth and restricted client registration must be enabled")
    if dcr.get("allow_any_on_localhost") or dcr.get("allow_any_on_loopback"):
        raise ValueError("Unexpected OAuth loopback client permission")
    return app


def provision(values, hostname):
    if hostname != DEFAULT_HOSTNAME:
        raise ValueError("Only the configured exact hostname is supported")
    account = values["CLOUDFLARE_ACCOUNT_ID"]
    zone = locate_zone(values, account)
    zone_obj = api(values, "GET", f"/zones/{zone}")
    if zone_obj.get("name") != ZONE_NAME or zone_obj.get("account", {}).get("id") != account:
        raise ValueError("Zone/account verification failed")
    cert_data = api(values, "GET", f"/zones/{zone}/ssl/certificate_packs")
    certs = cert_data.get("certs", []) if isinstance(cert_data, dict) else cert_data
    if not any(c.get("status") == "active" and any(certificate_covers(h, hostname)
               for h in c.get("hosts", [])) for c in certs):
        raise ValueError("An active certificate must already cover the hostname")
    account_path = f"/accounts/{account}"
    organization = api(values, "GET", account_path + "/access/organizations")
    if not isinstance(organization, dict) or not organization.get("auth_domain"):
        raise ValueError("Cloudflare Access organization is unavailable")
    team = organization["auth_domain"].split(".")[0]
    if not team or "." in team:
        raise ValueError("Invalid Access organization domain")
    state = safe_dir()
    checkpoint_path = state / "cloudflare-state.json"
    credentials_path = state / "credentials.json"
    checkpoint = read_checkpoint(checkpoint_path)
    dns = paginated(values, f"/zones/{zone}/dns_records?name={urllib.parse.quote(hostname)}")
    if dns and (len(dns) != 1 or not checkpoint.get("dns_id") or dns[0].get("id") != checkpoint["dns_id"]):
        raise ValueError("DNS hostname already exists; refusing to overwrite")

    # Detect both exact app domains and wildcard/path variants before creating anything.
    apps = paginated(values, account_path + "/access/apps")
    named_apps = [a for a in apps if a.get("name") == RESOURCE_NAME]
    conflicts = [a for a in apps if hostname_conflicts(hostname, [a])]
    if conflicts and (len(named_apps) != 1 or conflicts != named_apps):
        raise ValueError("Conflicting Access application exists; refusing to modify it")
    if len(named_apps) > 1:
        raise ValueError("Duplicate dedicated Access applications exist")
    for app in named_apps:
        if app.get("domain") != hostname:
            raise ValueError("Dedicated application has drifted")

    tunnels = paginated(values, account_path + "/cfd_tunnel?is_deleted=false")
    named_tunnels = [t for t in tunnels if t.get("name") == RESOURCE_NAME]
    if len(named_tunnels) > 1:
        raise ValueError("Duplicate dedicated tunnels exist")
    tunnel = named_tunnels[0] if named_tunnels else None
    if tunnel and checkpoint.get("tunnel_id") != tunnel.get("id"):
        raise ValueError("Existing tunnel is not checkpoint-owned; refusing to adopt")
    if not tunnel:
        if checkpoint.get("tunnel_id"):
            raise ValueError("Checkpoint tunnel is missing; refusing to recreate")
        secret = base64.b64encode(secrets.token_bytes(32)).decode("ascii")
        tunnel = api(values, "POST", account_path + "/cfd_tunnel", {
            "name": RESOURCE_NAME, "config_src": "local", "tunnel_secret": secret})
        if not tunnel or not tunnel.get("id"):
            raise ValueError("Tunnel creation response was incomplete")
        save_private(credentials_path, {"AccountTag": account, "TunnelID": tunnel["id"], "TunnelSecret": secret})
        checkpoint["tunnel_id"] = tunnel["id"]
        save_private(checkpoint_path, checkpoint)
    elif not credentials_path.exists():
        raise ValueError("Checkpoint-owned tunnel credentials are missing")
    else:
        cred = read_checkpoint(credentials_path)
        if cred.get("TunnelID") != tunnel.get("id") or cred.get("AccountTag") != account or not cred.get("TunnelSecret"):
            raise ValueError("Private tunnel credential mismatch")

    app_id = checkpoint.get("app_id")
    app = named_apps[0] if named_apps else None
    if app:
        if app_id != app.get("id"):
            raise ValueError("Existing Access app is not checkpoint-owned; refusing to adopt")
        app = verify_app(values, account_path, app_id, hostname)
    else:
        if app_id:
            raise ValueError("Checkpoint Access app is missing; refusing to recreate")
        app = api(values, "POST", account_path + "/access/apps", {
            "name": RESOURCE_NAME, "domain": hostname, "type": "self_hosted",
            "session_duration": "24h", "app_launcher_visible": False,
            "oauth_configuration": {"enabled": True,
                "dynamic_client_registration": {"enabled": True, "allowed_uris": list(REDIRECT_URIS),
                    "allow_any_on_localhost": False, "allow_any_on_loopback": False},
                "grant": {"access_token_lifetime": "15m", "session_duration": "168h"}},
            "policies": [{"name": RESOURCE_NAME + " owner", "decision": "allow",
                "include": [{"email": {"email": values["CLOUDFLARE_ACCESS_EMAIL"]}}],
                "exclude": [], "require": []}],
        })
        if not app or not app.get("id") or not (app.get("aud") or app.get("audience")):
            raise ValueError("Access application response incomplete; checkpoint not yet saved")
        checkpoint["app_id"] = app["id"]
        checkpoint["aud"] = app.get("aud") or app.get("audience")
        save_private(checkpoint_path, checkpoint)

    app = verify_app(values, account_path, checkpoint["app_id"], hostname)
    settings = controller_settings(hostname, tunnel["id"], team, app["aud"], credentials_path)
    controller = runpy.run_path(str(Path(__file__).with_name("desktop-commander-mcp.py")))
    controller["validate"](settings)
    settings_path = state / "config.json"
    if settings_path.exists() and read_checkpoint(settings_path) != settings:
        raise ValueError("Existing local configuration differs; refusing to overwrite")
    save_private(settings_path, settings)

    # DNS stays last: only the verified, owner-restricted Access app authorizes the hostname.
    dns = paginated(values, f"/zones/{zone}/dns_records?name={urllib.parse.quote(hostname)}")
    if dns:
        owned = checkpoint.get("dns_id")
        if len(dns) != 1 or not owned or dns[0].get("id") != owned or dns[0].get("type") != "CNAME" or \
                dns[0].get("content") != tunnel["id"] + ".cfargotunnel.com" or dns[0].get("proxied") is not True:
            raise ValueError("DNS hostname already exists; refusing to overwrite")
        print("Provisioning already checkpointed; resources verified, services not started")
        return
    if app.get("domain") != hostname or not (app.get("aud") or app.get("audience")):
        raise ValueError("Access app/domain/audience verification failed")
    dns_record = api(values, "POST", f"/zones/{zone}/dns_records", {
        "type": "CNAME", "name": hostname, "content": tunnel["id"] + ".cfargotunnel.com", "proxied": True})
    if not dns_record or not dns_record.get("id"):
        raise ValueError("DNS creation response incomplete")
    checkpoint["dns_id"] = dns_record["id"]
    save_private(checkpoint_path, checkpoint)

    print("Provisioning complete: dedicated Access app, tunnel and proxied DNS created/verified; services not started")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command")
    sub.add_parser("inspect", help="Read-only Cloudflare preflight (default)")
    create = sub.add_parser("provision", help="Create only dedicated Cloudflare resources")
    create.add_argument("--hostname", default=DEFAULT_HOSTNAME)
    args = parser.parse_args(argv)
    values = environment()
    if args.command == "provision":
        provision(values, args.hostname)
    else:
        inspect(values)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, TypeError, KeyError, IndexError):
        sys.exit("Cloudflare operation failed; details and response contents hidden")

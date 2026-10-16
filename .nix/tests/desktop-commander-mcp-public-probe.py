import json
import re
import sys
import urllib.error
import urllib.request
from urllib.parse import urlparse


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def request(url, body=None, headers=None):
    req = urllib.request.Request(url, data=body, headers={"User-Agent": "desktop-commander-mcp-probe/1.0", **(headers or {})})
    opener = urllib.request.build_opener(NoRedirect)
    try:
        response = opener.open(req, timeout=20)
    except urllib.error.HTTPError as exc:
        response = exc
    with response:
        return response.status, response.headers, response.read(1024 * 1024)


def main():
    hostname = sys.argv[1]
    if not re.fullmatch(r"[a-z0-9.-]+\.[a-z]{2,63}", hostname):
        sys.exit("Expected a hostname only")
    base = f"https://{hostname}"
    body = json.dumps({
        "jsonrpc": "2.0", "id": 1, "method": "initialize",
        "params": {"protocolVersion": "2025-03-26", "capabilities": {},
                   "clientInfo": {"name": "unauthenticated-probe", "version": "1"}},
    }).encode()
    headers = {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"}
    status, response_headers, _ = request(base + "/mcp", body, headers)
    if status != 401:
        sys.exit(f"FAIL: expected OAuth 401 without credentials, got {status}; a browser redirect is not MCP OAuth")
    challenge = response_headers.get("WWW-Authenticate", "")
    if not challenge.lower().startswith("bearer "):
        sys.exit("FAIL: missing Bearer discovery challenge")
    status, _, _ = request(base + "/mcp", body, {**headers, "Authorization": "Bearer invalid-probe-not-a-secret"})
    if status not in (401, 403):
        sys.exit(f"FAIL: invalid bearer was not rejected ({status})")
    metadata_url = re.search(r'resource_metadata="([^"]+)"', challenge)
    discovery_url = metadata_url.group(1) if metadata_url else base + "/.well-known/oauth-authorization-server"
    if urlparse(discovery_url).scheme != "https":
        sys.exit("FAIL: OAuth discovery must use HTTPS")
    status, _, payload = request(discovery_url)
    if status != 200:
        sys.exit(f"FAIL: OAuth discovery returned {status}")
    metadata = json.loads(payload)
    if "authorization_servers" in metadata:
        issuer = metadata["authorization_servers"][0].rstrip("/")
        if urlparse(issuer).scheme != "https":
            sys.exit("FAIL: authorization server must use HTTPS")
        parsed = urlparse(issuer)
        discovery = f"{parsed.scheme}://{parsed.netloc}/.well-known/oauth-authorization-server{parsed.path}"
        status, _, payload = request(discovery)
        if status != 200:
            sys.exit(f"FAIL: authorization server discovery returned {status}")
        metadata = json.loads(payload)
    for name in ("authorization_endpoint", "token_endpoint", "registration_endpoint"):
        if urlparse(metadata.get(name, "")).scheme != "https":
            sys.exit(f"FAIL: missing HTTPS {name}; check dynamic client registration")
    if "S256" not in metadata.get("code_challenge_methods_supported", []):
        sys.exit("FAIL: OAuth discovery does not advertise PKCE S256")
    print("PASS: public TLS verified, unauthenticated/invalid bearer rejected, OAuth discovery and PKCE available")
    print("PENDING: authorized OAuth flow, token refresh and real calls originating in ChatGPT Web")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, IndexError):
        sys.exit("FAIL: public TLS/DNS/OAuth probe failed (response and credential values hidden)")

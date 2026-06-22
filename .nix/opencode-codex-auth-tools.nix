{ pkgs }:

pkgs.writeShellApplication {
  name = "opencode-codex-auth-import";
  runtimeInputs = [ pkgs.python3 ];
  text = ''
    python3 - "$@" <<'PY'
    import base64
    import json
    import os
    import sys
    import time
    from pathlib import Path


    def fail(message: str) -> None:
        print(f"error: {message}", file=sys.stderr)
        sys.exit(1)


    def decode_jwt(token: str) -> dict:
        try:
            payload = token.split(".")[1]
            payload += "=" * ((4 - len(payload) % 4) % 4)
            return json.loads(base64.urlsafe_b64decode(payload))
        except Exception:
            return {}


    def jwt_exp_ms(token: str) -> int:
        exp = int(decode_jwt(token).get("exp", 0) or 0)
        if exp > 0:
            return exp * 1000
        return int((time.time() + 3600) * 1000)


    def account_id_from_token(token: str) -> str | None:
        claims = decode_jwt(token)
        if claims.get("chatgpt_account_id"):
            return claims["chatgpt_account_id"]
        auth_claims = claims.get("https://api.openai.com/auth", {})
        if isinstance(auth_claims, dict) and auth_claims.get("chatgpt_account_id"):
            return auth_claims["chatgpt_account_id"]
        orgs = claims.get("organizations")
        if isinstance(orgs, list) and orgs and isinstance(orgs[0], dict):
            return orgs[0].get("id")
        return None


    home = Path.home()
    codex_home = Path(os.environ.get("CODEX_HOME", "~/.codex")).expanduser()
    src = codex_home / "auth.json"

    xdg_data_home = Path(os.environ.get("XDG_DATA_HOME", home / ".local/share")).expanduser()
    dst = Path(os.environ.get("OPENCODE_AUTH_FILE", xdg_data_home / "opencode/auth.json")).expanduser()

    if not src.exists():
        fail(f"Codex auth not found: {src}")

    with src.open("r", encoding="utf-8") as handle:
        codex_auth = json.load(handle)

    tokens = codex_auth.get("tokens", codex_auth)
    access = tokens.get("access_token") or tokens.get("access")
    refresh = tokens.get("refresh_token") or tokens.get("refresh")
    id_token = tokens.get("id_token") or tokens.get("id")

    if not access or not refresh:
        fail("Codex auth file does not contain access and refresh tokens")

    account_id = account_id_from_token(id_token or access)
    if not account_id:
        fail("Codex auth token does not contain a ChatGPT account id")

    openai_auth = {
        "type": "oauth",
        "access": access,
        "refresh": refresh,
        "expires": jwt_exp_ms(access),
        "accountId": account_id,
    }

    old_umask = os.umask(0o077)
    try:
        dst.parent.mkdir(parents=True, exist_ok=True)
        existing = {}
        if dst.exists():
            with dst.open("r", encoding="utf-8") as handle:
                existing = json.load(handle)
            if not isinstance(existing, dict):
                existing = {}

        data = {**existing, "openai": openai_auth}
        tmp = dst.with_suffix(".json.tmp")
        with tmp.open("w", encoding="utf-8") as handle:
            json.dump(data, handle, indent=2)
            handle.write("\n")
        os.chmod(tmp, 0o600)
        tmp.replace(dst)
    finally:
        os.umask(old_umask)

    print(f"Imported Codex OAuth credentials into {dst}")
    print("Provider: openai")
    print("Do not commit this file.")
    PY
  '';
}

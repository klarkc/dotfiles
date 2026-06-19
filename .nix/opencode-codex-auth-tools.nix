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


    def jwt_exp_ms(token: str) -> int:
        try:
            payload = token.split(".")[1]
            payload += "=" * ((4 - len(payload) % 4) % 4)
            claims = json.loads(base64.urlsafe_b64decode(payload))
            exp = int(claims.get("exp", 0))
            if exp > 0:
                return exp * 1000
        except Exception:
            pass
        return int((time.time() + 3600) * 1000)


    codex_home = Path(os.environ.get("CODEX_HOME", "~/.codex")).expanduser()
    src = codex_home / "auth.json"
    dst = Path(os.environ.get("OPENCODE_AUTH_FILE", "~/.opencode/auth/openai.json")).expanduser()

    if not src.exists():
        fail(f"Codex auth not found: {src}")

    with src.open("r", encoding="utf-8") as handle:
        codex_auth = json.load(handle)

    tokens = codex_auth.get("tokens", codex_auth)
    access = tokens.get("access_token") or tokens.get("access")
    refresh = tokens.get("refresh_token") or tokens.get("refresh")

    if not access or not refresh:
        fail("Codex auth file does not contain access and refresh tokens")

    opencode_auth = {
        "type": "oauth",
        "access": access,
        "refresh": refresh,
        "expires": jwt_exp_ms(access),
    }

    old_umask = os.umask(0o077)
    try:
        dst.parent.mkdir(parents=True, exist_ok=True)
        tmp = dst.with_suffix(".json.tmp")
        with tmp.open("w", encoding="utf-8") as handle:
            json.dump(opencode_auth, handle, indent=2)
            handle.write("\n")
        os.chmod(tmp, 0o600)
        tmp.replace(dst)
    finally:
        os.umask(old_umask)

    print(f"Imported Codex OAuth credentials into {dst}")
    print("Do not commit this file.")
    PY
  '';
}

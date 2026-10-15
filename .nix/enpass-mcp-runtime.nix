{ pkgs, enpass-mcp-src }:
let
  # Bump note: keep `version` in sync with the pinned upstream package.json
  # and the enpass-mcp-src flake input; recompute npmDepsHash by building
  # .#enpass-mcp-runtime, then run its read-only MCP smoke test.
  version = "0.2.0";
  upstream = pkgs.buildNpmPackage {
    pname = "enpass-mcp";
    inherit version;
    src = enpass-mcp-src;
    nodejs = pkgs.nodejs_22;
    npmDepsHash = "sha256-JFqxllZ8jNrZPrY6cD39GzNSUn3qiqu08JkSIY9tXUM=";
    dontNpmBuild = true;
    # The upstream project uses native SQLCipher and keyring npm modules.
    # All npm sources are prefetched by Nix, never downloaded at build time.
    nativeBuildInputs = with pkgs; [
      python3
      pkg-config
    ];
    # See enpass-mcp-sqlcipher-compat.h: declarations are missing when
    # node-gyp picks the toolchain SQLite header over bundled sqlite3mc.
    NIX_CFLAGS_COMPILE = "-include ${./enpass-mcp-sqlcipher-compat.h}";
  };
in
pkgs.runCommand "enpass-mcp-runtime-${version}" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
  mkdir -p "$out/bin"
  makeWrapper ${pkgs.nodejs_22}/bin/node "$out/bin/enpass-mcp" \
    --add-flags "${upstream}/lib/node_modules/enpass-mcp/src/cli.js"
  # Enforce read-only even if an inherited environment requests write tools.
  # Do not accept master password from env: use the OS Secret Service keyring.
  cat > "$out/bin/enpass-mcp-readonly" <<'SH'
  #!${pkgs.runtimeShell}
  # Remove inherited master passwords even when named for extra vaults.
  for name in $(compgen -e); do
    case "$name" in ENPASS_MASTER_PASSWORD*) unset "$name" ;; esac
  done
  # Never load a local .env containing secret fallback passwords.
  export ENPASS_MCP_CONFIG_DIR="$HOME/.config/enpass-mcp"
  if [ -e "$ENPASS_MCP_CONFIG_DIR/.env" ]; then
    echo "enpass-mcp-readonly: use Secret Service keyring, not a .env file" >&2
    exit 1
  fi
  export ENPASS_MCP_ALLOW_WRITES=0
  exec ${pkgs.nodejs_22}/bin/node ${upstream}/lib/node_modules/enpass-mcp/src/cli.js serve
  SH
  chmod 755 "$out/bin/enpass-mcp-readonly"
''

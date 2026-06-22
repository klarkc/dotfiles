{ pkgs, opencode-src }:

let
  opencodeBase = pkgs.opencode.overrideAttrs (old: {
    src = opencode-src;

    node_modules = old.node_modules.overrideAttrs (_: {
      src = opencode-src;
      outputHash = "sha256-4yjQlxN+U4CKwA/hE8gACuvA4bBeTrX0ACVBIK4UQCg=";
    });
  });

  opencodeCodexAuthImport = pkgs.callPackage ./opencode-codex-auth-tools.nix { };

  syncCodexAuth = pkgs.writeText "opencode-sync-codex-auth.sh" ''
    codex_auth="''${CODEX_HOME:-$HOME/.codex}/auth.json"
    data_home="''${XDG_DATA_HOME:-$HOME/.local/share}"
    opencode_auth="''${OPENCODE_AUTH_FILE:-$data_home/opencode/auth.json}"

    if [ -f "$codex_auth" ] && { [ ! -f "$opencode_auth" ] || [ "$codex_auth" -nt "$opencode_auth" ]; }; then
      ${pkgs.lib.getExe opencodeCodexAuthImport} >/dev/null 2>&1 || true
    fi
  '';
in
pkgs.symlinkJoin {
  name = "${opencodeBase.name}-with-codex-auth";
  paths = [ opencodeBase ];
  nativeBuildInputs = [ pkgs.makeWrapper ];

  postBuild = ''
    wrapProgram $out/bin/opencode \
      --run '. ${syncCodexAuth}'
  '';
}

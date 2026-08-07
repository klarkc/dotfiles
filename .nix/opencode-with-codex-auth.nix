{ pkgs }:

let
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
  name = "${pkgs.opencode.name}-with-codex-auth";
  paths = [ pkgs.opencode ];
  nativeBuildInputs = [ pkgs.makeWrapper ];

  postBuild = ''
    wrapProgram $out/bin/opencode \
      --run '. ${syncCodexAuth}'
  '';
}

{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenvNoCC.mkDerivation {
  pname = "fusion-npm";
  version = "0.27.0";

  nativeBuildInputs = with pkgs; [
    cacert
    makeWrapper
    nodejs
  ];

  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = pkgs.lib.fakeHash;

  buildCommand = ''
    export HOME="$TMPDIR/home"
    export npm_config_cache="$TMPDIR/npm-cache"
    export npm_config_update_notifier=false
    export npm_config_fund=false
    export npm_config_audit=false

    mkdir -p "$HOME" "$npm_config_cache" "$out"

    npm install --global \
      --prefix "$out" \
      --cache "$npm_config_cache" \
      --no-audit \
      --no-fund \
      @runfusion/fusion@0.27.0

    patchShebangs "$out/bin"

    wrapProgram "$out/bin/fusion" \
      --prefix PATH : ${pkgs.lib.makeBinPath (with pkgs; [
        coreutils
        curl
        docker-client
        findutils
        gawk
        gh
        git
        gnugrep
        gnused
        openssh
        python3
        tmux
        uv
      ])}
  '';
}

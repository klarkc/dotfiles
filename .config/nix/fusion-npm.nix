let
  nixpkgs = builtins.fetchTree {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ed67bc86e84e51d4a88e73c7fd36006dc876476f";
    narHash = "sha256-62EWg6lI0qyzm7oAx5cAnGkLutvJsRBe0KkEW2JDZCE=";
  };

  pkgs = import nixpkgs {
    system = builtins.currentSystem;
  };

  fusionNpmPayload = pkgs.stdenvNoCC.mkDerivation {
    pname = "fusion-npm-payload";
    version = "0.28.1";

    nativeBuildInputs = with pkgs; [
      cacert
      nodejs
    ];

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-wL9TQt6liejTOubroJg4WtdeSLSzQhrzNFi5Pt8/Zxg=";

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
        @runfusion/fusion@0.28.1
    '';
  };

  runtimePath = pkgs.lib.makeBinPath (with pkgs; [
    coreutils
    curl
    docker-client
    findutils
    gawk
    gh
    git
    gnugrep
    gnused
    nodejs
    openssh
    python3
    tmux
    uv
  ]);
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "fusion-runtime";
  version = "0.28.1";

  nativeBuildInputs = with pkgs; [
    makeWrapper
  ];

  dontUnpack = true;

  installPhase = ''
    mkdir -p "$out/bin" "$out/lib"
    cp -a ${fusionNpmPayload}/lib/node_modules "$out/lib/"

    makeWrapper ${pkgs.nodejs}/bin/node "$out/bin/fusion" \
      --prefix PATH : ${runtimePath} \
      --add-flags "$out/lib/node_modules/@runfusion/fusion/dist/bin.js"

    ln -s ${pkgs.tmux}/bin/tmux "$out/bin/tmux"
  '';
}

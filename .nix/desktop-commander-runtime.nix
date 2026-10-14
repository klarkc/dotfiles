{
  pkgs,
  desktop-commander-src,
}:
let
  version = "0.2.52";

  desktopCommander = pkgs.buildNpmPackage {
    pname = "desktop-commander";
    inherit version;
    src = desktop-commander-src;
    npmDepsHash = "sha256-1VGtHVTHyJy9vJ26IEjtrhgrRE5ZccJ3QsnBJkDFTvs=";
    patches = [ ./desktop-commander-optmem-probe.patch ];
    npmBuildScript = "build";
    dontNpmPrune = true;
    npmRebuildFlags = [ "--ignore-scripts" ];
    nativeBuildInputs = [ pkgs.makeWrapper ];

    postInstall = "wrapProgram \"$out/bin/desktop-commander\" --prefix PATH : \"${
      pkgs.lib.makeBinPath [
        pkgs.coreutils
        pkgs.which
        pkgs.ripgrep
      ]
    }\"";
  };

  state = pkgs.writeShellApplication {
    name = "desktop-commander-state";
    runtimeInputs = with pkgs; [ coreutils ];
    text = builtins.readFile ./desktop-commander-state.sh;
  };

  remote = pkgs.writeShellApplication {
    name = "desktop-commander-remote";
    runtimeInputs = [
      pkgs.coreutils
      desktopCommander
    ];
    text = builtins.readFile ./desktop-commander-remote.sh;
  };
in
pkgs.buildEnv {
  name = "desktop-commander-runtime-${version}";
  paths = [
    desktopCommander
    state
    remote
  ];
}

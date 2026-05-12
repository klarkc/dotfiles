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
    config.allowUnfree = true;
  };

  python = pkgs.python312;
  pythonWithPip = python.withPackages (ps: with ps; [
    pip
    setuptools
    wheel
    packaging
  ]);

  pytorchPackages = [
    "torch==2.11.0+cu130"
    "torchvision==0.26.0+cu130"
    "torchaudio==2.11.0+cu130"
  ];

  vllmPackages = [
    "vllm==0.20.1"
  ];

  allPythonPackages = pytorchPackages ++ vllmPackages;

  wheelhouse = pkgs.stdenvNoCC.mkDerivation {
    pname = "vllm-wheelhouse";
    version = "0.20.1-cu130";

    nativeBuildInputs = with pkgs; [ cacert pythonWithPip ];

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-9NCRzlIrDFaZr6lLz/1KoWzapG2+0ff0TBtRPRPkDos=";

    buildCommand = ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME" "$out"
      ${pythonWithPip}/bin/python3.12 -m pip download --dest "$out" --extra-index-url https://download.pytorch.org/whl/cu130 ${pkgs.lib.escapeShellArgs allPythonPackages}
    '';
  };

  runtimePath = pkgs.lib.makeBinPath (with pkgs; [
    bash
    coreutils
    findutils
    gnugrep
    git
    gcc
    cmake
    pkg-config
  ]);

  runtimeLibraryPath = pkgs.lib.makeLibraryPath [
    pkgs.stdenv.cc.cc.lib
  ];
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "vllm-runtime";
  version = "0.20.1-cu130";

  nativeBuildInputs = with pkgs; [ makeWrapper pythonWithPip ];

  dontUnpack = true;

  installPhase = ''
    mkdir -p "$out"
    ${pythonWithPip}/bin/python3.12 -m venv "$out"
    "$out/bin/python" -m pip install --no-index --find-links ${wheelhouse} ${pkgs.lib.escapeShellArgs allPythonPackages}
    wrapProgram "$out/bin/vllm" \
      --prefix PATH : ${runtimePath} \
      --prefix LD_LIBRARY_PATH : ${runtimeLibraryPath}
  '';
}

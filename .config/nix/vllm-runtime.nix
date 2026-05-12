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

  pythonBootstrapTools = [
    "pip==26.1.1"
    "wheel==0.47.0"
    "setuptools==82.0.1"
    "packaging==26.2"
  ];

  pytorchPackages = [
    "torch==2.11.0+cu130"
    "torchvision==0.26.0+cu130"
    "torchaudio==2.11.0+cu130"
  ];

  vllmPackages = [
    "vllm==0.20.1"
  ];

  allPythonPackages = pythonBootstrapTools ++ pytorchPackages ++ vllmPackages;

  wheelhouse = pkgs.stdenvNoCC.mkDerivation {
    pname = "vllm-wheelhouse";
    version = "0.20.1-cu130";

    nativeBuildInputs = with pkgs; [
      cacert
      uv
      python
    ];

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = pkgs.lib.fakeHash;

    buildCommand = ''
      export HOME="$TMPDIR/home"
      export UV_CACHE_DIR="$TMPDIR/uv-cache"
      mkdir -p "$HOME" "$UV_CACHE_DIR" "$out"

      uv pip download \
        --python ${python}/bin/python3.12 \
        --torch-backend cu130 \
        --exclude-newer 2026-05-08T23:59:59Z \
        --dest "$out" \
        ${pkgs.lib.escapeShellArgs allPythonPackages}
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
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "vllm-runtime";
  version = "0.20.1-cu130";

  nativeBuildInputs = with pkgs; [
    makeWrapper
    python
  ];

  dontUnpack = true;

  installPhase = ''
    mkdir -p "$out"
    ${python}/bin/python3.12 -m venv "$out"
    "$out/bin/python" -m pip install \
      --no-index \
      --find-links ${wheelhouse} \
      ${pkgs.lib.escapeShellArgs allPythonPackages}

    wrapProgram "$out/bin/vllm" \
      --prefix PATH : ${runtimePath}
  '';
}

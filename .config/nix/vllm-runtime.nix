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
    outputHash = "sha256-YXDMzKklFhOcuxEY8b2fHl97k4uT4qk1DvH7ehi3oT0=";

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
    mkdir -p "$out" "$out/nix-support"
    ${pythonWithPip}/bin/python3.12 -m venv "$out"
    "$out/bin/python" -m pip install --no-index --find-links ${wheelhouse} ${pkgs.lib.escapeShellArgs allPythonPackages}

    site_packages="$out/lib/python3.12/site-packages"
    wheel_library_path="${runtimeLibraryPath}:$site_packages"
    wheel_lib_dirs="$(${pkgs.findutils}/bin/find "$site_packages" -maxdepth 2 -type d \( -iname '*.libs' -o -path '*/nvidia/*/lib' -o -path '*/torch/lib' \) -print 2>/dev/null | ${pkgs.coreutils}/bin/paste -sd: - || true)"
    if [ -n "$wheel_lib_dirs" ]; then
      wheel_library_path="$wheel_lib_dirs:$wheel_library_path"
    fi

    printf '%s\n' "$wheel_library_path" > "$out/nix-support/ld-library-path"
    wrapProgram "$out/bin/vllm" \
      --prefix PATH : ${runtimePath} \
      --prefix LD_LIBRARY_PATH : "$wheel_library_path"
  '';
}

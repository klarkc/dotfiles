{ pkgs, version }:
let
  python = pkgs.python312;
  pythonWithPip = python.withPackages (
    ps: with ps; [
      pip
      setuptools
      wheel
      packaging
    ]
  );

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
    inherit version;

    nativeBuildInputs = with pkgs; [
      cacert
      pythonWithPip
    ];

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-/q1l/qMNmCgPHxiksUQMzBfZHjNzQ2RXxcK9y6IXSWc=";

    buildCommand = ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME" "$out"
      ${pythonWithPip}/bin/python3.12 -m pip download --dest "$out" --extra-index-url https://download.pytorch.org/whl/cu130 ${pkgs.lib.escapeShellArgs allPythonPackages}
    '';
  };

  runtimePath = pkgs.lib.makeBinPath (
    with pkgs;
    [
      bash
      coreutils
      findutils
      gnugrep
      git
      gcc
      cmake
      pkg-config
    ]
  );

  runtimeLibraryPath = pkgs.lib.makeLibraryPath [
    pkgs.stdenv.cc.cc.lib
    pkgs.zstd
  ];
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "vllm-runtime";
  inherit version;

  nativeBuildInputs = with pkgs; [
    makeWrapper
    pythonWithPip
  ];

  dontUnpack = true;

  installPhase = ''
        mkdir -p "$out/lib/python3.12/site-packages" "$out/bin" "$out/nix-support"
        export HOME="$TMPDIR/home"
        export PIP_NO_INDEX=1
        export PIP_FIND_LINKS=${wheelhouse}
        export PIP_DISABLE_PIP_VERSION_CHECK=1
        export PIP_NO_CACHE_DIR=1

        ${pythonWithPip}/bin/python3.12 -m pip install \
          --pre \
          --no-index \
          --find-links ${wheelhouse} \
          --target "$out/lib/python3.12/site-packages" \
          ${pkgs.lib.escapeShellArgs allPythonPackages}

        makeWrapper ${pythonWithPip}/bin/python3.12 "$out/bin/python" \
          --set PYTHONNOUSERSITE 1 \
          --prefix PYTHONPATH : "$out/lib/python3.12/site-packages" \
          --prefix PATH : "${runtimePath}"

        cat > "$out/bin/vllm" <<EOF
    #!/bin/sh
    export PYTHONNOUSERSITE=1
    export PYTHONPATH="$out/lib/python3.12/site-packages:\''${PYTHONPATH:-}"
    export PATH="${runtimePath}:\''${PATH:-}"
    export LD_LIBRARY_PATH="${runtimeLibraryPath}:\''${LD_LIBRARY_PATH:-}"
    exec ${pythonWithPip}/bin/python3.12 -m vllm.entrypoints.cli.main "\$@"
    EOF
        chmod 0755 "$out/bin/vllm"

        printf '%s\n' '${runtimeLibraryPath}' > "$out/nix-support/ld-library-path"
  '';
}

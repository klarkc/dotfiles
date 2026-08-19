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

  # Build vLLM from the requested GitHub PR instead of installing a released
  # vLLM wheel. Pinning the PR ref keeps the user-facing requirement identical
  # to:
  #   pip install -v "vllm @ git+https://github.com/vllm-project/vllm.git@refs/pull/52729/head"
  vllmRequirement = "vllm @ git+https://github.com/vllm-project/vllm.git@refs/pull/52729/head";

  # The PR head currently requires the CUDA 13 generation of its dependencies.
  # Let vLLM's own metadata resolve exact runtime dependencies; the cu130 index
  # below ensures CUDA-flavoured PyTorch wheels are available to pip.

  wheelhouse = pkgs.stdenvNoCC.mkDerivation {
    pname = "vllm-pr52729-wheelhouse";
    inherit version;

    nativeBuildInputs = with pkgs; [
      cacert
      git
      pythonWithPip
    ];

    # This changes whenever the PR head or any resolved dependency changes.
    # Run `nix build .#vllm-runtime`; Nix will print the actual recursive hash,
    # then replace lib.fakeHash with that value.
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = pkgs.lib.fakeHash;

    dontUnpack = true;

    buildCommand = ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME" "$out"
      export PIP_DISABLE_PIP_VERSION_CHECK=1
      export PIP_NO_CACHE_DIR=1

      # The requested PR change is Python-side CUDA dispatch. Reuse the
      # matching precompiled vLLM extensions instead of requiring a complete
      # CUDA compiler toolchain inside this Nix fixed-output derivation.
      export VLLM_USE_PRECOMPILED=1
      export VLLM_MAIN_CUDA_VERSION=13.0

      # Make the locally-built PR wheel reproducible enough for a Nix FOD.
      export SOURCE_DATE_EPOCH=315532800
      export PYTHONHASHSEED=0

      ${pythonWithPip}/bin/python3.12 -m pip wheel -v \
        --wheel-dir "$out" \
        --extra-index-url https://download.pytorch.org/whl/cu130 \
        --extra-index-url https://flashinfer.ai/whl/ \
        ${pkgs.lib.escapeShellArg vllmRequirement}
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

        # Install the wheel built from refs/pull/52729/head plus all of its resolved
        # dependencies, completely offline from the fixed-output wheelhouse.
        ${pythonWithPip}/bin/python3.12 -m pip install -v \
          --no-index \
          --find-links ${wheelhouse} \
          --target "$out/lib/python3.12/site-packages" \
          vllm

        makeWrapper ${pythonWithPip}/bin/python3.12 "$out/bin/python" \
          --set PYTHONNOUSERSITE 1 \
          --prefix PYTHONPATH : "$out/lib/python3.12/site-packages" \
          --prefix PATH : "${runtimePath}"

        cat > "$out/bin/vllm" <<EOF2
    #!/bin/sh
    export PYTHONNOUSERSITE=1
    export PYTHONPATH="$out/lib/python3.12/site-packages:\''${PYTHONPATH:-}"
    export PATH="${runtimePath}:\''${PATH:-}"
    export LD_LIBRARY_PATH="${runtimeLibraryPath}:\''${LD_LIBRARY_PATH:-}"
    exec ${pythonWithPip}/bin/python3.12 -m vllm.entrypoints.cli.main "\$@"
    EOF2
        chmod 0755 "$out/bin/vllm"

        printf '%s\n' '${runtimeLibraryPath}' > "$out/nix-support/ld-library-path"
  '';
}

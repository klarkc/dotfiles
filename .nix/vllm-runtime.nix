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

  vllmRequirement = "vllm @ git+https://github.com/vllm-project/vllm.git@refs/pull/52729/head";

  wheelhouse = pkgs.stdenvNoCC.mkDerivation {
    pname = "vllm-pr52729-wheelhouse";
    inherit version;

    nativeBuildInputs = with pkgs; [
      cacert
      git
      cmake
      ninja
      pythonWithPip
    ];

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-XpIO5SsjA/6KQhdYJxjSjZE57nYrph5S/o8ZThE6b7U=";

    dontUnpack = true;

    buildCommand = ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME" "$out"
      export PIP_DISABLE_PIP_VERSION_CHECK=1
      export PIP_NO_CACHE_DIR=1
      export VLLM_USE_PRECOMPILED=1
      export VLLM_MAIN_CUDA_VERSION=13.0
      export SOURCE_DATE_EPOCH=315532800
      export PYTHONHASHSEED=0

      # Avoid pip's opaque PEP 517 build environment. vLLM's PR head declares
      # these build requirements in pyproject.toml; stage them explicitly so
      # we can make the binary PyTorch wheel's shared libraries visible.
      buildsite="$TMPDIR/vllm-build-site"
      mkdir -p "$buildsite"
      ${pythonWithPip}/bin/python3.12 -m pip install -v \
        --target "$buildsite" \
        --extra-index-url https://download.pytorch.org/whl/cu130 \
        "cmake>=3.26.1" \
        ninja \
        "packaging>=24.2" \
        "setuptools>=77.0.3,<81.0.0" \
        "setuptools-scm>=8.0" \
        "setuptools-rust>=1.9.0" \
        "torch==2.13.0" \
        wheel \
        jinja2

      export PYTHONPATH="$buildsite''${PYTHONPATH:+:$PYTHONPATH}"
      build_ld="${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib ]}"
      [ ! -d "$buildsite/torch/lib" ] || build_ld="$buildsite/torch/lib:$build_ld"
      if [ -d "$buildsite/nvidia" ]; then
        nvidia_ld="$(find "$buildsite/nvidia" -type d -name lib -print 2>/dev/null | paste -sd: - || true)"
        [ -z "$nvidia_ld" ] || build_ld="$nvidia_ld:$build_ld"
      fi
      export LD_LIBRARY_PATH="$build_ld''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

      echo "Checking staged PyTorch before building vLLM..."
      echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
      ${pythonWithPip}/bin/python3.12 -c 'import torch; print("torch build dependency:", torch.__version__, "CUDA:", torch.version.cuda)'

      ${pythonWithPip}/bin/python3.12 -m pip wheel -v \
        --no-build-isolation \
        --wheel-dir "$out" \
        --extra-index-url https://download.pytorch.org/whl/cu130 \
        --extra-index-url https://flashinfer.ai/whl/ \
        ${pkgs.lib.escapeShellArg vllmRequirement}
    '';
  };

  # FlashInfer JITs attention kernels at runtime for shapes that are not already
  # available as prebuilt artifacts. Give it a small CUDA_HOME containing only
  # the compiler, runtime/driver stubs, and CUDA 13 CRT headers instead of the
  # full CUDA toolkit. FlashInfer hard-codes lib64 paths, while Nixpkgs uses lib.
  cudaJitToolkit = pkgs.symlinkJoin {
    name = "vllm-cuda-jit-toolkit";
    paths =
      with pkgs.cudaPackages;
      [
        cuda_nvcc
        cuda_cudart
      ]
      ++ pkgs.lib.optional (pkgs.cudaPackages ? cuda_crt) pkgs.cudaPackages.cuda_crt;
    postBuild = ''
      if [ -d "$out/lib" ] && [ ! -e "$out/lib64" ]; then
        ln -s lib "$out/lib64"
      fi
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
      ninja
      pkg-config
      cudaJitToolkit
    ]
  );

  runtimeLibraryPath = pkgs.lib.makeLibraryPath [
    pkgs.stdenv.cc.cc.lib
    pkgs.zstd
    pkgs.cudaPackages.cuda_cudart
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

        ${pythonWithPip}/bin/python3.12 -m pip install -v \
          --no-index \
          --find-links ${wheelhouse} \
          --target "$out/lib/python3.12/site-packages" \
          vllm

        makeWrapper ${pythonWithPip}/bin/python3.12 "$out/bin/python" \
          --set PYTHONNOUSERSITE 1 \
          --set CUDA_HOME "${cudaJitToolkit}" \
          --set CUDA_PATH "${cudaJitToolkit}" \
          --prefix PYTHONPATH : "$out/lib/python3.12/site-packages" \
          --prefix PATH : "${runtimePath}"

        cat > "$out/bin/vllm" <<EOF2
    #!/bin/sh
    export PYTHONNOUSERSITE=1
    export PYTHONPATH="$out/lib/python3.12/site-packages:\''${PYTHONPATH:-}"
    export CUDA_HOME="${cudaJitToolkit}"
    export CUDA_PATH="${cudaJitToolkit}"
    export PATH="${runtimePath}:\''${PATH:-}"
    export LD_LIBRARY_PATH="${runtimeLibraryPath}:\''${LD_LIBRARY_PATH:-}"
    exec ${pythonWithPip}/bin/python3.12 -m vllm.entrypoints.cli.main "\$@"
    EOF2
        chmod 0755 "$out/bin/vllm"

        printf '%s\n' '${runtimeLibraryPath}' > "$out/nix-support/ld-library-path"
  '';
}

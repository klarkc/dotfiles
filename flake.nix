{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
    nix-fast-build.inputs.nixpkgs.follows = "nixpkgs";
    nix-fast-build.inputs.treefmt-nix.follows = "treefmt-nix";
    git-hooks.url = "github:klarkc/git-hooks.nix/add-flake-follows-hook";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    kolu.url = "github:juspay/kolu";
    herdr.url = "github:ogulcancelik/herdr";
    herdr.inputs.nixpkgs.follows = "nixpkgs";
    alacritty-ligatures-src = {
      url = "github:ink-splatters/alacritty-ligatures/master";
      flake = false;
    };
    nixGL = {
      url = "git+https://github.com/nix-community/nixGL?ref=refs/pull/223/head";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Bump note: Fusion source. fetchPnpmDeps requires pnpm-lock.yaml which only
    # exists in source tags (not in published npm tarballs). When bumping Fusion:
    # 1) bump this ref (`Runfusion/Fusion/v<X.Y.Z>`),
    # 2) refresh the `fusion-cli-pnpm-deps` hash in `.nix/fusion-runtime.nix`
    #    via `nix build .#fusion-runtime` (initial build will fail and report
    #    expected hash), then commit the reported hash,
    # 3) update `fusionRuntime` version in this flake, and
    # 4) rerun `nix build .#fusion-runtime` + `result/bin/fusion --version`.
    fusion-src = {
      url = "github:Runfusion/Fusion/v0.73.0";
      flake = false;
    };
    # Bump note: QMD source. Fusion's memory backend invokes the `qmd` CLI as a
    # separate runtime process; bump this only if upstream Fusion docs/code
    # require a newer QMD CLI or the current `qmd --help` smoke check fails.
    # Refresh the `qmd-cli-pnpm-deps` hash in `.nix/fusion-runtime.nix` similarly.
    qmd-src = {
      url = "github:tobi/qmd/v2.1.0";
      flake = false;
    };
    # Bump note: vLLM-only nixpkgs input. Isolated experiment to access newer
    # nixpkgs vLLM/Torch/CUDA packaging without dragging the rest of the user
    # profile onto upstream master. Update only when nixpkgs-unstable (preferred)
    # or an upstream nixpkgs PR/ref has evidence the selected vLLM/CUDA/Python
    # stack builds, or when CUDA 13 support reaches nixpkgs with no known
    # vLLM closure blockers. Each update must include `nix flake check --no-build`,
    # `nix build .#vllm-runtime`, CLI smoke, and 35B A3B benchmark evidence.
    # Collapse back to a single `nixpkgs` input once nixpkgs-unstable provides a
    # CUDA `>= 13.0` vLLM stack that passes acceptance; at that point delete this
    # input and consume `inputs.nixpkgs` directly from `.nix/vllm-runtime.nix`.
    nixpkgs-vllm = {
      url = "github:NixOS/nixpkgs/master";
    };
  };

  outputs =
    { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        overlays = [ inputs.herdr ];
        make-pkgs =
          system:
          import inputs.nixpkgs {
            inherit system;
            #config.contentAddressedByDefault = true;
          };
      }
      (
        {
          pkgs,
          system,
          ...
        }@ctx:
        let
          nvidiaVersion = "610.43.03";
          nvidiaHash = "sha256-ReLUwTSiPDXlDyU6SqY+fl6NF+PRhdSgfIpY6WEu05I=";

          alacrittyDeps = {
            nativeBuildInputs = with pkgs; [
              cmake
              fontconfig
              freetype
              makeWrapper
              pkg-config
              python3
            ];
            buildInputs = with pkgs; [
              expat
              fontconfig
              freetype
              libGL
              libxkbcommon
              wayland
              libx11
              libxcursor
              libxi
              libxrandr
            ];
            runtimeLibs = with pkgs; [
              libglvnd
              libxkbcommon
              wayland
              libx11
              libxcursor
              libxi
              libxrandr
            ];
          };

          alacrittyWithLigatures = pkgs.callPackage ./.nix/alacritty-ligatures.nix {
            alacritty-ligatures-src = inputs.alacritty-ligatures-src;
            alacrittyDeps = alacrittyDeps;
          };

          nixGLPkgs = import inputs.nixGL {
            pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
            inherit nvidiaVersion nvidiaHash;
          };

          nixGLNvidiaDrv = nixGLPkgs.nixGLNvidia;

          alacrittyWithHostGL = pkgs.writeShellApplication {
            name = "alacritty";
            runtimeInputs = [ nixGLNvidiaDrv alacrittyWithLigatures ];
            text = ''
              # Guardrail: check host NVIDIA driver version matches the pinned nixGL version
              if [[ -f /proc/modules ]]; then
                host_version=$(modinfo -F version nvidia 2>/dev/null || true)
                if [[ -z "$host_version" ]]; then
                  host_version=$(nvidia-smi --query-gpu=driver_version --format=csv,noheader 2>/dev/null | tr -d ' ')
                fi
                if [[ -z "$host_version" ]]; then
                  echo "[alacritty] WARNING: unable to detect NVIDIA driver version (modinfo and nvidia-smi both failed)."
                  echo "[alacritty] Proceeding anyway — if Alacritty crashes with a GLX error, check your NVIDIA driver."
                elif [[ "$host_version" != "${nvidiaVersion}" ]]; then
                  echo "[alacritty] ERROR: NVIDIA driver version mismatch!"
                  echo ""
                  echo "  Expected (pinned): ${nvidiaVersion}"
                  echo "  Detected (host):   $host_version"
                  echo ""
                  echo "Alacritty will likely crash with a cryptic GLX error."
                  echo "Fix one of:"
                  echo "  1. Upgrade your host driver to ${nvidiaVersion}:"
                  echo "     yay -S nvidia-open"
                  echo "  2. Pin nixGL to your current host driver version:"
                  echo "     nix store prefetch-file --hash-type sha256 --json \"https://us.download.nvidia.com/XFree86/Linux-x86_64/$host_version/NVIDIA-Linux-x86_64-$host_version.run\""
                  echo ""
                  echo "After changing either side, rebuild with: cd /home/klarkc && nix profile upgrade klarkc"
                  exit 1
                fi
              fi

              exec "${nixGLNvidiaDrv}/bin/nixGLNvidia-${nvidiaVersion}" "${alacrittyWithLigatures}/bin/alacritty" "$@"
            '';
          };
          opencodeWithCodexAuth = pkgs.callPackage ./.nix/opencode-with-codex-auth.nix { };
          opencodeCodexAuthTools = pkgs.callPackage ./.nix/opencode-codex-auth-tools.nix { };
          fusionRuntime = pkgs.callPackage ./.nix/fusion-runtime.nix {
            # Bump note: Fusion runtime. Coupled bumps: see `.nix/fusion-runtime.nix`.
            version = "0.73.0";
            fusion-src = inputs.fusion-src;
            qmd-src = inputs.qmd-src;
          };
          vllmPkgs = import inputs.nixpkgs-vllm {
            inherit system;
            config = {
              cudaSupport = true;
              allowUnfree = true;
              cudaForwardCompat = true;
              cudaCapabilities = [
                "8.9"
                "9.0"
              ];
            };
            # Bump note: scoped CUDA 13 overlay for the `nixpkgs-vllm` import.
            # Forces `torch`, `triton[-cuda]`, `torchvision`, `torchaudio`,
            # `cupy`, `flashinfer`, `accelerate`, and `vllm` to share the same
            # CUDA 13 package set so their derivation trees pull CUDA 13 libs
            # coherently. Add additional packages here only when a real runtime
            # closure audit shows another CUDA-sensitive dep leaking CUDA 12.9
            # into the built output. Do not copy this pattern into the main
            # `nixpkgs` import without separate justification; do not revert to
            # a global `doCheck = false` overlay without design review.
            overlays = [
              (final: prev: {
                python312Packages = prev.python312Packages.overrideScope (
                  pyFinal: pyPrev: let
                    cuda = final.cudaPackages_13_0;
                  in {
                    triton-cuda = pyPrev.triton-cuda.override { cudaPackages = cuda; };
                    triton = pyPrev.triton.override { cudaPackages = cuda; };
                    torch = pyPrev.torch.override {
                      cudaPackages = cuda;
                      triton-cuda = pyFinal.triton-cuda;
                      triton = pyFinal.triton;
                    };
                    cupy = pyPrev.cupy.override { cudaPackages = cuda; };
                    flashinfer = pyPrev.flashinfer.override {
                      cudaPackages = cuda;
                      torch = pyFinal.torch;
                    };
                    interegular = pyPrev.interegular.overridePythonAttrs (_: {
                      # Narrow evidence-backed override: `interegular` 0.3.3's
                      # `test_slow_example` asserts a <1s wall-clock bound and
                      # fails on heavy parallel build hosts. Previously seen at
                      # ~1.1s and ~33s in CUDA vLLM closure builds. Disable
                      # only that test rather than the whole package's checks.
                      doCheck = false;
                    });
                    accelerate = pyPrev.accelerate.override {
                      torch = pyFinal.torch;
                      torchvision = pyPrev.torchvision;
                      cudatoolkit = cuda.cuda_nvcc;
                    };
                    vllm = pyPrev.vllm.override {
                      cudaPackages = cuda;
                      torch = pyFinal.torch;
                      cupy = pyFinal.cupy;
                      flashinfer = pyFinal.flashinfer;
                    };
                  }
                );
              })
            ];
          };
          vllmRuntime = vllmPkgs.callPackage ./.nix/vllm-runtime.nix {
            # Bump note: vLLM runtime label is taken from the nixpkgs-vllm
            # `python312Packages.vllm` version; coupled bumps: see
            # `.nix/vllm-runtime.nix` and the `nixpkgs-vllm` input above.
            version = vllmPkgs.python312Packages.vllm.version;
          };
          nixProfile = pkgs.writeText "nix-profile" ''
            export NIX_PATH="nixpkgs=flake:${inputs.nixpkgs}"
          '';
          treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs {
            projectRootFile = "flake.nix";

            programs.nixfmt.enable = true;
            programs.ormolu.enable = true;
            programs.prettier.enable = true;
            programs.shfmt.enable = true;
            programs.taplo.enable = true;

            settings.formatter.prettier.excludes = [
              ".github/workflows/dependency-monitor.yml"
            ];

            settings.formatter.shfmt.includes = [
              "*.sh"
              ".bash_profile"
              ".bashrc"
              ".profile"
              ".local/bin/bench-vllm"
              ".local/bin/cleanup"
              ".local/bin/home-cleanup"
              ".local/bin/home-cleanup-post"
              ".local/bin/pacman-clean"
              ".local/bin/pacman-paccache"
              ".local/bin/pacman-pacreport"
              ".local/bin/pacman-report"
            ];

            settings.formatter.taplo.includes = [
              "*.toml"
              ".*.toml"
            ];
          };
          pre-commit-check = inputs.git-hooks.lib.${system}.run {
            src = ./.;
            hooks.flake-follows.enable = true;
            hooks.treefmt = {
              enable = true;
              package = treefmtEval.config.build.wrapper;
            };
          };
        in
        {
          formatter = treefmtEval.config.build.wrapper;

          checks = {
            formatting = treefmtEval.config.build.check self;
            pre-commit-check = pre-commit-check;
          };

          devShells.default = pkgs.mkShell {
            inherit (pre-commit-check) shellHook;
            buildInputs = pre-commit-check.enabledPackages ++ [
              treefmtEval.config.build.wrapper
            ];
          };

          packages.default = pkgs.buildEnv {
            name = "klarkc-dotfiles_profile";
            paths =
              with pkgs;
              with ctx;
              [
                (pkgs.runCommand "profile" { } ''
                  mkdir -p $out/etc/profile.d
                  cp ${nixProfile} $out/etc/profile.d/nix.sh
                '')
                alacrittyWithHostGL
                direnv
                nixos-rebuild
                nix-output-monitor
                nix-fast-build
                flake-edit
                nodejs
                uv
                gh
                codex
                pi-coding-agent
                opencodeWithCodexAuth
                opencodeCodexAuthTools
                kolu
                herdr
                fusionRuntime
                vllmRuntime
              ];
          };

          packages.alacritty = alacrittyWithLigatures;
          packages.fusion-runtime = fusionRuntime;
          packages.vllm-runtime = vllmRuntime;
        }
      );
}

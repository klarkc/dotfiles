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
    opencode-src = {
      url = "github:anomalyco/opencode";
      flake = false;
    };
    nixGL = {
      url = "git+https://github.com/nix-community/nixGL?ref=refs/pull/223/head";
      inputs.nixpkgs.follows = "nixpkgs";
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

          alacrittyWithHostGL =
            pkgs.writeShellApplication {
              name = "alacritty";
              runtimeInputs = [ nixGLNvidiaDrv alacrittyWithLigatures ];
              text = ''
                # Guardrail: check host NVIDIA driver version matches the pinned nixGL version
                if [[ -f /proc/modules ]]; then
                  host_version=$(modinfo -F version nvidia 2>/dev/null || true)
                  if [[ -z "$host_version" ]]; then
                    echo "[alacritty] WARNING: unable to detect NVIDIA driver version (modinfo failed)."
                    echo "[alacritty] If you have an NVIDIA GPU, ensure the kernel module is loaded:"
                    echo "[alacritty]   sudo modprobe nvidia"
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
                    echo "     nix-prefetch-url download https://international.download.nvidia.com/XFree86/Linux-x86_64/${nvidiaVersion}/NVIDIA-Linux-x86_64-${nvidiaVersion}.run"
                    echo ""
                    echo "After changing either side, rebuild with: nix profile install '${self}'#"
                    exit 1
                  fi
                fi

                exec "${nixGLNvidiaDrv}/bin/nixGLNvidia-${nvidiaVersion}" "${alacrittyWithLigatures}/bin/alacritty" "$@"
              '';
            };
          opencodeWithReasoning = pkgs.callPackage ./.nix/opencode-with-reasoning.nix {
            opencode-src = inputs.opencode-src;
          };
          opencodeCodexAuthTools = pkgs.callPackage ./.nix/opencode-codex-auth-tools.nix { };
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
                opencodeWithReasoning
                opencodeCodexAuthTools
                kolu
                herdr
              ];
          };

          packages.alacritty = alacrittyWithLigatures;
        }
      );
}

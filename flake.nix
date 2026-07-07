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
      url = "github:anomalyco/opencode/pull/30477/head";
      flake = false;
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
                alacrittyWithLigatures
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

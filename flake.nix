{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
    nix-fast-build.inputs.nixpkgs.follows = "nixpkgs";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
    kolu.url = "github:juspay/kolu";
    herdr.url = "github:ogulcancelik/herdr";
    herdr.inputs.nixpkgs.follows = "nixpkgs";
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
          opencodeWithReasoning = pkgs.opencode.overrideAttrs (
            old:
            let
              src = inputs.opencode-src;
            in
            {
              inherit src;
              node_modules = old.node_modules.overrideAttrs (_: {
                inherit src;
                outputHash = "sha256-ZBdR7Vz4N0aKeXzHI7G70j9vE6hLlDw+Dam5WLruVoI=";
              });
            }
          );
          nixProfile = pkgs.writeText "nix-profile" ''
            export NIX_PATH="nixpkgs=flake:${inputs.nixpkgs}"
          '';
          checkFlakeFollows = pkgs.writeShellApplication {
            name = "check-flake-follows";
            runtimeInputs = with pkgs; [
              coreutils
              diffutils
              flake-edit
            ];
            text = ''
              set -euo pipefail

              if [ ! -f flake.nix ]; then
                exit 0
              fi

              if [ ! -f flake.lock ]; then
                echo "flake.lock not found; run: nix flake lock"
                exit 1
              fi

              tmp="$(mktemp -d)"
              trap 'rm -rf "$tmp"' EXIT

              cp flake.nix "$tmp/flake.nix"
              cp flake.lock "$tmp/flake.lock"

              flake-edit \
                --flake "$tmp/flake.nix" \
                --lock-file "$tmp/flake.lock" \
                --no-lock \
                --non-interactive \
                follow >/dev/null

              if ! diff -u flake.nix "$tmp/flake.nix"; then
                echo
                echo "flake.nix follows are out of date."
                echo "Run:"
                echo "  nix develop -c flake-edit --no-lock --non-interactive follow"
                exit 1
              fi
            '';
          };
          pre-commit-check = inputs.git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixfmt.enable = true;
              flake-follows = {
                enable = true;
                name = "flake-edit follow";
                entry = "${checkFlakeFollows}/bin/check-flake-follows";
                files = "^flake\\.(nix|lock)$";
                pass_filenames = false;
              };
            };
          };
        in
        {
          formatter = pkgs.writeShellScriptBin "pre-commit-run" ''
            ${pkgs.lib.getExe pre-commit-check.config.package} run --all-files --config ${pre-commit-check.config.configFile}
          '';

          checks.pre-commit-check = pre-commit-check;

          devShells.default = pkgs.mkShell {
            inherit (pre-commit-check) shellHook;
            buildInputs = pre-commit-check.enabledPackages ++ [
              pkgs.flake-edit
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
                kolu
                herdr
              ];
          };
        }
      );
}

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
  };

  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        make-pkgs = system: import inputs.nixpkgs {
          inherit system;
          #config.contentAddressedByDefault = true;
        };
      }
      ({ pkgs, ... }@ctx:
        let
          nixProfile = pkgs.writeText "nix-profile" ''
            export NIX_PATH="nixpkgs=flake:${inputs.nixpkgs}"
          '';

          fusionRuntimeInputs = with pkgs; [
            bashInteractive
            cacert
            coreutils
            curl
            docker-client
            findutils
            gawk
            gh
            git
            gnugrep
            gnused
            nodejs
            openssh
            python3
            tmux
            uv
          ];

          fusionDashboardService = pkgs.writeShellApplication {
            name = "fusion-dashboard-service";
            runtimeInputs = fusionRuntimeInputs;
            text = ''
              set -euo pipefail

              runtime_dir="''${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR is required}/fusion"
              socket="''${runtime_dir}/tmux.sock"
              session="fusion"
              fusion_bin="''${HOME}/.npm-packages/bin/fusion"
              fusion_workdir="''${HOME}/Sources/Fusion"

              rm -f "$socket"

              tmux -S "$socket" new-session -d -s "$session" -c "$fusion_workdir" \
                "$fusion_bin dashboard --host 0.0.0.0 --port 4040 --no-auth"

              while tmux -S "$socket" has-session -t "$session" 2>/dev/null; do
                sleep 5
              done
            '';
          };

          fusionRuntime = pkgs.buildEnv {
            name = "fusion-runtime";
            paths = fusionRuntimeInputs ++ [
              fusionDashboardService
            ];
          };
        in
        {
          packages.fusion-runtime = fusionRuntime;
          packages.fusion-dashboard-service = fusionDashboardService;

          packages.default = pkgs.buildEnv {
            name = "klarkc-dotfiles_profile";
            paths = with pkgs; [
              (pkgs.runCommand "profile" { } ''
                mkdir -p $out/etc/profile.d
                cp ${nixProfile} $out/etc/profile.d/nix.sh
              '')
              direnv
              nixos-rebuild
              nix-output-monitor
              nix-fast-build
              codex
              fusionRuntime
            ];
          };
        });
}

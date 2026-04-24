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

          ntm = pkgs.writeShellApplication {
            name = "ntm";
            runtimeInputs = with pkgs; [ git go ];
            text = ''
              set -euo pipefail

              repo_dir="''${XDG_DATA_HOME:-$HOME/.local/share}/ntm/source"

              if [ ! -d "$repo_dir/.git" ]; then
                mkdir -p "$(dirname "$repo_dir")"
                git clone https://github.com/Dicklesworthstone/ntm "$repo_dir"
              fi

              cd "$repo_dir"
              exec go run . "$@"
            '';
          };

          zeroclaw = pkgs.writeShellApplication {
            name = "zeroclaw";
            runtimeInputs = with pkgs; [ git go ];
            text = ''
              set -euo pipefail

              repo_dir="''${XDG_DATA_HOME:-$HOME/.local/share}/zeroclaw/source"

              if [ ! -d "$repo_dir/.git" ]; then
                mkdir -p "$(dirname "$repo_dir")"
                git clone https://github.com/zeroclaw-labs/zeroclaw "$repo_dir"
              fi

              cd "$repo_dir"
              exec go run . "$@"
            '';
          };

          crush-agent = pkgs.writeShellApplication {
            name = "crush-agent";
            runtimeInputs = with pkgs; [ bash coreutils direnv git tmux ];
            text = ''
              set -euo pipefail

              task="''${1:-}"
              if [ -z "$task" ]; then
                echo "usage: crush-agent <task>" >&2
                exit 64
              fi

              session="''${CRUSH_AGENT_SESSION:-crush-$(basename "$PWD" | tr -cs '[:alnum:]_.-' '-') }"
              command="cd '$PWD' && direnv allow . && direnv exec . crush '$task'"

              if tmux has-session -t "$session" 2>/dev/null; then
                tmux send-keys -t "$session" "$command" C-m
              else
                tmux new-session -d -s "$session" "$command"
              fi

              echo "$session"
            '';
          };

          ntm-crush = pkgs.writeShellApplication {
            name = "ntm-crush";
            runtimeInputs = [ crush-agent ];
            text = ''
              exec crush-agent "$@"
            '';
          };

          zeroclaw-ntm-foreman = pkgs.writeShellApplication {
            name = "zeroclaw-ntm-foreman";
            runtimeInputs = with pkgs; [ bash coreutils git jq tmux ntm zeroclaw ];
            text = ''
              set -euo pipefail

              state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/zeroclaw-ntm"
              mkdir -p "$state_dir"

              cat > "$state_dir/README" <<'EOF'
ZeroClaw NTM foreman state directory.

This wrapper intentionally stays conservative: it starts the ZeroClaw runtime
and expects ZeroClaw tools/prompts to call NTM for session orchestration.
Configure channels, provider keys, autonomy, and schedules in ZeroClaw's own
configuration files under XDG_CONFIG_HOME.
EOF

              exec zeroclaw "$@"
            '';
          };
        in
        {
          packages = {
            inherit ntm zeroclaw crush-agent ntm-crush zeroclaw-ntm-foreman;
            default = pkgs.buildEnv {
              name = "klarkc-dotfiles_profile";
              paths = with pkgs; [
                (pkgs.runCommand "profile" { } ''
                  mkdir -p $out/etc/profile.d
                  cp ${nixProfile} $out/etc/profile.d/nix.sh
                '')
                devenv
                direnv
                git
                jq
                nixos-rebuild
                nix-output-monitor
                nix-fast-build
                nodejs
                codex
                uv
                tmux
                ntm
                zeroclaw
                crush-agent
                ntm-crush
                zeroclaw-ntm-foreman
              ];
            };
          };

          apps = {
            ntm.program = "${ntm}/bin/ntm";
            zeroclaw.program = "${zeroclaw}/bin/zeroclaw";
            crush-agent.program = "${crush-agent}/bin/crush-agent";
            ntm-crush.program = "${ntm-crush}/bin/ntm-crush";
            zeroclaw-ntm-foreman.program = "${zeroclaw-ntm-foreman}/bin/zeroclaw-ntm-foreman";
          };

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              bash
              git
              jq
              nixfmt-rfc-style
              shellcheck
              statix
              tmux
              devenv
              direnv
              ntm
              zeroclaw
              crush-agent
              ntm-crush
              zeroclaw-ntm-foreman
            ];
          };

          checks.default = pkgs.runCommand "dotfiles-check" { nativeBuildInputs = with pkgs; [ nixfmt-rfc-style shellcheck ]; } ''
            nixfmt --check ${./flake.nix}
            shellcheck ${./.local/bin/ntm-crush-session} ${./.local/bin/zeroclaw-ntm-watch}
            touch $out
          '';
        });
}

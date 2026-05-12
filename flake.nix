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

          fusionRuntime = pkgs.buildEnv {
            name = "fusion-runtime";
            paths = with pkgs; [
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
          };
        in
        {
          packages.fusion-runtime = fusionRuntime;

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

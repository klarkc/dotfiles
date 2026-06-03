{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
    kolu.url = "github:juspay/kolu";
    herdr.url = "github:ogulcancelik/herdr";
    opencode-src = {
      url = "github:anomalyco/opencode/pull/30477/head";
      flake = false;
    };
  };

  outputs = { self, utils, herdr, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        overlays = [ herdr ];
        make-pkgs = system: import inputs.nixpkgs {
          inherit system;
          #config.contentAddressedByDefault = true;
        };
      }
      ({ pkgs, kolu, ... }@ctx:
        let
          opencodeWithReasoning = pkgs.opencode.overrideAttrs (old:
            let
              src = inputs.opencode-src;
            in
            {
              inherit src;
              node_modules = old.node_modules.overrideAttrs (_: {
                inherit src;
                outputHash = pkgs.lib.fakeHash;
              });
            });
          nixProfile = pkgs.writeText "nix-profile" ''
            export NIX_PATH="nixpkgs=flake:${inputs.nixpkgs}"
          '';
        in
        {
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
              nodejs
              codex
              uv
              gh
              opencodeWithReasoning
              kolu
              herdr
            ];
          };
        });
}

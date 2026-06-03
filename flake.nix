{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
    nix-fast-build.inputs.nixpkgs.follows = "nixpkgs";
    kolu.url = "github:juspay/kolu";
    herdr.url = "github:ogulcancelik/herdr";
    herdr.inputs.nixpkgs.follows = "nixpkgs";
    opencode-src = {
      url = "github:anomalyco/opencode/pull/30477/head";
      flake = false;
    };
  };

  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        overlays = [ inputs.herdr ];
        make-pkgs = system: import inputs.nixpkgs {
          inherit system;
          #config.contentAddressedByDefault = true;
        };
      }
      ({ pkgs, ... }@ctx:
        let
          opencodeWithReasoning = pkgs.opencode.overrideAttrs (old:
            let
              src = inputs.opencode-src;
            in
            {
              inherit src;
              node_modules = old.node_modules.overrideAttrs (_: {
                inherit src;
                outputHash = "sha256-ZBdR7Vz4N0aKeXzHI7G70j9vE6hLlDw+Dam5WLruVoI=";
              });
            });
          nixProfile = pkgs.writeText "nix-profile" ''
            export NIX_PATH="nixpkgs=flake:${inputs.nixpkgs}"
          '';
        in
        {
          packages.default = pkgs.buildEnv {
            name = "klarkc-dotfiles_profile";
            paths = with pkgs; with ctx; [
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
        });
}

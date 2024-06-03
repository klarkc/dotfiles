{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    attic.url = "github:zhaofengli/attic";
    attic.inputs.nixpkgs.follows = "nixpkgs";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
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
      ({ pkgs, haskell-nix, ... }@ctx:
        let
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
              rnix-lsp
              marksman
              lorri
              direnv
              node2nix
              nix-output-monitor
              nix-tree
              nixos-rebuild
              haskell-nix.hix
              ctx.attic
            ];
          };
        });
}

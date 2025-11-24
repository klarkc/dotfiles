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
        in
        {
          packages.default = pkgs.buildEnv {
            name = "klarkc-dotfiles_profile";
            paths = with pkgs; [
              (pkgs.runCommand "profile" { } ''
                mkdir -p $out/etc/profile.d
                cp ${nixProfile} $out/etc/profile.d/nix.sh 
              '')
              devenv
              direnv
              nixos-rebuild
              nix-output-monitor
              nix-fast-build
              nodejs
            ];
          };
        });
}

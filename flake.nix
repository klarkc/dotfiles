{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.utils.url = "github:ursi/flake-utils";

  outputs = { self, utils, ... }@inputs:
  utils.apply-systems
  {
    inherit inputs;
    make-pkgs = system: import inputs.nixpkgs {
      inherit system;
      config.contentAddresedByDefault = true;
    };
  }
  ({ pkgs, ... }: {
      packages.default = pkgs.buildEnv {
        name = "klarkc-dotfiles_profile";
        paths = with pkgs; [
          rnix-lsp
          lorri
          direnv
          cachix
          nix-output-monitor
        ];
      };
  });
}

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
    kolu.url = "github:juspay/kolu";
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
      ({ pkgs, kolu, ... }@ctx:
        let
          opencodePatched = pkgs.opencode.overrideAttrs (old: {
            postPatch = (old.postPatch or "") + ''
              patched=0
              for file in \
                packages/opencode/src/provider/provider.ts \
                packages/opencode/src/provider/models.ts
              do
                [ -f "$file" ] || continue
                if grep -q 'Schema.Literals(\["reasoning_content", "reasoning_details"\])' "$file"; then
                  substituteInPlace "$file" \
                    --replace-fail 'Schema.Literals(["reasoning_content", "reasoning_details"])' \
                                   'Schema.Literals(["reasoning_content", "reasoning_details", "reasoning"])'
                  patched=1
                fi
                if grep -q '"reasoning_content" | "reasoning_details"' "$file"; then
                  substituteInPlace "$file" \
                    --replace-fail '"reasoning_content" | "reasoning_details"' \
                                   '"reasoning_content" | "reasoning_details" | "reasoning"'
                  patched=1
                fi
              done
              if [ "$patched" -ne 1 ]; then
                echo "failed to patch opencode interleaved reasoning field enum" >&2
                exit 1
              fi
            '';
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
              opencodePatched
              kolu
            ];
          };
        });
}

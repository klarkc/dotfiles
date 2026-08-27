{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:ursi/flake-utils";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
    nix-fast-build.inputs.nixpkgs.follows = "nixpkgs";
    nix-fast-build.inputs.treefmt-nix.follows = "treefmt-nix";
    git-hooks.url = "github:klarkc/git-hooks.nix/add-flake-follows-hook";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    kolu.url = "github:juspay/kolu";
    herdr.url = "github:ogulcancelik/herdr";
    herdr.inputs.nixpkgs.follows = "nixpkgs";
    alacritty-ligatures-src = {
      url = "github:ink-splatters/alacritty-ligatures/ligature";
      flake = false;
    };
    nixGL = {
      url = "git+https://github.com/nix-community/nixGL?ref=refs/pull/223/head";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Bump note: Fusion source. fetchPnpmDeps requires pnpm-lock.yaml which only
    # exists in source tags (not in published npm tarballs). When bumping Fusion:
    # 1) bump this ref (`Runfusion/Fusion/v<X.Y.Z>`),
    # 2) refresh the `fusion-cli-pnpm-deps` hash in `.nix/fusion-runtime.nix`
    #    via `nix build .#fusion-runtime` (initial build will fail and report
    #    expected hash), then commit the reported hash,
    # 3) update `fusionRuntime` version in this flake, and
    # 4) rerun `nix build .#fusion-runtime` + `result/bin/fusion --version`.
    fusion-src = {
      url = "github:Runfusion/Fusion/v0.73.0";
      flake = false;
    };
    # Bump note: QMD source. Fusion's memory backend invokes the `qmd` CLI as a
    # separate runtime process; bump this only if upstream Fusion docs/code
    # require a newer QMD CLI or the current `qmd --help` smoke check fails.
    # Refresh the `qmd-cli-pnpm-deps` hash in `.nix/fusion-runtime.nix` similarly.
    qmd-src = {
      url = "github:tobi/qmd/v2.1.0";
      flake = false;
    };
    # Bump note: `mcp-remote` source. Used by
    # `.local/bin/atlassian-smoke-test oauth` as the OAuth bridge for the
    # Atlassian Rovo MCP integration. The upstream project does not publish
    # stable tags, so the rev is pinned to a commit hash. To bump:
    #   1) update this `url` to the new commit hash, and
    #   2) rerun `nix build .#mcp-remote-runtime`; the initial build will
    #      print the expected `pnpmDeps.hash`; commit it in
    #      `.nix/mcp-remote-runtime.nix` together with the rev bump so the
    #      source, the lockfile hash, and the build reference stay together.
    mcp-remote-src = {
      url = "github:punkpeye/mcp-remote/77bbcfcd7892d339c27b5a14818b63cb5c4d3293";
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
          nvidiaVersion = "610.57.04";
          nvidiaHash = "sha256-suk1xmuDuwDAyFe8jg7g/VLekoa0DJzB7sKafOfrEW0=";

          alacrittyDeps = {
            nativeBuildInputs = with pkgs; [
              cmake
              fontconfig
              freetype
              makeWrapper
              pkg-config
              python3
            ];
            buildInputs = with pkgs; [
              expat
              fontconfig
              freetype
              libGL
              libxkbcommon
              wayland
              libx11
              libxcb
              libxcursor
              libxi
              libxrandr
            ];
            runtimeLibs = with pkgs; [
              libglvnd
              libxkbcommon
              wayland
              libx11
              libxcursor
              libxi
              libxrandr
            ];
          };

          alacrittyWithLigatures = pkgs.callPackage ./.nix/alacritty-ligatures.nix {
            alacritty-ligatures-src = inputs.alacritty-ligatures-src;
            alacrittyDeps = alacrittyDeps;
          };

          nixGLPkgs = import inputs.nixGL {
            pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
            inherit nvidiaVersion nvidiaHash;
          };

          nixGLNvidiaDrv = nixGLPkgs.nixGLNvidia;

          alacrittyWithHostGL = pkgs.writeShellApplication {
            name = "alacritty";
            text = ''
              # Guardrail: check host NVIDIA driver version matches the pinned nixGL version
              if [[ -f /proc/modules ]]; then
                host_version=$(modinfo -F version nvidia 2>/dev/null || true)
                if [[ -z "$host_version" ]]; then
                  host_version=$(nvidia-smi --query-gpu=driver_version --format=csv,noheader 2>/dev/null || true)
                  host_version="''${host_version// /}"
                fi
                if [[ -z "$host_version" ]]; then
                  echo "[alacritty] WARNING: unable to detect NVIDIA driver version (modinfo and nvidia-smi both failed)."
                  echo "[alacritty] Proceeding anyway — if Alacritty crashes with a GLX error, check your NVIDIA driver."
                elif [[ "$host_version" != "${nvidiaVersion}" ]]; then
                  echo "[alacritty] ERROR: NVIDIA driver version mismatch!"
                  echo ""
                  echo "  Expected (pinned): ${nvidiaVersion}"
                  echo "  Detected (host):   $host_version"
                  echo ""
                  echo "Alacritty will likely crash with a cryptic GLX error."
                  echo "Fix one of:"
                  echo "  1. Upgrade your host driver to ${nvidiaVersion}:"
                  echo "     yay -S nvidia-open"
                  echo "  2. Pin nixGL to your current host driver version:"
                  echo "     nix store prefetch-file --hash-type sha256 --json \"https://us.download.nvidia.com/XFree86/Linux-x86_64/$host_version/NVIDIA-Linux-x86_64-$host_version.run\""
                  echo ""
                  echo "After changing either side, rebuild with: cd /home/klarkc && nix profile upgrade klarkc"
                  exit 1
                fi
              fi

              exec "${nixGLNvidiaDrv}/bin/nixGLNvidia-${nvidiaVersion}" "${alacrittyWithLigatures}/bin/alacritty" "$@"
            '';
          };
          opencodeWithCodexAuth = pkgs.callPackage ./.nix/opencode-with-codex-auth.nix { };
          opencodeCodexAuthTools = pkgs.callPackage ./.nix/opencode-codex-auth-tools.nix { };
          backupTools = pkgs.callPackage ./.nix/backup-tools.nix { };
          fusionRuntime = pkgs.callPackage ./.nix/fusion-runtime.nix {
            # Bump note: Fusion runtime. Coupled bumps: see `.nix/fusion-runtime.nix`.
            version = "0.73.0";
            fusion-src = inputs.fusion-src;
            qmd-src = inputs.qmd-src;
          };
          vllmRuntime = pkgs.callPackage ./.nix/vllm-runtime.nix {
            # Bump note: vLLM runtime label (version + CUDA variant). Coupled bumps: see `.nix/vllm-runtime.nix`.
            version = "0.24.0-cu130";
          };
          mcpRemoteRuntime = pkgs.callPackage ./.nix/mcp-remote-runtime.nix {
            # Bump note: `mcp-remote` runtime. Coupled bumps: see
            # `.nix/mcp-remote-runtime.nix`. The source rev and
            # `pnpmDeps.hash` in that derivation must move together.
            mcp-remote-src = inputs.mcp-remote-src;
          };
          # `buildEnv` rejects paths that share the same subpath. Both
          # `fusionRuntime` and `mcpRemoteRuntime` ship their own
          # `lib/node_modules/.pnpm/...` tree, so we merge them with
          # `symlinkJoin` first. `symlinkJoin` recursively merges and
          # last-write-wins on conflicts, which is what we want for
          # duplicated npm hoisted stores.
          jsRuntimes = pkgs.symlinkJoin {
            name = "klarkc-dotfiles_js-runtimes";
            paths = [
              fusionRuntime
              mcpRemoteRuntime
            ];
          };
          nixProfile = pkgs.writeText "nix-profile" ''
            export NIX_PATH="nixpkgs=flake:${inputs.nixpkgs}"
          '';
          treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs {
            projectRootFile = "flake.nix";

            programs.nixfmt.enable = true;
            programs.ormolu.enable = true;
            programs.prettier.enable = true;
            programs.shfmt.enable = true;
            programs.taplo.enable = true;

            settings.formatter.prettier.excludes = [
              ".github/workflows/dependency-monitor.yml"
            ];

            settings.formatter.shfmt.includes = [
              "*.sh"
              ".bash_profile"
              ".bashrc"
              ".profile"
              ".local/bin/atlassian-smoke-test"
              ".local/bin/bench-vllm"
              ".local/bin/cleanup"
              ".local/bin/home-cleanup"
              ".local/bin/home-cleanup-post"
              ".local/bin/pacman-clean"
              ".local/bin/pacman-paccache"
              ".local/bin/pacman-pacreport"
              ".local/bin/pacman-report"
            ];

            settings.formatter.taplo.includes = [
              "*.toml"
              ".*.toml"
            ];
          };
          pre-commit-check = inputs.git-hooks.lib.${system}.run {
            src = ./.;
            hooks.flake-follows.enable = true;
            hooks.treefmt = {
              enable = true;
              package = treefmtEval.config.build.wrapper;
            };
          };
        in
        {
          formatter = treefmtEval.config.build.wrapper;
          checks = {
            formatting = treefmtEval.config.build.check self;
            pre-commit-check = pre-commit-check;
            archive-pack-test =
              pkgs.runCommand "archive-pack-test" { nativeBuildInputs = [ backupTools.testScript ]; }
                ''
                  archive-pack-test
                  touch $out
                '';
            # Static verification that the opencode MCP integration for
            # Atlassian Rovo MCP is configured correctly without
            # committing any secret. The check fails if the MCP entry
            # is missing, points at the wrong endpoint, or contains an
            # Authorization header / Bearer / Basic value.
            opencode-mcp-atlassian-config =
              pkgs.runCommand "opencode-mcp-atlassian-config"
                {
                  nativeBuildInputs = [ pkgs.python3 ];
                  src = ./.config/opencode/opencode.json;
                }
                ''
                  python3 - "$src" <<'PY'
                  import json
                  import re
                  import sys

                  path = sys.argv[1]
                  with open(path) as f:
                      data = json.load(f)
                  mcp = data.get("mcp") or {}
                  atlassian = mcp.get("atlassian") or {}
                  if not atlassian:
                      sys.exit("FAIL: opencode config missing mcp.atlassian entry")
                  if atlassian.get("type") != "remote":
                      sys.exit("FAIL: mcp.atlassian.type must be 'remote'")
                  url = atlassian.get("url") or ""
                  expected = "https://mcp.atlassian.com/v1/mcp/authv2"
                  if url != expected:
                      sys.exit(f"FAIL: mcp.atlassian.url must be {expected}, got {url}")
                  if not atlassian.get("enabled", False):
                      sys.exit("FAIL: mcp.atlassian.enabled must be true")
                  # Scan only the mcp.atlassian subtree so that
                  # legitimate provider fields elsewhere in the config
                  # (e.g. openai/vllm apiKey) don't trip the check.
                  forbidden = re.compile(r"(?i)(authorization|basic|bearer|api[_-]?key|token|secret)")
                  raw = json.dumps(atlassian)
                  hits = [m.group(0) for m in forbidden.finditer(raw)]
                  if hits:
                      sys.exit(f"FAIL: mcp.atlassian subtree contains forbidden secrets: {hits}")
                  if "headers" in atlassian:
                      sys.exit("FAIL: mcp.atlassian must not define a headers block (would commit secrets)")
                  if "oauth" in atlassian and atlassian["oauth"] is False:
                      sys.exit("FAIL: mcp.atlassian.oauth=false would suppress OAuth auto-detection; remove the field")
                  print(f"opencode-mcp-atlassian-config: url={url} enabled={atlassian['enabled']} no_secrets=true")
                  PY
                  touch $out
                '';
          };

          devShells.default = pkgs.mkShell {
            inherit (pre-commit-check) shellHook;
            buildInputs = pre-commit-check.enabledPackages ++ [
              treefmtEval.config.build.wrapper
            ];
          };

          # `buildEnv` rejects paths that share the same subpath. Both
          # `fusionRuntime` and `mcpRemoteRuntime` ship their own
          # `lib/node_modules/.pnpm/...` tree, so we merge them with
          # `symlinkJoin` first (see the `let` binding). `symlinkJoin`
          # recursively merges and last-write-wins on conflicts, which
          # is what we want for duplicated npm hoisted stores.

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
                alacrittyWithHostGL
                direnv
                nixos-rebuild
                nix-output-monitor
                nix-fast-build
                flake-edit
                uv
                gh
                codex
                pi-coding-agent
                opencodeWithCodexAuth
                opencodeCodexAuthTools
                backupTools.packScript
                backupTools.testScript
                kolu
                herdr
                vllmRuntime
                jsRuntimes
              ];
          };

          packages.alacritty = alacrittyWithLigatures;
          packages.archive-pack = backupTools.packScript;
          packages.archive-pack-test = backupTools.testScript;
          packages.fusion-runtime = fusionRuntime;
          packages.vllm-runtime = vllmRuntime;
          packages.mcp-remote-runtime = mcpRemoteRuntime;
        }
      );
}

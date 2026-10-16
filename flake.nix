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
    # ZeroClaw is pinned upstream because nixpkgs can lag behind releases.
    # v0.8.5's upstream Nix package pins an older Rust than the crate requires,
    # so build the pinned source with this flake's current rustPlatform.
    zeroclaw-src = {
      url = "github:zeroclaw-labs/zeroclaw/v0.8.5";
      flake = false;
    };
    # Bump note: Ledgeur source. When bumping, refresh both the Cargo and pnpm
    # dependency hashes in `.nix/ledgeur-runtime.nix`, reapply the local patch,
    # and verify the native PT transcription + vLLM note path.
    ledgeur-src = {
      url = "github:maxbeech/ledgeur";
      flake = false;
    };
    # Bump note: sherpa-rs 0.6 is built with download-binaries disabled, so the
    # matching sherpa-onnx shared runtime is a locked flake input instead of a
    # network fetch during the Nix build. Review this together with Ledgeur's
    # sherpa-rs version when either side changes.
    sherpa-onnx-bin = {
      url = "https://github.com/k2-fsa/sherpa-onnx/releases/download/v1.12.9/sherpa-onnx-v1.12.9-linux-x64-shared.tar.bz2";
      flake = false;
    };
    alacritty-ligatures-src = {
      url = "github:ink-splatters/alacritty-ligatures/ligature";
      flake = false;
    };
    nixGL = {
      url = "git+https://github.com/nix-community/nixGL?ref=refs/pull/223/head";
      inputs.nixpkgs.follows = "nixpkgs";
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
    # Bump note: Enpass MCP is fixed to a reviewed revision of 0.2.0.
    # Bump together with .nix/enpass-mcp-runtime.nix version/npmDepsHash,
    # rerun nix build .#enpass-mcp-runtime and read-only MCP smoke tests.
    enpass-mcp-src = {
      url = "github:bitterdev/enpass-mcp/fe805b729de1f0e4196b2e9cdd5df18a8747db26";
      flake = false;
    };
    # Desktop Commander Remote is pinned because it is a long-lived local
    # service, not an ephemeral npx invocation. Keep this rev aligned with the
    # package version in .nix/desktop-commander-runtime.nix.
    desktop-commander-src = {
      url = "github:wonderwhy-er/DesktopCommanderMCP/ea3ed35a7be9f2a3ea3e89185ff9bbb03fe5ab57";
      flake = false;
    };
  };

  outputs =
    { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        systems = [ "x86_64-linux" ];
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
                  echo "After changing either side, rebuild with: cd /home/example && nix profile upgrade klarkc"
                  exit 1
                fi
              fi

              exec "${nixGLNvidiaDrv}/bin/nixGLNvidia-${nvidiaVersion}" "${alacrittyWithLigatures}/bin/alacritty" "$@"
            '';
          };
          opencodeWithCodexAuth = pkgs.callPackage ./.nix/opencode-with-codex-auth.nix { };
          opencodeCodexAuthTools = pkgs.callPackage ./.nix/opencode-codex-auth-tools.nix { };
          zeroclawWithLandlock = pkgs.rustPlatform.buildRustPackage {
            pname = "zeroclaw";
            version = "0.8.5";
            src = inputs.zeroclaw-src;
            # Rebase the local Landlock, typed context-overflow, and provider
            # timeout patches whenever the pinned upstream version changes.
            patches = [
              ./.nix/zeroclaw-readonly-roots.patch
              ./.nix/zeroclaw-context-overflow.patch
              ./.nix/zeroclaw-provider-timeout.patch
              # Keep safe Nix builds automatic, but elevate Nix/Git effects.
              ./.nix/zeroclaw-nix-approvals.patch
            ];
            cargoDeps = pkgs.rustPlatform.importCargoLock {
              lockFile = "${inputs.zeroclaw-src}/Cargo.lock";
            };
            cargoBuildFlags = [
              "-p"
              "zeroclaw"
              "--no-default-features"
              "--features"
              "agent-runtime,channel-discord,channel-webhook,gateway,sandbox-landlock"
            ];
            buildInputs = [ pkgs.stdenv.cc.cc ];
            doCheck = false;
          };
          backupTools = pkgs.callPackage ./.nix/backup-tools.nix { };
          vllmRuntime = pkgs.callPackage ./.nix/vllm-runtime.nix {
            # Bump note: vLLM runtime label (version + CUDA variant). Coupled bumps: see `.nix/vllm-runtime.nix`.
            version = "0.24.0-cu130";
          };
          ledgeurRuntime = pkgs.callPackage ./.nix/ledgeur-runtime.nix {
            ledgeur-src = inputs.ledgeur-src;
            sherpa-onnx-bin = inputs.sherpa-onnx-bin;
          };
          mcpRemoteRuntime = pkgs.callPackage ./.nix/mcp-remote-runtime.nix {
            # Bump note: `mcp-remote` runtime. Coupled bumps: see
            # `.nix/mcp-remote-runtime.nix`. The source rev and
            # `pnpmDeps.hash` in that derivation must move together.
            mcp-remote-src = inputs.mcp-remote-src;
          };
          desktopCommanderRuntime = pkgs.callPackage ./.nix/desktop-commander-runtime.nix {
            desktop-commander-src = inputs.desktop-commander-src;
          };
          desktopCommanderMcpRuntime = pkgs.callPackage ./.nix/desktop-commander-mcp-runtime.nix {
            inherit desktopCommanderRuntime;
          };
          enpassMcpRuntime = pkgs.callPackage ./.nix/enpass-mcp-runtime.nix {
            enpass-mcp-src = inputs.enpass-mcp-src;
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

            # This tracked JSON is intentionally updated by llm-config and
            # opencode-model-policy at runtime. JSON serialization can differ
            # from Prettier while remaining semantically valid, so do not
            # gate commits on incidental runtime formatting drift. Dedicated
            # opencode-mcp-atlassian-config and vllm-opencode-contract checks
            # continue to validate its structure and effective model policy.
            settings.formatter.prettier.excludes = [
              ".github/workflows/dependency-monitor.yml"
              ".config/opencode/opencode.json"
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
            # For human git commits run the FULL make test including live smokes.
            # A nix flake check sandbox deliberately avoids recursive invocation.
            hooks.make-test = {
              enable = true;
              name = "Full make test";
              entry = "bash .local/bin/git-hook-make-test";
              pass_filenames = false;
              always_run = true;
              require_serial = true;
              after = [
                "treefmt"
                "flake-follows"
              ];
            };
          };
        in
        {
          formatter = treefmtEval.config.build.wrapper;
          checks = {
            formatting = treefmtEval.config.build.check self;
            pre-commit-check = pre-commit-check;
            desktop-commander-mcp-contract =
              pkgs.runCommand "desktop-commander-mcp-contract" { nativeBuildInputs = [ pkgs.python3 ]; }
                ''
                  python3 ${./.nix/tests/desktop-commander-mcp-contract.py} ${./.nix/desktop-commander-mcp.py} ${./.}
                  python3 ${./.nix/tests/desktop-commander-mcp-cloudflare-contract.py} ${./.}/.nix/desktop-commander-mcp-cloudflare.py
                  touch $out
                '';
            # Enpass MCP: no write tools, approval-gated sensitive reads, and
            # manual-only write skill. Pure check runs without Enpass credentials.
            enpass-mcp-readonly-contract =
              pkgs.runCommand "enpass-mcp-readonly-contract"
                {
                  nativeBuildInputs = [ pkgs.python3 ];
                }
                ''
                  python3 ${./.nix/tests/enpass-mcp-contract.py} ${./.config/opencode/opencode.json} ${./.nix/enpass-mcp-runtime.nix} ${./.agents/skills/enpass-write-human-guidance/SKILL.md} ${./flake.nix}
                  touch $out
                '';
            # End-to-end MCP wire protocol check, without registering a real
            # Enpass vault, reading credentials, or touching the user's home.
            enpass-mcp-readonly-protocol =
              pkgs.runCommand "enpass-mcp-readonly-protocol"
                {
                  nativeBuildInputs = [ pkgs.python3 ];
                }
                ''
                  python3 ${./.nix/tests/enpass-mcp-readonly-smoke.py} ${enpassMcpRuntime}/bin/enpass-mcp-readonly
                  touch $out
                '';
            # Offline contract for the actual pre-commit gate. It tests
            # inherited GNU Make dry-run flags and verifies that failed
            # make commands stop Git commits rather than passing silently.
            git-hook-make-test-contract =
              pkgs.runCommand "git-hook-make-test-contract"
                {
                  nativeBuildInputs = [
                    pkgs.python3
                    pkgs.git
                    pkgs.bash
                    pkgs.gnumake
                  ];
                  hook = ./.local/bin/git-hook-make-test;
                  checker = ./.local/bin/git-hook-make-test-selftest;
                }
                ''
                  python3 "$checker" "$hook"
                  touch $out
                '';
            archive-pack-test =
              pkgs.runCommand "archive-pack-test" { nativeBuildInputs = [ backupTools.testScript ]; }
                ''
                  archive-pack-test
                  touch $out
                '';
            # Pure offline contract: unsafe Bitbucket mutations are rejected
            # before either API backend is reached.
            bitbucket-read-mcp-contract =
              pkgs.runCommand "bitbucket-read-mcp-contract"
                {
                  nativeBuildInputs = [ pkgs.python3 ];
                  bridge = ./.local/bin/bitbucket-read-mcp;
                  agentProbe = ./.local/bin/coding-agents-smoke-test;
                }
                ''
                  python3 "$bridge" --selftest
                  python3 "$agentProbe" --selftest
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
                  bb = mcp.get("bitbucket_read") or {}
                  if bb.get("type") != "local" or not bb.get("enabled", False):
                      sys.exit("FAIL: bitbucket_read local MCP must be enabled")
                  expected_command = ["sh", "-c", 'exec "$HOME/.local/bin/bitbucket-read-mcp"']
                  if bb.get("command") != expected_command:
                      sys.exit("FAIL: bitbucket_read MCP command differs from tracked read-only bridge")
                  if bb.get("environment") or bb.get("headers"):
                      sys.exit("FAIL: bitbucket_read MCP must inherit runtime credentials, not store secrets")
                  print("opencode-mcp-atlassian-config: OAuth + Bitbucket read-only MCP configured; secrets runtime-only")
                  PY
                  touch $out
                '';
            # Static verification that the vllm/opencode model-defaults
            # contract is durable across renames. Two checks:
            #   (a) vllm-patch-model-defaults must update the opencode
            #       top-level `model` to `vllm/<served-model-name>` so
            #       that `llm-config` alone is enough to switch the
            #       default agent model after a target change.
            #   (b) Every SERVED_MODEL_NAME declared in
            #       `.config/vllm/*.env` must exist as a key in
            #       `provider.vllm.models` in
            #       `.config/opencode/opencode.json`. Otherwise
            #       `llm-config` cannot keep opencode in sync with
            #       the active vLLM target.
            vllm-opencode-contract =
              pkgs.runCommand "vllm-opencode-contract"
                {
                  nativeBuildInputs = [ pkgs.python3 ];
                  patcher = ./.local/bin/vllm-patch-model-defaults;
                  modelPolicy = ./.local/bin/opencode-model-policy;
                  opencodeConfig = ./.config/opencode/opencode.json;
                  vllmConfigs = ./.config/vllm;
                }
                ''
                  python3 - "$patcher" "$opencodeConfig" "$vllmConfigs" "$modelPolicy" <<'PY'
                  import json
                  import re
                  import sys
                  from pathlib import Path

                  patcher_path = sys.argv[1]
                  opencode_path = sys.argv[2]
                  vllm_dir = Path(sys.argv[3])

                  # (a) Preserve the actual delegated policy, not the obsolete
                  # pre-policy string assignment to vllm/<served_model_name>.
                  patcher_src = Path(patcher_path).read_text(encoding="utf-8")
                  policy_path = Path(sys.argv[4])
                  policy_src = policy_path.read_text(encoding="utf-8")
                  if not all(
                      fragment in patcher_src
                      for fragment in ("opencode-model-policy", "--mode keep", "--local-key")
                  ):
                      sys.exit("FAIL: vLLM patcher must delegate to opencode-model-policy --mode keep")
                  if 'data["model"] = primary' not in policy_src:
                      sys.exit("FAIL: model policy no longer updates the OpenCode primary model")

                  # Exercise the policy itself using isolated copies, never $HOME.
                  import runpy
                  import shutil
                  import subprocess
                  import tempfile

                  constants = runpy.run_path(str(policy_path))
                  gpt_model = constants["GPT_MODEL"]
                  economy_model = constants["GPT_ECONOMY_MODEL"]
                  available = []
                  for candidate in vllm_dir.glob("*.env"):
                      for row in candidate.read_text(encoding="utf-8").splitlines():
                          if row.startswith("SERVED_MODEL_NAME="):
                              available.append((candidate.stem, row.split("=", 1)[1].strip().strip('"').strip("'")))
                              break
                  if not available:
                      sys.exit("FAIL: no local model environment with SERVED_MODEL_NAME")
                  local_key, local_id = sorted(available)[0]
                  with tempfile.TemporaryDirectory(prefix="opencode-policy-test-") as work:
                      for mode in ("local", "gpt", "full-gpt"):
                          config_dir = Path(work) / mode
                          config_dir.mkdir()
                          isolated = config_dir / "opencode.json"
                          shutil.copyfile(opencode_path, isolated)
                          command = [
                              sys.executable,
                              str(policy_path),
                              "--mode", mode,
                              "--local-key", local_key,
                              "--config-path", str(isolated),
                              "--env-directory", str(vllm_dir),
                          ]
                          result = subprocess.run(command, capture_output=True, text=True)
                          if result.returncode:
                              sys.exit(f"FAIL: model policy {mode}: {result.stderr.strip()}")
                          actual = json.loads(isolated.read_text(encoding="utf-8"))
                          primary = f"vllm/{local_id}" if mode == "local" else gpt_model
                          subagent = economy_model if mode == "full-gpt" else f"vllm/{local_id}"
                          if actual.get("model") != primary or actual.get("small_model") != subagent:
                              sys.exit(f"FAIL: model policy {mode}: unexpected primary/small model")
                          agents = actual.get("agent") or {}
                          for name in ("build", "plan", "design", "learn"):
                              if agents.get(name, {}).get("model") != primary:
                                  sys.exit(f"FAIL: {mode}: primary agent {name} model differs")
                          for name in ("general", "explore", "scout", "summary", "title"):
                              if agents.get(name, {}).get("model") != subagent:
                                  sys.exit(f"FAIL: {mode}: subagent {name} model differs")
                          command[command.index(mode)] = "keep"
                          result = subprocess.run(command, capture_output=True, text=True)
                          if result.returncode:
                              sys.exit(f"FAIL: model policy keep ({mode}): {result.stderr.strip()}")
                          kept = json.loads(isolated.read_text(encoding="utf-8"))
                          if kept.get("model") != primary or kept.get("small_model") != subagent:
                              sys.exit(f"FAIL: model policy keep unexpectedly changed mode {mode}")

                  # (b) Every tracked .env SERVED_MODEL_NAME must exist in
                  #     provider.vllm.models.
                  with open(opencode_path, encoding="utf-8") as f:
                      opencode = json.load(f)
                  if not isinstance(opencode, dict):
                      sys.exit(f"FAIL: {opencode_path} must contain a JSON object")
                  models = (
                      opencode.get("provider", {})
                      .get("vllm", {})
                      .get("models", {})
                  )
                  if not isinstance(models, dict):
                      sys.exit(
                          f"FAIL: {opencode_path}: provider.vllm.models must be a JSON object"
                      )

                  env_files = sorted(
                      p for p in vllm_dir.glob("*.env") if p.is_file()
                  )
                  if not env_files:
                      sys.exit(f"FAIL: no tracked *.env files under {vllm_dir}")

                  missing = []
                  warned = []
                  for env_file in env_files:
                      served = None
                      for raw in env_file.read_text(encoding="utf-8").splitlines():
                          line = raw.strip()
                          if not line or line.startswith("#"):
                              continue
                          key, sep, value = line.partition("=")
                          if not sep:
                              continue
                          if key.strip() == "SERVED_MODEL_NAME":
                              served = (
                                  value.strip().strip('"').strip("'")
                              )
                              break
                      if not served:
                          warned.append(env_file.name)
                          continue
                      if served not in models:
                          missing.append(
                              f"{env_file.name}: SERVED_MODEL_NAME={served} "
                              f"not in provider.vllm.models"
                          )
                  for name in warned:
                      print(
                          f"vllm-opencode-contract: WARN: {name} has no SERVED_MODEL_NAME"
                      )
                  if missing:
                      for line in missing:
                          print(f"FAIL: {line}")
                      sys.exit(
                          f"FAIL: {len(missing)} env file(s) have SERVED_MODEL_NAME "
                          f"absent from provider.vllm.models"
                      )
                  print(
                      f"vllm-opencode-contract: patcher=updates_top_level_model "
                      f"env_files={len(env_files)} all_served_models_present=true"
                  )
                  PY
                  touch $out
                '';
          };

          devShells.default = pkgs.mkShell {
            inherit (pre-commit-check) shellHook;
            buildInputs = pre-commit-check.enabledPackages ++ [
              treefmtEval.config.build.wrapper
              desktopCommanderMcpRuntime
              pkgs.python3
            ];
          };

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
                # Authentication helper only: Codex CLI refreshes ChatGPT OAuth,
                # which opencode-with-codex-auth imports when newer. Not a coding agent.
                codex
                opencodeWithCodexAuth
                opencodeCodexAuthTools
                backupTools.packScript
                backupTools.testScript
                kolu
                herdr
                ledgeurRuntime
                vllmRuntime
                mcpRemoteRuntime
                enpassMcpRuntime
                desktopCommanderRuntime
                desktopCommanderMcpRuntime
                zeroclawWithLandlock
              ];
          };

          packages.alacritty = alacrittyWithLigatures;
          packages.archive-pack = backupTools.packScript;
          packages.archive-pack-test = backupTools.testScript;
          packages.ledgeur-runtime = ledgeurRuntime;
          packages.vllm-runtime = vllmRuntime;
          packages.mcp-remote-runtime = mcpRemoteRuntime;
          packages.enpass-mcp-runtime = enpassMcpRuntime;
          packages.desktop-commander-runtime = desktopCommanderRuntime;
          packages.desktop-commander-mcp-runtime = desktopCommanderMcpRuntime;
          packages.zeroclaw = zeroclawWithLandlock;
        }
      );
}

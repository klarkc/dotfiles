# Agent Notes

- Read `README.md` first for repo context and usage.
- This repository is checked out directly as `$HOME`. Keep repository-support files hidden from the user's normal home listing.
- Repo-internal support directories must use dot paths, for example `.nix` for Nix expressions and `.patches` for patch files.
- Allowed root-level exceptions are conventional project files such as `README.md`, `AGENTS.md`, `CHANGELOG.md`, `Makefile`, `flake.nix`, `flake.lock`, and root dotfiles such as `.profile`, `.vimrc`, `.tmux.conf`, and `.alacritty.toml`.
- Do not add visible root directories such as `patches/`, `nix/`, `scripts/`, or `docs/` unless the user explicitly asks for them.
- Repo-maintained Nix-build determinism invariant: any external dependency used by Nix build code maintained in this repo must be declared through `flake.nix` inputs and locked in `flake.lock`. Do not add `builtins.fetchTree`, `builtins.fetchTarball`, `builtins.fetchurl`, `import <nixpkgs>`, live `npm install`, live `pip download`, live `bun install`, live `pnpm/yarn install`, `curl`, `wget`, or `git clone` inside repo-maintained Nix derivations/scripts unless the user explicitly approves an exception.
- Package-manager policy inside repo-maintained Nix builds: prefer Nix-native builders and Nix dependency declarations over ecosystem package managers. Do not run live `npm install`, `npm ci`, `npm rebuild`, `pip download`, `pip install`, `uv pip`, `bun install`, `pnpm install`, `yarn install`, or similar resolver/install commands inside Nix build code maintained in this repo. If an ecosystem tool is unavoidable in repo-maintained Nix code, it must run offline against artifacts already declared as flake inputs and must not resolve or download anything.
- Upstream package boundary: package-manager usage inside dependencies provided by flake inputs (for example nixpkgs package internals) is acceptable unless this repo overrides or vendors that logic. The policy forbids package-manager resolution in code we maintain here, not in upstream package implementations selected through locked flake inputs.
- Flake input URL policy: input URLs may point to branches/refs such as `nixpkgs-unstable`, `main`, `master`, or PR refs. Exact revision pinning belongs in `flake.lock`, not necessarily in `flake.nix`. Do not replace branch/ref input URLs with explicit commit URLs just for determinism; doing so prevents normal `nix flake update` bump behavior. Determinism is provided by the locked rev + narHash in `flake.lock`.
- Coupled dependency bump policy: whenever a dependency version is locked in code, Nix expressions, service wrappers, generated hashes, or flake inputs, document nearby what else must be reviewed or updated when that version changes. Prefer a short `# Bump note:` comment adjacent to the version/source/hash declaration. A version bump must update all coupled version declarations, source refs, lock/dependency hashes, generated vendor/dependency artifacts, wrapper assertions, service assumptions, and smoke checks in the same change. Do not change only the visible package version.
- Dependency-specific bump notes should be local and actionable: say what upstream files/release notes to inspect, which companion dependencies must move together, which generated hashes must be refreshed, and which verification commands prove the bump. If no coupling exists, a short local note saying the version is standalone is acceptable for non-obvious cases.
- Runtime package naming convention: when a Nix package in `.nix/` composes or builds a tool runtime intended to be installed into the user profile and consumed from `%h/.nix-profile/bin/`, name the file `<tool>-runtime.nix` (e.g. `fusion-runtime.nix`, `vllm-runtime.nix`). The corresponding flake input/output/attribute and service unit must use the same `<tool>-runtime` name so that input file, Nix attribute, and service reference tie together.
- Services must not run `nix build`/`nix-build` at startup. Profile-managed runtime dependencies must be realized by `nix profile upgrade klarkc` and consumed from `%h/.nix-profile/bin`.
- This repo is installed at `$HOME`; systemd `%h` is the repo root. Do not use `%h/Sources/Fusion/klarkc/dotfiles` as a flake root for this repo.
- Smoke test contract: scripts under `.local/bin/*-smoke-test` are
  self-executing and take no arguments. Each script runs **all of its
  scenarios** in a single invocation (e.g. `vllm-smoke-test` runs
  focused checks AND end-to-end; `atlassian-smoke-test` runs both
  api-token and oauth modes). Smoke tests **never** build dependencies
  themselves — they assume `nix profile install .` has populated PATH
  with the required tools and runtime libs. If any prerequisite is
  missing (binary, GPU, systemd service, env var), the smoke test
  fails with `FATAL: <reason>` on stderr and exit 2 — there is no
  silent skip. `make test` invokes each smoke script once with no
  arguments; the scripts own their scenario execution.
- vLLM smoke testing follows this three-layer procedure:
  1. **Fast / static**: `nix flake check`. No GPU, no network, no
     model. Catches formatting, pre-commit, archive-pack, and any
     static derivations declared in `flake.nix` `checks`.
  2. **Smoke (gated)**: `make test` (with `SMOKE_TESTS_ENABLED=true`,
     the default for local devs; CI uses `SMOKE_TESTS_ENABLED=false`).
     Runs every `.local/bin/*-smoke-test` in sequence. `vllm-smoke-test`
     auto-detects the scenario from the runtime environment: with GPU
     present, it runs focused checks AND end-to-end (vllm-config +
     `/v1/models` + completion); without GPU, it runs only the focused
     checks. Missing prerequisites fail with `FATAL`.
  3. **Manual workstation**: nothing additional. The `vllm-smoke-test`
     end-to-end scenario replaces the old `vllm-e2e-smoke` script and
     covers both targets.
     When bumping `vllmRequirement` or `version` in
     `.nix/vllm-runtime.nix` / `flake.nix`, run the layers in order:
     `nix flake check` (cheap sanity), then `nix profile upgrade klarkc`
     (so PATH has the new vllm), then `make test` (covers the focused
  - e2e scenarios via `vllm-smoke-test`). If any layer fails, the
    bump is not complete — fix the failure and re-run from the cheapest
    layer that still passes upward.

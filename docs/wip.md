# WIP: vLLM/Fusion conditional restart + profile-managed runtimes

## 2026-08-06 Build handoff: deterministic runtime packaging in progress

Build attempted the deterministic refactor and hit blockers. Status:

### Done in this session
- Added generic coupled dependency bump policy to `AGENTS.md`.
- Added local `# Bump note:` comments next to Fusion/vLLM versions in `flake.nix` and `.nix/fusion-npm.nix`.
- Switched Fusion/QMD packaging to upstream source tags via new flake inputs:
  - `fusion-src = "github:Runfusion/Fusion/v0.73.0"`
  - `qmd-src = "github:tobi/qmd/v2.1.0"`
- Replaced `npm install --global` / `npm rebuild --global` with `pkgs.fetchPnpmDeps` + `pnpmConfigHook` + `pnpm_10` against the upstream `pnpm-lock.yaml`.
- Iteratively derived the qmd-cli pnpm-deps hash: `sha256-Xaj82yrKh8oPPzhkrQaoIjSRaK9NJvP/udFXdc1UQzc=`.
- `flake.lock` regenerated to track the new source inputs.

### Current blocker (paused here)
- `pkgs.fetchPnpmDeps { fetcherVersion = 3; ... }` produced an offline store that **did not include platform-specific/optional deps** (e.g. `@modelcontextprotocol/sdk-1.29.0`). Subsequent `pnpm install --frozen-lockfile --offline` in the build phase fails with `ERR_PNPM_NO_OFFLINE_TARBALL`.
- Identical failure happens for both `fusion-cli-pnpm-deps` and `qmd-cli-pnpm-deps`. QMD is smaller but exhibits the same error.

### Suggested next steps for the next agent
1. Try `fetcherVersion = 4`. v3 was introduced to produce a reproducible tarball; v4 dumps the SQLite DB to SQL text. Either may include a different set of optional packages. Inspect `pkgs/development/tools/pnpm/generic.nix` to compare.
2. If still incomplete, try setting `pnpmInstallFlags = [ "--force" ]` on the `fetchPnpmDeps` call so pnpm fetches all transitive entries including platform-specific ones (still done once, locked via hash).
3. If still incomplete, the alternative is `fetcherVersion = 2` was deprecated; otherwise look at how other large pnpm monorepos in nixpkgs handle this (e.g. search `pkgs/development/node-packages/overrides.nix` and `pkgs/applications/networking/browsers/firefox.nix`).
4. The Fusion source build itself has not yet been exercised. After pnpm-deps are reproducible, expect additional work to make the build of `packages/cli` succeed in a sandboxed build environment; consider using `FUSION_CLI_FULL_PACKAGE=1` to match what was previously installed from the published npm tarball.
5. vLLM deterministic refactor remains TODO. vLLM 0.20.1 + CUDA 13.0 wheels must be packaged as Nix packages; do not reintroduce `pip download`/`pip install`.

### Files modified in this session (uncommitted)
- `AGENTS.md` (policy)
- `flake.nix` (new inputs + bump notes + `fusionRuntime` args)
- `flake.lock` (new inputs)
- `.nix/fusion-npm.nix` (deterministic refactor; **build currently fails** at `fetchPnpmDeps`)
- `docs/wip.md` (this section)

### Files unchanged from prior work
- `.config/systemd/user/fusion.service`
- `.config/systemd/user/vllm@.service`
- `.local/bin/vllm-config`
- `.local/bin/vllm-serve-pure`
- `.nix/vllm-runtime.nix` (still uses live `pip download`; needs the same deterministic refactor)
- `.nix/opencode-with-codex-auth.nix` (renamed from `opencode-with-reasoning.nix`)

### 2026-08-06 Design review of Build progress

Direction is broadly correct: source tags + `fetchPnpmDeps`/`pnpmConfigHook` are the right path for Fusion/QMD because the published npm tarballs do not include `pnpm-lock.yaml`. The generic `AGENTS.md` policy and local bump notes are also correct.

Implementation issues Build should fix before continuing:

1. `.nix/fusion-npm.nix` currently has `fusionCli.pnpmDeps.hash = ""` (line ~49). This is only acceptable during hash discovery; final code must pin the reported `sha256-...` hash.
2. `fusionCli.nativeBuildInputs` currently includes `pnpmConfigHook` but not `pnpm_10`. The hook explicitly requires a `pnpm` binary in PATH; QMD has `pnpm_10`, Fusion should too.
3. Both derivations call `pnpm install --frozen-lockfile --offline` in `buildPhase` after `pnpmConfigHook` already performs the offline install in `postConfigureHooks`. Prefer the standard helper pattern: let `pnpmConfigHook` install dependencies, then build only (`pnpm --filter @runfusion/fusion build...` / `pnpm run build`). If another install is required, document why.
4. The observed QMD failure is from `pnpmConfigHook`'s install step, before `buildPhase`. The hook unpacks the fixed `pnpmDeps` store and then runs `pnpm install --offline --ignore-scripts --frozen-lockfile`. The failing missing package is a **direct QMD dependency** (`@modelcontextprotocol/sdk@1.29.0`), not merely an optional/platform package. Treat this as a pnpm-deps/store-generation mismatch until proven otherwise.
5. Next likely fix: add `pnpmInstallFlags = [ "--force" ];` to the `fetchPnpmDeps` call **and** expose matching flags to `pnpmConfigHook` if needed (the hook appends `pnpmInstallFlags` to its install). If that changes output, discover and pin the new hash. Trying `fetcherVersion = 4` is also reasonable, but the key is that fetch and hook install flags must be consistent enough that the offline install consumes only the generated store.
6. Fusion source build should probably use the publish/full package mode (`FUSION_CLI_FULL_PACKAGE=1` or `pnpm --filter @runfusion/fusion build:package`) unless Build proves the reduced `build` output matches the old published npm tarball runtime surface. The old package installed a published tarball with `dist/client`, `dist/plugins`, migrations, skill assets, etc.
7. QMD install assumption needs verification after build succeeds: upstream `package.json` declares `bin.qmd = "bin/qmd"`, while `build` writes `dist/cli/qmd.js`. Ensure `bin/qmd` exists in source/tag and resolves to the built CLI after install, or wrap the actual built `dist/cli/qmd.js` directly.

Do not proceed to vLLM until `nix build .#fusion-runtime`, `fusion --version`, `fn --help`, and `qmd --help` pass.

### 2026-08-07 Design review after `fusion-runtime.nix` rename

Direction remains correct, and the rename to `.nix/fusion-runtime.nix` plus the generic `<tool>-runtime.nix` AGENTS policy are good. `nix build .#fusion-runtime --no-sandbox` now succeeds with source-tag pnpm builds, `fetcherVersion = 4`, and pinned hashes:

- Fusion pnpm deps: `sha256-owdk5R1fzfK+2/dxp3dQElTcY9Uko+aHJAanDPhd3YY=`
- QMD pnpm deps: `sha256-6lqkDOjY7C+l9M/Mas7wyXBQFbuO3/YH1s06Olp94oQ=`

Blocking runtime issue found by verification:

- `./result/bin/fusion --version` fails with `ERR_MODULE_NOT_FOUND` for `@earendil-works/pi-coding-agent` imported from `.../lib/node_modules/@runfusion/fusion/dist/bin.js`.
- `./result/bin/qmd --help` fails with `ERR_MODULE_NOT_FOUND` for `fast-glob` imported from `.../lib/node_modules/@tobilu/qmd/dist/cli/qmd.js`.
- Evidence from output layout: `result/lib/node_modules/@runfusion/fusion` contains only package files (`agent-browser.mjs`, `bin.mjs`, `dist`, `package.json`, etc.); there is no usable installed dependency tree under `result/lib/node_modules`.

Root cause:

- `pnpmConfigHook` installs dependencies into the build source tree, but the current `installPhase` copies only package source/build artifacts into `$out/lib/node_modules/...`; it does **not** copy the generated `node_modules` / `.pnpm` tree into `$out`.
- `NODE_PATH` is not enough here: these are ESM imports, and Node resolves packages by walking ancestor `node_modules` directories from the importing file. The required packages must exist under an ancestor such as `$out/lib/node_modules/<dep>` with the `.pnpm` backing tree available.

Recommended next Build fix:

1. In each derivation's `installPhase`, copy the pnpm-installed dependency tree into `$out/lib/node_modules` before/alongside copying the package directory, for example conceptually:
   ```sh
   mkdir -p "$out/lib/node_modules"
   cp -R node_modules/. "$out/lib/node_modules/"
   mkdir -p "$out/lib/node_modules/@runfusion/fusion"
   cp -R packages/cli/{bin.mjs,agent-browser.mjs,dist,skill,package.json,README.md} \
     "$out/lib/node_modules/@runfusion/fusion/"
   ```
   For QMD, same idea: copy `node_modules/.` to `$out/lib/node_modules/`, then copy QMD package files into `$out/lib/node_modules/@tobilu/qmd/`.
2. Preserve `.pnpm` and symlink structure; use a copy mode that does not flatten symlinks incorrectly. If Nix fixup reports references to the build dir, adjust copy strategy.
3. Add `pnpmInstallFlags = [ "--force" ];` as a top-level derivation attr if Build wants the hook install flags to match the fetcher flags. Currently it is only passed to `fetchPnpmDeps`; the build succeeded, but consistency is safer and easier to reason about.
4. Re-run:
   - `nix build .#fusion-runtime --no-sandbox`
   - `./result/bin/fusion --version`
   - `./result/bin/fn --help`
   - `./result/bin/qmd --help`

Do not start vLLM until the runtime dependency tree is copied correctly and the Fusion/QMD CLI smoke checks pass.

### 2026-08-07 Design review after latest `node_modules` copy change

Progress is still directionally correct, but Fusion remains blocked at runtime.

Verification run:

```sh
nix build .#fusion-runtime --no-sandbox
./result/bin/fusion --version
```

Build result:

- `nix build .#fusion-runtime --no-sandbox` succeeds.
- `./result/bin/qmd --help` succeeds.
- `./result/bin/fusion --version` and `./result/bin/fn --help` still fail with:
  ```text
  Error [ERR_MODULE_NOT_FOUND]: Cannot find package '@earendil-works/pi-coding-agent'
  imported from .../lib/node_modules/@runfusion/fusion/dist/bin.js
  ```

Evidence from the latest output layout:

- Latest Fusion output has only `lib/node_modules` under `$out/lib`; it does **not** have top-level `$out/lib/node_modules/@earendil-works`.
- The required package exists under pnpm's hoisted alias area / store:
  ```text
  $out/lib/node_modules/.pnpm/@earendil-works+pi-coding-agent@...
  ```
  but Node does not search `node_modules/.pnpm/node_modules` automatically for bare ESM imports.

Root cause:

- Copying `node_modules` to `$out/lib/` preserves pnpm's `.pnpm` store, but it does not expose every package as a top-level `node_modules/<name>` / `node_modules/@scope/<name>` entry.
- The previous attempt to copy `.pnpm/node_modules/.` into top-level `node_modules` preserved pnpm's internal symlink targets verbatim, which made links like `@earendil-works/pi-coding-agent -> ../../@earendil-works+...` resolve outside `.pnpm` and become broken in `$out`.

Recommended Build fix:

1. Keep copying the whole `node_modules` tree to `$out/lib/node_modules` so the `.pnpm` store stays intact.
2. After replacing `$out/lib/node_modules/@runfusion/fusion` with the real built package directory, synthesize **top-level package aliases** that point into `.pnpm/node_modules`, instead of copying `.pnpm/node_modules` symlinks verbatim.

Conceptual shell:

```sh
aliases="$out/lib/node_modules/.pnpm/node_modules"
top="$out/lib/node_modules"

for entry in "$aliases"/*; do
  name="$(basename "$entry")"
  case "$name" in
    @*)
      mkdir -p "$top/$name"
      for scoped in "$entry"/*; do
        scoped_name="$(basename "$scoped")"
        # Preserve real shipped package dirs such as @runfusion/fusion.
        [ "$name/$scoped_name" = "@runfusion/fusion" ] && continue
        rm -f "$top/$name/$scoped_name"
        ln -s "../.pnpm/node_modules/$name/$scoped_name" "$top/$name/$scoped_name"
      done
      ;;
    *)
      rm -f "$top/$name"
      ln -s ".pnpm/node_modules/$name" "$top/$name"
      ;;
  esac
done
```

For QMD, the same alias-synthesis pattern is safer than copying `.pnpm/node_modules/.` verbatim, but QMD currently passes `qmd --help` even with the less-clean layout.

3. Avoid `dontFixup = true` if possible. It hides broken symlinks. Once aliases are synthesized correctly, re-enable fixup or at least run an explicit check that the key aliases resolve:
   ```sh
   test -e "$out/lib/node_modules/@earendil-works/pi-coding-agent/package.json"
   test -e "$out/lib/node_modules/@runfusion/fusion/dist/bin.js"
   ```
   If full `noBrokenSymlinks` still fails because unused workspace plugin symlinks remain, remove only those unused workspace symlinks rather than disabling all fixup.

4. Re-run mandatory smoke checks:
   ```sh
   nix build .#fusion-runtime --no-sandbox
   ./result/bin/fusion --version
   ./result/bin/fn --help
   ./result/bin/qmd --help
   ```

Do not proceed to vLLM until these checks pass.

### 2026-08-07 Design correction: avoid hand-editing pnpm internals

User concern is valid: manually copying `.pnpm/node_modules` and synthesizing symlinks is too invasive and brittle. It relies on pnpm's internal node_modules layout and can easily break across pnpm versions or lockfile structure changes.

Preferred Build direction now:

1. Keep using Nix's pnpm helpers for deterministic dependency fetching/install:
   - `pkgs.fetchPnpmDeps`
   - `pkgs.pnpmConfigHook`
   - pinned `pnpmDeps.hash`
2. Do **not** hand-copy `.pnpm/node_modules` internals or synthesize pnpm symlinks unless all higher-level options fail.
3. Let pnpm produce a portable package output after the hook-installed workspace is available. First candidate:
   ```sh
   pnpm --filter @runfusion/fusion deploy --prod --legacy --config.node-linker=hoisted "$out/lib/node_modules/@runfusion/fusion"
   ```
   Then copy/overlay the built `dist` if `deploy` does not include it, and wrap:
   ```sh
   makeWrapper "$out/lib/node_modules/@runfusion/fusion/bin.mjs" "$out/bin/fusion" ...
   makeWrapper "$out/lib/node_modules/@runfusion/fusion/bin.mjs" "$out/bin/fn" ...
   ```
4. For QMD, try the analogous higher-level packaging path before manual symlink logic:
   ```sh
   pnpm deploy --prod --legacy --config.node-linker=hoisted "$out/lib/node_modules/@tobilu/qmd"
   ```
   If `pnpm deploy` is workspace-only and QMD does not support it directly, prefer a documented pnpm-supported alternative (`pnpm pack` + install/copy from the package output) before touching `.pnpm` internals.
5. The `pnpm deploy` command must still run offline against the already fetched `pnpmDeps` store. It is acceptable under AGENTS policy only if it does not resolve/download; it is a packaging/copy step over the lockfile-installed dependency graph.

Why this is better:

- `pnpmConfigHook` handles the deterministic install in the build tree.
- `pnpm deploy` is pnpm's supported way to materialize a deployable package from a workspace with the correct production dependency layout.
- The Nix derivation no longer depends on implementation details like relative symlink targets inside `.pnpm/node_modules`.

Current locked nixpkgs note:

- This nixpkgs revision exposes `fetchPnpmDeps` and `pnpmConfigHook`, but not a top-level `buildPnpmPackage` helper. Therefore Build still needs custom `buildPhase`/`installPhase`, but the install phase should delegate dependency layout to pnpm (`deploy`) rather than reconstructing pnpm internals by hand.

Suggested rollback from current Build attempt:

- Remove manual `.pnpm` copying/symlink synthesis from `.nix/fusion-runtime.nix`.
- Remove `dontFixup = true` if `deploy` produces a clean tree. If broken workspace symlinks remain, fix/remove only those specific unused links and keep Nix's broken-symlink check active where possible.
- Re-run:
  ```sh
  nix build .#fusion-runtime --no-sandbox
  ./result/bin/fusion --version
  ./result/bin/fn --help
  ./result/bin/qmd --help
  ```

### 2026-08-07 pnpm helper research

User asked whether a higher-level helper exists and whether bumping nixpkgs is worth it.

Findings:

- There is no top-level `pkgs.buildPnpmPackage` in the locked nixpkgs currently used by this repo.
- A nixpkgs bump is **not required** to get the relevant newer pnpm build helper: locked nixpkgs already exposes `pkgs.pnpmBuildHook` (`nix eval ... pkgs ? pnpmBuildHook` returned `yes`).
- Upstream nixpkgs master documents the supported pnpm pattern as:
  - `fetchPnpmDeps` to create a fixed-output pnpm store;
  - `pnpmConfigHook` to configure/install that store offline;
  - optional `pnpmBuildHook` to run `pnpm run <script>` instead of writing a custom build phase.
- `pnpmBuildHook` only replaces the build phase. It does **not** solve the runtime packaging/install layout problem by itself.
- pnpm's own supported solution for a deployable workspace runtime is `pnpm deploy`:
  - docs: “Deploy a package from a workspace”; files and all dependencies, including workspace deps, are installed inside an isolated `node_modules` directory at the target; target is portable and executable without additional steps.
  - use `--legacy` when `inject-workspace-packages=true` is not configured.

Recommended Build direction after this research:

1. Do **not** bump nixpkgs just looking for `buildPnpmPackage`; the current lock already has the pieces we need (`fetchPnpmDeps`, `pnpmConfigHook`, `pnpmBuildHook`, `pnpm_11`).
2. Replace manual build phase with `pnpmBuildHook` where straightforward:
   ```nix
   nativeBuildInputs = [ nodejs pnpmConfigHook pnpmBuildHook pnpm_11 makeWrapper ];
   pnpmWorkspaces = [ "@runfusion/fusion" ];
   pnpmBuildScript = "build";
   ```
   If Fusion needs a custom build command/env, a custom `buildPhase` is still acceptable, but `pnpmBuildHook` is preferred for the plain `pnpm --filter ... build` case.
3. Replace invasive install layout logic with `pnpm deploy` for Fusion:
   ```sh
   pnpm --filter @runfusion/fusion --prod deploy --legacy "$out/lib/node_modules/@runfusion/fusion"
   ```
   Run this after build so `dist` is included. If `deploy --prod` omits generated files, copy/overlay `packages/cli/dist` and other expected package files afterward.
4. For QMD (not a workspace package), `pnpm deploy` may not apply. First try a supported package output path:
   - `pnpm pack --pack-destination "$TMPDIR"`
   - unpack the resulting `.tgz` into `$out/lib/node_modules/@tobilu/qmd`
   - ensure production dependencies are available through a supported `pnpm install --prod --offline --config.node-linker=hoisted` inside that output if needed.
   Avoid manual `.pnpm` symlink reconstruction unless no supported pnpm packaging path works.
5. Keep `pnpmInstallFlags = [ "--force" ];` coupled between `fetchPnpmDeps` and the derivation hook if it is needed for store completeness.
6. Re-enable normal fixup/no-broken-symlink checks if `deploy`/`pack` creates a clean portable tree.

### 2026-08-07 Nixpkgs pnpm docs/examples research — helper-only path

Research source: locked nixpkgs docs and package examples under `/nix/store/swsq9fz5xzbzqd2864z9k0xkq009cpg6-source` plus current upstream nixpkgs docs.

Findings from Nixpkgs docs:

- Locked nixpkgs documents pnpm packaging in `doc/languages-frameworks/javascript.section.md`:
  - use `fetchPnpmDeps` to create a fixed-output pnpm store;
  - use `pnpmConfigHook` to configure/install that pre-fetched store offline;
  - pin a pnpm major (`pnpm_10`, `pnpm_11`, etc.) and pass the same pnpm to `fetchPnpmDeps`;
  - `pnpmInstallFlags` can be passed to both `fetchPnpmDeps` and the derivation hook, with documented example `pnpmInstallFlags = [ "--shamefully-hoist" ];`;
  - `pnpmWorkspaces` can scope dependency fetch/install for workspaces.
- Locked nixpkgs also documents `pnpmBuildHook` in `doc/hooks/pnpm.section.md`; this hook overrides the build phase and runs a pnpm build script using helper-controlled variables (`pnpmBuildScript`, `pnpmBuildFlags`, `pnpmWorkspaces`, `pnpmRoot`).
- There is **no** `buildPnpmPackage` in locked nixpkgs, and upstream master research did not identify a top-level full pnpm app builder. A nixpkgs bump is therefore not justified just to find `buildPnpmPackage`.
- There is also no documented `pnpmInstallHook`. For app installation, nixpkgs examples either write custom install phases or combine pnpm setup with `npmHooks.npmInstallHook`.

Relevant nixpkgs examples:

- `pkgs/by-name/zi/zigbee2mqtt/package.nix` combines:
  - `fetchPnpmDeps`
  - `pnpmConfigHook`
  - `pnpm_10`
  - `npmHooks.npmInstallHook`
  - `dontNpmPrune = true`
  This is important because `npmInstallHook` is a Nixpkgs helper that knows how to package Node apps into `$out/lib/node_modules/<package-name>` and install bin entries, while pnpm provided the already-installed dependency tree.
- `pkgs/by-name/as/astro-language-server/package.nix` and `pkgs/by-name/ze/zenn-cli/package.nix` show the common workspace pattern, but they still use direct `pnpm` commands in custom phases. Under this repo's stricter invariant, treat those as prior art for flags/layout only, not as an acceptable direct-command pattern.
- `pkgs/by-name/ag/agent-browser/package.nix` shows a pnpm workspace frontend build with `pnpmWorkspaces`, but again uses a custom direct `pnpm --filter ... build`; for this repo prefer `pnpmBuildHook` when possible.

Helper-only Build path to try next:

1. Replace custom Fusion/QMD `buildPhase` direct `pnpm ... build` with `pkgs.pnpmBuildHook`:
   ```nix
   nativeBuildInputs = [ nodejs pnpmConfigHook pnpmBuildHook pnpm_11 makeWrapper ];
   pnpmWorkspaces = [ "@runfusion/fusion" ];
   pnpmBuildScript = "build";
   ```
   If the hook's exact invocation cannot express Fusion's build, document why before falling back.
2. Add `pnpmInstallFlags = [ "--shamefully-hoist" ];` (or a smaller documented hoist setting if enough) to both:
   - the derivation attributes consumed by `pnpmConfigHook`;
   - `fetchPnpmDeps` via `inherit (finalAttrs) pnpmInstallFlags;`.
   This is the documented Nixpkgs-supported way to change pnpm's node_modules layout; do not synthesize `.pnpm` symlinks manually.
3. Try `npmHooks.npmInstallHook` as the install helper, following `zigbee2mqtt`:
   - add `npmHooks.npmInstallHook` to `nativeBuildInputs`;
   - set `dontNpmPrune = true` initially to avoid npm pruning/resolution;
   - for Fusion workspace packaging, test whether `npmWorkspace = "packages/cli"` is sufficient. If the hook's `packageOut` uses the root package name unexpectedly, use a minimal helper-driven install wrapper that changes directory to `packages/cli` and calls `npmInstallHook`, rather than invoking npm/pnpm commands directly.
4. For QMD, because it is not a workspace package, `npmHooks.npmInstallHook` should be simpler: after `pnpmConfigHook` + `pnpmBuildHook`, let `npmInstallHook` package `@tobilu/qmd` and install its `bin.qmd` entry. Keep `dontNpmPrune = true` unless pruning is proven offline/safe.
5. Re-enable normal fixup if helper-generated output has no broken symlinks. If broken links remain, prefer documented hoist/install flags over manual deletion/synthesis.

Research conclusion:

- Do not bump nixpkgs just for pnpm helpers at this point.
- The best helper-only path available in the current lock is: `fetchPnpmDeps` + `pnpmConfigHook` + `pnpmBuildHook` + `npmHooks.npmInstallHook`, with documented `pnpmInstallFlags` such as `--shamefully-hoist` to make runtime layout packageable.
- If that cannot express Fusion's workspace runtime without custom package-manager commands, the remaining strict-policy option is a generated Nix dependency graph (dream2nix/node2nix/pnpm-lock importer style) or an explicit policy exception.

### 2026-08-07 Design review of latest helper-only Build attempt

Current state reviewed from `.nix/fusion-runtime.nix` and a fresh build:

```sh
nix build .#fusion-runtime --no-sandbox
```

Result: still failing, but the failure moved forward. `pnpmConfigHook` and `pnpmBuildHook` are now active, hashes are pinned, and the build reaches Fusion's `tsup` build. The latest hard failure is:

```text
packages/desktop/node_modules/electron/package.json: ENOENT
@fusion/desktop@0.73.0 build: `tsx scripts/build.ts`
Error: pnpm --filter @fusion/desktop build exited with code 1
@runfusion/fusion@0.73.0 build: `tsup`
```

Key finding:

- Build set `env.CI = "true"` to avoid pnpm prompts, but Fusion's `packages/cli/tsup.config.ts` treats `CI=true` as a request for the **full publish package**. `wantsFullCliPackage()` returns true when `env.CI === "true" || env.CI === "1"`, unless `FUSION_CLI_FULL_PACKAGE=0|false` is explicitly set first.
- That is why the CLI build now tries to build `@fusion/desktop`, which then fails because desktop/electron dependencies are not installed in the filtered workspace output.

Recommended next Build fix:

1. Do not rely on `CI=true` alone for noninteractive pnpm behavior. Either remove `env.CI = "true"` or explicitly force Fusion's local/default package mode:
   ```nix
   env.FUSION_CLI_FULL_PACKAGE = "0";
   ```
   Because Fusion's config checks `FUSION_CLI_FULL_PACKAGE` before `CI`, setting it to `0` should prevent the desktop/full-publish branch even if `CI=true` remains needed for pnpm.
2. Prefer removing `CI=true` if `confirm-modules-purge=false` / documented install flags are sufficient. The goal is to avoid accidentally enabling upstream CI-only/full-release behavior.
3. Current `prePnpmInstall` runs direct `pnpm config set ...` commands. Under the stricter user policy this is only tolerable as a temporary workaround; prefer the documented Nix helper variable `pnpmInstallFlags` if possible.
4. Likely way to make `pnpmInstallFlags` work correctly: set
   ```nix
   __structuredAttrs = true;
   strictDeps = true;
   pnpmInstallFlags = [
     "--shamefully-hoist"
     "--config.confirmModulesPurge=false"
   ];
   ```
   in each derivation, matching the `pnpmBuildHook` documentation example. Without structured attrs, the Nix list was observed to collapse into one shell argument (`'shamefully-hoist --config.confirmModulesPurge'`). Structured attrs are likely the intended way for hook arrays such as `pnpmInstallFlags`, `pnpmBuildFlags`, and `pnpmWorkspaces` to survive as arrays.
5. If structured attrs fixes hook flag passing, remove `prePnpmInstall` entirely and pass the same `pnpmInstallFlags` to `fetchPnpmDeps` with `inherit (finalAttrs) pnpmInstallFlags;`.
6. Be cautious with `pnpmWorkspaces`: `pnpmBuildHook` runs the build script for every filtered workspace. Adding many workspace names can build more than needed. The immediate desktop failure is from Fusion's `CI=true` full-package path, not from `pnpmWorkspaces` containing desktop (it does not). After disabling full package mode, reassess whether the workspace list can be reduced to the packages actually needed by CLI build.

Do not start vLLM until Fusion/QMD reaches:

```sh
nix build .#fusion-runtime --no-sandbox
./result/bin/fusion --version
./result/bin/fn --help
./result/bin/qmd --help
```

### 2026-08-07 Design correction: stricter no package-manager phases

User rejected the `pnpm deploy` / `pnpm pack` direction because it still relies on explicit package-manager commands in repo-maintained build phases. This is a valid stricter interpretation of the determinism invariant.

Updated rule for this work:

- Repo-maintained derivation phases should not call `pnpm`, `npm`, `bun`, `pip`, etc. directly.
- It is acceptable to use Nixpkgs-provided hooks/build helpers that encapsulate package-manager behavior for deterministic dependency materialization, but custom `buildPhase`/`installPhase` shell should not invoke package-manager CLIs.
- If Nixpkgs does not provide a helper for a required package-manager operation (for example `pnpm deploy`), do not reimplement that operation manually by poking `.pnpm` internals.

Consequences for Fusion/QMD:

- Current `.nix/fusion-runtime.nix` direction is no longer acceptable as-is because it has direct `pnpm` calls in `buildPhase`, and the suggested `pnpm deploy` install path is rejected.
- `pnpmConfigHook` and `pnpmBuildHook` are still candidates because they are Nixpkgs helpers; however, this locked nixpkgs does not expose an install/deploy hook that creates a portable runtime tree.
- Since there is no `buildPnpmPackage`/`pnpmInstallHook` in the locked nixpkgs, Build should not keep iterating on direct pnpm commands or manual `.pnpm` symlink manipulation.

Preferred paths now, in order:

0. **Read Nix/Nixpkgs pnpm packaging documentation first**
   - Before choosing a Nix implementation strategy, inspect the Nixpkgs JavaScript/pnpm packaging docs and hook docs for the supported way to build/package pnpm projects in Nix.
   - Relevant Nix docs to review include the JavaScript `pnpm` section, `fetchPnpmDeps`, `pnpmConfigHook`, `pnpmBuildHook`, `pnpmWorkspaces`, `pnpmInstallFlags`, `pnpmRoot`/`sourceRoot`, and examples of packaging pnpm workspaces/apps in nixpkgs.
   - Use those Nix docs and in-tree nixpkgs examples to decide what helper/generator path to use. Do not infer or reconstruct `.pnpm` layout manually from pnpm internals.

1. **Find/use an existing Nixpkgs package/helper for this exact packaging shape**
   - Search nixpkgs and upstream for existing Fusion/QMD packages or a newer Nixpkgs helper that provides a full pnpm app build+install abstraction (not just `pnpmBuildHook`).
   - If such a helper exists in a newer nixpkgs revision, evaluate a targeted nixpkgs bump only if it materially reduces repo-maintained package-manager shell logic and does not destabilize unrelated packages.
2. **Generate a Nix dependency graph from the upstream lockfile**
   - Use a generator-style solution where the package-manager resolver is run outside the Nix build to produce checked-in/generated Nix metadata, and the build consumes only Nix fetchers/links.
   - Examples to investigate: pnpm-lock-to-Nix tooling, dream2nix, node2nix-style approaches, or any maintained Nixpkgs-supported pnpm lock importer.
   - Generated metadata must be committed/locked and have local bump notes explaining how to regenerate and verify.
3. **Escalate/accept exception explicitly**
   - If neither a Nix helper nor generated Nix metadata can package Fusion/QMD pragmatically, the remaining choices are either keep the old live package-manager derivation as an explicit policy exception or defer Fusion/QMD packaging until a suitable helper exists. Do not silently weaken the invariant.

Immediate Build recommendation:

- Stop implementing the direct `pnpm deploy` / manual `.pnpm` layout path.
- Preserve only the naming/policy/bump-note changes that are independently useful.
- Reassess with a fresh design pass focused on generator/helper options before making more packaging changes.

## 2026-07-28 request: only restart Fusion from vLLM when Fusion is enabled

User direction:

1. `vllm-config`/`vllm@.service` must only restart `fusion.service` when `fusion.service` is **enabled** in the user systemd manager.
2. Every dependency in this repo must be declared as a flake input. No `builtins.fetchTree` for `nixpkgs`.
3. `fusion.service` and `vllm@.service` must consume runtimes from the installed `klarkc` profile (`nix profile upgrade klarkc`), not build during service startup.

## Architecture after this work

```text
$HOME (= repo root, this dotfiles checkout, %h)
├── flake.nix
│   - packages.fusion-runtime = callPackage ./.nix/fusion-npm.nix { version = "0.73.0"; }
│   - packages.vllm-runtime   = callPackage ./.nix/vllm-runtime.nix { version = "0.20.1-cu130"; }
│   - packages.default (buildEnv "klarkc-dotfiles_profile") now includes
│       fusionRuntime + vllmRuntime along with the existing entries.
└── nix profile upgrade klarkc
    -> installs /nix/store/.../bin/fusion (+ /nix/store/.../bin/fn)
    -> installs /nix/store/.../bin/vllm
    -> symlinks into %h/.nix-profile/bin/

systemd user manager:
- fusion.service        -> ExecStart runs %h/.nix-profile/bin/fusion dashboard ...
- vllm@<model>.service  -> ExecStart runs %h/.local/bin/vllm-serve-pure
                            (script resolves %h/.nix-profile/bin/vllm; no nix build)
                            -> after readiness, conditional Fusion restart only if enabled.
```

## Implementation (✅ DONE — 2026-08-06)

### `.nix/fusion-npm.nix`

- Refactored to function form: `{ pkgs, version }: ...`. No more `builtins.fetchTree`.
- `version = "0.73.0"`.
- `fusionNpmPayload` `outputHash` pinned: `sha256-3Dpcx4PL08Q/Ki1msYiMmz9jET3IW0Au55eWRx26lh8=` (recursive sha256). Re-pinned because the npm registry tree hash differs between runs (transitive deps like `electron` install scripts).
- `fusion-runtime` `nativeBuildInputs` extended with `cacert`, and `NODE_EXTRA_CA_CERTS=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt` exported in `installPhase`, so the `electron` postinstall (new transitive dep in 0.73.0) can verify GitHub releases TLS during `npm rebuild`.

### `.nix/vllm-runtime.nix`

- Refactored to function form: `{ pkgs, version }: ...`. No more `builtins.fetchTree`.
- `wheelhouse` `outputHash` pinned: `sha256-NFCLSOvtThz1xbCU1l2MCBJqH6Sl/f85dhYmpSzTUug=`.
- `version` passed in from the flake (`"0.20.1-cu130"`).

### `flake.nix`

- Two new outputs:
  ```nix
  packages.fusion-runtime = fusionRuntime;
  packages.vllm-runtime  = vllmRuntime;
  ```
- `packages.default.paths` now includes `fusionRuntime` and `vllmRuntime` so `nix profile upgrade klarkc` builds/installs them.

### `.config/systemd/user/vllm@.service`

- `VLLM_NIX_CONFIG` / `VLLM_RUNTIME_REF` env vars and `%h/Sources/Fusion/klarkc/dotfiles`-style paths are gone.
- `ExecStartPost=/bin/sh -c 'systemctl --user is-enabled --quiet fusion.service && systemctl --user restart fusion.service || true'` — Fusion restart only happens when `fusion.service` is enabled.
- `ReadOnlyPaths` reduced to `~/.config/vllm/%i.env`, `vllm-patch-model-defaults`, `vllm-serve-pure`, `vllm-wait-ready` — service no longer needs flake files or `.nix/*.nix`.
- `ExecStart=%h/.local/bin/vllm-serve-pure` unchanged (template instance name still selects the env file + runtime dir, but the same shared wrapper is used).

### `.local/bin/vllm-serve-pure`

- `find_nix`, `nix build`, `--print-out-paths`, `VLLM_RUNTIME_REF` removed.
- Resolves vLLM runtime from the installed profile:
  ```bash
  vllm_bin="${VLLM_BIN:-$HOME/.nix-profile/bin/vllm}"
  if [ ! -x "$vllm_bin" ]; then
    echo "vLLM binary not found in klarkc profile: $vllm_bin" >&2
    echo "Run: nix profile upgrade klarkc" >&2
    exit 127
  fi
  runtime="$(dirname "$(dirname "$(readlink -f "$vllm_bin")")")"
  ```
  Then reuses the existing `$runtime/lib`, `$runtime/bin/vllm`, and `$runtime/nix-support/ld-library-path` logic with no other behavioral change.

### `.config/systemd/user/fusion.service`

- `FUSION_NIX_CONFIG` / `FUSION_RUNTIME_REF` / `WorkingDirectory=%h/Sources/Fusion/klarkc/dotfiles` removed.
- `ExecStart` now resolves the profile-installed CLI directly:
  ```sh
  cli=""
  for candidate in "$HOME/.nix-profile/bin/fusion" "$HOME/.nix-profile/bin/fn"; do
    [ -x "$candidate" ] && { cli="$candidate"; break; }
  done
  [ -z "$cli" ] && { echo "Fusion CLI not found in klarkc profile (looked for %h/.nix-profile/bin/fusion and .../fn); run: nix profile upgrade klarkc" >&2; exit 127; }
  exec "$cli" dashboard --host 0.0.0.0 --port 4040 --no-auth
  ```
- `ReadOnlyPaths` updated to point at the profile binary and SSH config; no flake paths.

### `.local/bin/vllm-config`

- `fusion_enabled` detection via `systemctl --user is-enabled --quiet fusion.service` (line 72-75).
- All Fusion-related operations (stop, reset-failed, status, log follow, wait loop) gate on `$fusion_enabled`.
- Upfront notice printed when Fusion is disabled (line 78-80).
- Wait loop and error paths correctly branch on `$fusion_enabled` so a vLLM or target failure does not query Fusion status when Fusion is disabled.
- `bash -n` passes.

## Acceptance criteria

`vllm-config`:

- With `fusion.service` enabled:
  - stops Fusion, switches vLLM model, restarts Fusion after vLLM is ready, exits when both are active.
  - failed Fusion restart still produces non-zero exit.
- With `fusion.service` disabled:
  - does not stop/start/restart Fusion.
  - does not include Fusion in status / log follow.
  - prints the upfront note.
  - exits as soon as the selected `vllm@...service` is active.
  - failure diagnostics do not call `systemctl status fusion.service`.

`fusion.service`:

- Starts by execing `%h/.nix-profile/bin/fusion dashboard ...` (or `fn` fallback if Fusion shipped `fn` as primary).
- If the runtime is not installed in the klarkc profile, the unit fails fast with a clear pointer to `nix profile upgrade klarkc` (no Nix build is performed at service start).

`vllm@.service` / `vllm-serve-pure`:

- Starts the model server by execing the profile-installed `vllm` (resolved via `%h/.nix-profile/bin/vllm`).
- After vLLM is ready, conditional Fusion restart only when `fusion.service` is enabled.
- If vLLM is not installed, the wrapper exits non-zero and tells the user to run `nix profile upgrade klarkc`.
- vLLM unit template substitution `%i` continues to drive configuration and runtime dir; the wrapper binary does not change per instance.

Dependency surface:

- No `builtins.fetchTree` of `nixpkgs` anywhere under `.nix/`.
- No `nix build`, `nix-build`, `--file`, or `--print-out-paths` in `vllm-config`, `vllm-serve-pure`, `fusion.service`, `vllm@.service`, or any helper invoked from a systemd user unit.
- All nixpkgs dependencies flow through the flake `nixpkgs` input.

## Verified

- `bash -n .local/bin/vllm-config` ✓
- `bash -n .local/bin/vllm-serve-pure` ✓
- `nix flake check --no-build` ✓ (all packages green)
- `nix build .#fusion-runtime --no-sandbox` ✓ (Fusion CLI reports `0.73.0`)
- `nix build .#vllm-runtime --no-sandbox` ✓ (vllm wrapper present)
- `nix build .#default --no-sandbox` ✓ (`/nix/store/...-klarkc-dotfiles_profile` produced; profile bin contains `fusion`, `fn`, `vllm`).
- Direct runtime resolution in `vllm-serve-pure` style: `runtime` derives correctly from `%h/.nix-profile/bin/vllm` symlink — `/nix/store/...-vllm-runtime-0.20.1-cu130`, with `bin/vllm`, `lib/`, and `nix-support/ld-library-path` all present.

## Risks / follow-ups

1. **`outputHash` non-determinism for `fusionNpmPayload`** — the npm install output hash varies between runs because the upstream npm tree differs. The current pinned hash matches a recent run; if a re-build produces a mismatch, re-pin with the freshly-printed hash. Mitigation options for a future iteration: pin specific tarball URLs (`tarballHash` per package), or use `npm ci` with a checked-in lockfile.
2. **Service template `%i` does not currently change anything about the runtime binary** — confirmed; this is intentional. Different models select different `~/.config/vllm/<i>.env` files and different cache dirs, but always run the same `vllm` from the klarkc profile.
3. **`packages.default` is now larger** — `nix profile upgrade klarkc` will pull in `fusion-runtime` and `vllm-runtime` (vLLM is ~hundreds of MB). If that is undesirable, move them out of `packages.default` and require an explicit `nix profile install .#fusion-runtime` and `nix profile install .#vllm-runtime`, then have the services still consume those profile entries directly.

## Current Build handoff — deterministic runtime packaging

Design status: service/runtime integration is implemented and verified, but two repo-maintained Nix derivations still violate the new deterministic-build invariant by resolving package-manager graphs during builds. Build should fix these before final merge if the invariant must be true immediately.

### 1. Fusion/QMD: build from upstream pnpm-locked source tags

Primary recommendation:

- Add flake inputs for upstream source tags:
  ```nix
  fusion-src = {
    url = "github:Runfusion/Fusion/v0.73.0";
    flake = false;
  };

  qmd-src = {
    url = "github:tobi/qmd/v2.1.0";
    flake = false;
  };
  ```
- Refactor `.nix/fusion-npm.nix` to consume `{ fusion-src, qmd-src, ... }`.
- Use locked nixpkgs `pkgs.pnpm_10.fetchDeps` + `pkgs.pnpm_10.configHook` with each upstream repo's committed `pnpm-lock.yaml`.
- Remove live `npm install --global` and `npm rebuild --global` entirely.
- Preserve documented external runtime tools in `runtimePath` (for example `docker-client`, `tmux`, `git`, `gh`, `openssh`, `python3`, `uv`) as Nix runtime dependencies/wrapper PATH entries. These are not npm dependencies and do not need npm graph entries unless Fusion upstream declares them as packages.
- Do not keep extra ad hoc npm top-level installs for packages Fusion already declares itself:
  - `@runfusion/fusion@0.73.0` `packages/cli/package.json` declares `node-pty` as `npm:@homebridge/node-pty-prebuilt-multiarch@^0.13.1` and `dockerode` as `^4.0.12`.
  - The workspace also declares `node-pty` in engine/dashboard packages and `dockerode` in core, confirming these are upstream-managed package dependencies when building from source/lockfile.
  - Therefore Build should let the upstream `pnpm-lock.yaml` materialize those packages rather than installing separate top-level `node-pty`/`dockerode` entries with potentially different versions.
- `send` is different: quick checks of the Fusion 0.73.0 README/docs/manifests found no direct `send` package declaration; occurrences in docs are ordinary text/CLI verbs (for example message send). Treat `send` as transitive-only if pulled by another package (e.g. Express stack), and do not add it as a top-level npm dependency unless Build finds a concrete runtime import/failure requiring it.

Code-annotation requirement for Build:

- When refactoring `.nix/fusion-npm.nix`, add a short comment near the Nix `runtimePath` list explaining that entries such as `docker-client`, `tmux`, `git`, `gh`, `openssh`, `python3`, and `uv` are external runtime tools exposed on Fusion's wrapper `PATH`, not npm dependencies.
- Add a short comment near the pnpm package build/install logic explaining that Fusion's upstream manifests/lockfile own npm packages such as `node-pty` and `dockerode`; do not add separate top-level npm installs for them unless a future upstream/runtime change provides concrete evidence.
- If `send` is intentionally added later, include a code comment with the exact upstream import/runtime failure that justified promoting it from transitive dependency to explicit top-level dependency.

Acceptance:

- No `npm install`, `npm ci`, or `npm rebuild` in `.nix/fusion-npm.nix`.
- `nix build .#fusion-runtime` succeeds.
- Result/profile provides `fusion`, `fn`, and `qmd`.
- `fusion --version` reports `0.73.0`; `qmd --help` works.

### 2. vLLM: prefer a declared wheelhouse artifact, not a live pip resolver

Evidence:

- Locked nixpkgs `python312Packages.vllm` is `0.16.0`; the repo currently targets `vllm==0.20.1` with PyTorch CUDA 13.0 wheels.
- The locked nixpkgs expression is source-oriented and tailored to `0.16.0` (`pkgs/development/python-modules/vllm/default.nix`, `version = "0.16.0"`), so blindly overriding it to `0.20.1` is likely high-risk.
- PyPI metadata for `vllm==0.20.1` publishes `cp38-abi3-manylinux_2_35_{x86_64,aarch64}` wheels plus sdist. For this repo's current Linux x86_64/Python 3.12 path, an x86_64 ABI3 wheel should be usable.
- PyTorch CUDA 13.0 index has `torch-2.11.0+cu130-cp312-cp312-manylinux_2_28_x86_64.whl` and matching Python variants.

Primary recommendation:

1. Generate the full resolved wheelhouse outside the Nix build, once, for the target platform/interpreter:
   - Python: CPython 3.12
   - Platform: Linux x86_64
   - Requirements:
     - `torch==2.11.0+cu130`
     - `torchvision==0.26.0+cu130`
     - `torchaudio==2.11.0+cu130`
     - `vllm==0.20.1`
   - Extra index: `https://download.pytorch.org/whl/cu130`
2. Publish or store that wheelhouse as a single immutable archive artifact, with an adjacent manifest containing all wheel filenames + hashes.
3. Add the archive as a `flake = false` input, for example:
   ```nix
   vllm-wheelhouse = {
     url = "https://.../vllm-0.20.1-cu130-cp312-manylinux-x86_64-wheelhouse.tar.zst";
     flake = false;
   };
   ```
   If there is no suitable artifact host, the fallback is one flake input per wheel/sdist URL.
4. Refactor `.nix/vllm-runtime.nix` to accept `vllm-wheelhouse` and unpack/copy it into a local `$wheelhouse` directory.
5. Delete the `wheelhouse = pkgs.stdenvNoCC.mkDerivation { ... pip download ... }` derivation.
6. Keep only offline installation in the runtime derivation, with network disabled by construction:
   ```bash
   python3.12 -m pip install \
     --no-index \
     --find-links "$wheelhouse" \
     --target "$out/lib/python3.12/site-packages" \
     torch==2.11.0+cu130 \
     torchvision==0.26.0+cu130 \
     torchaudio==2.11.0+cu130 \
     vllm==0.20.1
   ```
   This is allowed by the invariant because pip is not resolving/downloading; it is installing from an artifact declared in `flake.lock`.

Rejected/secondary option:

- Do not switch to nixpkgs `python312Packages.vllm` unless the user accepts a downgrade to `0.16.0` or Build carries a full nixpkgs-style update for `0.20.1` and all dependency/API changes. That is a much larger maintenance surface than the current profile-runtime goal.

Acceptance:

- No `pip download` in `.nix/vllm-runtime.nix`.
- Any `pip install` is strictly `--no-index --find-links` against flake input artifact(s).
- No live package index URLs in build phases.
- `nix build .#vllm-runtime` succeeds.
- Result/profile provides `vllm`; `vllm --version` reports `0.20.1`.

Alternative: full nixpkgs-helper/source package update

- It is possible to avoid the offline wheelhouse approach by packaging vLLM with nixpkgs Python helpers (`buildPythonPackage`, declared `dependencies`, `pythonRelaxDepsHook`/patches as needed), ideally by updating/overriding the existing nixpkgs vLLM expression.
- This would be cleaner from a Nix packaging perspective because there would be no `pip install` at all in `.nix/vllm-runtime.nix`.
- Current evidence makes this a larger/high-risk change, not the smallest safe refactor:
  - Locked nixpkgs `python312Packages.vllm` is `0.16.0`.
  - Locked nixpkgs `python312Packages.torch` is `2.12.0` and `cudaPackages.cudaMajorMinorVersion` is `12.9`.
  - The current repo runtime intentionally installs PyTorch CUDA wheel versions `torch==2.11.0+cu130`, `torchvision==0.26.0+cu130`, `torchaudio==2.11.0+cu130`, plus `vllm==0.20.1`.
  - vLLM 0.20.1 PyPI metadata pins `torch==2.11.0`, `torchaudio==2.11.0`, and `torchvision==0.26.0` and adds several newer runtime dependencies (`flashinfer-python`, `flashinfer-cubin`, `tilelang`, `apache-tvm-ffi`, `nvidia-cudnn-frontend`, etc.) that may not already exist in the locked nixpkgs Python set at compatible versions.
- Therefore, if Build chooses the nixpkgs-helper route, acceptance should include proving one of these explicitly:
  1. A nixpkgs-style vLLM 0.20.1 package builds and runs against Nix-provided Torch/CUDA, and the user accepts any CUDA/Torch version difference; or
  2. Nix expressions package the required `+cu130` PyTorch wheel stack and all vLLM dependencies with declared sources/hashes, without live pip resolution.
- Recommendation remains: use the declared wheelhouse artifact for this iteration unless the user explicitly wants the larger nixpkgs-style package update.

### 2026-08-07 Final design review of helper-only Fusion/QMD packaging

Direction is now correct and the build passes the smoke checks:

```sh
$ ./result/bin/fusion --version
0.73.0

$ ./result/bin/fn --help
fn — AI-orchestrated task board
Usage: ...

$ ./result/bin/qmd --help
qmd — Quick Markdown Search
Usage: ...
```

What the working path uses:

- `pkgs.fetchPnpmDeps` (fixed-output store) for both Fusion and QMD.
- `pnpmConfigHook` (offline install against that store).
- `pnpmBuildHook` to run the workspace build via the documented
  `pnpmBuildScript` / `pnpmWorkspaces` mechanism. The `...` workspace suffix
  (`pnpmWorkspaces = [ "@runfusion/fusion..." ];`) was the key to pull in
  Fusion's workspace dep closure without manually listing every package.
- `__structuredAttrs = true; strictDeps = true;` so the derivation's
  `pnpmInstallFlags` list survives as a real shell array in the hook.
- Documented pnpm 10 hoist flag (`--shamefully-hoist`) plus
  `--config.confirmModulesPurge=false` so the offline install works without
  TTY prompts.
- `env.FUSION_CLI_FULL_PACKAGE = "0"` to stop Fusion's tsup config from
  taking the full publish/desktop branch when `CI=true` is set for pnpm.
- Custom `installPhase` modeled after `pkgs/by-name/t3/t3code/package.nix`,
  not `npmInstallHook`. The latter is npm-workspace-oriented and fails
  with `npm pack --workspace=...` on pnpm-managed repos.
- The custom install phase is mechanical (cp, chmod, patchShebangs,
  makeWrapper) and does **not** touch `.pnpm` symlinks or other pnpm
  internals.

Notes for future work:

1. The `agent-browser` binary requires `chmod +x` on a sub-binary at runtime
   (`agent-browser-linux-x64`); this fails with `EPERM` in the Nix store.
   Upstream check: latest Fusion on npm is `0.75.1` and still depends on
   `agent-browser = 0.26.0`. Latest `agent-browser` on npm is `0.33.2`, but
   both `0.26.0` and `0.33.2` ship `bin/agent-browser-linux-x64` with mode
   `0644` and `bin/agent-browser.js` still calls `chmodSync(binaryPath,
   0o755)` with the same "Cannot make binary executable" error path. So this
   does not appear fixed upstream by a Fusion bump or an `agent-browser` bump.
   User chose to keep and patch `agent-browser`. Build should pre-chmod the
   platform binary in `.nix/fusion-runtime.nix` during `fusionCli.installPhase`,
   after copying `node_modules` / staging `@runfusion/fusion` and before
   `find "$out/lib" -xtype l -delete` / wrapper creation:
   ```sh
   find "$out/lib/node_modules" \
     -path '*/agent-browser/bin/agent-browser-linux-x64' \
     -exec chmod 755 {} +
   ```
   Then verify:
   ```sh
   nix build .#fusion-runtime --no-sandbox
   ./result/bin/fusion --version
   ./result/bin/fn --help
   ./result/bin/qmd --help
   ./result/bin/agent-browser --help
   ```
   Current acceptance already has `fusion` / `fn` / `qmd` passing; this patch
   extends acceptance to the upstream-exposed `agent-browser` wrapper.
2. `vllm-runtime.nix` still uses live `pip download`; not part of this
   refactor and is documented in `docs/wip.md` as the next item to
   tackle. The user has not asked to refactor it yet.
3. End-to-end `nix build .#default` and `nix flake check` were not
   re-run after the Fusion/QMD refactor; recommended as final commit
   verification step.

Verdict: this refactor satisfies the stricter interpretation of the
determinism invariant:

- No `npm install` / `npm rebuild` / live package-manager resolution in
  repo-maintained builds.
- All dependencies go through `fetchPnpmDeps` (lockfile-pinned, hash-locked).
- Only Nixpkgs helpers (`pnpmConfigHook`, `pnpmBuildHook`) and a small
  mechanical install phase are used.

## Final milestone status (2026-08-07)

Scope of this milestone: deterministic Fusion/QMD packaging with the
stricter helper-only Nix path. vLLM refactor is **explicitly deferred**.

What ships in this milestone:

- `.nix/fusion-runtime.nix` (renamed from `.nix/fusion-npm.nix`):
  source tags + `fetchPnpmDeps` + `pnpmConfigHook` + `pnpmBuildHook` +
  mechanical install phase. No live package-manager resolution. All
  wrappers work, including `agent-browser` (chmod patch in install).
- `.nix/opencode-with-codex-auth.nix` (renamed from
  `.nix/opencode-with-reasoning.nix`): uses `pkgs.opencode` directly.
- `AGENTS.md`: generic coupled-dependency bump policy and
  `<tool>-runtime.nix` naming policy.
- `flake.nix` / `flake.lock`: source-tag inputs for Fusion/QMD and
  updated runtime wiring.
- `.config/systemd/user/{fusion.service,vllm@.service}`: profile-runtime
  consumption (unchanged from prior work).
- `.local/bin/{vllm-config,vllm-serve-pure}`: profile-binary resolution
  (unchanged from prior work).

What is intentionally **not** fixed in this milestone:

- `.nix/vllm-runtime.nix` still uses `pip download` (line 41) and
  `pip install --no-index --find-links` (line 83). It is **not**
  deterministic per the strict invariant. vLLM is still in
  `packages.default` because the service profile architecture expects
  `nix profile upgrade klarkc` to install vLLM into
  `%h/.nix-profile/bin/vllm`. Removing it from the default profile
  would break the service startup contract.
- The deterministic vLLM refactor work-in-progress was attempted after the
  Fusion/QMD milestone, but the old stash conflicted and contained obsolete
  pre-milestone packaging work. Current `git stash list` has no
  `vllm-and-cuda` stash to apply. The next milestone should start from current
  `HEAD` and the vLLM design notes below, not from a stale stash.

Small but in-scope cleanup included in this milestone:

- `.nix/vllm-runtime.nix` was changed to a function signature
  `{ pkgs, version }:` instead of self-contained `builtins.fetchTree`
  + `builtins.currentSystem`. This removes a `builtins.fetchTree`
  violation against the locked-nixpkgs deterministic invariant and
  makes the file `callPackage`-compatible, but the pip calls remain.
- The wheelhouse fixed-output hash was updated to the current value
  after re-derivation.

Final verification (run before this commit):

```sh
$ nix flake check --no-build
✅ packages.x86_64-linux.default
✅ packages.x86_64-linux.fusion-runtime
✅ packages.x86_64-linux.vllm-runtime
✅ packages.x86_64-linux.alacritty
✅ devShells.x86_64-linux.default
✅ formatter.x86_64-linux
✅ checks.x86_64-linux.formatting
✅ checks.x86_64-linux.pre-commit-check

$ nix build .#default --no-sandbox
# builds complete profile

$ ./result/bin/fusion --version
0.73.0

$ ./result/bin/fn --help             # works
$ ./result/bin/qmd --help            # works
$ ./result/bin/agent-browser --help  # works
$ ./result/bin/opencode --version    # 1.17.13
$ ./result/bin/codex --version       # codex-cli 0.142.5
$ ./result/bin/vllm --version        # 0.20.1
```

What is **not** claimed by this milestone:

- The repo as a whole does **not** satisfy the strict no-live-pip
  invariant. vLLM is the remaining exception. Do not phrase the
  commit message in a way that claims it does.
- vLLM drift is the only remaining open invariant violation. The
  pinned wheelhouse hash was re-derived during this milestone, but
  future re-derivations may still drift because `pip download` is not
  reproducible in the strict Nix sense.

Decision update: Option B is discarded. Use the nixpkgs-style source package route, but **not** a shallow package override.

- Build should start from the locked nixpkgs `python312Packages.vllm` expression because it already encodes important vLLM build knowledge: CMake flags, vendored source replacement for CUTLASS/FlashMLA/triton kernels/qutlass, CUDA/ROCm conditionals, dependency wiring, and patch patterns.
- Build should not implement this as only:
  ```nix
  pkgs.python312Packages.vllm.overrideAttrs { version = "0.20.1"; src = ...; }
  ```
  That is too likely to produce a package that evaluates but fails during build/runtime, because vLLM 0.20.1 changed dependency pins and bundled/native components relative to nixpkgs' current 0.16.0 expression.
- Preferred implementation shape:
  1. Add a repo-local `.nix/vllm-0_20_1.nix` (or similarly named hidden Nix file) derived from the locked nixpkgs vLLM expression.
  2. Update it intentionally for vLLM `0.20.1`: source hash, dependency set, patches/substitutions, native components, and Python import/runtime checks.
  3. Wire `.nix/vllm-runtime.nix` to wrap/use that Nix-built vLLM package instead of running pip.
  4. Keep any required external runtime libraries/tools in the wrapper (`LD_LIBRARY_PATH`, `PATH`) only when runtime evidence requires them.
- Torch/CUDA decision for Build:
  - Do **not** try locked nixpkgs Torch/CUDA as a compatibility shortcut. User confirmed vLLM will not work correctly with different Torch/CUDA versions.
  - Version-selection rule: use only Torch/CUDA combinations documented as supported by the target vLLM release. Check upstream vLLM release notes/changelog and release-tag docs first. If they do not identify a more specific supported combination for the chosen variant, preserve the current versions already encoded in this repo.
  - For the current repo target, the fallback/current code versions are therefore the required CUDA wheel-stack versions unless Build finds stronger upstream release documentation for vLLM `0.20.1`:
    - `torch == 2.11.0+cu130`
    - `torchvision == 0.26.0+cu130`
    - `torchaudio == 2.11.0+cu130`
    - vLLM `0.20.1`
  - Package these artifacts as Nix packages/derivations; do not reintroduce `pip download`, `pip install`, or wheelhouse archives.
  - Upstream reference notes for Build comments/docs:
    - vLLM 0.20.1 CUDA requirements are documented in the tag at `requirements/cuda.txt`: `torch==2.11.0`, `torchaudio==2.11.0`, and `torchvision==0.26.0` with the note “These must be updated alongside torch”.
    - vLLM 0.20.1 GPU install docs (`docs/getting_started/installation/gpu.cuda.inc.md`) state that vLLM compiles many CUDA kernels and this creates binary incompatibility with other CUDA/PyTorch versions/configurations; if CUDA/PyTorch differs, vLLM must be rebuilt from source.
    - The same install docs state default vLLM binaries are CUDA 12.9, but release variants include CUDA 13.0 (`cu130`) and show installing a specific CUDA variant by selecting the matching `vllm-${version}+cu${CUDA_VERSION}-...whl` plus matching PyTorch index.
    - GitHub release metadata for `v0.20.1` documents published assets for `vllm-0.20.1+cu129-...` and default `vllm-0.20.1-...` wheels; PyTorch's CUDA index provides the required `+cu130` Torch wheel stack. For this repo, keep comments tied to the exact chosen `+cu130` artifacts/hashes.
- Acceptance for this decided route:
  - `.nix/vllm-runtime.nix` contains no `pip download` and no `pip install`.
  - `nix build .#vllm-runtime` builds a wrapper around a Nix-built Python package set.
  - `vllm --version` reports `0.20.1`.
  - At least a basic import/CLI smoke check runs during build or verification: `python -c 'import vllm'` and `vllm --version`.
  - If dependency pins are relaxed, comments in the Nix file document exactly which upstream pins were relaxed and what smoke tests justify it.

### 2026-08-07 Current Design review of Build progress

Repository evidence at review time:

- `git status --short` is clean; there are no uncommitted Build changes in this
  worktree to review.
- Latest commit is `3ae4637 feat(runtime): package Fusion via deterministic pnpm helpers`.
- `.nix/fusion-runtime.nix` is in the accepted helper-only shape:
  `fetchPnpmDeps` + `pnpmConfigHook` + `pnpmBuildHook` + mechanical copy/wrapper
  install, with pinned Fusion/QMD pnpm hashes and no direct package-manager
  resolver/install commands in repo-maintained phases.
- `.nix/vllm-runtime.nix` remains the open blocker: line 41 still runs
  `pip download`, and line 83 still runs `pip install`. This is the only
  remaining strict deterministic-build invariant violation in the runtime scope.

Verdict: Build is going in the right direction if he treats Fusion/QMD as done
and starts vLLM from current `HEAD` using the decided nixpkgs-style source
package route. Do **not** spend more time on Fusion unless a fresh smoke check
fails, and do **not** resurrect stale stash contents.

Next Build steps:

1. Inspect the locked nixpkgs `python312Packages.vllm` expression and vendor
   companion files/patches before editing; copy the relevant expression into a
   repo-local hidden Nix file only after understanding the native-source/vendor
   substitutions it performs.
2. Add a repo-local vLLM `0.20.1` package expression derived from that nixpkgs
   expression, not a shallow `overrideAttrs { version = ...; }`.
3. Package the required Torch/CUDA stack as Nix-declared artifacts/sources
   compatible with vLLM `0.20.1` (`torch==2.11.0+cu130`,
   `torchvision==0.26.0+cu130`, `torchaudio==2.11.0+cu130`) unless upstream
   release docs provide a stronger supported combination and the user accepts it.
4. Refactor `.nix/vllm-runtime.nix` into a thin wrapper around the Nix-built
   vLLM package. Acceptance remains: no `pip download`, no `pip install`,
   `nix build .#vllm-runtime`, `result/bin/vllm --version` reports `0.20.1`,
   and at least `python -c 'import vllm'` is verified by Build.
5. Keep `vllmRuntime` in `packages.default`; removing it would break the
   profile-managed service contract (`%h/.nix-profile/bin/vllm`).

### 2026-08-07 vLLM version policy update: model compatibility wins

User clarified that the existing vLLM/Torch/CUDA version pins are likely a
means to run the configured model weights, not a hard product requirement by
themselves. Therefore the vLLM decision should be reframed:

- Hard requirement: the profile-installed `vllm` must be able to serve the
  configured model targets/weights reliably.
- Soft requirement: preserve `vLLM 0.20.1 + torch 2.11.0 + CUDA 13.0` only if
  that remains the smallest proven way to run those model targets.
- Acceptable alternative: bump nixpkgs and use a newer nixpkgs-provided vLLM if
  Build proves it can run the proposed model configs/weights, or updates the
  configs to newer compatible model/model-version weights with user acceptance.

Current model targets to validate:

- `.config/vllm/qwen3.6-35B-a3b.env`
  - `MODEL=Intel/Qwen3.6-35B-A3B-int4-mixed-AutoRound`
  - `MODEL_REVISION=65f69c73f17488236c85c85211f6ba28d7106157`
  - `SERVED_MODEL_NAME=qwen3.6-35b-a3b`
  - important launch features: `--trust-remote-code`, `--dtype half`,
    `--max-model-len 49152`, chunked prefill, prefix caching, async scheduling,
    language-model-only, Qwen3 reasoning/parser flags, auto tool choice, MTP
    speculative config, `--default-chat-template-kwargs {preserve_thinking:true}`.
- `.config/vllm/qwen3.6-27B.env`
  - `MODEL=Intel/Qwen3.6-27B-int4-AutoRound`
  - `MODEL_REVISION=` currently unpinned; Build should either pin a tested
    revision or document why tracking latest is intentional.
  - `SERVED_MODEL_NAME=qwen3.6-27b`
  - same important launch features as above.
  - User clarified this target is broken with the current GPU/specs. Treat it
    as a candidate/proposed target, not mandatory acceptance for preserving the
    current runtime. A nixpkgs/vLLM bump may replace or repair this target, but
    Build should not block acceptance of the known-working 35B path solely on
    the current 27B config failing.

Updated preferred Build evaluation order:

1. First evaluate a nixpkgs bump in isolation because it may replace the
   repo-local pip/wheel runtime with a maintained Nix package and reduce local
   maintenance. Keep `nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable"`;
   determinism comes from `flake.lock`.
2. If newer nixpkgs provides an acceptable `python312Packages.vllm`, refactor
   `.nix/vllm-runtime.nix` into a thin profile-wrapper around that package.
   Preserve the service contract (`%h/.nix-profile/bin/vllm`) and any runtime
   `LD_LIBRARY_PATH`/driver-library setup needed by `.local/bin/vllm-serve-pure`.
3. Validate both API/CLI compatibility and model compatibility before accepting
   the bump:
   - `nix flake check --no-build`
   - `nix build .#vllm-runtime --no-sandbox`
   - `result/bin/vllm --version`
   - `result/bin/vllm serve --help` contains/accepts all flags emitted by
     `.local/bin/vllm-serve-pure`.
   - Real GPU smoke for the known-working/primary target first: start the 35B
     service/command with the env file, wait for `/v1/models`, verify
     `qwen3.6-35b-a3b`, and run at least one short chat/completions request that
     exercises the Qwen3 chat template/reasoning path. For 27B, either prove a
     repaired/replacement model config works or document that it remains broken
     on the current GPU/specs and keep it out of mandatory acceptance.
   - Then run the maintained benchmark wrapper against the active target:
     `vllm-benchmark --check` first, followed by `vllm-benchmark` for the full
     small/medium/long-context benchmark matrix. The service writes
     `benchmark.env` under `~/.cache/vllm-<instance>/`, and benchmark artifacts
     are archived under `~/.cache/vllm-benchmarks/`.
4. If a newer vLLM needs newer/different weights, Build may propose updated
   `MODEL`/`MODEL_REVISION` values, but must document the compatibility reason
   and require user acceptance before replacing the configured models.
5. If nixpkgs' newer vLLM cannot serve the configured/proposed models, fall back
   to the local nixpkgs-style source package route described above. The fallback
   still must remove `pip download`/`pip install` from `.nix/vllm-runtime.nix`.

This supersedes the earlier stronger preference to preserve vLLM `0.20.1`.
The invariant remains unchanged: no live `pip download`/`pip install` in
repo-maintained Nix build code.

### 2026-08-07 CUDA runtime/linking decision

User agreed with the Nixpkgs-canonical CUDA direction:

- If using nixpkgs-provided vLLM, first try it without carrying over the old
  wheel-runtime `LD_LIBRARY_PATH` customization from `.nix/vllm-runtime.nix` and
  `.local/bin/vllm-serve-pure`.
- The old customization exists because the current runtime is pip/wheel-based
  and manually assembles `site-packages`; it scans `*.libs`, `torch/lib`, and
  `site-packages/nvidia/**/lib` to compensate for missing Nix RPATH/runpath
  metadata. That is a workaround, not the canonical Nix CUDA path.
- Nixpkgs CUDA docs and the nixpkgs vLLM expression point to the canonical path:
  import/use CUDA-enabled nixpkgs (`cudaSupport`, appropriate unfree allowance,
  and GPU capabilities as needed), depend on `cudaPackages`, and let setup hooks
  such as `autoAddDriverRunpath` patch runtime discovery.
- Therefore Build should start minimal:
  1. bump/evaluate nixpkgs;
  2. ensure the selected nixpkgs package set is CUDA-enabled, not accidentally
     CPU-only;
  3. make `.nix/vllm-runtime.nix` a thin adapter around nixpkgs vLLM;
  4. remove the wheelhouse and all `pip` commands;
  5. do not add custom `LD_LIBRARY_PATH` unless real GPU smoke fails with a
     specific missing-library error.
- If a failure mentions `libcuda.so.1` or another NVIDIA/CUDA shared object, add
  only the smallest targeted runtime fix and document the exact error that
  justified it. Do not preserve the broad wheel-runtime scanning by inertia.

Build follow-up required: update `AGENTS.md` with durable generic runtime-linker
guidance plus CUDA-specific Nix guidance so future agents do not reintroduce
broad runtime-linker workarounds by default. Suggested wording:

```md
- Runtime linker path policy: prefer proper Nix packaging with declared
  dependencies, normal fixup/RPATH/RUNPATH handling, and package-specific setup
  hooks over broad `LD_LIBRARY_PATH` wrappers. Add `LD_LIBRARY_PATH` or similar
  runtime library path customization only after a real runtime smoke test fails
  with a specific missing-library/dlopen error that normal Nix linking cannot
  address. Keep the workaround minimal, document the exact error and smoke test
  next to the fix, and do not preserve package-manager/binary-runtime library
  scans by inertia when moving a package to proper Nix packaging.
- CUDA packages: prefer Nixpkgs' CUDA-enabled package variants and CUDA setup
  hooks (`cudaSupport`, appropriate unfree allowance, GPU capabilities,
  `cudaPackages`, `autoAddDriverRunpath`) before adding runtime linker-path
  workarounds. If `libcuda.so.1` or another driver/runtime library is missing at
  runtime, document the exact hardware smoke failure and add only the minimal
  targeted fix.
```

### 2026-08-11 vLLM source-packaging decision update

User discarded the declared-wheel-artifact bridge as too cumbersome and not a
real solution. Do not pursue a repo-maintained wheel list / offline wheel
unpacking path unless the user explicitly reopens it.

Design checked nixpkgs vLLM history via the GitHub commits API for
`pkgs/development/python-modules/vllm/default.nix`; upstream nixpkgs appears to
move vLLM directly from `0.16.0` to `0.24.0` in commit
`924212e109e218908b73f05c6ecb6b4c3edd7fa3` (`vllm: 0.16.0 -> 0.24.0`).
Therefore there is currently no evidence of an official older/intermediate
nixpkgs rev that provides `python312Packages.vllm = 0.20.1`, let alone the exact
`0.20.1 + torch 2.11.0 + CUDA 13.0` stack.

Updated C2 interpretation:

- C2 still means eliminating the current wheelhouse and building vLLM from
  source using Nix-native package expressions, CUDA setup hooks, and declared
  source dependencies.
- Do not expect to find an older official nixpkgs rev that already packages
  `vLLM 0.20.1 + cu130`. If Build wants that exact runtime, assume a repo-local
  source package derived from nixpkgs' vLLM expression will be needed.
- Before implementing that local package, Build should perform a short research
  pass for an upstream nixpkgs PR/ref that packaged vLLM `0.20.1` or the exact
  Torch/CUDA pairing. If none exists, choose between:
  1. local vLLM `0.20.1` source package against a nixpkgs-provided Torch/CUDA
     stack, accepting that it may not match the current wheel runtime exactly;
  2. local/forked ML stack including Torch/CUDA 13.0, accepting much higher cost;
  3. deferring until nixpkgs-unstable catches up with a cacheable vLLM that
     passes the 35B A3B benchmark.
- The failed `nixpkgs-vllm = master` experiment remains evidence that raw master
  can expose `vllm 0.24.0` but may require chasing unrelated upstream Python test
  failures before any GPU/model proof is possible.

User clarified the C2b target does not need to be strictly equal to the old
wheel runtime versions; equal or newer is acceptable if the result is source
packaged and can be verified against the 35B A3B model. Optimistic C2b target:

- Reuse the official nixpkgs vLLM source-build expression around commit
  `924212e109e218908b73f05c6ecb6b4c3edd7fa3` (`vllm: 0.16.0 -> 0.24.0`) rather
  than tracking raw moving `master` casually. Per flake input URL policy, do not
  hard-code that commit in `flake.nix` just for determinism; keep the exact rev
  in `flake.lock`. Add a nearby bump note documenting when to update
  `nixpkgs-vllm`: update when nixpkgs-unstable (preferred) or an upstream
  nixpkgs PR/ref has evidence that the selected vLLM/CUDA/Python stack builds,
  or when CUDA 13 support reaches nixpkgs with no known flake/evaluation/test
  blockers for the vLLM closure. Each update must include `nix flake check
  --no-build`, `nix build .#vllm-runtime`, CLI smoke, and 35B A3B benchmark
  evidence.
- Evidence from evaluating that commit with CUDA enabled:
  - `python312Packages.vllm.version = "0.24.0"`
  - `python312Packages.torch.version = "2.12.0"`
  - `cudaPackages.cudaMajorMinorVersion = "12.9"`
  - `mcp = 1.27.1`, `scipy = 1.18.0`, `interegular = 0.3.3`,
    `lm-format-enforcer = 0.11.3`.
- Important correction: because the current wheel runtime uses CUDA `13.0`
  (`cu130`) and user clarified equal-or-newer versions are acceptable, the
  evaluated CUDA `12.9` stack is **not** an acceptable final runtime target.
  Treat PR `#498040` / commit `924212e...` as a reusable vLLM source-packaging
  example only. Build must still select or adapt a package set with CUDA
  `>= 13.0` before accepting the runtime.
- Why this is a reusable C2b example:
  - source-builds vLLM using `buildPythonPackage.override { stdenv = torch.stdenv; }`;
  - uses CUDA setup hooks (`cuda_nvcc`, `autoAddDriverRunpath`) and Nix CUDA
    packages (`cuda_cudart`, `cccl`, `libcurand`, `libcusparse`, `libcusolver`,
    `cuda_nvtx`, `cuda_nvrtc`, `libcublas`, `nccl`, `cudnn`, `libcufile`);
  - declares upstream submodule/vendor sources explicitly with hashes: CUTLASS
    `v4.4.2`, FlashMLA, DeepGEMM, FMHA/SM100 MSA, triton-kernels `v3.6.0`,
    QUTLASS, and `vllm-flash-attn 2.7.2.post1` plus Hopper build patches;
  - relaxes/removes upstream Python deps in the package expression instead of
    running `pip download`/`pip install`.
- Build should prefer a lockfile rev at or near this upstream package-expression
  baseline over current moving `master`; the `flake.nix` URL may still point to
  a branch/ref, but the lock should be moved deliberately and reviewed before
  every bump.
- GitHub evidence found for the nixpkgs-based C2b direction:
  - NixOS/nixpkgs PR `#498040` (`vllm: 0.16.0 -> 0.24.0`) merged as commit
    `924212e109e218908b73f05c6ecb6b4c3edd7fa3`. Its PR body marks
    `x86_64-linux` built, but says the author only tested ROCm initially.
  - The same PR contains CUDA-relevant user evidence: a contributor reported
    getting CUDA to run, later reported "Still works fine after rebuild", and
    later attached a CUDA build fix patch. The PR thread also says `vllm` and
    `pkgsRocm.vllm` built in nixpkgs-review at commit
    `998b78d15a78be2a8675fb97b0db80d4bc2186c6`, while `python313Packages.vllm`
    still failed. Later nixpkgs-review at commit
    `293b0b205eea70eaee8c988fd6b8ff72a593b91c` reported `vllm` and
    `python313Packages.vllm` built, but the thread still noted CUDA dependency
    issues. Treat this as partial evidence, not a guarantee.
  - PR `#549327` (`vllm: 0.24.0 -> 0.26.0`) is open/draft. Comments state
    `tokenspeed-triton` was failing and that `flashinfer` update PR `#526691`
    is required. One contributor said the 0.26 CUDA build "compiles fine for me
    with CUDA enabled besides" missing `tml-fa4`, `pynvvideocodec`, and `nvtx`
    fixes. This is useful future evidence but not a stable base now.
  - Earlier torch ecosystem PR `#377785` (`torch 2.5.1 -> 2.6.0`) includes a
    matrix where `vllm (CUDA)` was marked building, but that stack is older than
    the desired vLLM 0.24 direction and not sufficient by itself.
- Interpretation: the known flaky tests and failures are largely dependency
  churn exposed by the vLLM jump/CUDA path, not vLLM tests alone. Raw master made
  this worse by pulling later unrelated dependency versions (for example
  `mcp 1.29.0` vs `mcp 1.27.1` near the vLLM 0.24 merge). Prefer a lock near the
  merged `#498040` baseline as packaging reference or wait for nixpkgs-unstable
  to contain a CUDA `>= 13.0` vLLM stack, rather than using latest master. Do not
  accept a CUDA `12.9` runtime as satisfying the equal-or-newer policy.
- Start with no broad global `doCheck = false`. If build evidence reproduces the
  known flaky failures, add only narrow overlays in the isolated vLLM package set
  and document the exact failures:
  - `interegular 0.3.3` `test_slow_example` wall-clock assertion `<1s` failed
    under load at about 1.1s/33s in prior attempts;
  - `scipy 1.18.0` `test_support_moments_sample` failed on a tiny FP/fuzz diff
    in prior attempts.
- Acceptance does not change: successful build, `vllm --version`,
  `vllm serve --help`, then real 35B A3B `vllm-benchmark --check` and
  `vllm-benchmark`.

### 2026-08-11 Design decision after CUDA 13 feasibility pass

Build confirmed that the nixpkgs vLLM 0.24 package-expression baseline can be
evaluated with CUDA 13 packages: overriding vLLM with `cudaPackages_13_0`
produces a different vLLM derivation whose tree references CUDA 13.0 libraries
such as `cuda_cudart-13.0.96`, `cuda_nvcc-13.0.88`, `nccl-2.30.7-1`, and
`libcublas-13.1.1.3`. This is useful, but it is not final proof.

Design strategy:

- Proceed with a bounded optimistic C2b spike, not a broad master chase and not
  a passive wait. Use the official nixpkgs vLLM 0.24 source packaging from PR
  `#498040` as the expression baseline, but require CUDA `>= 13.0` for the
  actual runtime.
- Do not override only vLLM's direct `cudaPackages` and assume success. The ML
  stack must be internally coherent: Torch, torchvision, torchaudio, vLLM, and
  CUDA-sensitive companion packages must consume the same CUDA 13 package set
  where applicable. Before long builds, Build must inspect/evaluate derivation
  references and confirm no unintended CUDA 12.9 libraries remain in the vLLM
  runtime closure.
- Keep main `nixpkgs` on `nixpkgs-unstable`; if split input is still needed, keep
  the URL branch/ref-style in `flake.nix` and move only `flake.lock` to the
  selected reviewed rev/ref. The split input can be collapsed once main
  `nixpkgs-unstable` provides a CUDA `>= 13.0` vLLM stack that passes acceptance.
- Use staged verification before the full expensive build:
  1. Evaluate/report versions: vLLM, Torch, CUDA major/minor, and key companion
     packages.
  2. Evaluate/report derivation paths for Torch and vLLM with the CUDA 13
     override.
  3. Inspect derivation/reference trees for CUDA 13 vs CUDA 12 leakage.
  4. Build likely flaky transitive test packages in isolation first if they are
     in the closure; this avoids parallel-load timing failures being rediscovered
     late in the full vLLM build.
  5. Run the full `nix build .#vllm-runtime --no-sandbox` only after the closure
     looks coherent.
- Start with no broad/global `doCheck = false`. If failures reproduce, add only
  narrow, isolated overlays with exact log evidence. If the build requires more
  than a small number of unrelated test overrides, stop and ask Design/user
  rather than accumulating overlay debt.
- Success criteria remain: no wheelhouse, no `pip download`, no `pip install`,
  profile-installed `%h/.nix-profile/bin/vllm`, CLI smoke, and 35B A3B benchmark
  proof. If CUDA 13 source packaging cannot be made coherent or buildable with
  small targeted fixes, keep the current wheelhouse temporarily and mark C2b
  blocked pending nixpkgs-unstable catching up.

### 2026-08-11 Design intervention after Build CUDA 13 closure check

Build tested the staged coherence gate before implementing code, which was the
right move. The result blocks the cheap C2b path:

- `python312Packages.vllm.override { cudaPackages = pkgs.cudaPackages_13_0; }`
  at the vLLM 0.24 nixpkgs baseline makes vLLM's direct derivation references
  use CUDA 13.0 packages.
- However, the transitive derivation tree still contains CUDA 12.9 packages at
  runtime-relevant depths (`cuda12.9-cuda_cudart`, `cuda12.9-cuda_nvcc`,
  `cuda12.9-libcublas`, `cuda12.9-nccl`, `cuda12.9-cudnn`, etc.).
- Therefore the simple vLLM-level override does **not** satisfy the coherent
  CUDA `>= 13.0` requirement. Do not spend a long build on that expression and
  do not accept it as a runtime candidate.

Design decision:

- Stop the current C2b implementation spike. The remaining C2b path is no
  longer a thin adapter plus one override; it is a broader ML-stack overlay or
  local package-set fork that makes Torch, torchvision, torchaudio, vLLM, cupy,
  xformers, flashinfer, and CUDA-sensitive companions all consume the same CUDA
  13 package set.
- That broader overlay is large enough to require a separate explicit user
  decision and should not be started opportunistically in this milestone.
- Keep the current wheelhouse vLLM runtime temporarily, with the known invariant
  violation documented, until either:
  1. nixpkgs-unstable provides a coherent CUDA `>= 13.0` vLLM stack that passes
     the 35B A3B benchmark; or
  2. the user explicitly approves a larger local ML-stack overlay/fork effort.
- Build should not modify `.nix/vllm-runtime.nix`, `flake.nix`, or `flake.lock`
  for vLLM right now. The only safe next implementation work is committing the
  policy/design documentation if the user wants the decision recorded.

### 2026-08-11 Search for existing CUDA 13 ML-stack overlay/fork

User asked to verify whether an existing GitHub overlay/fork already provides
the coherent CUDA 13 vLLM/Torch stack before giving up on C2b.

Search performed:

- GitHub issue/PR API queries for combinations of `cudaPackages_13_0`,
  `cudaPackages_13`, `python312Packages.vllm`, `vllm`, and `torch`.
- GitHub repository search queries for `vllm nix cuda`, `nixpkgs vllm cuda`, and
  `torch cuda13 nix`.
- Public GitHub code search pages for `cudaPackages_13_0 vllm`,
  `cudaPackages_13 vllm torch`, and `python312Packages.vllm cudaPackages` were
  inaccessible without GitHub sign-in, so this is not a mathematically exhaustive
  code search.

Result:

- No public reusable overlay/fork was found that demonstrates a coherent
  `vLLM >= 0.20.1` + `Torch >= 2.11` + CUDA `>= 13.0` nixpkgs-based stack.
- The only `cudaPackages_13_0` + vLLM issue result was NixOS/nixpkgs PR
  `#515928` (`release-cuda: add cudaCapabilities parameter`), which is an
  eval/Hydra release job parameter change and not a vLLM/Torch overlay.
- Repository searches returned no obvious candidate repos for a vLLM/CUDA13 Nix
  overlay.

Additional local nixpkgs evidence:

- Nixpkgs' Python package set has useful internal propagation points:
  `torchvision`, `torchaudio`, `xformers`, and some other Python CUDA packages
  inherit CUDA settings from `torch`; `torch` has `_tritonEffective` and comments
  explicitly warning that overlays/nixpkgsFun are preferred over ad-hoc
  attributes such as `torchWithCuda` because those can consume wrong arguments.
- A tested `overrideScope` shape that overrides `torch`, `triton`,
  `triton-cuda`, `cupy`, `flashinfer`, `accelerate`, and `vllm` to CUDA 13 makes
  vLLM's direct references CUDA 13.0, but the derivation tree still contains CUDA
  12.9 via deeper dependencies/check paths such as `accelerate`/`triton`/
  `onnxruntime` style dependencies.
- Some of that CUDA 12.9 evidence is in derivation/build-time trees, not yet a
  proven built-output runtime closure. However, it is enough evidence that a
  complete C2b overlay cannot be assumed from a simple override; it still needs a
  dedicated staged implementation and closure audit.

Design implication:

- There is no found off-the-shelf overlay/fork to reuse today.
- C2b is not hopeless: nixpkgs' package structure suggests a coherent overlay
  may be smaller than a full local fork if implemented carefully through
  `python312Packages.overrideScope` and by following Torch's CUDA/Triton
  propagation points.
- But it remains a separate explicit implementation effort. Do not restart it in
  the current milestone unless the user approves that larger overlay spike with
  enough time/disk budget for iterative builds and runtime proof.
- 2026-08-11 attempted spike: a scoped `python312Packages.overrideScope` was
  wired in `flake.nix` that overrides `triton[-cuda]`, `torch`, `cupy`,
  `flashinfer`, `accelerate`, and `vllm` to use `cudaPackages_13_0`. Direct
  derivation references for vLLM 0.24.0 then resolved to CUDA 13 packages
  (cuda_cudart-13.0.96, cuda_nvcc-13.0.88, libcublas-13.1.1.3, cudnn-9.22.0.52,
  etc.). A narrow evidence-backed `doCheck = false` overlay was added only for
  `interegular` to bypass the wall-clock `test_slow_example` flake seen under
  heavy parallel builds. No broad `doCheck = false` was introduced.
- Build attempts in this worktree were repeatedly interrupted by tool timeouts
  and the detached-builds caused severe system load (multiple parallel `nvcc`
  invocations per derivation), so the full `nix build .#vllm-runtime` was not
  completed here. The code/overlay changes are preserved in the working tree
  for the user to resume in a session with more time, and the docs here are
  the durable handoff. Acceptance remains: `nix build .#vllm-runtime`,
  `result/bin/vllm --version`, `vllm serve --help`, then real 35B A3B
  `vllm-benchmark --check` and `vllm-benchmark`.
- 2026-08-11 inner-parallelism investigation (no code change): user observed a
  `make -j16` inside the build chain. Root cause analysis from the locked
  `nixpkgs-vllm` source (`/nix/store/fgn9...-source`):
  - `nix build -j 1` only throttles how many derivations Nix evaluates in
    parallel. It does **not** prevent an individual derivation from spawning
    many `g++`/`nvcc` jobs via its own `make -jN`.
  - The screenshot showed `samples/cpp/CMakeFiles/samples.dir/build` and
    `test/cpp/CMakeFiles/tests.dir/build` driving `g++ -fPIC -c` jobs in parallel.
    That matches `pkgs/development/cuda-modules/packages/cudnn-frontend/package.nix`,
    which has `enableParallelBuilding = true` and builds `samples`, `legacy_samples`,
    and `tests` outputs.
  - For our vLLM runtime we do not need those samples/tests: both vLLM
    (`pythonRemoveDeps = [ ... "nvidia-cudnn-frontend" ... ]`) and `flashinfer`
    (same removal) drop the `nvidia-cudnn-frontend` Python wrapper, and vLLM only
    needs the header-only C++ library. Transformer-engine consumes
    `cudaPackages.cudnn-frontend`'s `include/`, not its samples/tests.
  - Two narrow fixes worth trying before restarting the build:
    1. Override `cudaPackages.cudnn-frontend` to disable samples/tests:
       ```nix
       cudaPackages = (prev.cudaPackages_13_0.overrideScope (final': prev':
         prev'.cudnn-frontend.override { withSamples = false; withTests = false; }
       ));
       ```
       This skips ~150+ compile units that account for most of the `make -j16`
       burst. Same shape can be applied to `cudaPackages.nccl` (also
       `enableParallelBuilding = true`) via `makeFlags = [ "-j1" ];` if a
       per-derivation override is needed.
    2. Force single-job inner parallelism in addition to `NIX_BUILD_CORES=1`:
       ```sh
       export NIX_BUILD_CORES=1
       export MAKEFLAGS="-j1"
       export CMAKE_BUILD_PARALLEL_LEVEL=1
       export NINJAFLAGS="-j1"
       nix build .#vllm-runtime --no-sandbox -j 1 --cores 1
       ```
       The `MAKEFLAGS`/`CMAKE_BUILD_PARALLEL_LEVEL` env vars do reach most
       generic `make`/`cmake` builds, but are not guaranteed for every
       derivation (CMake-driven projects often read their own parallel level
       from `ProcessorCount()` and ignore env). Pairing (1) + (2) gives the
       strongest mitigation available without forking upstream expressions.
  - Per-derivation overrides are still preferred over a global disable because
    `AGENTS.md` runtime-linker policy says we add `doCheck = false` only with
    explicit evidence and never as a blanket. The narrow `interegular` overlay
    remains the only `doCheck = false` change in the current vllm overlay.
   - When the build is resumed, recommended command shape:
     ```sh
     NIX_BUILD_CORES=1 MAKEFLAGS="-j1" CMAKE_BUILD_PARALLEL_LEVEL=1 \
       nix build .#vllm-runtime --no-sandbox -j 1 --cores 1
     ```
     plus the `cudaPackages.cudnn-frontend` override in `flake.nix`'s
     `vllmPkgs` import to actually remove the `samples`/`tests` build.

### 2026-08-11 ===> 2026-08-19 Runtime rollback + wheelhouse bump

The C2B attempt (nixpkgs-vllm split input + CUDA 13 overlay) was abandoned
on 2026-08-19 after multiple reproducer OOMs on the 27 GB host. The
implementation was reverted and the runtime was bumped within the wheelhouse
approach instead. The new runtime is:
- `.nix/vllm-runtime.nix`: thin symlinkJoin over `pkgs.python312Packages.vllm`
  was reverted to the declared wheelhouse derivation. Versions bumped from
  `vllm==0.20.1` + `torch==2.11.0+cu130` to `vllm==0.24.0` +
  `torch==2.12.0+cu130` (latest cu130 wheels available for the vLLM 0.24.x
  line). The `outputHash` is a placeholder and must be recomputed on first
  build — the failure message will print the actual sha256.
- `flake.nix`: reverted to the wheelhouse state (`vllmRuntime` from main
  `pkgs.callPackage ./.nix/vllm-runtime.nix`, `version = "0.24.0-cu130"`).
  The `nixpkgs-vllm` split input is removed from `flake.nix` and `flake.lock`.

The runtime still violates the deterministic-build invariant
(`AGENTS.md` repo-maintained Nix-build determinism invariant section):

> any external dependency used by Nix build code maintained in this repo must
> be declared through `flake.nix` inputs and locked in `flake.lock`. Do not
> add `builtins.fetchTree`, `builtins.fetchTarball`, `builtins.fetchurl`,
> `import <nixpkgs>`, live `npm install`, live `pip download`, live
> `bun install`, live `pnpm/yarn install`, `curl`, `wget`, or `git clone`
> inside repo-maintained Nix derivations/scripts unless the user explicitly
> approves an exception.

The wheelhouse runs `pip download` and `pip install` against
`https://download.pytorch.org/whl/cu130` from inside a Nix derivation. This
is the explicit, user-approved exception. The outputHash + recursive pinning
keeps the build reproducible once the hash is set on a working host.

#### When can this exception be removed?

The exception is revisable when **any one of the following** is true.
Each path is documented with the eval/build evidence needed to retire the
wheelhouse runtime.

1. **vLLM from nixpkgs-unstable (CUDA 12.9, no cu130)** is the
   lightest path. The locked `nixpkgs-unstable` (`f205b5574…`) already
   exposes `python312Packages.vllm = 0.16.0` as a CUDA-enabled source
   derivation with `cuda_nvcc`, `auto-add-driver-runpath`, `cudnn`, `nccl`,
   `libcublas`, `libcufile`. Caveats:
   - The expression uses `cudaPackages` (CUDA 12.9) by default — no cu130
     override needed since 12.9 >= 12.0 and the locked CUDA is supported.
   - vLLM 0.16.0 is older than vLLM 0.24.0. The 35B A3B AutoRound model
     hasn't been served against vLLM 0.16.0 — runtime acceptance must
     re-run `vllm-benchmark --check` and `vllm-benchmark` against the
     35B A3B target (`qwen3.6-35B-a3b`) before retiring the wheelhouse.
   - Required user action: change the `vllmRuntime` wiring in `flake.nix`
     from `pkgs.callPackage ./.nix/vllm-runtime.nix` to a thin symlinkJoin
     adapter over `pkgs.python312Packages.vllm`, and delete `.nix/vllm-runtime.nix`.

2. **rLLM (`ghyathmoussa/rLLM`)** is a younger Rust single-binary
   inference engine. Lower maintenance but lossy on our target:
   - Does NOT support AutoRound (the 35B A3B model is AutoRound-quantized).
   - Does NOT support MTP (Multi-Token Prediction, required for MoE
     offloading on Qwen3.x).
   - No public evidence rLLM serves `Qwen3.6-35B-A3B` correctly.
   - 23 stars, single maintainer, 3-month-old — not production-ready.
   - Required user action: convert the 35B A3B AutoRound weights to
     AWQ/GPTQ/GGUF, then evaluate `rllm` against the converted weights.
     Likely not viable for the current 35B A3B target.

3. **mistral.rs (`EricLBuehler/mistral.rs`)** is more mature than rLLM
   (7.6k stars, 2.5 years, MIT, in `nixpkgs` master as `mistral-rs 0.9.1`)
   and supports Qwen3 + MTP. Status against our target:
   - **Broken on our model**: upstream issue
     [`EricLBuehler/mistral.rs#2378`](https://github.com/EricLBuehler/mistral.rs/issues/2378)
     reproduces a `moe experts forward / PendingIsqLayer is in an invalid
     transitional state` error on `Qwen/Qwen3.6-35B-A3B` (the exact model
     we serve). Mistral.rs 0.9.0 cannot run inference on it.
   - Related work in PR
     [`#2380`](https://github.com/EricLBuehler/mistral.rs/pull/2380)
     also shows mistral.rs is still finding 200x MoE decode regressions on
     the same model family.
   - Required user action: wait for `[mistral.rs#2378]` to be resolved
     upstream, then re-evaluate. Until then, mistral.rs is **not** a viable
     replacement for the 35B A3B target.

4. **sglang in nixpkgs** would be the strongest long-term option
   (Qwen3-native MTP, deep MoE offloading, official Qwen3.6 target). But
   `python3Packages.sglang` is **not yet in nixpkgs** — PR
   [`NixOS/nixpkgs#525141`](https://github.com/NixOS/nixpkgs/pull/525141)
   has been open with merge conflicts since 2026-05-28 and depends on 15+
   unmerged companion PRs. Estimated timeline: multiple months at nixpkgs
   review cadence.
   - Required user action: monitor upstream PR or vendor a local fork.
     Removing the wheelhouse exception via sglang is **not** a small task.

5. **vLLM 0.24.x via wheelhouse** (the current path) is the most recent
   stable outcome. The wheelhouse is the closest we have to a deterministic
   runtime without forking the entire CUDA/Torch/vLLM stack. It is
   appropriate as long as no in-nixpkgs alternative can serve the 35B A3B
   target. The version is always bumpable by editing `.nix/vllm-runtime.nix`
   (which is what was done 2026-08-19: 0.20.1 → 0.24.0).

#### Recommendation

In the current memory-tight host, the wheelhouse is the only viable runtime
for the 35B A3B target. Re-evaluate options 1 and 3 in the next 30-60 days:
- Option 1 (nixpkgs-unstable vLLM 0.16.0) is the cheapest to try — just needs
  a thin adapter + a 30 min benchmark on the 35B A3B target.
- Option 3 (mistral.rs) depends on upstream issue resolution.
- Option 4 (sglang) is the strategic long-term direction but multi-month.

Until one of these resolves, the wheelhouse exception is mandatory.

## Upgrade notes for future Fusion/vLLM bumps

Keep these notes near any eventual code comments in the Nix files. They are meant to prevent partial bumps where the visible package version changes but coupled runtime/dependency inputs stay stale.

### Fusion bump checklist

When bumping Fusion:

1. Update all Fusion version sources together:
   - `flake.nix` `fusionRuntime` version.
   - future `fusion-src` input tag/ref, once the pnpm-helper refactor is implemented.
   - any version assertion in `.nix/fusion-npm.nix` / successor files.
2. Inspect upstream `Runfusion/Fusion` at the target tag:
   - root `package.json` `packageManager` field; if it changes from the current pnpm major, reassess `pkgs.pnpm_10`.
   - root `pnpm-lock.yaml`; refresh `pnpmDeps` hashes generated by `pkgs.pnpm_10.fetchDeps`.
   - `packages/cli/package.json` `bin` entries; ensure wrappers still expose `fusion`, `fn`, and `agent-browser` if upstream still ships them.
   - `packages/cli/package.json`, `packages/core/package.json`, `packages/engine/package.json`, and `packages/dashboard/package.json` for direct dependencies that require native builds or runtime tools.
3. Do not add duplicate top-level npm packages for upstream-owned deps:
   - `node-pty` and `dockerode` are Fusion-owned npm deps at 0.73.0; future versions should come from upstream manifests/lockfile unless upstream removes them and runtime evidence says this repo must provide something else.
   - `send` should remain transitive-only unless a concrete upstream import/runtime failure justifies promoting it.
4. Re-check external runtime tools separately from npm deps:
   - Nix `runtimePath` entries such as `docker-client`, `tmux`, `git`, `gh`, `openssh`, `python3`, and `uv` are wrapper/runtime tools, not npm dependencies.
   - On a Fusion bump, scan upstream docs/release notes for new external CLIs/services and update `runtimePath` only when Fusion actually invokes or documents them.
5. QMD coupling:
   - Fusion 0.73.0 uses the `qmd` memory backend at runtime but does not appear to declare `@tobilu/qmd` as an npm dependency. This repo currently packages QMD explicitly.
   - When bumping Fusion, inspect upstream memory/backend docs and code for QMD CLI assumptions. Bump `qmd-src` / `@tobilu/qmd` only if Fusion docs/code require a newer QMD CLI or the current `qmd --help`/memory smoke checks fail.
   - If QMD is bumped, update its source tag/ref and pnpm dependency hash together.
6. Verification after Fusion/QMD bump:
   - `nix build .#fusion-runtime`
   - `result/bin/fusion --version` equals target Fusion version.
   - `result/bin/fn --help` works.
   - `result/bin/qmd --help` works.
   - If possible, run a lightweight dashboard start/help smoke without requiring network or credentials.

Dependency monitor note: `.local/bin/deps-check` currently checks GitHub flake inputs in `flake.lock`. After Fusion/QMD become flake inputs, it can flag source ref drift. It will not automatically understand coupled npm package-manager versions, pnpm lock hash changes, or QMD compatibility; use the checklist above for those.

### vLLM bump checklist

When bumping vLLM:

1. Update all vLLM version sources together:
   - `flake.nix` `vllmRuntime` version label (include CUDA variant in the label, e.g. `0.20.1-cu130`).
   - repo-local vLLM package file (`.nix/vllm-0_20_1.nix` or successor) `version`, source tag/hash, patches, CMake/native component pins, and import checks.
   - wrapper/version assertions in `.nix/vllm-runtime.nix`.
2. Before choosing Torch/CUDA versions, inspect upstream vLLM target-tag documentation:
   - `requirements/cuda.txt` for exact `torch`, `torchaudio`, `torchvision`, and CUDA-side Python dependency pins.
   - `docs/getting_started/installation/gpu.cuda.inc.md` for CUDA binary compatibility notes and supported wheel variants.
   - GitHub release assets for the target tag to confirm which CUDA variants upstream publishes.
   - public release notes/changelog if available; `RELEASE.md` explains release process, while per-release details may be on GitHub Releases / `vllm.ai/releases`.
3. Version-selection rule:
   - Use only a Torch/CUDA combination documented/supported by that vLLM release.
   - Do not substitute locked nixpkgs Torch/CUDA just because it evaluates.
   - If upstream docs do not identify a more specific supported combo for the chosen variant, keep the versions already encoded in this repo until there is stronger evidence.
4. For the current vLLM target, the current/fallback coupled set is:
   - `vllm == 0.24.0`
   - `torch == 2.12.0+cu130`
   - `torchvision == 0.27.0+cu130`
   - `torchaudio == 2.12.0+cu130`
   - PyTorch CUDA index variant: `cu130`
   - Older fallback (was the runtime before 2026-08-19 bump):
     - `vllm == 0.20.1`
     - `torch == 2.11.0+cu130`
     - `torchvision == 0.26.0+cu130`
     - `torchaudio == 2.11.0+cu130`
   - The `outputHash` in `.nix/vllm-runtime.nix` must be recomputed on first
     build after any version bump — the failure message will print the
     actual sha256.
5. Native/component coupling:
   - Starting from nixpkgs' vLLM expression is recommended because it tracks non-Python sources such as CUTLASS, FlashMLA, triton kernels, qutlass, and CUDA/ROCm CMake flags.
   - On each vLLM bump, compare the target tag's `CMakeLists.txt` and `cmake/external_projects/*.cmake` against the repo-local Nix package and bump associated source hashes when upstream changes them.
6. No package-manager resolver regressions:
   - Do not reintroduce `pip download`, `pip install`, live PyPI/PyTorch index resolution, or wheelhouse archives.
   - If using upstream wheels as Nix-packaged artifacts for Torch/CUDA, each wheel must be represented by an explicit Nix fetcher/flake input with a fixed hash and installed/exposed as a Nix package, not by pip resolver behavior.
7. Verification after vLLM bump:
   - `nix build .#vllm-runtime`
   - `result/bin/vllm --version` equals target vLLM version.
   - Python import smoke: run the wrapped Python/package import path equivalent of `python -c 'import vllm'`.
   - If CUDA is available on the target machine, run a minimal server/readiness smoke with one configured local model before trusting the bump.

Dependency monitor note: vLLM/Torch/PyTorch wheel URLs or fixed-output Nix fetchers may not appear as GitHub flake inputs. If Build implements them as plain Nix fetchers with hashes, `.local/bin/deps-check` will not report drift; future bump PRs must manually inspect upstream vLLM requirements/release assets.

### AGENTS.md policy addition needed

Design cannot edit `AGENTS.md`; Build should add a **generic** coupled-version policy near the existing repo-maintained Nix-build determinism / locked dependency rules. Keep AGENTS policy dependency-agnostic; package-specific instructions belong next to the actual version declarations in code/docs.

```markdown
- Coupled dependency bump policy: whenever a dependency version is locked in code, Nix expressions, service wrappers, generated hashes, or flake inputs, document nearby what else must be reviewed or updated when that version changes. Keep the note next to the locked version or source declaration, not only in a central doc. A version bump must update all coupled version declarations, source refs, lock/dependency hashes, generated vendor/dependency artifacts, wrapper assertions, service assumptions, and smoke checks in the same change. Do not change only the visible package version.
- Dependency-specific bump notes should be local and actionable: say what upstream files/release notes to inspect, which companion dependencies must move together, which generated hashes must be refreshed, and which verification commands prove the bump. If no coupling exists, a short local note saying the version is standalone is acceptable for non-obvious cases.
```

Suggested location: directly after the current `Repo-maintained Nix-build determinism invariant` and package-manager policy bullets, because this is the operational rule for changing locked runtime versions safely.

### Local version-note additions needed

Build should place dependency-specific upgrade notes immediately beside the locked versions/source declarations when implementing the deterministic refactors. Suggested examples:

- In `flake.nix`, near `fusionRuntime` version / future `fusion-src` input:
  - Note that bumping Fusion requires inspecting the target upstream tag's root `package.json` `packageManager`, `pnpm-lock.yaml`, `packages/cli/package.json` `bin`, package manifests for native/runtime deps, and docs/release notes.
  - Note that the pnpm dependency hash must be refreshed with the version/source tag.
  - Note that `fusion`, `fn`, and `qmd` smoke checks must be rerun.
- In the Fusion Nix package, near the `runtimePath` list:
  - Note that entries like `docker-client`, `tmux`, `git`, `gh`, `openssh`, `python3`, and `uv` are external runtime tools exposed on wrapper `PATH`, not npm dependencies.
- In the Fusion Nix package, near pnpm build/install logic:
  - Note that npm packages come from Fusion's upstream manifests/lockfile; do not add ad hoc top-level npm dependencies such as `node-pty`, `dockerode`, or `send` unless a concrete upstream/runtime failure justifies it.
- In `flake.nix`, near `vllmRuntime` version label / future vLLM source declaration:
  - Note that bumping vLLM requires inspecting the target tag's `requirements/cuda.txt`, GPU installation docs, GitHub release assets, and release notes/changelog.
  - Note that vLLM, Torch, torchvision, torchaudio, CUDA variant label, native component source hashes, wrapper assertions, and import/CLI smoke checks move together.
- In the vLLM Nix package, near Torch/CUDA version declarations:
  - Note that only upstream-supported Torch/CUDA combinations are allowed; if docs do not identify a more specific supported combo, preserve the current repo versions.
  - Note that nixpkgs Torch/CUDA must not be substituted merely because it evaluates.

## 2026-08-06 nondeterminism audit + invariant

User invariant: every external dependency that participates in **Nix build code maintained in this repo** must be represented by Nix flake inputs. Nix derivations/scripts we maintain in this repo must not fetch/resolve floating external resources with npm/pip/bun/pnpm/yarn/curl/git directly. Transitive implementation details inside upstream nixpkgs packages are acceptable as part of the selected flake input. User-runtime package managers and non-Nix setup commands are out of scope unless moved into repo-maintained Nix derivations/profile build code.

`AGENTS.md` currently covers `$HOME` checkout and hidden repo-support directories, but it does **not** explicitly prevent build nondeterminism. Required AGENTS wording to add:

```markdown
- Repo-maintained Nix-build determinism invariant: any external dependency used by Nix build code maintained in this repo must be declared through `flake.nix` inputs and locked in `flake.lock`. Do not add `builtins.fetchTree`, `builtins.fetchTarball`, `builtins.fetchurl`, `import <nixpkgs>`, live `npm install`, live `pip download`, live `bun install`, live `pnpm/yarn install`, `curl`, `wget`, or `git clone` inside repo-maintained Nix derivations/scripts unless the user explicitly approves an exception.
- Flake input URL policy: input URLs may point to branches/refs such as `nixpkgs-unstable`, `main`, `master`, or PR refs. Exact revision pinning belongs in `flake.lock`, not necessarily in `flake.nix`. Do not replace branch/ref input URLs with explicit commit URLs just for determinism; doing so prevents normal `nix flake update` bump behavior. Determinism is provided by the locked rev + narHash in `flake.lock`.
- Package-manager policy inside repo-maintained Nix builds: prefer Nix-native builders and Nix dependency declarations over ecosystem package managers. Do not run live `npm install`, `npm ci`, `npm rebuild`, `pip download`, `pip install`, `uv pip`, `bun install`, `pnpm install`, `yarn install`, or similar resolver/install commands inside Nix build code maintained in this repo. If an ecosystem tool is unavoidable in repo-maintained Nix code, it must run offline against artifacts already declared as flake inputs and must not resolve or download anything.
- Upstream package boundary: package-manager usage inside dependencies provided by flake inputs (for example nixpkgs package internals) is acceptable unless this repo overrides or vendors that logic. The policy forbids package-manager resolution in code we maintain here, not in upstream package implementations selected through locked flake inputs.
- User-runtime package managers are allowed outside Nix builds. Makefile setup commands, pacman/yay bootstrap, vim-plug, tmux TPM, npm user config, cleanup scripts, and runtime curl/API checks are not prohibited by this rule unless they are moved into a Nix derivation/profile build.
- Services must not run `nix build`/`nix-build` at startup. Profile-managed runtime dependencies must be realized by `nix profile upgrade klarkc` and consumed from `%h/.nix-profile/bin`.
- This repo is installed at `$HOME`; systemd `%h` is the repo root. Do not use `%h/Sources/Fusion/klarkc/dotfiles` as a flake root for this repo.
```

### Findings requiring Build fixes

#### 1. `.nix/fusion-npm.nix` — nondeterministic npm graph

Current issue:

```bash
npm install --global \
  @runfusion/fusion@${version} \
  @tobilu/qmd@2.1.0 \
  node-pty \
  dockerode \
  send
```

This violates the invariant because npm resolves and downloads the dependency graph during the build. Even with a fixed-output derivation, the output hash can drift because transitive dependencies and postinstall artifacts can change.

Required flake-input-compliant design:

1. Create a generated dependency manifest under a hidden path, for example:
   - `.nix/fusion-npm/package.json`
   - `.nix/fusion-npm/package-lock.json` (used only as graph source)
   - `.nix/fusion-npm-inputs.nix` or generated `flake.nix` input block mapping every npm tarball in the lockfile to a `flake = false` input.
2. Add every npm tarball (top-level and transitive) as a flake input in `flake.nix`, locked in `flake.lock`.
3. Change `.nix/fusion-npm.nix` to accept the generated npm input attrset from `flake.nix` and construct an offline npm cache or `node_modules` tree from `/nix/store` paths only.
4. Run npm in offline/no-resolution mode only, e.g. `npm ci --offline --ignore-scripts` against the local cache, or avoid npm resolution and unpack tarballs deterministically.
5. Prevent Electron binary downloads during rebuild:
   ```bash
   export ELECTRON_SKIP_BINARY_DOWNLOAD=1
   export ELECTRON_SKIP_DOWNLOAD=1
   ```
6. Rebuild only required native modules if possible (e.g. `node-pty`) instead of `npm rebuild --global` over the entire graph.

Acceptance:

- No live `npm install` or registry access in `.nix/fusion-npm.nix`.
- Every npm package tarball consumed by the build is an `inputs.<name>` entry with `flake = false`.
- Repeated `nix build .#fusion-runtime` does not require re-pinning `outputHash`.

#### 2. `.nix/vllm-runtime.nix` — nondeterministic pip wheelhouse

Current issue:

```bash
python3.12 -m pip download --dest "$out" --extra-index-url https://download.pytorch.org/whl/cu130 ...
```

This violates the invariant because pip resolves and downloads the wheel graph during the build.

Required flake-input-compliant design:

1. Generate a complete wheel manifest for:
   - `torch==2.11.0+cu130`
   - `torchvision==0.26.0+cu130`
   - `torchaudio==2.11.0+cu130`
   - `vllm==0.20.1`
   - all transitive dependencies.
2. Add every wheel/sdist URL as a `flake = false` input in `flake.nix`, locked in `flake.lock`.
3. Change `.nix/vllm-runtime.nix` to accept a wheel input attrset, symlink/copy all wheels into a local wheelhouse, and install with:
   ```bash
   pip install --no-index --find-links "$wheelhouse" ...
   ```
4. Remove `pip download` from the build.

Acceptance:

- No live pip resolver/download in `.nix/vllm-runtime.nix`.
- Every wheel/sdist consumed by the build is a flake input.
- Repeated `nix build .#vllm-runtime` is stable.

#### 3. `.nix/opencode-with-reasoning.nix` / `pkgs.opencode` — stale local source override

Audit evidence:

- Local wrapper `.nix/opencode-with-reasoning.nix` overrides `pkgs.opencode` and its `node_modules` derivation:
  ```nix
  opencodeBase = pkgs.opencode.overrideAttrs (old: {
    src = opencode-src;
    node_modules = old.node_modules.overrideAttrs (_: {
      src = opencode-src;
      outputHash = "sha256-9oSXcvvISB6WAqI6f/GBZ3i9IBwYrRQvKs82SLibJNo=";
    });
  });
  ```
- `pkgs.opencode` main derivation itself builds with the vendored `node_modules` and uses `bun --skip-install`, which is good:
  ```sh
  cp -R /nix/store/...-opencode-node_modules-.../. .
  bun --bun ./script/build.ts --single --skip-install
  ```
- But the `opencode-node_modules` fixed-output derivation runs live Bun install:
  ```sh
  export BUN_INSTALL_CACHE_DIR=$(mktemp -d)
  bun install \
    --cpu="*" \
    --frozen-lockfile \
    --filter ./ \
    --filter ./packages/app \
    --filter ./packages/desktop \
    --filter ./packages/opencode \
    --filter ./packages/shared \
    --ignore-scripts \
    --no-progress \
    --os="*"
  ```
- The derivation is fixed-output (`outputHashMode = "recursive"`), but it still violates the invariant because Bun resolves/downloads package artifacts during the Nix build.

Selected policy interpretation: `pkgs.opencode` comes from the locked `nixpkgs` flake input. Its internal Bun usage is an upstream nixpkgs implementation detail, not repo-maintained Nix build code. This is acceptable. What is not needed anymore is this repo's local `opencode-src` override and local `node_modules.outputHash` override.

Required Build changes:

1. Drop `opencode-src` from `flake.nix` inputs and `flake.lock`.
2. Rename `.nix/opencode-with-reasoning.nix` to `.nix/opencode-with-codex-auth.nix`.
3. Rename `opencodeWithReasoning` to `opencodeWithCodexAuth` in `flake.nix`.
4. Rewrite the wrapper to use `pkgs.opencode` directly and keep only Codex auth sync behavior.

Acceptance:

- This repo no longer overrides opencode `src` or `node_modules`.
- No `opencode-src` input remains.
- The wrapper only adds Codex-auth sync to `pkgs.opencode`.
- Reasoning-field support is preserved through nixpkgs' current `pkgs.opencode`, which already includes the merged upstream PR.

Can Bun be removed entirely from the opencode Nix build?

- If "remove Bun" means **remove live `bun install`/dependency resolution from Nix builds**: yes, required. Replace the `opencode-node_modules` FOD with a flake-input-backed JS dependency graph or an offline cache built exclusively from flake input artifacts.
- If "remove Bun" means **no Bun executable in any opencode build phase**: only if we stop building opencode from source and consume a prebuilt upstream opencode artifact as a flake input, or if upstream provides a non-Bun build path. Current nixpkgs `opencode` uses Bun for the actual source build:
  ```sh
  bun --bun ./script/build.ts --single --skip-install
  bun --bun ./script/schema.ts config.json tui.json
  ```
  That is not dependency resolution; it is the upstream build tool. Removing it while still source-building would require reimplementing upstream's build scripts in Nix/Node/shell, which is higher risk and likely not worth it.

Superseded decision: zero Bun in the full transitive opencode package closure is **not** required. The policy only forbids package-manager usage in Nix build code maintained in this repo. Since `pkgs.opencode` is provided by locked nixpkgs, its internal Bun usage is acceptable. Do not implement the prebuilt-artifact Option A unless the user separately requests no Bun in any transitive closure.

Naming note: `opencode-with-reasoning` originated when `opencode-src` pointed at upstream PR `anomalyco/opencode#30477` (`github:anomalyco/opencode/pull/30477/head`). That PR was titled “feat: add \"reasoning\" as interleaved field option for vLLM providers” and added support for vLLM's `message.reasoning` field. The PR is merged upstream, and the current locked `opencode-src` already includes the support:

- `packages/core/src/v1/config/provider.ts`: `field: Schema.Literals(["reasoning", "reasoning_content", "reasoning_details"])`
- `packages/core/src/models-dev.ts`: same accepted field list.
- generated SDK types include `field: "reasoning" | "reasoning_content" | "reasoning_details"`.

Therefore the local package/file name is now stale. When switching to a prebuilt stable artifact, Build should rename:

- `opencodeWithReasoning` -> `opencodeWithCodexAuth`
- `.nix/opencode-with-reasoning.nix` -> `.nix/opencode-with-codex-auth.nix`

The package now represents the Codex-auth-sync wrapper, not a reasoning-specific fork/patch.

Additional verification: nixpkgs' current `pkgs.opencode` source also includes the merged reasoning-field support. The source used by `pkgs.opencode` (`opencode-1.17.13`) contains:

- `packages/opencode/src/provider/provider.ts`: `field: Schema.Literals(["reasoning", "reasoning_content", "reasoning_details"])`
- `packages/core/src/v1/config/provider.ts`: same accepted field list.
- `packages/core/src/models-dev.ts`: same accepted field list.

Therefore Build can drop `opencode-src` and the `node_modules` override **if** we are comfortable using nixpkgs' `pkgs.opencode` package. This would simplify to:

```nix
opencodeWithCodexAuth = pkgs.symlinkJoin {
  name = "${pkgs.opencode.name}-with-codex-auth";
  paths = [ pkgs.opencode ];
  nativeBuildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/opencode \
      --run '. ${syncCodexAuth}'
  '';
};
```

Tradeoff:

- Dropping `opencode-src` + `node_modules` override removes local source/hash maintenance and confirms the reasoning patch is no longer needed.
- It does **not** remove nixpkgs' internal Bun-based `opencode-node_modules` fixed-output derivation. If the user still requires zero Bun in Nix builds, keep Option A (prebuilt artifact flake input). If the narrowed goal is only to avoid this repo's stale source override and reasoning-specific fork, using `pkgs.opencode` directly is the smallest cleanup.

Recommended decision point for Build/user:

Selected Build direction: drop `opencode-src`, rename wrapper to `opencode-with-codex-auth`, and wrap `pkgs.opencode` directly.

#### 4. `Makefile` — nondeterministic setup downloads (out of scope for Nix-build invariant)

Current issue:

```make
curl ... releases/latest ... Nordic.tar.xz
curl ... git.io/papirus-icon-theme-install | sh
curl ... releases/latest ... Papirus-Nord.tar.xz
curl ... releases/latest ... dir_colors
curl ... raw.githubusercontent.com/.../master/plug.vim
git clone https://github.com/tmux-plugins/tpm .tmux/plugins/tpm
```

These are install/setup dependencies and are **out of scope** for the current Nix-build-only invariant because they do not run inside Nix derivations/profile builds. Keep them as user-runtime/setup package-manager behavior unless they are moved into Nix packages/profile outputs.

If the user later wants repo setup determinism beyond Nix builds, recommended design:

1. Declare each source as a flake input with `flake = false` and exact revision/archive URL:
   - `nordic-theme-src`
   - `papirus-icon-theme-src` or exact installer source if still needed
   - `papirus-nord-src`
   - `nord-dircolors-src` or exact file archive source
   - `vim-plug-src`
   - `tmux-tpm-src`
2. Replace `curl`/`git clone` Makefile targets with copy/install from Nix store input paths, or move these installs into a Nix package/activation script in the `klarkc` profile.
3. Avoid `latest`, `master`, and `git.io` indirections.

Optional acceptance for a future broader cleanup:

- `Makefile` no longer uses live `curl` or `git clone` for repo-managed dependencies.
- Theme/plugin sources are locked in `flake.lock`.

#### 5. `flake.nix` branch/ref input URLs — accepted policy

Current examples:

```nix
nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
alacritty-ligatures-src.url = "github:ink-splatters/alacritty-ligatures/master";
nixGL.url = "git+https://github.com/nix-community/nixGL?ref=refs/pull/223/head";
```

Policy: these are acceptable and should remain branch/ref-style when that is the intended update channel. `flake.lock` pins exact rev+narHash for deterministic builds. Do **not** rewrite these to explicit commit URLs solely for determinism; explicit commit URLs prevent expected `nix flake update` bump behavior. If a dependency should never move except by manual URL edit, then an explicit commit URL is acceptable, but that is a separate policy decision.

### Priority order

1. Fusion npm graph — currently in `packages.default` and proven hash-drifting.
2. vLLM wheel graph — currently in `packages.default`, live pip resolution.
3. opencode Bun/node_modules graph — currently in `packages.default` through `opencodeWithReasoning`, live Bun install in fixed-output derivation.
4. Makefile downloads — out of scope for Nix-build-only invariant unless moved into Nix/profile.
5. Keep branch/ref-style flake inputs as-is unless the user wants to disable normal flake update behavior for a specific input.

## Design recommendation: remove repo-maintained npm/pip by using flake-input closure artifacts

Evidence checked:

- nixpkgs has `pkgs.vllm` / `pkgs.python312Packages.vllm`, but the locked nixpkgs version is `0.16.0`, while this repo currently packages/uses `vllm==0.20.1` with CUDA 13.0 wheels. Replacing the custom runtime with nixpkgs vLLM would be a version downgrade and may break current model/runtime assumptions.
- nixpkgs does not provide `pkgs.fusion` for `@runfusion/fusion`.

Recommendation: do **not** attempt to manually list every npm package/wheel as separate flake inputs in `flake.nix`. The graphs are large, brittle, and expensive to maintain. Instead, use one locked, prebuilt dependency-closure artifact per runtime as a `flake = false` input:

1. Fusion:
   - Create externally: `fusion-npm-payload-0.73.0-linux-x86_64.tar.zst` containing the deterministic `lib/node_modules` + npm `.bin`/bin layout required by Fusion/qmd/node-pty/dockerode/send.
   - Declare as flake input, e.g.:
     ```nix
     fusion-npm-payload = {
       url = "https://github.com/klarkc/dotfiles/releases/download/fusion-npm-payload-0.73.0/fusion-npm-payload-0.73.0-linux-x86_64.tar.zst";
       flake = false;
     };
     ```
   - Refactor `.nix/fusion-npm.nix` to accept `{ fusionNpmPayloadSrc, ... }`, unpack this input, patch shebangs, create wrappers, and run version checks. No `npm install`, no `npm rebuild`, no registry access in repo-maintained Nix code.
   - Native modules (for example `node-pty`) must already be built in the payload against a compatible Node/ABI, or the payload-generation process must include a deterministic native build step. Do not rebuild via npm inside this repo's Nix derivation.

2. vLLM:
   - Create externally: `vllm-wheelhouse-0.20.1-cu130-py312-linux-x86_64.tar.zst` containing every required wheel for `torch==2.11.0+cu130`, `torchvision==0.26.0+cu130`, `torchaudio==2.11.0+cu130`, `vllm==0.20.1`, and all transitive deps.
   - Declare as flake input, e.g.:
     ```nix
     vllm-wheelhouse = {
       url = "https://github.com/klarkc/dotfiles/releases/download/vllm-wheelhouse-0.20.1-cu130/vllm-wheelhouse-0.20.1-cu130-py312-linux-x86_64.tar.zst";
       flake = false;
     };
     ```
   - Refactor `.nix/vllm-runtime.nix` to accept `{ vllmWheelhouseSrc, ... }`, unpack the wheelhouse, and install wheels without pip resolution/download.
   - Preferred installer: use `python312Packages.installer` (`python -m installer`) against local wheel files. This is not a package manager/resolver; it is an offline wheel installer. If that is unavailable, direct wheel unpacking into `site-packages` can work but is less correct for `.data` layouts.
   - No `pip download` and no `pip install` in repo-maintained Nix code.

Why this is preferred:

- Satisfies the invariant: external dependency closures are explicit flake inputs locked in `flake.lock`.
- Removes npm/pip resolver/install commands from repo-maintained Nix derivations.
- Avoids maintaining hundreds/thousands of per-package flake input declarations.
- Keeps updates intentional: update the external closure artifact URL/ref, then run `nix flake lock`.

Tradeoffs:

- The closure artifacts become release artifacts that must be produced by a trusted external generation process.
- Payloads are platform/ABI-specific (`linux-x86_64`, Python 3.12, Node version/ABI, CUDA 13.0).
- Build cannot independently audit every transitive artifact from `flake.nix` alone; provenance belongs to the closure-generation process/release notes.

Acceptance for Build:

- `.nix/fusion-npm.nix` has no `npm install`, `npm ci`, or `npm rebuild`.
- `.nix/vllm-runtime.nix` has no `pip download` or `pip install`.
- `flake.nix` declares `fusion-npm-payload` and `vllm-wheelhouse` as `flake = false` inputs.
- `nix build .#default` succeeds and profile binaries still report:
  - `fusion --version` -> `0.73.0`
  - `vllm --version` -> `0.20.1`
- Services continue to consume `%h/.nix-profile/bin/{fusion,vllm}` only.

### Superseding Fusion/QMD recommendation: use upstream pnpm locks + nixpkgs pnpm helpers

Further audit found a better Nix-native path for Fusion/QMD than prebuilt closure artifacts or synthetic `package-lock.json`:

- Published npm tarballs for `@runfusion/fusion@0.73.0` and `@tobilu/qmd@2.1.0` contain `package.json` only, no lockfiles.
- But their upstream Git tags do include `pnpm-lock.yaml`:
  - `Runfusion/Fusion` tag `v0.73.0` exists and has `pnpm-lock.yaml`.
  - `tobi/qmd` tag `v2.1.0` exists and has `pnpm-lock.yaml`.
- Locked nixpkgs has `pkgs.pnpm_10.fetchDeps` and `pkgs.pnpm_10.configHook`, so Build can use nixpkgs' pnpm helpers instead of shelling out to live npm.

Updated Fusion/QMD Build direction:

1. Add source flake inputs (branch/ref URL policy still applies; tag refs are okay and locked in `flake.lock`):
   ```nix
   fusion-src = {
     url = "github:Runfusion/Fusion/v0.73.0";
     flake = false;
   };

   qmd-src = {
     url = "github:tobi/qmd/v2.1.0";
     flake = false;
   };
   ```
2. Refactor `.nix/fusion-npm.nix` to consume `{ fusion-src, qmd-src, ... }` and build two pnpm-backed packages (or one combined runtime):
   - `fusionCli`: source `fusion-src`, dependency graph from upstream `pnpm-lock.yaml`, install/wrap `fusion` + `fn` from `packages/cli`.
   - `qmdCli`: source `qmd-src`, dependency graph from upstream `pnpm-lock.yaml`, install/wrap `qmd`.
   - final `fusion-runtime`: `symlinkJoin`/wrapper package combining `fusionCli`, `qmdCli`, `tmux`, and runtime tools.
3. Use nixpkgs pnpm helper pattern, roughly:
   ```nix
   pnpmDeps = pkgs.pnpm_10.fetchDeps {
     inherit src pname version;
     hash = "sha256-...";
   };

   nativeBuildInputs = [ pkgs.nodejs pkgs.pnpm_10.configHook ... ];
   inherit pnpmDeps;
   ```
   Exact phases depend on each upstream repo (`pnpm --filter ... build`, `pnpm --filter ... deploy`, or copying already-built dist if present).
4. Remove from `.nix/fusion-npm.nix`:
   - `npm install --global`
   - top-level ad hoc `node-pty`, `dockerode`, `send`
   - `npm rebuild --global`
5. Dependency cleanup evidence:
   - Fusion 0.73.0 already declares `node-pty` (`@homebridge/node-pty-prebuilt-multiarch`) and `dockerode` as dependencies.
   - Current top-level `node-pty`/`dockerode` are different ad hoc versions and should not be installed unless a concrete runtime failure proves they are needed.
   - Fusion/QMD metadata do not declare top-level `send`; quick grep of Fusion dist did not find `require("send")`/`from "send"`; remove it unless Build finds a concrete runtime requirement.

Acceptance for Fusion/QMD:

- No npm commands in `.nix/fusion-npm.nix`.
- Fusion/QMD dependencies are resolved by nixpkgs pnpm helpers from upstream lockfiles, not live package-manager resolution.
- `nix build .#fusion-runtime` succeeds.
- Profile provides `fusion`, `fn`, and `qmd`; `fusion --version` reports `0.73.0`; `qmd --help` works.

{
  pkgs,
  version,
  fusion-src,
  qmd-src,
}:
let
  # Bump note: Fusion runtime path. Entries below (`docker-client`, `tmux`,
  # `git`, `gh`, `openssh`, `python3`, `uv`) are external runtime tools exposed
  # on the wrapper's `PATH`, NOT npm dependencies. Do not add ad hoc top-level
  # npm packages for upstream-owned deps like `node-pty` or `dockerode`; those
  # come from the Fusion source's `pnpm-lock.yaml`. Bump `qmd-src` together
  # only if Fusion docs/code require a newer QMD CLI.
  runtimePath = pkgs.lib.makeBinPath (
    with pkgs;
    [
      coreutils
      curl
      docker-client
      findutils
      gawk
      gh
      git
      gnugrep
      gnused
      nodejs
      openssh
      python3
      tmux
      uv
    ]
  );

  # pnpm major pinned for `lockfileVersion: 9` (Fusion and QMD both use 9).
  # Using pnpm 10 because `--config.shamefully-hoist=true` does not work in
  # pnpm 11 the same way (pnpm 11 reorganizes the store and does not honor the
  # legacy hoist in all cases). pnpm 10 supports the lockfile and the config.
  pnpm = pkgs.pnpm_10;

  # Documented Nixpkgs-supported pnpm layout flags. Passed to both
  # `fetchPnpmDeps` and `pnpmConfigHook` through structured attrs so store
  # generation and build-time install stay in sync. Do NOT do manual `.pnpm`
  # symlink surgery.
  fetchPnpmFlags = [
    "--shamefully-hoist"
    "--config.confirmModulesPurge=false"
  ];

  fusionCli = pkgs.stdenv.mkDerivation (finalAttrs: {
    pname = "fusion-cli";
    inherit version;
    src = fusion-src;

    # Bump note: refresh `pnpmDeps.hash` via `nix build .#fusion-runtime` (the
    # initial attempt prints the expected hash). Update Fusion source tag, this
    # hash, the runtime version, and rerun the smoke checks together.
    pnpmDeps = pkgs.fetchPnpmDeps {
      inherit pnpm;
      pname = "fusion-cli";
      inherit version;
      src = fusion-src;
      fetcherVersion = 4;
      inherit (finalAttrs) pnpmInstallFlags pnpmWorkspaces;
      hash = "sha256-DqNLwPQECpjAc+IGUMSSsSyhKKoq1py3PvucSyAYL5Y=";
    };

    pnpmWorkspaces = [ "@runfusion/fusion..." ];

    # `pnpmBuildHook` reads `pnpmInstallFlags` as a real shell array when
    # `__structuredAttrs = true; strictDeps = true;` is enabled, matching the
    # `pnpmBuildHook` documentation example. Otherwise the hook collapses the
    # Nix list into a single shell string and pnpm reports the combined arg
    # as an unknown option.
    __structuredAttrs = true;
    strictDeps = true;

    # Disable Fusion's full-publish package mode. Fusion's tsup config treats
    # `CI=true` as a request for the full release package, which then tries
    # to build `@fusion/desktop` and fails because Electron/desktop deps are
    # not in the filtered workspace. Setting `FUSION_CLI_FULL_PACKAGE=0`
    # forces the default local package surface (bin.js, extension.js, PG
    # migrations) and avoids the desktop path entirely.
    env.FUSION_CLI_FULL_PACKAGE = "0";

    # Tell pnpm not to prompt in a non-TTY build environment.
    env.CI = "true";

    pnpmInstallFlags = [
      "--shamefully-hoist"
      "--config.confirmModulesPurge=false"
    ];

    nativeBuildInputs = with pkgs; [
      cacert
      nodejs
      pnpmConfigHook
      pnpmBuildHook
      pnpm
      makeWrapper
      writableTmpDirAsHomeHook
    ];

    pnpmBuildScript = "build";

    # Custom install phase modeled after `pkgs/by-name/t3/t3code/package.nix`:
    # pnpmBuildHook handles the recursive workspace build, then we stage the
    # CLI's `node_modules` and built `dist` into the output without using
    # `npmInstallHook` (which calls `npm pack --workspace=...` and fails for
    # pnpm-managed workspaces). Copy preserving source mode (so `bin.mjs`
    # stays executable) and use `patchShebangs` on the staged tree.
    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib"
      cp --recursive node_modules "$out/lib/"

      # The hoisted `node_modules` already has `@runfusion/fusion` as a
      # symlink to the workspace source dir. Remove it and stage the real
      # built package so ESM resolution finds the actual `bin.mjs`/`dist/`.
      rm -rf "$out/lib/node_modules/@runfusion/fusion"
      mkdir -p "$out/lib/node_modules/@runfusion/fusion"
      cp --recursive \
        packages/cli/{dist,bin.mjs,agent-browser.mjs,skill,package.json,README.md} \
        "$out/lib/node_modules/@runfusion/fusion/"
      chmod +x "$out/lib/node_modules/@runfusion/fusion/bin.mjs"
      if [ -e "$out/lib/node_modules/@runfusion/fusion/agent-browser.mjs" ]; then
        chmod +x "$out/lib/node_modules/@runfusion/fusion/agent-browser.mjs"
      fi
      patchShebangs "$out/lib/node_modules/@runfusion/fusion"

      # `agent-browser` (npm dep, currently pinned at 0.26.0 by Fusion 0.75.1
      # and unchanged up to 0.33.2) ships its platform binary with mode 0644
      # and tries to chmod it at runtime, which fails with EPERM in the Nix
      # store. Pre-chmod the binary here so the upstream `agent-browser`
      # wrapper works without requiring a writable install location.
      find "$out/lib/node_modules" \
        -path '*/agent-browser/bin/agent-browser-linux-x64' \
        -exec chmod 755 {} +

      # Clean up any dangling workspace symlinks that point back into the
      # build directory.
      find "$out/lib" -xtype l -delete

      mkdir -p "$out/bin"
      makeWrapper "$out/lib/node_modules/@runfusion/fusion/bin.mjs" "$out/bin/fusion" \
        --prefix PATH : "${runtimePath}"
      makeWrapper "$out/lib/node_modules/@runfusion/fusion/bin.mjs" "$out/bin/fn" \
        --prefix PATH : "${runtimePath}"
      if [ -e "$out/lib/node_modules/@runfusion/fusion/agent-browser.mjs" ]; then
        makeWrapper "$out/lib/node_modules/@runfusion/fusion/agent-browser.mjs" \
          "$out/bin/agent-browser" \
          --prefix PATH : "${runtimePath}"
      fi

      runHook postInstall
    '';

    meta = {
      description = "Fusion CLI (runtime) packaged via Nixpkgs pnpm helpers";
      mainProgram = "fusion";
    };
  });

  qmdCli = pkgs.stdenv.mkDerivation (finalAttrs: {
    pname = "qmd-cli";
    version = "2.1.0";
    src = qmd-src;

    pnpmDeps = pkgs.fetchPnpmDeps {
      inherit pnpm;
      pname = "qmd-cli";
      version = "2.1.0";
      src = qmd-src;
      fetcherVersion = 4;
      pnpmInstallFlags = fetchPnpmFlags;
      hash = "sha256-wWd/IeyJUPNHeqxtpr03g4NimHh+XX24LQHYA8IQYXY=";
    };

    # Same Nixpkgs-pnpm helper pattern as Fusion. `__structuredAttrs` +
    # `strictDeps` keep `pnpmInstallFlags` as a real shell array for the
    # hook install. `CI=true` is needed by pnpm to skip TTY prompts.
    __structuredAttrs = true;
    strictDeps = true;

    env.CI = "true";

    pnpmInstallFlags = [
      "--shamefully-hoist"
      "--config.confirmModulesPurge=false"
    ];

    nativeBuildInputs = with pkgs; [
      cacert
      nodejs
      pnpmConfigHook
      pnpmBuildHook
      pnpm
      makeWrapper
      writableTmpDirAsHomeHook
    ];

    pnpmBuildScript = "build";

    # Custom install phase modeled after `pkgs/by-name/t3/t3code/package.nix`:
    # copy `node_modules` and built `dist` to the output, then wrap the `qmd`
    # entry point. `npmInstallHook` is omitted because QMD is a pnpm-managed
    # repo and `npm pack --workspace=...` would fail.
    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib"
      cp --recursive node_modules "$out/lib/"

      # Remove any pre-existing `@tobilu/qmd` link/dir and stage the real
      # built package so ESM resolution finds `dist/cli/qmd.js`.
      rm -rf "$out/lib/node_modules/@tobilu/qmd"
      mkdir -p "$out/lib/node_modules/@tobilu/qmd"
      cp --recursive dist "$out/lib/node_modules/@tobilu/qmd/dist"
      cp --recursive bin "$out/lib/node_modules/@tobilu/qmd/bin"
      cp package.json README.md "$out/lib/node_modules/@tobilu/qmd/"
      chmod +x "$out/lib/node_modules/@tobilu/qmd/bin/qmd"
      patchShebangs "$out/lib/node_modules/@tobilu/qmd/bin"

      find "$out/lib" -xtype l -delete

      mkdir -p "$out/bin"
      for f in "$out/lib/node_modules/@tobilu/qmd/bin"/*; do
        [ -e "$f" ] || continue
        name="$(basename "$f")"
        makeWrapper "$f" "$out/bin/$name" \
          --prefix PATH : "${runtimePath}"
      done

      runHook postInstall
    '';

    meta = {
      description = "qmd CLI (runtime) packaged via Nixpkgs pnpm helpers";
      mainProgram = "qmd";
    };
  });

  # Bump note: the assembled runtime profile. When bumping Fusion or QMD,
  # rerun `nix build .#fusion-runtime` and verify `fusion --version`,
  # `fn --help`, and `qmd --help`.
in
pkgs.symlinkJoin {
  name = "fusion-runtime-${version}";
  paths = [
    fusionCli
    qmdCli
  ];
}

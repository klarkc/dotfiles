{
  pkgs,
  mcp-remote-src,
}:
let
  # Bump note: `mcp-remote` runtime version follows the upstream npm
  # `package.json`. To bump:
  #   1) update `version` to the upstream npm package version,
  #   2) update `inputs.mcp-remote-src.url` in `flake.nix` to the new
  #      upstream commit hash (the project does not publish stable tags),
  #   3) rerun `nix build .#mcp-remote-runtime`; the initial attempt
  #      prints the expected `pnpmDeps.hash`; commit the reported hash
  #      together with the source rev and the version so the package
  #      version, the source reference, and the lockfile hash stay in
  #      sync. Do not change only the version or only the hash.
  #   4) verify `mcp-remote --version` against the new upstream tag.
  version = "0.1.38";

  # Runtime PATH for `mcp-remote`. `mcp-remote` is an ESM Node CLI that
  # uses native fetch; the only external runtime tools it needs are the
  # usual shell helpers plus nodejs itself.
  runtimePath = pkgs.lib.makeBinPath (
    with pkgs;
    [
      coreutils
      curl
      findutils
      gawk
      gnugrep
      gnused
      nodejs
    ]
  );

  # pnpm major pinned for `lockfileVersion: 9` (matches mcp-remote's
  # pnpm-lock.yaml). Using pnpm 10 because it supports the legacy
  # `--shamefully-hoist` flag without the pnpm 11 store reorganization.
  pnpm = pkgs.pnpm_10;

  # Documented Nixpkgs-supported pnpm layout flags. Passed to both
  # `fetchPnpmDeps` and `pnpmConfigHook` so store generation and
  # build-time install stay in sync. Do NOT do manual `.pnpm` symlink
  # surgery.
  fetchPnpmFlags = [
    "--shamefully-hoist"
    "--config.confirmModulesPurge=false"
  ];

  mcpRemote = pkgs.stdenv.mkDerivation (finalAttrs: {
    pname = "mcp-remote";
    inherit version;
    src = mcp-remote-src;

    # Bump note: refresh `pnpmDeps.hash` via `nix build .#mcp-remote-runtime`
    # (initial attempt prints the expected hash). Update source rev, this
    # hash, and the runtime version together.
    pnpmDeps = pkgs.fetchPnpmDeps {
      pname = "mcp-remote";
      version = version;
      inherit pnpm;
      src = mcp-remote-src;
      fetcherVersion = 4;
      inherit (finalAttrs) pnpmInstallFlags;
      hash = "sha256-UWqbaZkz8leP6QjT54MdP7fcFeBd1wlkB7BZOiypb6A=";
    };

    # `pnpmBuildHook` reads `pnpmInstallFlags` as a real shell array when
    # `__structuredAttrs = true; strictDeps = true;` is enabled, matching
    # the `pnpmBuildHook` documentation example. Otherwise the hook
    # collapses the Nix list into a single shell string and pnpm reports
    # the combined arg as an unknown option.
    __structuredAttrs = true;
    strictDeps = true;

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
    # pnpmBuildHook handles the recursive build, then we stage the CLI's
    # `node_modules` and built `dist` into the output without using
    # `npmInstallHook` (which fails on pnpm-managed repos). Copy preserving
    # source mode (so the bundled CLI stays executable) and patchShebangs.
    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib"
      cp --recursive node_modules "$out/lib/"

      # The hoisted `node_modules` already has `mcp-remote` as a symlink
      # to the source dir. Remove it and stage the real built package so
      # ESM resolution finds `dist/proxy.js` / `dist/client.js`.
      rm -rf "$out/lib/node_modules/mcp-remote"
      mkdir -p "$out/lib/node_modules/mcp-remote"
      cp --recursive dist package.json README.md LICENSE "$out/lib/node_modules/mcp-remote/"
      chmod +x "$out/lib/node_modules/mcp-remote/dist/proxy.js"
      chmod +x "$out/lib/node_modules/mcp-remote/dist/client.js"
      patchShebangs "$out/lib/node_modules/mcp-remote"

      find "$out/lib" -xtype l -delete

      mkdir -p "$out/bin"
      makeWrapper "$out/lib/node_modules/mcp-remote/dist/proxy.js" \
        "$out/bin/mcp-remote" \
        --prefix PATH : "${runtimePath}"
      makeWrapper "$out/lib/node_modules/mcp-remote/dist/client.js" \
        "$out/bin/mcp-remote-client" \
        --prefix PATH : "${runtimePath}"

      runHook postInstall
    '';

    meta = {
      description = "mcp-remote OAuth bridge runtime (read-only-friendly local proxy for remote MCP servers)";
      mainProgram = "mcp-remote";
      license = pkgs.lib.licenses.mit;
    };
  });
in
mcpRemote

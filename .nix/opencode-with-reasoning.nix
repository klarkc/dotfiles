{
  pkgs,
  opencode-src,
  opencode-codex-auth-src,
}:

let
  opencodeBase = pkgs.opencode.overrideAttrs (old: {
    src = opencode-src;

    node_modules = old.node_modules.overrideAttrs (_: {
      src = opencode-src;
      outputHash = "sha256-4yjQlxN+U4CKwA/hE8gACuvA4bBeTrX0ACVBIK4UQCg=";
    });
  });

  codexAuthPlugin = pkgs.buildNpmPackage {
    pname = "opencode-openai-codex-auth";
    version = "4.4.0";
    src = opencode-codex-auth-src;

    postPatch = ''
      ${pkgs.nodejs}/bin/node -e '
        const fs = require("fs")

        function stripOverrides(path) {
          if (!fs.existsSync(path)) return
          const data = JSON.parse(fs.readFileSync(path, "utf8"))
          delete data.overrides
          if (data.packages && data.packages[""]) {
            delete data.packages[""].overrides
          }
          fs.writeFileSync(path, JSON.stringify(data, null, 2) + "\n")
        }

        stripOverrides("package.json")
        stripOverrides("package-lock.json")
      '
    '';

    npmDepsHash = "sha256-YioaOUkDoC9j/vwtQ+tlg8vBCTobM0rlbiBvqyMUNpA=";

    installPhase = ''
      runHook preInstall

      mkdir -p $out/lib/node_modules/opencode-openai-codex-auth
      cp -r \
        dist \
        package.json \
        README.md \
        LICENSE \
        node_modules \
        $out/lib/node_modules/opencode-openai-codex-auth/

      for optional_path in assets config scripts; do
        if [ -e "$optional_path" ]; then
          cp -r "$optional_path" $out/lib/node_modules/opencode-openai-codex-auth/
        fi
      done

      runHook postInstall
    '';
  };

  codexAuthPluginUrl = "file://${codexAuthPlugin}/lib/node_modules/opencode-openai-codex-auth/dist";
  injectCodexAuthPlugin = pkgs.writeText "opencode-inject-codex-auth-plugin.sh" ''
    config_dir="''${OPENCODE_CONFIG_DIR:-$HOME/.config/opencode}"
    config_path="''${OPENCODE_CONFIG:-$config_dir/opencode.json}"

    # Remove stale generated local plugin links from older versions of this wrapper.
    rm -f "$HOME/.config/opencode/plugins/openai-codex-auth"
    rm -f "$HOME/.config/opencode/plugins/openai-codex-auth.js"

    if [ -z "''${OPENCODE_CONFIG_CONTENT:-}" ] && [ -f "$config_path" ]; then
      runtime_root="''${XDG_RUNTIME_DIR:-''${TMPDIR:-/tmp}}/opencode"
      mkdir -p "$runtime_root"
      injected_config="$runtime_root/opencode-codex-auth-config.json"

      ${pkgs.python3}/bin/python3 - "$config_path" "$injected_config" "${codexAuthPluginUrl}" <<'PY'
    import json
    import sys
    from pathlib import Path

    config_path = Path(sys.argv[1])
    injected_config = Path(sys.argv[2])
    plugin_url = sys.argv[3]

    with config_path.open("r", encoding="utf-8") as handle:
        config = json.load(handle)

    plugins = config.get("plugin") or []
    if isinstance(plugins, str):
        plugins = [plugins]

    plugins = [
        plugin
        for plugin in plugins
        if plugin != "opencode-openai-codex-auth"
        and not (
            isinstance(plugin, str)
            and plugin.startswith("file:///home/")
            and plugin.endswith("/openai-codex-auth")
        )
    ]

    config["plugin"] = [plugin_url] + [plugin for plugin in plugins if plugin != plugin_url]
    injected_config.write_text(json.dumps(config, separators=(",", ":")) + "\n", encoding="utf-8")
    PY

      export OPENCODE_CONFIG="$injected_config"
    fi
  '';
in
pkgs.symlinkJoin {
  name = "${opencodeBase.name}-with-codex-auth";
  paths = [ opencodeBase ];
  nativeBuildInputs = [ pkgs.makeWrapper ];

  postBuild = ''
    wrapProgram $out/bin/opencode \
      --prefix PATH : ${
        pkgs.lib.makeBinPath [
          pkgs.bun
          pkgs.nodejs
        ]
      } \
      --run '. ${injectCodexAuthPlugin}'
  '';
}

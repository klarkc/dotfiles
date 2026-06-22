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
    if [ -z "''${OPENCODE_CONFIG_CONTENT:-}" ]; then
      config_dir="''${OPENCODE_CONFIG_DIR:-$HOME/.config/opencode}"
      config_path="''${OPENCODE_CONFIG:-$config_dir/opencode.json}"

      if [ -f "$config_path" ]; then
        export OPENCODE_CONFIG_CONTENT="$(${pkgs.python3}/bin/python3 - "$config_path" "${codexAuthPluginUrl}" <<'PY'
    import json
    import sys
    from pathlib import Path

    config_path = Path(sys.argv[1])
    plugin_url = sys.argv[2]

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
    print(json.dumps(config, separators=(",", ":")))
    PY
    )"
      fi
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

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
      node -e '
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

    npmDepsHash = pkgs.lib.fakeHash;

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

  codexAuthPluginLoader = pkgs.writeText "openai-codex-auth.js" ''
    export { default, OpenAIAuthPlugin } from "${codexAuthPlugin}/lib/node_modules/opencode-openai-codex-auth/dist/index.js";
  '';
in
pkgs.symlinkJoin {
  name = "${opencodeBase.name}-with-codex-auth";
  paths = [ opencodeBase ];
  nativeBuildInputs = [ pkgs.makeWrapper ];

  postBuild = ''
    wrapProgram $out/bin/opencode \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bun pkgs.nodejs ]} \
      --run 'mkdir -p "$HOME/.config/opencode/plugins"' \
      --run 'ln -sfn ${codexAuthPluginLoader} "$HOME/.config/opencode/plugins/openai-codex-auth.js"'
  '';
}

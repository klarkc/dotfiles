let
  version = "0.32.0";
  nixpkgs = builtins.fetchTree {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ed67bc86e84e51d4a88e73c7fd36006dc876476f";
    narHash = "sha256-62EWg6lI0qyzm7oAx5cAnGkLutvJsRBe0KkEW2JDZCE=";
  };

  pkgs = import nixpkgs {
    system = builtins.currentSystem;
  };

  fusionNpmPayload = pkgs.stdenvNoCC.mkDerivation {
    inherit version;
    pname = "fusion-npm-payload";

    nativeBuildInputs = with pkgs; [
      cacert
      nodejs
    ];

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-tBlBvgz4G2nb1ff4wEcMUmBpjHCJcTq9zxllcI7Cenk=";

    buildCommand = ''
      export HOME="$TMPDIR/home"
      export npm_config_cache="$TMPDIR/npm-cache"
      export npm_config_update_notifier=false
      export npm_config_fund=false
      export npm_config_audit=false
      export NODE_LLAMA_CPP_SKIP_DOWNLOAD=1
      export NODE_LLAMA_CPP_SKIP_DOWNLOAD_GPU=1

      mkdir -p "$HOME" "$npm_config_cache" "$out"

      npm install --global \
        --ignore-scripts \
        --prefix "$out" \
        --cache "$npm_config_cache" \
        --no-audit \
        --no-fund \
        @runfusion/fusion@${version} \
        @tobilu/qmd@2.1.0 \
        node-pty \
        dockerode \
        send
    '';
  };

  runtimePath = pkgs.lib.makeBinPath (with pkgs; [
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
  ]);
in
pkgs.stdenv.mkDerivation {
  inherit version;
  pname = "fusion-runtime";

  nativeBuildInputs = with pkgs; [
    gnumake
    nodejs
    pkg-config
    python3
  ];

  dontUnpack = true;

  installPhase = ''
    export HOME="$TMPDIR/home"
    export npm_config_cache="$TMPDIR/npm-cache"
    export npm_config_update_notifier=false
    export npm_config_fund=false
    export npm_config_audit=false
    export NODE_LLAMA_CPP_SKIP_DOWNLOAD=1
    export NODE_LLAMA_CPP_SKIP_DOWNLOAD_GPU=1
    export PYTHON="${pkgs.python3}/bin/python3"
    export npm_config_python="$PYTHON"
    export npm_config_build_from_source=true
    export npm_config_nodedir="${pkgs.nodejs}"

    mkdir -p "$HOME" "$npm_config_cache" "$out/bin" "$out/lib"
    cp -a ${fusionNpmPayload}/lib/node_modules "$out/lib/"
    chmod -R u+rwX "$out/lib/node_modules"
    patchShebangs "$out/lib/node_modules"

    npm rebuild --global \
      --prefix "$out" \
      --cache "$npm_config_cache" \
      --build-from-source \
      --no-audit \
      --no-fund

    rm -rf "$out/bin"
    mkdir -p "$out/bin"

    make_npm_bin_wrapper() {
      src="$1"
      name="$(basename "$src")"
      resolved="$(readlink -f "$src")"
      case "$resolved" in
        ${fusionNpmPayload}/lib/node_modules/*)
          target="$out/lib/node_modules/''${resolved#${fusionNpmPayload}/lib/node_modules/}"
          ;;
        *)
          echo "Unsupported npm bin target for $name: $resolved" >&2
          exit 1
          ;;
      esac

      cat > "$out/bin/$name" <<EOF
#!/bin/sh
export PATH="${runtimePath}:$out/bin:\$PATH"
export NODE_PATH="$out/lib/node_modules"
export NPM_CONFIG_PREFIX="$out"
export npm_config_prefix="$out"
export npm_config_global=true
exec "$target" "\$@"
EOF
      chmod 0755 "$out/bin/$name"
    }

    for src in ${fusionNpmPayload}/bin/*; do
      [ -e "$src" ] || continue
      make_npm_bin_wrapper "$src"
    done

    if [ ! -e "$out/bin/fusion" ] && [ -e "$out/bin/fn" ]; then
      cat > "$out/bin/fusion" <<EOF
#!/bin/sh
exec "$out/bin/fn" "\$@"
EOF
      chmod 0755 "$out/bin/fusion"
    fi

    ln -sf ${pkgs.tmux}/bin/tmux "$out/bin/tmux"

    fusion_version="$($out/bin/fusion --version)"
    if [ "$fusion_version" != "${version}" ]; then
      echo "Error: expected fusion ${version} but got $fusion_version" >&2
      echo "Bundled package manifest:" >&2
      ${pkgs.nodejs}/bin/node -e 'const p=process.argv[1]; console.error(JSON.stringify(require(p), null, 2))' "$out/lib/node_modules/@runfusion/fusion/package.json" >&2
      exit 1
    fi
    "$out/bin/qmd" --help >/dev/null
  '';
}

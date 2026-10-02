{
  lib,
  pkgs,
  stdenv,
  rustPlatform,
  ledgeur-src,
  sherpa-onnx-bin,
}:

let
  pnpm = pkgs.pnpm_10;
  gstPluginPath = lib.makeSearchPath "lib/gstreamer-1.0" [
    pkgs.gst_all_1.gstreamer.out
    pkgs.gst_all_1.gst-plugins-base
    pkgs.gst_all_1.gst-plugins-good
    pkgs.gst_all_1.gst-libav
  ];
in
rustPlatform.buildRustPackage (finalAttrs: {
  pname = "ledgeur-runtime";
  version = "1.1.2";

  src = ledgeur-src;
  patches = [ ./patches/ledgeur-local-runtime.patch ];

  # Bump note: Ledgeur is a pnpm workspace plus a Rust/Tauri core. A source
  # bump must refresh both hashes below and revalidate the local patch.
  cargoRoot = "apps/desktop/src-tauri";
  buildAndTestSubdir = finalAttrs.cargoRoot;
  cargoHash = "sha256-OYfWw7fGNSDQtNrLGqA+KIOdYaRrcl9IWAswxx0grR4=";

  pnpmDeps = pkgs.fetchPnpmDeps {
    inherit pnpm;
    pname = "ledgeur-runtime";
    inherit (finalAttrs) version src;
    fetcherVersion = 4;
    hash = "sha256-2I8PEZjgi7HsI7+RnnOwLihE7g/4IYnKYazqoViSTs0=";
  };

  __structuredAttrs = true;
  strictDeps = true;

  env = {
    # sherpa-rs is patched to disable its network-downloading build feature.
    SHERPA_LIB_PATH = "${sherpa-onnx-bin}";
    SHERPA_BUILD_SHARED_LIBS = "1";

    # Native whisper.cpp should not consume the RTX used by vLLM.
    LEDGEUR_WHISPER_LANGUAGE = "pt";

    # Build the React frontend pointed at the already-running local vLLM.
    # The patch discovers the currently served model from /v1/models.
    VITE_LOCAL_LLM_URL = "http://127.0.0.1:8000/v1";
    VITE_PREFER_EXTERNAL_LLM = "1";
  };

  nativeBuildInputs = with pkgs; [
    cmake
    nodejs
    pkg-config
    pnpm
    pnpmConfigHook
    rustPlatform.bindgenHook
    writableTmpDirAsHomeHook
    wrapGAppsHook4
  ];

  buildInputs = with pkgs; [
    glib
    gst_all_1.gstreamer.out
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-libav
    gtk3
    libsoup_3
    openssl
    webkitgtk_4_1
  ];

  buildFeatures = [
    "custom-protocol"
    "native-ai"
  ];

  # cargo alone does not run Tauri's beforeBuildCommand. Build the Vite
  # frontend explicitly from the already-populated offline pnpm store.
  preBuild = ''
    pnpm --filter @ledgeur/desktop build
  '';

  doCheck = false;

  installPhase = ''
        runHook preInstall

        install -Dm755       "target/${stdenv.hostPlatform.rust.rustcTarget}/release/ledgeur"       "$out/bin/ledgeur"

        install -Dm644       "apps/desktop/src-tauri/icons/128x128.png"       "$out/share/icons/hicolor/128x128/apps/ledgeur.png"

        mkdir -p "$out/share/applications"
        cat > "$out/share/applications/ledgeur.desktop" <<'EOF'
    [Desktop Entry]
    Type=Application
    Name=Ledgeur
    Comment=Private local meeting transcription and notes
    Exec=ledgeur
    Icon=ledgeur
    Terminal=false
    Categories=Office;AudioVideo;
    EOF

        runHook postInstall
  '';

  preFixup = ''
    # Keep sherpa's shared runtime reachable after installation and avoid the
    # WebKit/NVIDIA dmabuf path that is fragile under Nix wrappers.
    gappsWrapperArgs+=(
      --prefix LD_LIBRARY_PATH : "${sherpa-onnx-bin}/lib"
      --set GST_PLUGIN_SYSTEM_PATH_1_0 "${gstPluginPath}"
      --set GST_PLUGIN_PATH_1_0 "${gstPluginPath}"
      --set-default LEDGEUR_WHISPER_LANGUAGE "pt"
      --set-default WEBKIT_DISABLE_DMABUF_RENDERER "1"
    )
  '';

  meta = {
    description = "Ledgeur desktop meeting notes with native PT Whisper and local vLLM";
    homepage = "https://github.com/maxbeech/ledgeur";
    license = lib.licenses.mit;
    platforms = [ "x86_64-linux" ];
    mainProgram = "ledgeur";
  };
})

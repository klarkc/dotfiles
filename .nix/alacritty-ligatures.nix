{
  pkgs,
  alacritty-ligatures-src,
  alacrittyDeps,
}:

pkgs.rustPlatform.buildRustPackage {
  pname = "alacritty";
  version = "0.14.0-dev-ligatures";

  src = alacritty-ligatures-src;

  cargoLock.lockFile = "${alacritty-ligatures-src}/Cargo.lock";

  nativeBuildInputs = alacrittyDeps.nativeBuildInputs;
  buildInputs = alacrittyDeps.buildInputs;

  buildAndTestSubdir = "alacritty";

  postInstall = ''
    install -Dm644 extra/logo/alacritty-term.svg "$out/share/pixmaps/Alacritty.svg"
    install -Dm644 extra/linux/Alacritty.desktop "$out/share/applications/Alacritty.desktop"
    install -Dm644 extra/completions/alacritty.bash "$out/share/bash-completion/completions/alacritty"
    install -Dm644 extra/completions/_alacritty "$out/share/zsh/site-functions/_alacritty"
    install -Dm644 extra/completions/alacritty.fish "$out/share/fish/vendor_completions.d/alacritty.fish"
    wrapProgram "$out/bin/alacritty" \
      --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath alacrittyDeps.runtimeLibs}"
  '';

  meta = with pkgs.lib; {
    description = "Alacritty terminal built from the ligature fork";
    homepage = "https://github.com/ink-splatters/alacritty-ligatures";
    license = licenses.asl20;
    mainProgram = "alacritty";
  };
}

{ pkgs, ratty-src }:

pkgs.rustPlatform.buildRustPackage {
  pname = "ratty";
  version = "0.4.1-ligatures";

  src = ratty-src;

  patches = [ ../patches/ratty-enable-ligatures.patch ];

  cargoLock.lockFile = "${ratty-src}/Cargo.lock";

  nativeBuildInputs = with pkgs; [
    pkg-config
  ];

  buildInputs = with pkgs; [
    fontconfig
    libxkbcommon
    wayland
    xorg.libX11
    xorg.libXcursor
    xorg.libXi
    xorg.libXrandr
  ];

  meta = with pkgs.lib; {
    description = "Ratty terminal with local ligature support patch";
    homepage = "https://github.com/orhun/ratty";
    license = licenses.mit;
    mainProgram = "ratty";
  };
}

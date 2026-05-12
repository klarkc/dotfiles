let
  nixpkgs = builtins.fetchTree {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ed67bc86e84e51d4a88e73c7fd36006dc876476f";
    narHash = "sha256-62EWg6lI0qyzm7oAx5cAnGkLutvJsRBe0KkEW2JDZCE=";
  };

  pkgs = import nixpkgs {
    system = builtins.currentSystem;
    config.allowUnfree = true;
  };

  stdenv = if pkgs.cudaPackages ? backendStdenv then pkgs.cudaPackages.backendStdenv else pkgs.stdenv;

  src = builtins.fetchGit {
    url = "https://github.com/turbo-tan/llama.cpp-tq3";
    rev = "c87aeb58b7eee5d5b67382f1d4860976baced29a";
  };
in
stdenv.mkDerivation {
  pname = "llama-cpp-tq3-cuda-kvunified";
  version = "c87aeb58b7eee5d5b67382f1d4860976baced29a";
  inherit src;

  nativeBuildInputs = with pkgs; [
    cmake
    ninja
    pkg-config
    cudaPackages.cuda_nvcc
  ];

  buildInputs = with pkgs; [
    cudaPackages.cuda_cudart
    cudaPackages.cuda_cccl
    cudaPackages.libcublas
    curl
    zlib
    openssl
    brotli
    zstd
    nghttp2
    libssh2
  ];

  cmakeFlags = [
    "-DGGML_CUDA=ON"
    "-DGGML_CUDA_GRAPHS=OFF"
    "-DCMAKE_CUDA_ARCHITECTURES=86"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DLLAMA_BUILD_TESTS=OFF"
  ];

  meta = {
    mainProgram = "llama-server";
  };
}

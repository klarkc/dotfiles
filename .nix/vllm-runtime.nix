{ pkgs, version }:
# Bump note: thin runtime adapter over nixpkgs `python312Packages.vllm`.
# Earlier versions of this file shipped a pip/wheelhouse build of vLLM 0.20.1
# with torch 2.11.0+cu130; that violated the repo-maintained Nix-build
# determinism invariant (live `pip download` / `pip install` inside Nix code)
# and the resulting wheel runtime required broad `LD_LIBRARY_PATH` scanning to
# find Python/CUDA shared libraries. Per AGENTS runtime-linker policy, prefer
# proper Nix packaging with normal RPATH/RUNPATH/fixup handling. The vLLM
# runtime is now consumed from a CUDA-enabled nixpkgs instance supplied by
# `flake.nix` (currently `inputs.nixpkgs-vllm = github:NixOS/nixpkgs/master`
# for an isolated experiment; collapse back to `inputs.nixpkgs` once
# `nixpkgs-unstable` ships a vLLM that passes the 35B A3B GPU benchmark
# acceptance). No `pip download`/`pip install` lives in this file anymore.
pkgs.symlinkJoin {
  name = "vllm-runtime-${version}";

  paths = [ pkgs.python312Packages.vllm ];

  postBuild = ''
    if [ ! -x "$out/bin/vllm" ]; then
      echo "vllm-runtime: expected $out/bin/vllm after symlinkJoin; not found" >&2
      exit 1
    fi
  '';

  meta = {
    description = "vLLM runtime adapter (thin wrapper over nixpkgs python312Packages.vllm)";
    mainProgram = "vllm";
  };
}

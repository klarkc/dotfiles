# Agent Notes

- Read `README.md` first for repo context and usage.
- This repository is checked out directly as `$HOME`. Keep repository-support files hidden from the user's normal home listing.
- Repo-internal support directories must use dot paths, for example `.nix` for Nix expressions and `.patches` for patch files.
- Allowed root-level exceptions are conventional project files such as `README.md`, `AGENTS.md`, `CHANGELOG.md`, `Makefile`, `flake.nix`, `flake.lock`, and root dotfiles such as `.profile`, `.vimrc`, `.tmux.conf`, and `.alacritty.toml`.
- Do not add visible root directories such as `patches/`, `nix/`, `scripts/`, or `docs/` unless the user explicitly asks for them.

## Adding Python packages from GitHub

- Use raw git flake inputs (with pinned versions/tags) and build via `buildPythonPackage`, do not use `fetchFromGitHub`.
- Pin to a tag or commit in the flake input URL, e.g.:
  ```nix
  inputs = {
    ...
    llm-github-models.url = "github:tonybaloney/llm-github-models/v0.18.0";
    apm.url = "github:microsoft/apm/v0.15.0";
  };
  ```
- Then use the flake input directly as source:
  ```nix
  apm-pkg = pkgs.python3Packages.buildPythonApplication {
    pname = "apm";
    version = "0.15.0";
    src = inputs.apm;
    pyproject = true;
  };
  ```

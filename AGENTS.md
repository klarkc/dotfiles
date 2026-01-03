# Repository Guidelines

## Project Structure & Module Organization
- This is a dotfiles repository rooted at the home directory layout.
- Core configuration lives in `.config/` (window manager, terminal, system/user services).
- Shell and editor configs are in top-level dotfiles such as `.bashrc`, `.vimrc`, `.tmux.conf`, and `.gitconfig`.
- Scripts and helpers live in `.local/bin/` and `bin/`.
- Nix/flake setup is at `flake.nix` and `flake.lock`.
- Installation and setup automation is in `Makefile` and `.github/` (if CI is added later).

## Build, Test, and Development Commands
- `make`: installs key themes, icons, vim plugins, tmux plugins, and Nix profile (see `Makefile`).
- `make clean`: removes installed themes, icons, plugins, and symlinks.
- `nix --extra-experimental-features "nix-command flakes" profile install .`: installs the flake profile (also run by `make`).
- `systemctl --user daemon-reload`: reloads user services after installing systemd units.

## Coding Style & Naming Conventions
- Indentation is tabs by default; YAML uses spaces (see `.editorconfig`).
- Keep filenames consistent with their targets (e.g., `.dotfile_override` for local overrides).
- Prefer descriptive, lower-case names for scripts in `.local/bin/`.

## Testing Guidelines
- There are no automated tests in this repository. Validate changes manually:
  - Run `make` or `nix profile install .`.
  - Restart or reload relevant services (e.g., `systemctl --user daemon-reload`).

## Commit & Pull Request Guidelines
- Commit messages follow Conventional Commits: `feat(scope): …`, `chore: …`, `fix(scope): …`.
- Prefer small, focused commits that touch one area (e.g., `feat(vim): …`).
- PRs should include a short summary, installation or validation steps, and any platform-specific notes.

## Security & Configuration Tips
- Do not commit secrets, tokens, or machine-specific credentials.
- Use override files (e.g., `.dotfile_override`) for local, private, or machine-specific tweaks.

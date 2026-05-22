# Dotfiles — AI Agent Instructions

## Repository Overview

This is a Linux development environment dotfiles repository for Arch Linux. It manages system configuration, AI tools, and environment setup using Nix flakes.

## Architecture

- **Single-repo, multi-branch**: Each machine gets its own branch (e.g., `ssdinarch`, `sol098`). The `sol098` branch is the default (work machine).
- **Nix-based**: The entire profile is managed via `flake.nix` and `nix profile install .`
- **Makefile-driven setup**: `make` handles all package installs, theme installs, and plugin downloads.
- **Override system**: Files named `.dotfile_override` can override corresponding `.dotfile` files.

## Key Directories

- `/.config/` — Application configuration files (XMonad, git, shell, vLLM, etc.)
- `/.local/bin/` — Custom shell scripts (vLLM tools, backups, cleanup)
- `/.codex/` — OpenAI Codex configuration
- `/.config/crush/` — Charm Bracelet Crush AI agent config
- `/.config/systemd/` — User systemd services and timers
- `/docs/` — Documentation files
- `/.config/nix/` — Nix-specific configs (Fusion package, vLLM runtime)

## Build & Install

```bash
# Install Nix profile
nix --extra-experimental-features "nix-command flakes" profile install .

# Run Make targets
make                          # Full setup
make nix.Profile              # Nix profile only
make git.Config               # Git configuration
make vim.PluginInstall        # Vim plugin install
make tmux.TpmInstall          # Tmux TPM install

# Clean
make clean                    # Remove installed assets
```

## AI Tools

- **Codex** (`~/.codex/config.toml`): OpenAI Codex configuration with sandbox modes
- **Crush** (`~/.config/crush/crush.json`): Charm Bracelet Crush — local LLM agent config with vLLM and llama.cpp providers
- **vLLM**: Local LLM serving via `~/.local/bin/vllm-*` scripts (vllm-config, vllm-benchmark, vllm-serve-pure, vllm-wait-ready)
- **AgentRC**: AI readiness measurement via `agentrc` commands (see Makefile)

## vLLM Setup

Local LLM targets are managed via systemd user targets:
- `vllm-qwen3.6-35B-a3b.target` — Primary model
- `vllm-qwen3.6-27B.target` — Experimental/secondary model

Configure with `vllm-config` or `vllm-config <model-id>`.

## Fusion Integration

Fusion is packaged via `.config/nix/fusion-npm.nix` and run as a systemd user service (`fusion.service`). Access the dashboard at `http://127.0.0.1:4040`. Use `fusion-attach` to view logs.

## Commit Guidelines

See `COMMIT_GUIDELINES.md` for commit message format.

## Safety Notes

- **Backup first** before running `git checkout main` — this replaces home files.
- Do NOT merge PRs without review.
- Do NOT make architectural decisions — escalate to CTO.

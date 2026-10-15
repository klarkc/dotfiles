# Dotfiles

Optioned Linux for a dev environment

```bash
git clone --no-checkout https://github.com/klarkc/dotfiles
mv dotfiles/.git ~
rm -r dotfiles
```

⚠ Next command will replace current home files with repo files (backup first!)

```bash
git checkout main
```

## Features

- [Lemurs](https://github.com/coastalwhite/lemurs) Window Manager
- [Pipewire](https://pipewire.org) Sound Server
- [Alacritty](https://github.com/alacritty/alacritty) with ligatures + [Tmux](https://github.com/tmux/tmux) + [vim](https://github.com/vim/vim)
- [Vim Language Server Protocol](https://github.com/prabirshrestha/vim-lsp) + [Automatic Servers](https://github.com/mattn/vim-lsp-settings)
- [Nord Theme](https://www.nordtheme.com/) for the desktop; Alacritty uses its default color theme
- [Fira Code](https://github.com/tonsky/FiraCode) with ligatures support
- [LSD](https://github.com/Peltoche/lsd) replaces `ls` with the modern `lsd` alternative
- CapsLock as Escape
- Ctrl+ç as Ctrl+b through keyd, while ç remains Unicode without Ctrl
- Using [satty](https://github.com/gabm/Satty) with [scrot](https://github.com/resurrecting-open-source-projects/scrot) for annotated screenshots
- [OpenCode](https://github.com/anomalyco/opencode) as the coding agent (Codex CLI retained only for ChatGPT OAuth login)
- [kolu](https://github.com/juspay/kolu) for coding agent orchestration

## Machines

Each machine has specific configurations and enabled features so I'm splitting it between different branches.

- `ssdinarch`: my personal laptop
- `main` (default): work machine

### Aliases

- `l`: `ls -l`
- `ll`: `ls -la`
- `lt`: `ls -lt`

## Optional Features

- Maintenance [systemd scripts](https://github.com/klarkc/dotfiles/tree/main/.config/systemd/user) and [upstream dependency monitoring](.local/bin/deps-check)
- [Sunshine](https://github.com/LizardByte/Sunshine) game streaming server (to play games on my TV)
- [Handlr](https://github.com/Anomalocaridid/handlr-regex) manage default apps
- AI models with [ollama](https://ollama.com/)
- AI coding with OpenCode (and optional Lumen); Codex is not an active coding agent
- [ZeroClaw](https://github.com/zeroclaw-labs/zeroclaw) with Landlock-confined tools and a hardened user service

## Supported setups

Below are the supported distro setups

### Arch Linux with NVidia

#### Dependencies

```bash
pacman -Syu yay
yay -Syu openssh pwvucontrol pipewire pipewire-audio pipewire-pulse pipewire-alsa git git-lfs gvim qt5-styleplugins nix ttf-fira-code noto-fonts-emoji lsd dconf-editor picom xorg-xmodmap xclip keyd dunst libnotify lemurs haskell-language-server xmonad xmonad-contrib xorg-xsetroot xorg-xset xorg-xmessage feh the_silver_searcher satty scrot wget xorg-server taffybar libappindicator-gtk3 blueman dmenu sword i3lock xss-lock
sudo chmod +s .local/bin/pacman-*
systemctl enable --now nix-daemon.socket
systemctl enable --now lemurs
```

Alacritty is installed by the Nix profile from the `ink-splatters/alacritty-ligatures` fork, so it is intentionally not installed from pacman/yay here.

> Below dependencies are not mandatory but highly recommended

```bash
yay -Syu ffmpeg unzip htop
```

> Below dependencies are not mandatory (see [Optional Features](#optional-features))

```bash
yay -Syu snapper pacreport yay-cache-cleanup-hook sunshine bat git-delta ripgrep handlr ollama-cuda discord enpass-bin btdu btop
```

> Lumen

```bash
yay -Syu lumen
yay --asdeps fzf mdcat
```

> Yazi as file manager with optional deps

```bash
yay -Syu yazi
yay -S --asdeps 7zip jq poppler fd fzf zoxide resvg imagemagick xclip xsel chafa
```

#### Installation

```bash
nix profile install .
make
systemctl --user daemon-reload
```

#### Verification (`make test`)

`make test` is the standard verification entrypoint for this repo. It runs `nix flake check` (static config + no-network self-tests) followed by every `.local/bin/*-smoke-test` script (out-of-band live checks that require network and user secrets).

Smoke tests are enabled by default and can be disabled with `SMOKE_TESTS_ENABLED=false` (or the legacy `SKIP_SMOKE=1` alias) so static checks can run without sourcing `~/.profile_override`.

```bash
# Local developer: full run with smoke tests
make test

# CI / static-only run
SMOKE_TESTS_ENABLED=false make test
# or
make test SMOKE_TESTS_ENABLED=false
# backwards-compatible alias
SKIP_SMOKE=1 make test
```

Use `make fmt` separately if you want to apply formatting locally; CI does not auto-format.

The CI workflow at `.github/workflows/test.yml` invokes `SMOKE_TESTS_ENABLED=false make test` instead of bare `nix flake check`.

Smoke tests follow the repo convention `.local/bin/*-smoke-test`. They are out-of-band: they need network access and user secrets, must never be added to `nix flake check`, and must never print tokens, Authorization headers, or generated Basic base64 strings.

The repo ships three live smoke tests:

- `atlassian-smoke-test`: Rovo MCP connectivity, Jira and Confluence resources, and available Atlassian tools.
- `bitbucket-read-mcp-smoke-test`: real PR listing, metadata, diff and comments through the read-only MCP. The generic `api-token` pass tests Rovo; the `oauth` pass tests the independent Bitbucket REST fallback.
- `coding-agents-smoke-test`: invokes **OpenCode only** and verifies a real Bitbucket PR MCP call (optional full mode also verifies Jira and Confluence). Codex is deliberately not probed. To override a slow default model, set `CODING_AGENTS_MODEL=openai/gpt-5.6-sol` if authenticated.

#### Bitbucket PRs (read-only MCP)

OpenCode's `bitbucket_read` local MCP is backed by `.local/bin/bitbucket-read-mcp`. It exposes **only** `get` / `list` for repositories and `list` / `get` / `diff` / `comments` / `listTasks` for pull requests. Write attempts fail before any network request. The remote `atlassian` MCP remains available for Jira and Confluence OAuth.

The bridge prefers the official Rovo MCP with `ATLASSIAN_USER_EMAIL` + `ATLASSIAN_MCP_TOKEN`, falling back to Bitbucket REST with `SOLO098_ATLASSIAN_USER_EMAIL` + `SOLO098_ATLASSIAN_BB_READ_TOKEN`. It also accepts the generic runtime alias `BITBUCKET_READ_TOKEN`. Existing variables are loaded from `~/.profile_override`; **never commit secrets, materialize them in Nix evaluation, or configure MCP Authorization headers in tracked JSON**. No new secret or token is necessary.

```bash
# Offline safety/contract checks
~/.local/bin/bitbucket-read-mcp --selftest

# Real read tests, no agent or write API calls
~/.local/bin/bitbucket-read-mcp-smoke-test rovo
~/.local/bin/bitbucket-read-mcp-smoke-test rest

# OpenCode-mediated PR test
CODING_AGENTS_MODEL=openai/gpt-5.6-sol ~/.local/bin/coding-agents-smoke-test api-token
# Optional deeper agent-mediated Jira/Confluence check:
CODING_AGENTS_MODEL=openai/gpt-5.6-sol CODING_AGENTS_TIMEOUT_SECONDS=360 ~/.local/bin/coding-agents-smoke-test full
```

Use `BITBUCKET_READ_BACKEND=rovo` or `rest` to force a backend for diagnosis (default `auto`). PR write guidance stays manual through `.agents/skills/atlassian-write-human-guidance/SKILL.md`.

#### OpenCode + Codex OAuth (Codex is an authentication helper only)

The `codex` executable remains in the Nix profile **only** to perform or renew ChatGPT web OAuth login. It is not configured as a separate coding agent, MCP client, reviewer, or smoke-test target. The tracked `.codex/config.toml` and Codex-specific Atlassian MCP have been retired. Preserve the private runtime `~/.codex/auth.json`: it is still consumed by OpenCode's OAuth import wrapper.

`opencode` uses OpenCode's native auth store at `~/.local/share/opencode/auth.json`. The wrapped `opencode` command lazily syncs Codex OAuth before launch when `~/.codex/auth.json` exists and is newer than the OpenCode auth file, or when the OpenCode auth file does not exist yet.

Run the sync explicitly after authenticating Codex or when rotating accounts:

```bash
opencode-codex-auth-import
```

Verify the imported OpenAI provider auth:

```bash
jq '.openai | {type, has_access: has("access"), has_refresh: has("refresh"), has_accountId: has("accountId"), expires}' ~/.local/share/opencode/auth.json
```

Install the Lemurs XMonad session wrapper after checkout. It starts XMonad with a valid D-Bus session when needed, imports the graphical environment into D-Bus and systemd user activation, and starts a notification daemon if one is installed.

```bash
sudo install -Dm755 .local/bin/xmonad-session /etc/lemurs/wms/xmonad
```

Test desktop notifications after logging in again:

```bash
notify-send "dotfiles" "desktop notifications work"
```

Configure the system-wide keyd remap so `ç` remains the regular Unicode character, while `Ctrl+ç` is emitted as `Ctrl+b` for tmux and terminal shortcuts. This requires sudo because the keyd config lives in `/etc/keyd` and the daemon runs system-wide.

```bash
sudo install -d /etc/keyd
sudo tee /etc/keyd/default.conf >/dev/null <<'EOF'
[ids]
*

[control]
semicolon = C-b
EOF
sudo keyd check
sudo systemctl enable --now keyd
sudo keyd reload
```

If the `ç` key is not reported as `semicolon` on a machine, check it with:

```bash
sudo keyd monitor
```

> Below steps are not mandatory (see [Optional Features](#optional-features))

```bash
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable pacreport.timer
systemctl --user enable --now sunshine.service
systemctl --user enable --now kolu
```

#### ZeroClaw

ZeroClaw is pinned to an upstream release in `flake.nix` and built with the
`sandbox-landlock` feature. The daemon keeps normal host networking; Landlock
is the filesystem boundary for agent tool subprocesses.

`~/.zeroclaw/config.toml` is versioned as the declarative, secret-free runtime
configuration. Provider and channel secrets must not be written to that file;
they come from the ignored `~/.profile_override` through `llm-config`. After
upgrading the profile, migrate old schemas with the pinned ZeroClaw binary and
review the resulting diff before committing it. The default risk profile keeps
`sandbox_enabled = true`, `sandbox_backend = "landlock"`,
`workspace_only = true`, and an empty `shell_env_passthrough`.

`llm-config` owns the private systemd user-manager environment. It reads
`VLLM_API_KEY` and, when present, `DISCORD_BOT` from the ignored
`~/.profile_override`, imports only the variables needed by vLLM/ZeroClaw, and
never teaches the generic `.profile` about provider-specific secret names.
`llm-environment.service` calls `llm-config --restore-environment` when the user
manager starts, because `systemctl --user import-environment` state does not
survive a reboot. The tracked config contains no Discord token, so hosts that
enable the Discord channel must provide `DISCORD_BOT` in `.profile_override`.

```bash
nix profile install --profile ~/.local/state/nix/profiles/zeroclaw .#zeroclaw
~/.local/state/nix/profiles/zeroclaw/bin/zeroclaw config migrate
systemctl --user daemon-reload
llm-config
systemctl --user enable --now llm-environment.service zeroclaw.service
```

The unit applies host-level systemd hardening without `ProtectHome` or network
isolation, leaving per-tool filesystem confinement to Landlock. Enable user
lingering separately if the daemon must start before interactive login.

The local ZeroClaw package carries a narrow patch that exposes the runtime's
existing read-only root tier to risk profiles. This lets Nix work without
making all supporting system paths writable in Landlock. The intended grants
are `/nix/store` as the executable/read-write tier (the Nix store's own DAC
still prevents user writes), plus read-only access to the daemon socket,
`/etc/nix/sentry-endpoint`, `/proc/meminfo`, and `/proc/stat`. Both `nix` and
`nix-collect-garbage` are explicitly allowlisted commands.

To expose a user service running HTTPS on port `4443` through local port `443`,
allow user processes to bind ports down to `443` once at the system level:

```bash
sudo install -Dm644 /dev/stdin /etc/sysctl.d/99-unprivileged-ports.conf <<'EOF'
net.ipv4.ip_unprivileged_port_start=443
EOF
sudo sysctl --system
```

Then enable the socket-activated user proxy:

```bash
systemctl --user enable --now https-proxy.socket
```

#### vLLM

The local vLLM workflow is target-based. Only one vLLM model target should run at a time:

- `vllm-qwen3.6-35B-a3b.target` starts `vllm@qwen3.6-35B-a3b.service`
- `vllm-qwen3.6-27B.target` starts `vllm@qwen3.6-27B.service`

Use `llm-config` to choose the active local model. It stops all vLLM units, disables the non-selected target, enables the selected target for future user-session starts, starts the selected target, and follows its journal until `vllm@...service` is active.

The vLLM service patches the local opencode default, starts the model, and waits for `GET /v1/models` to respond with the selected served model before systemd marks the service ready.

Pick the model interactively:

```bash
llm-config
```

Or switch directly:

```bash
llm-config qwen3.6-35B-a3b
llm-config qwen3.6-27B
```

Verify which target will start with the user systemd session:

```bash
systemctl --user is-enabled vllm-qwen3.6-35B-a3b.target
systemctl --user is-enabled vllm-qwen3.6-27B.target
```

The selected target should be `enabled`; the other targets should be `disabled`. The legacy single-model `vllm.service` is obsolete; `llm-config` stops and disables it when switching models.

User systemd services start when the user manager starts. To start the selected vLLM target after reboot before an interactive login, enable lingering once:

```bash
loginctl show-user "$USER" -p Linger
sudo loginctl enable-linger "$USER"
```

After lingering is enabled, re-check:

```bash
loginctl show-user "$USER" -p Linger
```

Expected:

```text
Linger=yes
```

Watch startup progress:

```bash
journalctl --user-unit vllm-qwen3.6-35B-a3b.target -f
journalctl --user-unit vllm@qwen3.6-35B-a3b.service -f
```

Run the maintained vLLM benchmark wrapper against the active target:

```bash
vllm-benchmark
```

Benchmark artifacts are written below `~/.cache/vllm-benchmarks/`.

For the 27B target, replace `35B-a3b` with `27B` in the commands above.

#### Generic archive pack (`archive-pack`)

The `archive-pack` and `archive-pack-test` commands are provided by the Nix flake
(`.nix/backup-tools.nix`) and installed into the user profile by `nix profile install .`.

Inputs live in `~/.backup/`:

- Any file or directory — archives (`*.tar`, `*.tar.gz`, `*.tar.bz2`, `*.tar.xz`,
  `*.tar.zst`, `*.tar.lz`, `*.zip`, `*.7z`) get extracted and their contents
  stored.
- Plain files are stored as-is.
- Directories are stored recursively.

Excluded by default: `stage/`, `logs/`, `manifest/`, `archive.lrz`,
`archive.lrz.SUMMARY.txt`, `archive-*.lrz`, `.gitignore`, `README.md`. Use
`--exclude GLOB` to add more.

Outputs:

- `~/.backup/archive.lrz` — single deduplicated archive (`lrzip`).
- `~/.backup/archive-YYYYMMDD-HHMMSS.lrz` — datestamped snapshots when `--retain > 0`.
- `~/.backup/archive.lrz.SUMMARY.txt` — generation report.
- `~/.backup/stage/` — staging directory (visible, kept unless `--clean-temp`).
- `~/.backup/logs/` — log directory (visible, kept unless `--clean-temp`).
- `~/.backup/manifest/` — list of files inside the archive.

The script is **append-only**: re-pack merges the previous archive with new
sources. Files removed from `~/.backup/` stay in the archive; lrzip dedups
identical content across all files.

Install/upgrade:

```bash
nix profile install .
# or
nix profile upgrade klarkc
```

Run with safe defaults (≈8 threads, ≈8 GB RAM cap, window 2 GB, level 6):

```bash
archive-pack
```

Tune resources:

```bash
archive-pack --threads 4 --maxram 40 --window 10 --level 6
```

Dry run:

```bash
archive-pack --dry-run
```

Skip per-file integrity check:

```bash
archive-pack --skip-source-integrity
```

Keep original archive files alongside their extracted contents:

```bash
archive-pack --keep-archives
```

Exclude additional paths (repeatable):

```bash
archive-pack --exclude '*.tmp' --exclude 'cache/'
```

Snapshot retention (default: keep last 5 datestamped archives):

```bash
archive-pack --retain 5
archive-pack --retain-days 30
```

Clean temporary staging and logs after a successful pack:

```bash
archive-pack --clean-temp
```

Clean original sources (everything in `~/.backup/` except the built-in
excludes and any `--exclude` patterns) after a successful pack and `lrzip -t`:

```bash
archive-pack --clean-source
```

Combine both:

```bash
archive-pack --clean-temp --clean-source
```

Verify the archive integrity without repacking:

```bash
archive-pack --verify
```

The final `archive.lrz` is **never** removed by the script.

Run the self-test (synthetic backup with append-only check):

```bash
archive-pack-test
# or, via flake check
nix flake check
```

`nix flake check` runs the formatting check, pre-commit checks and
`archive-pack-test` as part of `checks.archive-pack-test`.

How to extract later (no Nix required at extraction time):

```bash
lrzip -t ~/.backup/archive.lrz          # verify integrity
lrzip -d -o - ~/.backup/archive.lrz     # produces .tar on stdout
lrzip -d ~/.backup/archive.lrz          # produces archive.tar
mkdir -p restored && tar -xf archive.tar -C restored
```

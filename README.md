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
- [Alacritty](https://github.com/alacritty/alacritty) + [Tmux](https://github.com/tmux/tmux) + [vim](https://github.com/vim/vim)
- [Vim Language Server Protocol](https://github.com/prabirshrestha/vim-lsp) + [Automatic Servers](https://github.com/mattn/vim-lsp-settings)
- [Nord Theme](https://www.nordtheme.com/)
- [Fira Code](https://github.com/tonsky/FiraCode) with ligatures support
- [LSD](https://github.com/Peltoche/lsd) replaces `ls` with the modern `lsd` alternative
- CapsLock as Escape
- Using [satty](https://github.com/gabm/Satty) with [scrot](https://github.com/resurrecting-open-source-projects/scrot) for annotated screenshots

## Machines

Each machine has specific configurations and enabled features so I'm splitting it between different branches.

- `ssdinarch`: my personal laptop
- `sol098` (default): work machine

### Aliases

- `l`: `ls -l`
- `ll`: `ls -la`
- `lt`: `ls -lt`

## Optional Features

- Maintenance [systemd scripts](https://github.com/klarkc/dotfiles/tree/main/.config/systemd/user)
- [Sunshine](https://github.com/LizardByte/Sunshine) game streaming server (to play games on my TV)
- [Handlr](https://github.com/Anomalocaridid/handlr-regex) manage default apps
- AI models with [ollama](https://ollama.com/)
- AI Agents with [crush](https://github.com/charmbracelet/crush), [codex](https://github.com/openai/codex) and [lumen](https://github.com/jnsahaj/lumen)
- [Fusion](https://github.com/Runfusion/Fusion)

## Supported setups

Below are the supported distro setups

### Arch Linux with NVidia

#### Dependencies

```bash
pacman -Syu yay
# TODO re-enable alacritty-ligature-git
yay -Syu openssh pwvucontrol pipewire pipewire-audio pipewire-pulse pipewire-alsa git git-lfs gvim alacritty qt5-styleplugins nix ttf-fira-code noto-fonts-emoji lsd dconf-editor picom xorg-xmodmap notification-daemon lemurs haskell-language-server xmonad xmonad-contrib xorg-xsetroot feh the_silver_searcher satty scrot wget xorg-server taffybar libappindicator-gtk3 blueman dmenu sword
sudo chmod +s .local/bin/pacman-*
systemctl enable --now nix-daemon.socket
systemctl enable --now lemurs
```

> Below dependencies are not mandatory but highly recommended
```bash
yay -Syu ffmpeg unzip htop
```

> Below dependencies are not mandatory (see [Optional Features](#optional-features))

```bash
yay -Syu snapper pacreport yay-cache-cleanup-hook sunshine bat git-delta ripgrep handlr ollama-cuda discord enpass-bin crush btdu
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

Fusion's systemd service uses `~/.fusion/ssh_config` for Git SSH operations. Keep that file generated when OpenSSH configuration changes so sandboxed Fusion can read a stable SSH config.

Install the pacman hook so this generated SSH config is refreshed after `openssh` or `systemd` package updates:

```bash
sudo install -Dm644 .local/share/pacman/hooks/fusion-ssh-config.hook /etc/pacman.d/hooks/fusion-ssh-config.hook
```

> Below steps are not mandatory (see [Optional Features](#optional-features))

```bash
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable pacreport.timer
systemctl --user enable --now sunshine.service
systemctl --user enable --now fusion-backup.timer
```

#### vLLM + Fusion

The vLLM/Fusion workflow is target-based. Only one vLLM model target should run at a time:

- `vllm-qwen3.6-35B-a3b.target` starts `vllm@qwen3.6-35B-a3b.service`
- `vllm-qwen3.6-27B.target` starts `vllm@qwen3.6-27B.service`

Use `vllm-config` to choose the active local model. It stops Fusion and all vLLM units, disables the non-selected target, enables the selected target for future user-session starts, starts the selected target, and follows the relevant journal logs until `vllm@...service` and `fusion.service` are active.

The target starts only the selected vLLM service. The vLLM service then patches local Crush/Fusion/Pi defaults, starts the model, waits for `GET /v1/models` to respond with the selected served model, and only then restarts Fusion so it rereads changed config files. Fusion is intentionally not pulled directly by the target; readiness is owned by `vllm@...service`.

Pick the model interactively:

```bash
vllm-config
```

Or switch directly:

```bash
vllm-config qwen3.6-35B-a3b
vllm-config qwen3.6-27B
```

Verify which target will start with the user systemd session:

```bash
systemctl --user is-enabled vllm-qwen3.6-35B-a3b.target
systemctl --user is-enabled vllm-qwen3.6-27B.target
```

The selected target should be `enabled`; the other target should be `disabled`.

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
journalctl --user-unit fusion.service -f
```

For the 27B target, replace `35B-a3b` with `27B` in the commands above.

Both model configurations use at least 48k context and 2 parallel sequences. The current default local winner is `qwen3.6-35B-a3b`; `qwen3.6-27B` is kept available as an experimental/secondary target.

Run the benchmark wrapper instead of invoking vLLM's Python module directly:

```bash
vllm-benchmark
```

`vllm-benchmark` reads the selected model from the latest `~/.cache/vllm-*/benchmark.env`, uses the pinned Nix vLLM runtime, caps benchmark concurrency at 2, and runs small, medium, and long-context random request cases. It writes logs, summaries, systemd status, vLLM/Fusion journals, runtime metadata, process state, and GPU state to a timestamped tarball under:

```bash
~/.cache/vllm-benchmarks/
```

The default cases are:

```text
Small:  prompts=4, input=1024,  output=32, concurrency=2
Medium: prompts=4, input=4096,  output=64, concurrency=2
Long:   prompts=2, input=48000, output=64, concurrency=2
```

Any case can be overridden from the environment:

```bash
SMALL_PROMPTS=8 SMALL_OUTPUT_LEN=64 LONG_PROMPTS=1 vllm-benchmark
```

#### Fusion

Fusion is packaged by `.config/nix/fusion-npm.nix` and launched by the sandboxed user service. The service runs the long-lived Fusion dashboard/server process directly under systemd; it no longer owns a `tmux` session.

The Nix package preserves the npm global-install layout enough for Fusion to detect its installed package, but the package itself lives in the immutable Nix store. Do not use Fusion's self-update flow for this setup. Upgrade Fusion by bumping the version and fixed-output hash in `.config/nix/fusion-npm.nix`, then restart `fusion.service` or select a vLLM target with `vllm-config` so Fusion is restarted after model readiness.

Fusion 0.29 exposes `fn dashboard`/`fusion dashboard` as the command that starts the dashboard/server. There is currently no known CLI command that attaches a terminal UI to an already-running dashboard instance. Use the browser for the running dashboard:

```bash
xdg-open http://127.0.0.1:4040
```

`fusion-attach` is intentionally a log observer for now:

```bash
fusion-attach
```

It starts `fusion.service` if needed, then tails `journalctl --user-unit fusion.service`. Do not make `fusion-attach` start another `fusion dashboard`; that creates duplicate dashboard instances, port conflicts on `4040`, and Fusion peer/service-name collisions.

Trail tip for future agents: when upstream lands a real "connect to existing dashboard" or terminal UI attach command, replace `fusion-attach`'s journal tail with that command. Keep `fusion.service` as the single owner of the long-running dashboard/server process.

Service logs are available with:

```bash
journalctl --user -u fusion.service -f
```

Fusion project state from `~/Sources/Fusion/*/*/.fusion` except `.fusion/backups`, and git worktrees from `~/Sources/Fusion/*/*/.worktrees`, are snapshotted daily at 06:00 to `~/.fusion-backup/snapshots/<timestamp>/projects`. `~/.fusion-backup/latest` points to the newest snapshot. Snapshots use hardlinks to the previous snapshot when possible, so deleted worktrees remain recoverable from older snapshots without duplicating unchanged files. Snapshots are rotated after 7 days.

Fusion-generated `.fusion/backups` directories are kept outside snapshots and mirrored separately to `~/.fusion-backup/mirrors/projects` with `rsync --ignore-existing`, so backup files are not overwritten or removed by later runs.

## Customization

To customize a `.dotfile` you can write a corresponding `.dotfile_override`.

## Commit Guidelines

See `COMMIT_GUIDELINES.md.

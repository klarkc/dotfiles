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

- `main` (default): my personal laptop
- `sol098`: work machine

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
yay -Syu snapper pacreport yay-cache-cleanup-hook sunshine bat git-delta ripgrep handlr ollama-cuda discord enpass-bin brave-bin crush btdu
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

Each target also pulls in `fusion.service`. The selected `vllm@...service` patches local Crush/Fusion/Pi defaults, starts the model, and waits for `GET /v1/models` to respond before Fusion is allowed to start. Fusion is stopped and started during model switches so it rereads changed config files.

Pick the model interactively:

```bash
vllm-config
```

Or switch directly:

```bash
vllm-config qwen3.6-35B-a3b
vllm-config qwen3.6-27B
```

Watch startup progress:

```bash
journalctl --user-unit vllm-qwen3.6-35B-a3b.target -f
journalctl --user-unit vllm@qwen3.6-35B-a3b.service -f
journalctl --user-unit fusion.service -f
```

For the 27B target, replace `35B-a3b` with `27B` in the commands above.

Both model configurations use at least 48k context and 2 parallel sequences. Benchmark commands should exercise both Crush-style short/medium requests and Fusion-style long-context requests:

```bash
# Short/medium interactive requests
python -m vllm.benchmarks.benchmark_serving \
  --backend openai-chat \
  --base-url http://127.0.0.1:8000 \
  --model qwen3.6-35b-a3b \
  --num-prompts 32 \
  --random-input-len 1024 \
  --random-output-len 128

# Long-context Fusion-style requests
python -m vllm.benchmarks.benchmark_serving \
  --backend openai-chat \
  --base-url http://127.0.0.1:8000 \
  --model qwen3.6-35b-a3b \
  --num-prompts 8 \
  --random-input-len 48000 \
  --random-output-len 512
```

Use `--model qwen3.6-27b` after selecting the 27B target.

#### Fusion

Fusion is packaged by `.config/nix/fusion-npm.nix` and launched by the sandboxed user service. The service starts Fusion inside a named `tmux` session so the web dashboard runs under systemd while the interactive TUI remains attachable.

Attach to the running Fusion TUI:

```bash
fusion-attach
```

Detach without stopping Fusion with `Ctrl-a d`. Service logs are also available with:

```bash
journalctl --user -u fusion.service -f
```

Fusion project state from `~/Sources/Fusion/*/*/.fusion` except `.fusion/backups`, and git worktrees from `~/Sources/Fusion/*/*/.worktrees`, are snapshotted daily at 06:00 to `~/.fusion-backup/snapshots/<timestamp>/projects`. `~/.fusion-backup/latest` points to the newest snapshot. Snapshots use hardlinks to the previous snapshot when possible, so deleted worktrees remain recoverable from older snapshots without duplicating unchanged files. Snapshots are rotated after 7 days.

Fusion-generated `.fusion/backups` directories are kept outside snapshots and mirrored separately to `~/.fusion-backup/mirrors/projects` with `rsync --ignore-existing`, so backup files are not overwritten or removed by later runs.

## Customization

To customize a `.dotfile` you can write a corresponding `.dotfile_override`.

## Commit Guidelines

See `COMMIT_GUIDELINES.md`.

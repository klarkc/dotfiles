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
- [codex](https://github.com/openai/codex), [Crush](https://github.com/charmbracelet/crush) and [opencode](https://github.com/anomalyco/opencode) as coding agents
- [Fusion](https://github.com/Runfusion/Fusion) for agents orchestration
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
- AI Agents with [crush](https://github.com/charmbracelet/crush), [codex](https://github.com/openai/codex) and [lumen](https://github.com/jnsahaj/lumen)
- [Fusion](https://github.com/Runfusion/Fusion)

## Supported setups

Below are the supported distro setups

### Arch Linux with NVidia

#### Dependencies

```bash
pacman -Syu yay
yay -Syu openssh pwvucontrol pipewire pipewire-audio pipewire-pulse pipewire-alsa git git-lfs gvim qt5-styleplugins nix ttf-fira-code noto-fonts-emoji lsd dconf-editor picom xorg-xmodmap xclip keyd dunst libnotify lemurs haskell-language-server xmonad xmonad-contrib xorg-xsetroot xorg-xset feh the_silver_searcher satty scrot wget xorg-server taffybar libappindicator-gtk3 blueman dmenu sword i3lock xss-lock
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
yay -Syu snapper pacreport yay-cache-cleanup-hook sunshine bat git-delta ripgrep handlr ollama-cuda discord enpass-bin crush btdu btop
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

#### OpenCode + Codex OAuth

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
systemctl --user enable --now kolu
```

#### vLLM + Fusion

The vLLM/Fusion workflow is target-based. Only one vLLM model target should run at a time:

- `vllm-qwen3.6-35B-a3b.target` starts `vllm@qwen3.6-35B-a3b.service`
- `vllm-qwen3.6-27B.target` starts `vllm@qwen3.6-27B.service`

Use `vllm-config` to choose the active local model. It stops Fusion and all vLLM units, disables the non-selected target, enables the selected target for future user-session starts, starts the selected target, and follows the relevant journal logs until `vllm@...service` and `fusion.service` are active.

The target starts only the selected vLLM service. The vLLM service then patches local Crush/Fusion/Pi defaults, starts the model, waits for `GET /v1/models` to respond with the selected served model, and only then restarts Fusion so it rereads changed config files. Fusion is intentionally not pulled directly by the target; readiness is owned by `vLLM@...service`.

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

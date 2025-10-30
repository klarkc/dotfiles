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

- [Alacritty](https://github.com/alacritty/alacritty) + [Tmux](https://github.com/tmux/tmux) + [vim](https://github.com/vim/vim)
- [Vim Language Server Protocol](https://github.com/prabirshrestha/vim-lsp) + [Automatic Servers](https://github.com/mattn/vim-lsp-settings)
- [Lorri](https://github.com/nix-community/lorri/) for faster nix-shell environment
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
- [vimb](https://fanglingsu.github.io/vimb) web browser.
- [Handlr](https://github.com/Anomalocaridid/handlr-regex) manage default apps
- [spotifyd](https://github.com/Spotifyd/spotifyd) Spotify Connect service
- GPT with [CoderCookE/vim-chatgpt](https://github.com/CoderCookE/vim-chatgpt)
- Llama with [vim-llama](https://github.com/Dr4x14913/vim-llama)
- [Oterm](https://github.com/ggozad/oterm) with [ollama](https://ollama.com)
- [ShellGPT](https://github.com/TheR1D/shell_gpt) with ollama

## Supported setups

Below are the supported distro setups

### Arch Linux with XMonad

#### Dependencies

```bash
pacman -Syu yay
# TODO re-enable alacritty-ligature-git
yay -Syu git gvim alacritty qt5-styleplugins nix ttf-fira-code noto-fonts-emoji lsd dconf-editor xfconf picom xorg-xmodmap gnome-session gnome-settings-daemon notification-daemon xmonad xmonad-contrib xorg-xsetroot feh the_silver_searcher satty acrot wget gdm xorg-server taffybar blueman-applet dmenu
sudo chmod +s .local/bin/pacman-*
systemctl enable --now nix-daemon.socket
```

> Below dependencies are not mandatory (see [Optional Features](#optional-features))

```bash
yay -Syu git docker pacreport yay-cache-cleanup-hook sunshine bat git-delta ripgrep nyxt handlr spotifyd python-openai oterm ollama-cuda shell-gpt discord enpass-bin brave-bin
systemctl enable --now docker.socket
```

#### Build

```bash
make
```

#### Installation

```bash
nix profile install .
systemctl --user daemon-reload
systemctl --user enable lorri.socket
```

> Below steps are not mandatory (see [Optional Features](#optional-features))

```bash
systemctl enable gdm
systemctl --user enable docker-cleanup.timer
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable pacreport.timer
systemctl --user enable --now sunshine.service
systemctl --user enable --now spotifyd.service
systemctl --user enable --now ollama.service
```

Add in `/usr/share/xsessions/gnome-xmonad.desktop`:

```
[Desktop Entry]
Name=GNOME Xmonad
# TODO enable --systemd-service
Exec=gnome-session --session=gnome-xmonad
```

Add in `/etc/gdm/custom.conf`:

```ini
[security]
AllowCustomSessions=true
```

Disable Wayland in gdm config (xmonad does not support it)

Add in `.bashrc_override` (replace `hackme` with your [api key](https://platform.openai.com/account/api-keys)):

```bash
export OPENAI_API_KEY="hackme"
```

Download the llama models in vim:

```vimscript
:VLMAPull codellama
:VLMAPull llama3
```

## Customization

To customize a `.dotfile` you can write a corresponding `.dotfile_override`.

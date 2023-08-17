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

### Aliases

- `l`: `ls -l`
- `ll`: `ls -la`
- `lt`: `ls -lt`

## Optional Features

- [pacman-mirrorup](https://github.com/bpetlert/pacman-mirrorup) optimized mirrorlist
- Maintenance [systemd scripts](https://github.com/klarkc/dotfiles/tree/main/.config/systemd/user)
- [Sunshine](https://github.com/LizardByte/Sunshine) game streaming server (to play games on my TV)

## Supported setups

Below are the supported distro setups

### Arch Linux with XFCE and XMonad

#### Dependencies

```bash
pacman -Syu yay
yay -Syu git gvim alacritty-ligature-git qt5-styleplugins nix ttf-fira-code lsd
sudo chmod +s .local/bin/pacman-*
systemctl enable --now nix-daemon.socket
```

> Below dependencies are not mandatory (see [Optional Features](#optional-features))

```bash
yay -Syu git docker pacreport yay-cache-cleanup-hook sunshine pacman-mirrorup xmonad xmonad-contrib xmobar feh
systemctl enable --now docker.socket
nix-env -i lorri direnv
```

#### Build

```bash
make
```

#### Installation

```bash
systemctl --user daemon-reload
systemctl --user enable lorri.socket
```

> Below steps are not mandatory (see [Optional Features](#optional-features))

```bash
systemctl enable --now pacman-mirrorup.timer
systemctl --user enable docker-cleanup.timer
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable pacreport.timer
systemctl --user enable sunshine.service
```
## Customization

To customize a `.dotfile` you can write a corresponding `.dotfile_override`.

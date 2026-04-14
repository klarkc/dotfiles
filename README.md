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
- [Codex](https://github.com/openai/codex)

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
- AI models with [llama.cpp](https://github.com/ggml-org/llama.cpp)
- AI Agent with [crush](https://github.com/charmbracelet/crush)

## Supported setups

Below are the supported distro setups

### Arch Linux with NVidia

#### Dependencies

```bash
pacman -Syu yay
# TODO re-enable alacritty-ligature-git
yay -Syu openssh pwvucontrol pipewire pipewire-audio pipewire-pulse pipewire-alsa git git-lfs gvim alacritty qt5-styleplugins nix ttf-fira-code noto-fonts-emoji lsd dconf-editor picom xorg-xmodmap notification-daemon lemurs haskell-language-server xmonad xmonad-contrib xorg-xsetroot xfconf feh the_silver_searcher satty scrot wget xorg-server taffybar libappindicator-gtk3 blueman dmenu sword
sudo chmod +s .local/bin/pacman-*
systemctl enable --now nix-daemon.socket
systemctl enable --now lemurs
systemctl enable --now bluetooth
```

> Below dependencies are not mandatory but highly recommended
```bash
yay -Syu ffmpeg unzip htop
```

> Below dependencies are not mandatory (see [Optional Features](#optional-features))

```bash
yay -Syu snapper pacreport yay-cache-cleanup-hook sunshine bat git-delta ripgrep handlr llama.cpp-cuda discord enpass-bin brave-bin crush btdu
```

> Yazi as file manager with optional deps
```bash
yay -Syu yazi
yay -S --asdeps 7zip jq poppler fd fzf zoxide resvg imagemagick xclip xsel chafa
```

#### Build

```bash
make
```

#### Installation

```bash
nix profile install .
systemctl --user daemon-reload
```

#### Keyboard layout

For Arch Linux TTYs, set `/etc/vconsole.conf`:

```bash
sudoedit /etc/vconsole.conf
```

```ini
KEYMAP=br-abnt2
```

Apply it with a reboot or:

```bash
sudo systemctl restart systemd-vconsole-setup
```

If the initramfs/early boot prompt should also use `br-abnt2`, ensure the `keymap` hook is enabled in `/etc/mkinitcpio.conf` and rebuild:

```bash
sudo mkinitcpio -P
```

X11 does not inherit `vconsole.conf`. In this setup, the X11 layout is applied from [`.config/xmonad/xmonad.hs`](/home/klarkc/.config/xmonad/xmonad.hs).

> Below steps are not mandatory (see [Optional Features](#optional-features))

```bash
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable pacreport.timer
systemctl --user enable --now sunshine.service
```

## Customization

To customize a `.dotfile` you can write a corresponding `.dotfile_override`.

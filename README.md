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

Features:

- [Alacritty](https://github.com/alacritty/alacritty) + [Tmux](https://github.com/tmux/tmux) + [vim](https://github.com/vim/vim)
- [Vim Language Server Protocol](https://github.com/prabirshrestha/vim-lsp) + [Automatic Servers](https://github.com/mattn/vim-lsp-settings)
- [Lorri](https://github.com/nix-community/lorri/) for faster nix-shell environment
- [Nord Theme](https://www.nordtheme.com/)
- [Fira Code](https://github.com/tonsky/FiraCode) with ligatures support
- Maintenance [systemd scripts](https://github.com/klarkc/dotfiles/tree/main/.config/systemd/user)

### Supported setups

Below are the supported distro setups

#### Arch Linux with XFCE

##### Dependencies

```bash
pacman -Syu yay
yay -Syu git gvim alacritty-ligature-git qt5-styleplugins pacreport docker nix ttf-fira-code
sudo chmod +s .local/bin/pacman-*
systemctl enable --now nix-daemon.socket
systemctl enable --now docker.socket
```

##### Build

```bash
make
```

##### Installation

```bash
systemctl --user daemon-reload
systemctl --user enable docker-cleanup.timer
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable paccache.timer
systemctl --user enable pacreport.timer
systemctl --user enable lorri.socket
```

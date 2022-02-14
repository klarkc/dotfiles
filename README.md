# Dotfiles

Optioned user settings for a dev environment

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
- [Nord Theme](https://www.nordtheme.com/)
- Maintenance [systemd scripts](https://github.com/klarkc/dotfiles/tree/main/.config/systemd/user)

## Arch Linux

### Dependencies

```bash
pacman -Syu yay
yay -Syu git gvim alacritty-ligature-git qt5-styleplugins pacreport docker nix
sudo chmod +s /usr/bin/pacreport
sudo chmod +s /usr/bin/paccache
sudo chmod +s .local/bin/pacman-clean
systemctl enable --now nix-daemon.socket
systemctl enable --now docker.socket
```

### Build

```bash
make
```

### Installation

```bash
systemctl --user enable docker-cleanup.timer
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable paccache.timer
systemctl --user enable pacreport.timer
```

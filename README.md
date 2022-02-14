# Dotfiles

Install system wide requeriments and then build

## Arch

```bash
pacman -Syu yay
yay -Syu git vim alacritty-ligature-git qt5-styleplugins pacreport
sudo chmod +s /usr/bin/pacreport
sudo chmod +s /usr/bin/paccache
systemctl enable --now nix-daemon.socket
systemctl enable --now docker.socket
```

# Build

```bash
make
systemctl --user enable docker-cleanup.timer
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable paccache.timer
systemctl --user enable pacreport.timer
```

# Dotfiles

Install system wide requeriments and then build

## Arch Linux

```bash
pacman -Syu yay
yay -Syu git vim alacritty-ligature-git qt5-styleplugins pacreport
sudo chmod +s /usr/bin/pacreport
sudo chmod +s /usr/bin/paccache
sudo chmod +s .local/bin/pacman-clean
systemctl enable --now nix-daemon.socket
systemctl enable --now docker.socket
```

### Build and install

```bash
make
systemctl --user enable docker-cleanup.timer
systemctl --user enable home-cleanup.timer
systemctl --user enable nix-cleanup.timer
systemctl --user enable paccache.timer
systemctl --user enable pacreport.timer
```

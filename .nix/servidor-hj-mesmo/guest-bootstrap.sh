#!/usr/bin/env bash
# Run as root ON solo098 only after successful cloud-init and IAP/SSH verification.
# No private credentials or passwords should ever be passed to this script.
set -euo pipefail
[[ "$(id -u)" -eq 0 ]] || { echo 'Run as root via sudo.' >&2; exit 2; }
command -v pacman >/dev/null || { echo 'Expected Arch Linux' >&2; exit 2; }
[[ -d /home/klarkc ]] || { echo 'cloud-init has not created klarkc' >&2; exit 1; }
[[ ! -e /var/lib/solo098-guest-installed ]] || { echo 'Already provisioned; no changes.'; exit 0; }
pacman -Sy --noconfirm --needed archlinux-keyring
pacman -Syu --noconfirm
# GUI, Nix, audio, dev tools: preserve dotfiles features instead of stripping them.
pacman -S --noconfirm --needed \
  base-devel git git-lfs openssh sudo nix dbus \
  xorg-server xorg-xauth xorg-xinit xorg-xrandr xorg-xsetroot xorg-xset \
  xorg-xmodmap xorg-setxkbmap xmonad xmonad-contrib dmenu \
  pipewire pipewire-audio pipewire-pulse pipewire-alsa wireplumber \
  keyd dunst libnotify picom feh scrot satty i3lock xss-lock xclip \
  gvim haskell-language-server lsd dconf-editor \
  ttf-fira-code noto-fonts-emoji
systemctl enable --now nix-daemon.socket
systemctl enable --now sshd
if ! grep -Eq '^[[:space:]]*experimental-features[[:space:]]*=' /etc/nix/nix.conf 2>/dev/null; then
  printf '\nexperimental-features = nix-command flakes\n' >> /etc/nix/nix.conf
fi
home=/home/klarkc
if [[ ! -d "$home/.git" ]]; then
  stage="$(mktemp -d)"
  trap 'rm -rf -- "$stage"' EXIT
  git clone --depth 1 --branch servidor-hj-mesmo https://github.com/klarkc/dotfiles.git "$stage/dotfiles"
  # This new VM has no pre-existing user data; preserve SSH metadata-owned files.
  cp -a "$stage/dotfiles/." "$home/"
  chown -R klarkc:klarkc "$home"
  rm -rf -- "$stage"
  trap - EXIT
fi
# The original dotfiles includes all user services and XMonad configuration.
# RDP uses a conditional session wrapper, not the physical Lemurs login.
install -Dm755 "$home/.nix/servidor-hj-mesmo/xrdp-session.sh" /usr/local/bin/solo098-xrdp-session
install -Dm755 /dev/stdin /etc/xrdp/startwm.sh <<'SH'
#!/bin/sh
exec /usr/local/bin/solo098-xrdp-session
SH
# XRDP and xorgxrdp are AUR packages: build unprivileged, never as root.
build_dir="$(mktemp -d)"
chmod 755 "$build_dir"
chown klarkc:klarkc "$build_dir"
printf '%s\n' 'klarkc ALL=(root) NOPASSWD: /usr/bin/pacman' >/etc/sudoers.d/solo098-aur
chmod 0440 /etc/sudoers.d/solo098-aur
cleanup() { rm -f /etc/sudoers.d/solo098-aur; rm -rf -- "$build_dir"; }
trap cleanup EXIT
for package in xrdp xorgxrdp; do
  runuser -u klarkc -- git clone --depth 1 "https://aur.archlinux.org/$package.git" "$build_dir/$package"
  runuser -u klarkc -- bash -c 'cd "$1" && makepkg -si --noconfirm --needed' _ "$build_dir/$package"
done
cleanup
trap - EXIT
systemctl enable --now xrdp xrdp-sesman
# MCP runtime is already in the historical Nix flake and pulls cloudflared.
runuser -u klarkc -- bash -c 'cd "$HOME" && nix profile install .#desktop-commander-mcp-runtime'
loginctl enable-linger klarkc
install -d -m 0755 -o klarkc -g klarkc "$home/.config/systemd/user/default.target.wants"
ln -sfn ../desktop-commander-mcp-runtime.service "$home/.config/systemd/user/default.target.wants/desktop-commander-mcp-runtime.service"
ln -sfn ../desktop-commander-mcp-tunnel.service "$home/.config/systemd/user/default.target.wants/desktop-commander-mcp-tunnel.service"
chown -h klarkc:klarkc "$home/.config/systemd/user/default.target.wants/"*.service
touch /var/lib/solo098-guest-installed
echo 'Guest bootstrap completed. Private Cloudflare profile_override still requires one-time enrollment.'
echo 'Sunshine, CUDA/vLLM and local-display NVIDIA startup are NOT enabled.'

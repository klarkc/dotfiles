#!/usr/bin/env bash
# Run as root ON solo098 only after successful cloud-init and IAP/SSH verification.
# No private credentials or passwords should ever be passed to this script.
set -euo pipefail
[[ "$(id -u)" -eq 0 ]] || {
  echo 'Run as root via sudo.' >&2
  exit 2
}
command -v pacman >/dev/null || {
  echo 'Expected Arch Linux' >&2
  exit 2
}
[[ -d /home/klarkc ]] || {
  echo 'cloud-init has not created klarkc' >&2
  exit 1
}
[[ ! -e /var/lib/solo098-guest-installed ]] || {
  echo 'Already provisioned; no changes.'
  exit 0
}
pacman -Sy --noconfirm --needed archlinux-keyring
pacman -Syu --noconfirm
# GUI, Nix, audio, dev tools: preserve dotfiles features instead of stripping them.
pacman -S --noconfirm --needed \
  base-devel git git-lfs curl openssh sudo dbus \
  xorg-server xorg-xauth xorg-xinit xorg-xrandr xorg-xsetroot xorg-xset \
  xorg-xmodmap xorg-setxkbmap xmonad xmonad-contrib dmenu ghc taffybar \
  pipewire pipewire-audio pipewire-pulse pipewire-alsa wireplumber pamixer \
  keyd dunst libnotify picom feh scrot satty i3lock xss-lock xclip \
  gvim haskell-language-server lsd dconf-editor vimb discord \
  the_silver_searcher sword pacutils brightnessctl \
  ttf-fira-code noto-fonts-emoji
systemctl enable --now sshd
# Determinate Nix, not the Arch nix package. This installer is an immutable
# official GitHub release, pinned by both version and binary SHA256.
# Installing over pacman-managed Nix or partially initialized /nix is unsafe.
if pacman -Q nix >/dev/null 2>&1; then
  echo 'BLOCKED: pacman-managed nix is installed. Inspect and migrate it before rerunning.' >&2
  echo 'Read-only checks: pacman -Qi nix; systemctl status nix-daemon.socket; ls -ld /nix /etc/nix' >&2
  exit 1
fi
determinate_nix=/nix/var/nix/profiles/default/bin/nix
if [[ ! -x $determinate_nix ]]; then
  if [[ -e /nix || -e /etc/nix/nix.conf ]]; then
    echo 'BLOCKED: existing partial/unknown Nix state; review it rather than overwriting.' >&2
    exit 1
  fi
  installer="$(mktemp)"
  trap 'rm -f -- "$installer"' EXIT
  curl --proto '=https' --tlsv1.2 --fail --location --silent --show-error --retry 3 \
    https://github.com/DeterminateSystems/nix-installer/releases/download/v3.24.2/nix-installer-x86_64-linux \
    -o "$installer"
  printf '%s  %s\n' \
    '22ed14ee1b24d100d388f1fc245e3062bd5982433d159cffa6dec34c0eb15dab' "$installer" |
    sha256sum --check
  chmod 0700 "$installer"
  "$installer" install --determinate
  rm -f -- "$installer"
  trap - EXIT
fi
if [[ ! -r /nix/receipt.json || ! -d /nix/store || ! -x $determinate_nix ]]; then
  echo 'BLOCKED: Determinate Nix installation is incomplete.' >&2
  exit 1
fi
determinate_version="$("$determinate_nix" --version)"
if [[ $determinate_version != *"Determinate Nix"* ]]; then
  echo "BLOCKED: Nix distribution is not Determinate Nix: $determinate_version" >&2
  exit 1
fi
echo "PASS: $determinate_version"
home=/home/klarkc
# The VM records the exact SHA that cleared mandatory Test CI.
# Do not silently run an updated branch that was not admitted by provision-gce.
metadata_sha_url='http://metadata.google.internal/computeMetadata/v1/instance/attributes/solo098-dotfiles-sha'
expected_repo_sha="$(curl --fail --silent --show-error --max-time 10 \
  --header 'Metadata-Flavor: Google' "$metadata_sha_url")" || {
  echo 'BLOCKED: cannot read the provisioned dotfiles SHA from GCE metadata.' >&2
  exit 1
}
if [[ ! $expected_repo_sha =~ ^[0-9a-f]{40}$ ]]; then
  echo 'BLOCKED: invalid provisioned dotfiles SHA.' >&2
  exit 1
fi
if [[ ! -d "$home/.git" ]]; then
  stage="$(mktemp -d)"
  trap 'rm -rf -- "$stage"' EXIT
  git clone --depth 1 --branch servidor-hj-mesmo https://github.com/klarkc/dotfiles.git "$stage/dotfiles"
  cloned_sha="$(git -C "$stage/dotfiles" rev-parse HEAD)"
  if [[ $cloned_sha != "$expected_repo_sha" ]]; then
    echo 'BLOCKED: cloned branch HEAD differs from the provisioned, CI-tested SHA.' >&2
    exit 1
  fi
  # This new VM has no pre-existing user data; preserve SSH metadata-owned files.
  cp -a "$stage/dotfiles/." "$home/"
  chown -R klarkc:klarkc "$home"
  rm -rf -- "$stage"
  trap - EXIT
else
  existing_sha="$(git -C "$home" rev-parse HEAD)"
  if [[ $existing_sha != "$expected_repo_sha" ]]; then
    echo 'BLOCKED: existing dotfiles checkout differs from the provisioned SHA.' >&2
    exit 1
  fi
fi
# Shared Arch locale prerequisite for pt-BR GUI and en-US interactive shells.
bash "$home/.nix/servidor-hj-mesmo/configure-locale.sh"
# The original dotfiles includes all user services and XMonad configuration.
# RDP uses a conditional session wrapper, not the physical Lemurs login.
# XRDP will install its own startwm.sh; replace it only AFTER package installation.
# XRDP and xorgxrdp are AUR packages: build unprivileged, never as root.
build_dir="$(mktemp -d)"
chmod 755 "$build_dir"
chown klarkc:klarkc "$build_dir"
printf '%s\n' 'klarkc ALL=(root) NOPASSWD: /usr/bin/pacman' >/etc/sudoers.d/solo098-aur
chmod 0440 /etc/sudoers.d/solo098-aur
cleanup() {
  rm -f /etc/sudoers.d/solo098-aur
  rm -rf -- "$build_dir"
}
trap cleanup EXIT
# Pin reviewed AUR PKGBUILD revisions. A new revision requires manual
# inspection and a new commit; floating upstream code is never auto-executed.
declare -A aur_revisions=(
  [xrdp]=4701987b7d76df432dc41351875678dd370f42b8
  [xorgxrdp]=b04814d56be91d993f4d76370c473dda1b691641
  ["brave-bin"]=62caba7bf015ce1c53986cfab86b177625743fb4
)
for package in xrdp xorgxrdp brave-bin; do
  runuser -u klarkc -- git clone --depth 1 "https://aur.archlinux.org/$package.git" "$build_dir/$package"
  actual_revision="$(runuser -u klarkc -- git -C "$build_dir/$package" rev-parse HEAD)"
  if [[ $actual_revision != "${aur_revisions[$package]}" ]]; then
    echo "BLOCKED: AUR $package revision changed; review PKGBUILD and update its pin before proceeding." >&2
    exit 1
  fi
  runuser -u klarkc -- bash -c 'cd "$1" && makepkg -si --noconfirm --needed' _ "$build_dir/$package"
done
command -v brave >/dev/null || {
  echo 'BLOCKED: Brave executable missing after package installation.' >&2
  exit 1
}
# The default Enpass AUR package now uses the Wayland build; preserve X11.
# This local PKGBUILD pins the official Enpass X11 .deb and its SHA256.
enpass_build="$build_dir/enpass-x11"
install -d -m 0755 -o klarkc -g klarkc "$enpass_build"
install -m 0644 "$home/.nix/servidor-hj-mesmo/enpass-x11/PKGBUILD" "$enpass_build/PKGBUILD"
chown klarkc:klarkc "$enpass_build/PKGBUILD"
runuser -u klarkc -- bash -c 'cd "$1" && makepkg -si --noconfirm --needed' _ "$enpass_build"
[[ -x /opt/enpass/Enpass ]] || {
  echo 'BLOCKED: Enpass X11 executable missing after package installation.' >&2
  exit 1
}
cleanup
trap - EXIT
install -Dm755 "$home/.nix/servidor-hj-mesmo/xrdp-session.sh" /usr/local/bin/solo098-xrdp-session
install -Dm755 /dev/stdin /etc/xrdp/startwm.sh <<'SH'
#!/bin/sh
exec /usr/local/bin/solo098-xrdp-session
SH
systemctl enable --now xrdp xrdp-sesman
# Install the shared full CPU-only Nix workstation profile (including MCP).
# The original default profile retains its CUDA/vLLM feature.
runuser -u klarkc -- bash -c 'source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh && cd /home/klarkc && nix profile add .#solo098-gce'
loginctl enable-linger klarkc
install -d -m 0755 -o klarkc -g klarkc "$home/.config/systemd/user/default.target.wants"
ln -sfn ../desktop-commander-mcp-runtime.service "$home/.config/systemd/user/default.target.wants/desktop-commander-mcp-runtime.service"
ln -sfn ../desktop-commander-mcp-tunnel.service "$home/.config/systemd/user/default.target.wants/desktop-commander-mcp-tunnel.service"
chown -h klarkc:klarkc "$home/.config/systemd/user/default.target.wants/"*.service
touch /var/lib/solo098-guest-installed
echo 'Guest bootstrap completed. Private Cloudflare profile_override still requires one-time enrollment.'
echo 'Sunshine, CUDA/vLLM and local-display NVIDIA startup are NOT enabled.'

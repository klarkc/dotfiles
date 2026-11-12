#!/usr/bin/env bash
# Complete Arch XMonad workstation prerequisites; not a minimal cloud server.
# No GPU drivers, CUDA or application patches.
set -euo pipefail

if ((EUID != 0)); then
  echo 'Run as root (sudo bash .nix/servidor-hj-mesmo/host-packages.sh).' >&2
  exit 2
fi
if ! command -v pacman >/dev/null 2>&1; then
  echo 'Expected Arch Linux with pacman.' >&2
  exit 2
fi

# Full sync+upgrade avoids Arch partial upgrades. --needed is idempotent.
# Interactive make prompts for approval; bootstrap explicitly opts into
# noninteractive mode only after authorized first-boot provisioning.
pacman_flags=()
case "${1:-}" in
'') ;;
--non-interactive) pacman_flags=(--noconfirm) ;;
*)
  echo 'Only --non-interactive is supported.' >&2
  exit 2
  ;;
esac
# Bump note: keep these tools aligned with Makefile targets and RDP smoke
# checks when adding dependencies to the guest desktop.
pacman -Syu --needed "${pacman_flags[@]}" \
  nodejs npm tmux xfconf mesa mesa-utils \
  man-db man-pages less groff texinfo xterm \
  wget unzip htop jq ripgrep fd fzf file which rsync \
  xorg-xmessage xorg-xev xorg-xprop xorg-xwininfo \
  xdg-utils xdg-user-dirs desktop-file-utils shared-mime-info \
  noto-fonts ttf-dejavu

#!/usr/bin/env bash
# XRDP/Xorg virtual session, preserving the original XMonad dotfiles.
set -euo pipefail
export SOLO098_GCE_RDP=1
export XDG_CURRENT_DESKTOP=xmonad
export XDG_SESSION_TYPE=x11
if command -v setxkbmap >/dev/null 2>&1; then
  setxkbmap -option caps:escape >/dev/null 2>&1 || true
fi
if [[ -x "$HOME/.local/bin/xmonad-session" ]]; then
  exec "$HOME/.local/bin/xmonad-session"
fi
exec xmonad

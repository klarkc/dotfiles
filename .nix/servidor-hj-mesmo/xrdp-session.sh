#!/usr/bin/env bash
# XRDP/Xorg virtual session, preserving the original XMonad dotfiles.
set -euo pipefail
export SOLO098_GCE_RDP=1
export XDG_CURRENT_DESKTOP=xmonad
export XDG_SESSION_TYPE=x11
# XMonad's shared session selects ABNT2 and CapsLock-as-Escape.
if [[ -x "$HOME/.local/bin/xmonad-session" ]]; then
  exec "$HOME/.local/bin/xmonad-session"
fi
exec xmonad

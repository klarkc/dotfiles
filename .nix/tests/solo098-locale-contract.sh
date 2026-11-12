#!/usr/bin/env bash
# Deterministic locale contract; no downloads, root writes or graphical session.
set -euo pipefail
cd "$1"

bash -n .bashrc
bash -n .bash_profile
sh -n .local/bin/xmonad-session
bash -n .nix/servidor-hj-mesmo/configure-locale.sh

snippet="$(awk '
  /^# Interactive terminal locale:/ { capture = 1; next }
  capture && /^# set a fancy git prompt/ { exit }
  capture { print }
' .bashrc)"
[[ -n $snippet ]] || { echo 'Bash locale stanza missing' >&2; exit 1; }
printf '%s\n' "$snippet" \
  '[[ "$LANG" == en_US.UTF-8 ]]' \
  '[[ "$LANGUAGE" == en_US:en ]]' \
  '! env | grep -q "^LC_ALL="' \
  '! env | grep -q "^LC_MESSAGES="' \
  | env LANG=pt_BR.UTF-8 LANGUAGE=pt_BR:pt LC_ALL=C LC_MESSAGES=pt_BR.UTF-8 bash -e

grep -Fq 'export LANG=pt_BR.UTF-8' .local/bin/xmonad-session
grep -Fq 'export LANGUAGE=pt_BR:pt' .local/bin/xmonad-session
grep -Fq 'setxkbmap -layout br -variant abnt2 -option caps:escape' .local/bin/xmonad-session
grep -Fq 'localectl set-locale LANG=pt_BR.UTF-8' .nix/servidor-hj-mesmo/configure-locale.sh
grep -Fq 'bash "$home/.nix/servidor-hj-mesmo/configure-locale.sh"' .nix/servidor-hj-mesmo/guest-bootstrap.sh
grep -Fq 'sudo bash .nix/servidor-hj-mesmo/configure-locale.sh' Makefile
echo 'PASS: en-US shell, pt-BR XMonad, ABNT2 and Arch locale prerequisites'

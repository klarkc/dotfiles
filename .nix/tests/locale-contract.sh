#!/usr/bin/env bash
# Shared locale contract; no downloads, root writes or graphical session.
set -euo pipefail
cd "$1"

bash -n .bashrc
bash -n .bash_profile
sh -n .local/bin/xmonad-session
bash -n .nix/configure-locale-arch.sh

snippet="$(awk '
  /^# Interactive terminal locale:/ { capture = 1; next }
  capture && /^# set a fancy git prompt/ { exit }
  capture { print }
' .bashrc)"
if [[ -z $snippet ]]; then
  echo 'Bash locale stanza missing' >&2
  exit 1
fi

printf '%s\n' "$snippet" \
  '[[ "$LANG" == en_US.UTF-8 ]]' \
  '[[ "$LANGUAGE" == en_US:en ]]' \
  '! env | grep -q "^LC_ALL="' \
  '! env | grep -q "^LC_MESSAGES="' |
  env LANG=C LANGUAGE=pt_BR:pt LC_ALL=C LC_MESSAGES=C bash -e

grep -Fq 'export LANG=pt_BR.UTF-8' .local/bin/xmonad-session
grep -Fq 'export LANGUAGE=pt_BR:pt' .local/bin/xmonad-session
grep -Fq 'setxkbmap -layout br -variant abnt2 -option caps:escape' .local/bin/xmonad-session
grep -Fq 'localectl set-locale LANG=pt_BR.UTF-8' .nix/configure-locale-arch.sh
grep -Fq 'bash "$home/.nix/configure-locale-arch.sh"' .nix/servidor-hj-mesmo/guest-bootstrap.sh
grep -Fq 'all: $(GCE_HOST_PREREQUISITE) locale.Configure nix.Profile' Makefile
grep -Fq 'sudo bash .nix/configure-locale-arch.sh' Makefile
grep -Fq 'LANG=pt_BR.UTF-8' .config/environment.d/10-locale.conf
grep -Fq 'LANGUAGE=pt_BR:pt' .config/environment.d/10-locale.conf
echo 'PASS: shared en-US terminals, pt-BR GUI, ABNT2 and host locale setup'

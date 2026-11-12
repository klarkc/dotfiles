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

# Regression for the real Arch GCE image: only en_US was listed, and
# pt_BR was absent (not merely commented). Use disposable locale.gen files.
# The helper must preserve unrelated lines and be idempotent.
source .nix/configure-locale-arch.sh
fixture="$(mktemp)"
trap 'rm -f "$fixture"' EXIT
printf '%s\n' '# keep untouched' 'en_US.UTF-8 UTF-8' >"$fixture"
ensure_locale_entry "$fixture" en_US.UTF-8
ensure_locale_entry "$fixture" pt_BR.UTF-8
ensure_locale_entry "$fixture" pt_BR.UTF-8
test "$(grep -Ec '^en_US[.]UTF-8 UTF-8grep -Fq 'export LANGUAGE=pt_BR:pt' .local/bin/xmonad-session
grep -Fq 'setxkbmap -layout br -variant abnt2 -option caps:escape' .local/bin/xmonad-session
grep -Fq 'localectl set-locale LANG=pt_BR.UTF-8' .nix/configure-locale-arch.sh
grep -Fq 'localectl set-x11-keymap br pc105 abnt2 caps:escape' .nix/configure-locale-arch.sh
grep -Fq 'bash "$home/.nix/configure-locale-arch.sh"' .nix/servidor-hj-mesmo/guest-bootstrap.sh
grep -Fq 'all: $(GCE_HOST_PREREQUISITE) locale.Configure nix.Profile' Makefile
grep -Fq 'sudo bash .nix/configure-locale-arch.sh' Makefile
grep -Fq 'LANG=pt_BR.UTF-8' .config/environment.d/10-locale.conf
grep -Fq 'LANGUAGE=pt_BR:pt' .config/environment.d/10-locale.conf
echo 'PASS: shared en-US terminals, pt-BR GUI, ABNT2 and host locale setup'
 "$fixture")" -eq 1
test "$(grep -Ec '^pt_BR[.]UTF-8 UTF-8grep -Fq 'export LANGUAGE=pt_BR:pt' .local/bin/xmonad-session
grep -Fq 'setxkbmap -layout br -variant abnt2 -option caps:escape' .local/bin/xmonad-session
grep -Fq 'localectl set-locale LANG=pt_BR.UTF-8' .nix/configure-locale-arch.sh
grep -Fq 'localectl set-x11-keymap br pc105 abnt2 caps:escape' .nix/configure-locale-arch.sh
grep -Fq 'bash "$home/.nix/configure-locale-arch.sh"' .nix/servidor-hj-mesmo/guest-bootstrap.sh
grep -Fq 'all: $(GCE_HOST_PREREQUISITE) locale.Configure nix.Profile' Makefile
grep -Fq 'sudo bash .nix/configure-locale-arch.sh' Makefile
grep -Fq 'LANG=pt_BR.UTF-8' .config/environment.d/10-locale.conf
grep -Fq 'LANGUAGE=pt_BR:pt' .config/environment.d/10-locale.conf
echo 'PASS: shared en-US terminals, pt-BR GUI, ABNT2 and host locale setup'
 "$fixture")" -eq 1
grep -Fxq '# keep untouched' "$fixture"

printf '%s\n' '#pt_BR.UTF-8 UTF-8' >"$fixture"
ensure_locale_entry "$fixture" pt_BR.UTF-8
test "$(grep -Ec '^pt_BR[.]UTF-8 UTF-8grep -Fq 'export LANGUAGE=pt_BR:pt' .local/bin/xmonad-session
grep -Fq 'setxkbmap -layout br -variant abnt2 -option caps:escape' .local/bin/xmonad-session
grep -Fq 'localectl set-locale LANG=pt_BR.UTF-8' .nix/configure-locale-arch.sh
grep -Fq 'localectl set-x11-keymap br pc105 abnt2 caps:escape' .nix/configure-locale-arch.sh
grep -Fq 'bash "$home/.nix/configure-locale-arch.sh"' .nix/servidor-hj-mesmo/guest-bootstrap.sh
grep -Fq 'all: $(GCE_HOST_PREREQUISITE) locale.Configure nix.Profile' Makefile
grep -Fq 'sudo bash .nix/configure-locale-arch.sh' Makefile
grep -Fq 'LANG=pt_BR.UTF-8' .config/environment.d/10-locale.conf
grep -Fq 'LANGUAGE=pt_BR:pt' .config/environment.d/10-locale.conf
echo 'PASS: shared en-US terminals, pt-BR GUI, ABNT2 and host locale setup'
 "$fixture")" -eq 1
ensure_locale_entry "$fixture" pt_BR.UTF-8
test "$(grep -Ec '^pt_BR[.]UTF-8 UTF-8grep -Fq 'export LANGUAGE=pt_BR:pt' .local/bin/xmonad-session
grep -Fq 'setxkbmap -layout br -variant abnt2 -option caps:escape' .local/bin/xmonad-session
grep -Fq 'localectl set-locale LANG=pt_BR.UTF-8' .nix/configure-locale-arch.sh
grep -Fq 'localectl set-x11-keymap br pc105 abnt2 caps:escape' .nix/configure-locale-arch.sh
grep -Fq 'bash "$home/.nix/configure-locale-arch.sh"' .nix/servidor-hj-mesmo/guest-bootstrap.sh
grep -Fq 'all: $(GCE_HOST_PREREQUISITE) locale.Configure nix.Profile' Makefile
grep -Fq 'sudo bash .nix/configure-locale-arch.sh' Makefile
grep -Fq 'LANG=pt_BR.UTF-8' .config/environment.d/10-locale.conf
grep -Fq 'LANGUAGE=pt_BR:pt' .config/environment.d/10-locale.conf
echo 'PASS: shared en-US terminals, pt-BR GUI, ABNT2 and host locale setup'
 "$fixture")" -eq 1

grep -Fq 'export LANG=pt_BR.UTF-8' .local/bin/xmonad-session
grep -Fq 'export LANGUAGE=pt_BR:pt' .local/bin/xmonad-session
grep -Fq 'setxkbmap -layout br -variant abnt2 -option caps:escape' .local/bin/xmonad-session
grep -Fq 'localectl set-locale LANG=pt_BR.UTF-8' .nix/configure-locale-arch.sh
grep -Fq 'localectl set-x11-keymap br pc105 abnt2 caps:escape' .nix/configure-locale-arch.sh
grep -Fq 'bash "$home/.nix/configure-locale-arch.sh"' .nix/servidor-hj-mesmo/guest-bootstrap.sh
grep -Fq 'all: $(GCE_HOST_PREREQUISITE) locale.Configure nix.Profile' Makefile
grep -Fq 'sudo bash .nix/configure-locale-arch.sh' Makefile
grep -Fq 'LANG=pt_BR.UTF-8' .config/environment.d/10-locale.conf
grep -Fq 'LANGUAGE=pt_BR:pt' .config/environment.d/10-locale.conf
echo 'PASS: shared en-US terminals, pt-BR GUI, ABNT2 and host locale setup'

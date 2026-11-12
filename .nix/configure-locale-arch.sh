#!/usr/bin/env bash
# Shared Arch host locale prerequisites for all dotfiles profiles.
# Idempotent: keep all other locale.gen entries, user files and Nix untouched.
set -euo pipefail

# Locale lists on Arch images can have no pt_BR entry at all, rather than
# having a commented entry to uncomment. Preserve all unrelated lines and
# append only when the required UTF-8 entry is not already enabled.
# This pure function is exercised on temporary fixtures by locale-contract.sh.
ensure_locale_entry() {
  local locale_gen_file=$1
  local locale_name=$2
  local locale_regex=${locale_name//./[.]}

  if grep -Eq "^[[:space:]]*${locale_regex}[[:space:]]+UTF-8([[:space:]]|$)" "$locale_gen_file"; then
    return 0
  fi

  printf '%s UTF-8\n' "$locale_name" >>"$locale_gen_file"
}

# Sourcing this file during offline tests defines only the pure function.
if [[ ${BASH_SOURCE[0]} != "$0" ]]; then
  return 0
fi

# Read-only locale/keyboard check for unprivileged make.
if [[ ${1:-} == --check ]]; then
  status="$(localectl status 2>/dev/null)" || exit 1
  locale -a | grep -qx 'en_US.utf8' || exit 1
  locale -a | grep -qx 'pt_BR.utf8' || exit 1
  grep -Fq 'System Locale: LANG=en_US.UTF-8' <<<"$status" || exit 1
  grep -Fq 'X11 Layout: br' <<<"$status" || exit 1
  grep -Fq 'X11 Variant: abnt2' <<<"$status" || exit 1
  grep -Fq 'X11 Options: caps:escape' <<<"$status" || exit 1
  exit 0
fi
if [[ ${1:-} != '' ]]; then
  echo 'Only --check is supported.' >&2
  exit 2
fi

if ((EUID != 0)); then
  echo 'Run as root with sudo bash .nix/configure-locale-arch.sh' >&2
  exit 2
fi
if [[ ! -f /etc/locale.gen ]]; then
  echo 'Expected an Arch host with /etc/locale.gen; refusing unknown system.' >&2
  exit 2
fi

for locale_name in en_US.UTF-8 pt_BR.UTF-8; do
  locale_source=${locale_name%%.*}
  if [[ ! -r /usr/share/i18n/locales/$locale_source ]]; then
    echo "Missing glibc locale source: $locale_source (not editing locale.gen)." >&2
    exit 1
  fi
done

for locale_name in en_US.UTF-8 pt_BR.UTF-8; do
  ensure_locale_entry /etc/locale.gen "$locale_name"
done

locale-gen
# Host default stays English; only XMonad's child processes use pt_BR.
localectl set-locale LANG=en_US.UTF-8
# System Xorg defaults also use ABNT2 (XMonad reasserts it per session).
localectl set-x11-keymap br pc105 abnt2 caps:escape

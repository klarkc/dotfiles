#!/usr/bin/env bash
# Shared Arch host locale prerequisites for all dotfiles profiles.
# Idempotent: keep all other locale.gen entries, user files and Nix untouched.
set -euo pipefail

if ((EUID != 0)); then
  echo 'Run as root with sudo bash .nix/configure-locale-arch.sh' >&2
  exit 2
fi
if [[ ! -f /etc/locale.gen ]]; then
  echo 'Expected an Arch host with /etc/locale.gen; refusing unknown system.' >&2
  exit 2
fi

sed -i -E \
  -e 's/^#[[:space:]]*en_US[.]UTF-8[[:space:]]+UTF-8[[:space:]]*$/en_US.UTF-8 UTF-8/' \
  -e 's/^#[[:space:]]*pt_BR[.]UTF-8[[:space:]]+UTF-8[[:space:]]*$/pt_BR.UTF-8 UTF-8/' \
  /etc/locale.gen

for entry in 'en_US.UTF-8 UTF-8' 'pt_BR.UTF-8 UTF-8'; do
  if ! grep -Fxq "$entry" /etc/locale.gen; then
    echo "Locale not present in /etc/locale.gen: $entry" >&2
    exit 1
  fi
done

locale-gen
# Graphical default; ~/.bashrc overrides interactive terminal to en_US.
localectl set-locale LANG=pt_BR.UTF-8

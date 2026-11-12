#!/usr/bin/env bash
# Deterministic, no-build check: make GCE=1 changes only the Nix profile.
set -euo pipefail
repo="${1:?pass the dotfiles root as argument}"
cd "$repo"

default="$(make -Bn nix.Profile)"
gce="$(make -Bn GCE=1 nix.Profile)"
[[ $default == *'profile install "."'* ]] || {
  printf 'Default make profile unexpectedly changed: %s\n' "$default" >&2
  exit 1
}
[[ $gce == *'profile add ".#solo098-gce"'* ]] || {
  printf 'GCE make did not select the CPU-only profile: %s\n' "$gce" >&2
  exit 1
}
skip="$(make -Bn GCE=1 SKIP_NIX_PROFILE=1 nix.Profile)"
[[ $skip == *'nix.Profile: skipped'* ]] || {
  printf 'Skipping installed profile did not work: %s\n' "$skip" >&2
  exit 1
}
[[ $skip != *'nix --extra-experimental-features'* ]] || {
  echo 'Skip profile option still executes nix' >&2
  exit 1
}
host="$(make -Bn GCE=1 gce.HostPackages)"
[[ $host == *'host-packages.sh'* ]] || {
  echo 'GCE host packages prerequisite is missing' >&2
  exit 1
}
if make -Bn GCE=1 SKIP_NIX_PROFILE=bogus nix.Profile >/dev/null 2>&1; then
  echo 'Invalid SKIP_NIX_PROFILE was accepted' >&2
  exit 1
fi
if make -Bn GCE=invalid nix.Profile >/dev/null 2>&1; then
  echo 'Invalid GCE flag was accepted' >&2
  exit 1
fi
echo 'PASS: standard and GCE Makefile profile contracts'

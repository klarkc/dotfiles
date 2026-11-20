#!/usr/bin/env bash
# Deterministic, no-build check: make GCE=1 changes only the Nix profile.
set -euo pipefail
repo="${1:?pass the dotfiles root as argument}"
cd "$repo"

default="$(make -Bn nix.Profile)"
gce="$(make -Bn GCE=1 nix.Profile)"
[[ $default == *'.nix/profile-ensure.sh "default"'* ]] || {
  printf 'Default make profile unexpectedly changed: %s\n' "$default" >&2
  exit 1
}
[[ $gce == *'.nix/profile-ensure.sh "gce"'* ]] || {
  printf 'GCE make did not select the CPU-only profile: %s\n' "$gce" >&2
  exit 1
}
skip="$(make -Bn GCE=1 SKIP_NIX_PROFILE=1 nix.Profile)"
[[ $skip == *'nix.Profile: skipped'* ]] || {
  printf 'Skipping installed profile did not work: %s\n' "$skip" >&2
  exit 1
}
[[ $skip != *'.nix/profile-ensure.sh'* ]] || {
  echo 'Skip profile option still executes nix' >&2
  exit 1
}
host="$(make -Bn GCE=1 host.Verify)"
[[ $host == *'configure-locale-arch.sh --check'* && $host == *'host-packages.sh --check'* ]] || {
  echo 'Read-only host checks missing' >&2
  exit 1
}
whole="$(make -Bn GCE=1 SKIP_NIX_PROFILE=1 all)"
if printf '%s\n' "$whole" | grep -Eq '^[[:space:]]*(sudo|pacman -S|locale-gen)([[:space:]]|$)'; then
  echo 'Makefile must not execute system commands' >&2
  exit 1
fi
[[ $whole == *'install-tmux-plugins.sh'* ]] || {
  echo 'Headless tmux installer missing' >&2
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
# vim-plug declares :PlugInstall, never :PluginInstall. A synchronous command
# must finish downloads before Vim exits. This check is offline and bounded.
vim_target="$(make -Bn GCE=1 SKIP_NIX_PROFILE=1 vim.PluginInstall)"
[[ $vim_target == *"vim -c 'PlugInstall --sync' -c 'qa!'"* ]] || {
  echo 'Vim target does not use vim-plug synchronous installation' >&2
  exit 1
}
[[ $vim_target != *'vim +PluginInstall'* ]] || {
  echo 'Obsolete Vim plugin command remains in Makefile' >&2
  exit 1
}
grep -Fq 'call plug#begin(' .vimrc
grep -Fq 'call plug#end()' .vimrc

# The bootstrap action is tested with a mocked Nix CLI, not the real profile.
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir "$tmp/bin"
printf '#!%s\n' "$(command -v bash)" >"$tmp/bin/nix"
cat >>"$tmp/bin/nix" <<'PROFILE_MOCK_NIX'
set -euo pipefail
[[ "$1" == --extra-experimental-features && "$2" == 'nix-command flakes' && "$3" == profile ]]
shift 3
case "$1" in
  list) [[ "$2" == --json ]]; cat "$MOCK_PROFILE_JSON" ;;
  add|upgrade) printf '%s %s\n' "$1" "$2" >> "$MOCK_NIX_ACTION_LOG" ;;
  *) exit 99 ;;
esac
PROFILE_MOCK_NIX
chmod +x "$tmp/bin/nix"
python3 - "$repo" "$tmp" <<'PROFILE_MOCK_JSON'
import json, pathlib, sys
repo, tmp = sys.argv[1:]
root = str(pathlib.Path(repo).resolve())
base = "git+file://" + root + "?shallow=1"

def write(name, data):
    pathlib.Path(tmp, name).write_text(json.dumps({"elements": data}))

def item(pname, original=base):
    return {"active": True, "originalUrl": original,
            "storePaths": ["/nix/store/00000000000000000000000000000000-" + pname]}

write("absent.json", {})
write("gce.json", {"solo098-gce": item("klarkc-dotfiles_gce_profile")})
write("default.json", {"klarkc-dotfiles_profile": item("klarkc-dotfiles_profile")})
write("foreign.json", {"solo098-gce": item("klarkc-dotfiles_gce_profile", "git+file:///different")})
write("duplicate.json", {"solo098-gce": item("klarkc-dotfiles_gce_profile"),
                        "solo098-gce-1": item("klarkc-dotfiles_gce_profile")})
pathlib.Path(tmp, "invalid.json").write_text("{}")
PROFILE_MOCK_JSON
export MOCK_NIX_ACTION_LOG="$tmp/actions"
export PATH="$tmp/bin:$PATH"
MOCK_PROFILE_JSON="$tmp/absent.json" bash .nix/profile-ensure.sh gce
[[ "$(cat "$tmp/actions")" == 'add .#solo098-gce' ]]
: >"$tmp/actions"
MOCK_PROFILE_JSON="$tmp/gce.json" bash .nix/profile-ensure.sh gce
[[ "$(cat "$tmp/actions")" == 'upgrade solo098-gce' ]]
: >"$tmp/actions"
MOCK_PROFILE_JSON="$tmp/absent.json" bash .nix/profile-ensure.sh default
[[ "$(cat "$tmp/actions")" == 'add .' ]]
: >"$tmp/actions"
MOCK_PROFILE_JSON="$tmp/default.json" bash .nix/profile-ensure.sh default
[[ "$(cat "$tmp/actions")" == 'upgrade klarkc-dotfiles_profile' ]]
: >"$tmp/actions"
for state in foreign duplicate invalid; do
  if MOCK_PROFILE_JSON="$tmp/$state.json" bash .nix/profile-ensure.sh gce >/dev/null 2>&1; then
    echo "Unsafe Nix profile state was accepted: $state" >&2
    exit 1
  fi
  [[ ! -s "$tmp/actions" ]]
done
if bash .nix/profile-ensure.sh bogus >/dev/null 2>&1; then
  echo 'Invalid profile mode was accepted' >&2
  exit 1
fi
[[ ! -s "$tmp/actions" ]]
echo 'PASS: unprivileged Makefile, GCE profile and user setup contracts'

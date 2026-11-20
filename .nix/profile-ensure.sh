#!/usr/bin/env bash
# Rootless Nix profile bootstrap: install once or upgrade the same profile.
# Do not add duplicate profile entries: their bin/* paths conflict.
set -euo pipefail
mode="${1:?usage: profile-ensure.sh default-or-gce}"
case "$mode" in
gce)
  expected_profile='klarkc-dotfiles_gce_profile'
  installable='.#solo098-gce'
  ;;
default)
  expected_profile='klarkc-dotfiles_profile'
  installable='.'
  ;;
*)
  echo 'Unknown profile mode: expected default or gce.' >&2
  exit 2
  ;;
esac

profile_json="$(nix --extra-experimental-features 'nix-command flakes' profile list --json)"
# Nix names profile elements after derivations, not the flake output name.
# Identify our original source plus the exact stored derivation name.
# Fail closed if multiple installations or an unrelated same-name element exist.
found="$(printf '%s' "$profile_json" | python3 -c '
import json, pathlib, sys
mode, pname = sys.argv[1:]
root = pathlib.Path.cwd().resolve()
try:
    elements = json.load(sys.stdin)["elements"]
    if not isinstance(elements, dict):
        raise ValueError("elements is not an object")
    matches = []
    for name, element in elements.items():
        if not isinstance(element, dict):
            raise ValueError("non-object profile entry")
        paths = element.get("storePaths", [])
        if not isinstance(paths, list):
            raise ValueError("invalid storePaths entry")
        has_pkg = any(isinstance(path, str) and path.endswith("-"+pname) for path in paths)
        has_reserved_name = name == ("solo098-gce" if mode == "gce" else "default")
        if not has_pkg and not has_reserved_name:
            continue
        if not has_pkg:
            raise ValueError("reserved profile name refers to another package")
        origin = element.get("originalUrl", "")
        base = origin.split("?", 1)[0] if isinstance(origin, str) else ""
        if base not in ("git+file://"+str(root), "file://"+str(root), "path:"+str(root)):
            raise ValueError("matching profile originates from another repository")
        if element.get("active") is not True:
            raise ValueError("matching profile is inactive")
        matches.append(name)
    if len(matches) > 1:
        raise ValueError("duplicate profile elements; manual reconciliation required")
    print(matches[0] if matches else "absent")
except (KeyError, ValueError, TypeError, json.JSONDecodeError) as exc:
    print("Refusing invalid Nix profile state: " + str(exc), file=sys.stderr)
    sys.exit(1)
' "$mode" "$expected_profile")"

if [[ $found == absent ]]; then
  echo "nix.Profile: installing $mode profile"
  nix --extra-experimental-features 'nix-command flakes' profile add "$installable"
else
  echo "nix.Profile: upgrading existing $found profile"
  nix --extra-experimental-features 'nix-command flakes' profile upgrade "$found"
fi

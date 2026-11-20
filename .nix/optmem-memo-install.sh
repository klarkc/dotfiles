#!/usr/bin/env bash
# Install the exact reviewed upstream OptMem CLI required by the isolated smokes.
# This does not initialize memory, configure a service or activate a writer.
set -euo pipefail
umask 077

expected_sha="3dc120d01be3115ef6267eab4103e7909fc830d6227b549f20991ba999ee9ffb"
source_url="https://raw.githubusercontent.com/VictorTaelin/OptMem/1fb164cf39028047781f72ac3bb1e5a691c1dcb0/memo"
target_dir="${HOME:?}/.local/state/optmem-prototype-2026-10-08"
target="$target_dir/memo"

if [[ -L $target_dir || -L $target ]]; then
  echo 'Refusing symlinked OptMem test dependency path.' >&2
  exit 1
fi
if [[ -e $target ]]; then
  [[ -f $target ]] || {
    echo 'OptMem dependency is not a regular file.' >&2
    exit 1
  }
  actual="$(sha256sum "$target" | cut -d ' ' -f 1)"
  [[ $actual == "$expected_sha" ]] || {
    echo 'Existing OptMem dependency has an unexpected digest: refusing replacement.' >&2
    exit 1
  }
  echo 'OptMem pinned upstream memo already verified; no changes.'
  exit 0
fi
install -d -m 700 "$target_dir"
tmp="$(mktemp "$target_dir/.memo.download.XXXXXXXX")"
trap 'rm -f -- "$tmp"' EXIT
curl --fail --location --silent --show-error --max-time 35 "$source_url" --output "$tmp"
actual="$(sha256sum "$tmp" | cut -d ' ' -f 1)"
[[ $actual == "$expected_sha" ]] || {
  echo 'Downloaded OptMem memo failed SHA-256 verification; refusing installation.' >&2
  exit 1
}
if [[ -e $target || -L $target ]]; then
  echo 'OptMem dependency appeared concurrently; refusing overwrite.' >&2
  exit 1
fi
install -m 600 "$tmp" "$target"
[[ "$(sha256sum "$target" | cut -d ' ' -f 1)" == "$expected_sha" ]] || {
  echo 'Installed memo digest differs; refusing to continue.' >&2
  exit 1
}
echo 'Pinned upstream OptMem memo installed for isolated tests; no memory activated.'

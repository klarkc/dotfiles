set -euo pipefail
runtime_state="$HOME/.desktop-commander-device/device.json"
snapshot="${DESKTOP_COMMANDER_STATE_FILE:-$HOME/.profile_override.desktop-commander.json}"

restore_state() {
  [ -s "$runtime_state" ] && exit 0
  [ -r "$snapshot" ] || {
    echo "Missing Desktop Commander credential snapshot" >&2
    exit 1
  }
  umask 077
  mkdir -p "$(dirname "$runtime_state")"
  install -m 600 "$snapshot" "$runtime_state"
}

sync_state() {
  [ -r "$runtime_state" ] || {
    echo "Missing Desktop Commander runtime state" >&2
    exit 1
  }
  umask 077
  install -m 600 "$runtime_state" "$snapshot"
  echo "Desktop Commander credential snapshot synchronized"
}

case "${1:-}" in
restore) restore_state ;;
sync | migrate) sync_state ;;
*)
  echo "usage: desktop-commander-state {restore|sync|migrate}" >&2
  exit 2
  ;;
esac

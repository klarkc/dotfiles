set -euo pipefail
runtime_dir="${RUNTIME_DIRECTORY:-${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/desktop-commander-remote}"
mkdir -p "$runtime_dir"
chmod 700 "$runtime_dir"
probe="$runtime_dir/optmem-probe"
umask 077
od -An -N16 -tx1 /dev/urandom | tr -d " \n" >"$probe"
export DESKTOP_COMMANDER_OPTMEM_PROBE_FILE="$probe"
exec desktop-commander remote

#!/usr/bin/env bash
# Run TPM plugin installer in isolated, temporary tmux server; never root.
set -euo pipefail
tpm="$HOME/.tmux/plugins/tpm/scripts/install_plugins.sh"
if [[ ! -x $tpm || ! -f $HOME/.tmux.conf ]]; then
  echo 'Missing TPM executable or ~/.tmux.conf' >&2
  exit 1
fi
tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-tpm.XXXXXX")"
socket="$tmp_dir/socket"
cleanup() {
  tmux -S "$socket" kill-server >/dev/null 2>&1 || true
  rm -rf -- "$tmp_dir"
}
trap cleanup EXIT
tmux -S "$socket" -f "$HOME/.tmux.conf" new-session -d -s dotfiles-plugins 'sleep 600'
tmux -S "$socket" set-environment -g TMUX_PLUGIN_MANAGER_PATH "$HOME/.tmux/plugins/"
TMUX="$socket,0,0" "$tpm"

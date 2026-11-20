#!/usr/bin/env bash
# Deterministic terminal launcher contract, without X11 or real GUI processes.
set -euo pipefail
cd "${1:?pass the dotfiles root as argument}"
sh -n .local/bin/terminal-fallback

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir "$tmp/bin"
cat >"$tmp/bin/wezterm" <<'STUB'
#!/bin/sh
printf 'wezterm' >>"$TERMINAL_TEST_LOG"
printf ' <%s>' "$@" >>"$TERMINAL_TEST_LOG"
printf '\n' >>"$TERMINAL_TEST_LOG"
exit 0
STUB
chmod +x "$tmp/bin/wezterm"

# 1. Missing Alacritty: WezTerm with expected flags and argument propagation.
TERMINAL_TEST_LOG="$tmp/out" PATH="$tmp/bin" /bin/sh .local/bin/terminal-fallback --class demo
test "$(cat "$tmp/out")" = 'wezterm <start> <--always-new-process> <--class> <demo>'

cat >"$tmp/bin/alacritty" <<'STUB'
#!/bin/sh
printf 'alacritty\n' >>"$TERMINAL_TEST_LOG"
exit "$TERMINAL_TEST_EXIT"
STUB
chmod +x "$tmp/bin/alacritty"

# 2. Successful Alacritty: never open WezTerm.
: >"$tmp/out"
TERMINAL_TEST_EXIT=0 TERMINAL_TEST_LOG="$tmp/out" PATH="$tmp/bin" /bin/sh .local/bin/terminal-fallback
test "$(cat "$tmp/out")" = alacritty

# 3. Failed Alacritty: fail over to WezTerm with no duplicate window shortcut.
: >"$tmp/out"
TERMINAL_TEST_EXIT=1 TERMINAL_TEST_LOG="$tmp/out" PATH="$tmp/bin" /bin/sh .local/bin/terminal-fallback
test "$(cat "$tmp/out")" = "$(printf 'alacritty\nwezterm <start> <--always-new-process>')"

# The XMonad terminal and its one hotkey both call the same launcher.
grep -Fq 'terminal = "~/.local/bin/terminal-fallback"' .config/xmonad/xmonad.hs
grep -Fq '("M-S-<Return>", spawn "~/.local/bin/terminal-fallback")' .config/xmonad/xmonad.hs
! grep -Fq '("M-C-<Return>", spawn "xterm")' .config/xmonad/xmonad.hs
echo 'PASS: Alacritty preferred, WezTerm fallback, no duplicate shortcut'

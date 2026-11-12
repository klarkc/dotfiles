#!/usr/bin/env bash
# Deterministic terminal launcher contract, without X11 or Nix builds.
set -euo pipefail
cd "${1:?pass the dotfiles root as argument}"
sh -n .local/bin/terminal-fallback

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir "$tmp/bin"
cat >"$tmp/bin/xterm" <<'SH'
#!/bin/sh
printf 'xterm\n' >>"$TERMINAL_TEST_LOG"
exit 0
SH
chmod +x "$tmp/bin/xterm"

# 1. Missing Alacritty: launch xterm.
TERMINAL_TEST_LOG="$tmp/out" PATH="$tmp/bin" /bin/sh .local/bin/terminal-fallback
test "$(cat "$tmp/out")" = xterm

cat >"$tmp/bin/alacritty" <<'SH'
#!/bin/sh
printf 'alacritty\n' >>"$TERMINAL_TEST_LOG"
exit "$TERMINAL_TEST_EXIT"
SH
chmod +x "$tmp/bin/alacritty"

# 2. Normal Alacritty exit: never open xterm.
: >"$tmp/out"
TERMINAL_TEST_EXIT=0 TERMINAL_TEST_LOG="$tmp/out" PATH="$tmp/bin" /bin/sh .local/bin/terminal-fallback
test "$(cat "$tmp/out")" = alacritty

# 3. Alacritty startup failure: automatically open xterm.
: >"$tmp/out"
TERMINAL_TEST_EXIT=1 TERMINAL_TEST_LOG="$tmp/out" PATH="$tmp/bin" /bin/sh .local/bin/terminal-fallback
test "$(cat "$tmp/out")" = "$(printf 'alacritty\nxterm')"

# Both the default XMonad terminal and its single hotkey use this launcher.
grep -Fq 'terminal = "~/.local/bin/terminal-fallback"' .config/xmonad/xmonad.hs
grep -Fq '("M-S-<Return>", spawn "~/.local/bin/terminal-fallback")' .config/xmonad/xmonad.hs
! grep -Fq '("M-C-<Return>", spawn "xterm")' .config/xmonad/xmonad.hs
echo 'PASS: Alacritty preferred, automatic xterm fallback; one shortcut'

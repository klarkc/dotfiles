# OpenCode background browser (Brave + Playwright MCP)

OpenCode communicates with Playwright MCP over stdio. The MCP controls a
second Brave process, isolated from the user's Brave, on a dedicated Xvfb
display :92. It has its own persistent profile under
~/.local/state/opencode-browser/profile (mode 0700), independent cookies,
extension state, and browser history.

The launcher at ~/.local/bin/opencode-browser-mcp verifies the already
installed Enpass Chrome Web Store extension manifest at
~/.config/BraveSoftware/Brave-Browser/Default/Extensions/
kmcfomidfpdkfieipokbalgegidffkal/<version>/manifest.json and loads its
code only, NOT any user Brave profile. It never reads Enpass's local vault
nor writes a master password to any app, file or environment variable.

The dedicated X11 server is protected by an Xauthority cookie, has TCP
disabled, and is kept alive by the user systemd service. Xvfb's socket
may be served by Linux abstract sockets even without /tmp/.X11-unix/X92.
The launcher tests authenticated X11 connectivity (xset), not the socket
pathname. Do not enable PrivateTmp for this service.
Playwright MCP is supplied by pinned Nixpkgs; do not use npm at runtime.

Install:
systemctl --user daemon-reload
systemctl --user enable --now opencode-browser-display.service
nix profile upgrade klarkc
opencode mcp list

If starting from SSH, set the user runtime environment first:
export XDG_RUNTIME_DIR=/run/user/$(id -u)
  export DBUS_SESSION_BUS_ADDRESS=unix:path=$XDG_RUNTIME_DIR/bus

For initial Enpass pairing, run from a graphical terminal:
~/.local/bin/opencode-browser-pair
This briefly replaces Xvfb with a temporary Xephyr window showing the
separate Brave. Click the Enpass icon and authorize inside Enpass Desktop,
using PIN only inside Enpass if asked. Close temporary Brave to return
to Xvfb. The personal Brave is not touched. Human-supervised pairing
remains unverified; extension loading alone is not evidence of pairing.

For secure uploads to MEGA/Icedrive, have the user perform login manually
via the Enpass official extension; the agent can then use the resulting
session and only upload pre-encrypted backup files after approval.

The model is not allowed to use browser_evaluate or browser_run_code in the
OpenCode permission policy. This reduces casual DOM secret reads, but cannot
guarantee isolation of all credentials once typed in a webpage. Playwright's
secrets redaction and allowed-origins are NOT security boundaries. Browser
sessions contain cookies and must remain private (0700). The isolated
browser starts on demand, one OpenCode MCP process at a time per profile.

For diagnostics:
systemctl --user status opencode-browser-display
opencode mcp list

Bump note: pkgs.playwright-mcp follows the locked Nixpkgs input. Review
upstream MCP browser config/extension behavior and rerun:
nix build .#checks.x86_64-linux.opencode-browser-contract
make test
opencode mcp list
and test the extension in the separate profile after any significant bump.

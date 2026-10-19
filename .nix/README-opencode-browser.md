# OpenCode background browser (Brave + Playwright MCP)

OpenCode connects to one shared Playwright MCP server through Streamable HTTP
on http://localhost:18927/mcp. Its systemd service binds only 127.0.0.1
and uses --shared-browser-context so multiple OpenCode clients use the same
browser process and session instead of fighting for a locked profile.

The daemon controls a second Brave, isolated from the user's personal Brave,
on a dedicated Xvfb display :92. It has its own persistent profile under
`$HOME/.local/state/opencode-browser/profile` (mode 0700), independent cookies,
extension state, and browser history.

The user services opencode-browser-display.service (virtual X11 display) and
opencode-browser-mcp.service (HTTP transport) are separate and persistent.
The launcher at `$HOME/.local/bin/opencode-browser-mcp` verifies the already
installed Enpass Chrome Web Store extension manifest at
`$HOME/.config/BraveSoftware/Brave-Browser/Default/Extensions/<extension-id>/<version>/manifest.json` and loads its
code only, NOT any user Brave profile. It never reads Enpass's local vault
nor writes a master password to any app, file or environment variable.

The dedicated X11 server is protected by an Xauthority cookie, has TCP
disabled, and is kept alive by the user systemd service. Xvfb's socket
may be served by Linux abstract sockets even without a socket pathname under `/tmp`.
The launcher tests authenticated X11 connectivity (xset), not the socket
pathname. Do not enable PrivateTmp for this service.
Playwright MCP is supplied by pinned Nixpkgs; do not use npm at runtime.

Install after updating the profile:
systemctl --user daemon-reload
nix profile upgrade klarkc # only when flake Nix inputs/definitions change
systemctl --user enable --now opencode-browser-display.service
systemctl --user enable --now opencode-browser-mcp.service
opencode mcp list

If starting from SSH, set the user runtime environment first:
export XDG_RUNTIME_DIR=/run/user/$(id -u)
export DBUS_SESSION_BUS_ADDRESS=unix:path=$XDG_RUNTIME_DIR/bus

For initial Enpass pairing, run from a graphical terminal:
`$HOME/.local/bin/opencode-browser-pair`
The helper temporarily stops the shared MCP service and opens a visible
Xephyr window with the same isolated Brave profile. Active OpenCode browser
clients are disconnected during pairing; finish browser operations first.
Click the Enpass icon and authorize inside Enpass Desktop,
using PIN only inside Enpass if asked. Close temporary Brave to return
to Xvfb; the helper resumes the shared service, releasing the exclusive
profile lock before restart. The personal Brave is not touched.
The Enpass Desktop pairing prompt was observed, but synthetic credential
autofill has not yet been verified. Loading the extension alone is not
evidence of successful login autofill.

For secure uploads, the user performs login manually via the official
password-manager extension. The agent may use the resulting browser session
only for explicitly approved tasks and pre-encrypted backup files.

The OpenCode permission policy denies browser_evaluate/browser_run_code and
asks approval for explicit snapshots and screenshots. The server uses
--snapshot-mode none and --image-responses omit to avoid sending automatic
sensitive snapshots. This is defense in depth, NOT an isolation guarantee;
other MCP tools or local processes may still access browser data.

The Playwright MCP endpoint does not provide local HTTP authentication.
Binding to 127.0.0.1 limits network exposure but any local process able to
connect to localhost can potentially use the browser and its logged-in
sessions. Never port-forward, tunnel, or expose port 18927 to the LAN.
Cookies and extension pairing state are stored privately (0700) in the
browser profile; never copy the profile to other hosts.

All OpenCode agents connected to this MCP share tabs and session state.
Coordinate concurrent navigation/uploads; one agent can see or change another
agent's page. Session sharing is intentional; it is not tenant isolation.
Playwright's allowed-origins filter is not a security boundary.

For diagnostics:
systemctl --user status opencode-browser-display
systemctl --user status opencode-browser-mcp
opencode mcp list

# A 400 or 405 response without MCP headers can still mean a listening port.

curl --silent --output /dev/null --write-out '%{http_code}' http://localhost:18927/mcp

Multi-client live regression uses a disposable non-Enpass profile/server,
NEVER the production paired port 18927:
python3 .nix/tests/opencode-browser-shared-smoke.py --selftest
python3 .nix/tests/opencode-browser-shared-smoke.py --url http://localhost:18928/mcp

To stop all browser MCP access:
systemctl --user disable --now opencode-browser-mcp.service
systemctl --user disable --now opencode-browser-display.service

Bump note: pkgs.playwright-mcp follows the locked Nixpkgs input. Review
upstream MCP browser config/extension behavior and rerun:
nix build .#checks.x86_64-linux.opencode-browser-contract
make test
opencode mcp list
and test the extension in the separate profile after any significant bump.

The browser service uses its dedicated profile and does not modify the user's
primary browser profile. Extension loading does not prove that pairing has
completed; pairing remains a human-supervised action in the graphical session.

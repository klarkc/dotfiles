#!/usr/bin/env bash
# Rootless Firefox integration for legacy Arch solo098. Never disable signing.
set -euo pipefail
mode="${1:?usage: setup.sh default|chatqueue|import}"
cd "$(dirname "$0")/../.."
if ! command -v firefox >/dev/null 2>&1; then
  echo 'Firefox is required. Install it with the host package manager first.' >&2
  exit 1
fi
case "$mode" in
default)
  command -v xdg-mime >/dev/null || {
    echo 'xdg-mime missing' >&2
    exit 1
  }
  for mime in text/html application/xhtml+xml application/x-extension-html application/pdf x-scheme-handler/http x-scheme-handler/https x-scheme-handler/about x-scheme-handler/unknown x-scheme-handler/mailto x-scheme-handler/webcal; do
    xdg-mime default firefox.desktop "$mime"
  done
  if command -v xdg-settings >/dev/null 2>&1; then
    xdg-settings set default-web-browser firefox.desktop || {
      echo 'xdg-settings could not update this desktop; xdg-mime mappings were applied.' >&2
    }
  fi
  actual="$(xdg-mime query default x-scheme-handler/https)"
  [[ $actual == firefox.desktop ]] || {
    echo "HTTPS default not Firefox: $actual" >&2
    exit 1
  }
  echo 'Firefox set as the default browser for HTTPS/HTTP/HTML/PDF and browser schemes.'
  ;;
chatqueue)
  state="$(
    python3 - <<'PY2'
from pathlib import Path
import zipfile,json
home=Path.home()
profiles=[home/'.config/mozilla/firefox',home/'.mozilla/firefox',home/'.var/app/org.mozilla.firefox/.mozilla/firefox']
found=False
vm=False
for base in profiles:
    if not base.is_dir(): continue
    for xpi in base.glob('*/extensions/*.xpi'):
        if xpi.name=='chatqueue-standalone@sinadehghanian.ir.xpi':
            try:
                with zipfile.ZipFile(xpi) as z:
                    m=json.loads(z.read('manifest.json'))
                    signed=any(n.startswith('META-INF/') and n.endswith('.rsa') for n in z.namelist())
                if m.get('name')=='ChatQueue for Firefox' and signed:
                    found=True
                    print(f'ChatQueue {m.get("version")} official signed XPI already installed in {xpi.parent.parent.name}')
            except (ValueError,KeyError,zipfile.BadZipFile) as exc:
                print(f'Invalid ChatQueue XPI: {type(exc).__name__}')
        try:
            with zipfile.ZipFile(xpi) as z:
                m=json.loads(z.read('manifest.json'))
                gecko=m.get('browser_specific_settings',{}).get('gecko',{})
                # Mozilla's Violentmonkey package localizes its name using
                # __MSG_extName__, so the stable AMO extension ID is needed.
                if m.get('name')=='Violentmonkey' or gecko.get('id')=='{aecec67f-0d10-4fa7-b7c7-609a2db280cf}':
                    vm=True
        except (ValueError,KeyError,zipfile.BadZipFile): pass
print('CHATQUEUE_INSTALLED='+str(found).lower())
print('VIOLENTMONKEY_INSTALLED='+str(vm).lower())
PY2
  )"
  printf '%s\n' "$state"
  if [[ -n ${DISPLAY-} || -n ${WAYLAND_DISPLAY-} ]]; then
    if grep -q 'CHATQUEUE_INSTALLED=false' <<<"$state"; then
      firefox --new-tab 'https://addons.mozilla.org/en-US/firefox/addon/chatqueue-for-firefox/' >/dev/null 2>&1 &
    fi
    if grep -q 'VIOLENTMONKEY_INSTALLED=false' <<<"$state"; then
      firefox --new-tab 'https://addons.mozilla.org/en-US/firefox/addon/violentmonkey/' >/dev/null 2>&1 &
    fi
  fi
  echo 'If a signed add-on is missing, Make opens its AMO installer in a graphical session (Firefox requires your approval).'
  echo '  https://addons.mozilla.org/en-US/firefox/addon/chatqueue-for-firefox/'
  echo '  https://addons.mozilla.org/en-US/firefox/addon/violentmonkey/'
  echo 'Then install/import the original userscript in Violentmonkey:'
  echo "  $(pwd)/.nix/firefox/chatqueue-compat.user.js"
  echo 'The userscript only exposes the button ID, never types or sends messages.'
  echo 'After importing the userscript, reload a test tab and verify the fixed button selector.'
  echo 'Verify two separate ChatQueue jobs before enabling real broker heartbeats.'
  ;;
import)
  # The unsigned local script is our own; upstream addons remain signed.
  # Firefox/Violentmonkey must request approval in their own UI.
  if [[ ! -t 0 ]] || [[ -z ${DISPLAY-} && -z ${WAYLAND_DISPLAY-} ]]; then
    echo 'Import requires an interactive terminal and a graphical Firefox session.' >&2
    exit 2
  fi
  status="$(env -u DISPLAY -u WAYLAND_DISPLAY bash "$0" chatqueue)"
  if grep -qE '^(CHATQUEUE_INSTALLED|VIOLENTMONKEY_INSTALLED)=false$' <<<"$status"; then
    echo 'Install the signed ChatQueue and Violentmonkey addons first: make firefox.ChatQueueInstall' >&2
    exit 1
  fi
  script="$(pwd)/.nix/firefox/chatqueue-compat.user.js"
  runtime="${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}"
  mkdir -p "$runtime"
  tmpdir="$(mktemp -d "$runtime/solo098-userscript.XXXXXXXX")"
  server_pid=''
  cleanup() {
    if [[ -n $server_pid ]]; then
      kill "$server_pid" 2>/dev/null || true
      wait "$server_pid" 2>/dev/null || true
    fi
    rm -rf -- "$tmpdir"
  }
  trap cleanup EXIT
  python3 -u - "$script" >"$tmpdir/port" 2>"$tmpdir/error" <<'PY2' &
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import sys

payload = Path(sys.argv[1]).read_bytes()

class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path != '/chatqueue-compat.user.js':
            self.send_error(404)
            return
        self.send_response(200)
        self.send_header('Content-Type', 'application/javascript; charset=utf-8')
        self.send_header('Content-Length', str(len(payload)))
        self.send_header('Cache-Control', 'no-store')
        self.end_headers()
        self.wfile.write(payload)

    def log_message(self, *args):
        pass

with ThreadingHTTPServer(('127.0.0.1', 0), Handler) as server:
    print(server.server_port, flush=True)
    server.serve_forever()
PY2
  server_pid=$!
  for ((tries = 0; tries < 50; tries++)); do
    [[ -s $tmpdir/port ]] && break
    if ! kill -0 "$server_pid" 2>/dev/null; then
      echo 'Could not launch the loopback userscript installer.' >&2
      exit 1
    fi
    sleep 0.1
  done
  port="$(head -n 1 "$tmpdir/port")"
  if [[ ! $port =~ ^[0-9]{2,5}$ ]]; then
    echo 'Loopback userscript installer did not report a valid port.' >&2
    exit 1
  fi
  echo 'Opening the userscript import in Firefox (localhost only).'
  firefox --new-tab "http://127.0.0.1:$port/chatqueue-compat.user.js" >/dev/null 2>&1 &
  echo 'Review and confirm installation in Violentmonkey; no ChatQueue job will be started.'
  read -r -p 'After confirming the import in Firefox, press Enter to close the temporary server: ' _
  echo 'Import window closed. Verify the button selector in a test chat.'
  ;;
*)
  echo "Unknown setup mode: $mode" >&2
  exit 2
  ;;
esac

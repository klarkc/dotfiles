#!/usr/bin/env python3
"""Offline contract for default Firefox, non-GPU XMonad and ChatQueue shim."""
from pathlib import Path
import re
import sys

root = Path(sys.argv[1])
make = (root / 'Makefile').read_text()
xmonad = (root / '.config/xmonad/xmonad.hs').read_text()
mime = (root / '.config/mimeapps.list').read_text()
shim = (root / '.nix/firefox/chatqueue-compat.user.js').read_text()
setup = (root / '.nix/firefox/setup.sh').read_text()
readme = (root / '.nix/firefox/README.md').read_text()

assert 'firefox.Default firefox.ChatQueueInstall' in make
assert 'firefox.ChatQueueImport:' in make
assert '.nix/firefox/setup.sh import' in make
assert '.nix/firefox/setup.sh default' in make
assert '.nix/firefox/setup.sh chatqueue' in make
assert 'spawn "pgrep -x picom' not in xmonad
assert 'picom --backend' not in xmonad
for scheme in ('http', 'https'):
    assert f'x-scheme-handler/{scheme}=firefox.desktop' in mime
assert 'text/html=firefox.desktop' in mime
assert 'brave-browser.desktop' not in mime
assert 'form[data-chatgpt-composer]' in shim
assert 'button[type="submit"]' in shim
assert 'composer-submit-button' in shim
assert 'MutationObserver' in shim
assert '@match        https://chatgpt.com/*' in shim
assert not re.search(r'\.(?:click|submit)\s*\(', shim)
assert 'All Rights Reserved' in readme
assert 'META-INF/' in setup and 'moz-extension' not in setup
assert '{aecec67f-0d10-4fa7-b7c7-609a2db280cf}' in setup
assert 'xpinstall.signatures.required' not in setup
assert 'sudo ' not in setup
assert "ThreadingHTTPServer(('127.0.0.1', 0), Handler)" in setup
assert "self.path != '/chatqueue-compat.user.js'" in setup
assert 'trap cleanup EXIT' in setup
assert 'if [[ ! -t 0' in setup
assert 'make firefox.ChatQueueImport' in readme
assert 'firefox.RecurrenceTest' in make
assert 'node --test .nix/tests/chatqueue-recurrence-core.test.mjs' in make
assert (root / '.nix/firefox/chatqueue-recurrence-core.mjs').is_file()
assert (root / '.nix/tests/chatqueue-recurrence-core.test.mjs').is_file()
assert 'firefox.ChatQueueLocalBuild:' in make
assert 'build-chatqueue-local.mjs --check' in make
assert 'chatqueue-local-controller.test.mjs' in make
assert 'chatqueue-local-browser.test.mjs' in make
assert 'firefox.ChatQueueLocalE2E: firefox.ChatQueueLocalBuild' in make
assert 'firefox.ChatQueueLocalE2E' not in make.split('all:',1)[1].split('\n',1)[0]
assert 'firefox.ChatQueueLocalE2E' not in make.split('test: ',1)[1].split('\n',1)[0]
browser_e2e = root / '.nix/tests/chatqueue-firefox-e2e.py'
assert browser_e2e.is_file()
e2e_text = browser_e2e.read_text()
assert '127.0.0.1' in e2e_text
assert 'https://chatgpt.com' in e2e_text
assert 'driver.quit()' in e2e_text
assert 'NoSuchWindow' not in e2e_text
assert 'LAB_COOLDOWN_MS = 3_000' in e2e_text
assert 'LAB_TERMINAL_DEADLINE_MS = 8_000' in e2e_text
assert 'run_no_receipt_case' in e2e_text
assert 'run_manual_pause_case' in e2e_text
assert 'BASE_COOLDOWN = 360_000' in e2e_text
assert (root / '.nix/firefox/chatqueue-local-controller.mjs').is_file()
assert (root / '.nix/firefox/chatqueue-local-browser.js').is_file()
assert (root / '.nix/firefox/build-chatqueue-local.mjs').is_file()
assert '/.nix/firefox/chatqueue-local.user.js' in (root / '.gitignore').read_text()
lab = (root / '.nix/firefox/chatqueue-local-browser.js').read_text()
assert 'const MAX_CYCLES = 2' in lab
assert 'const LAB_PREFIX = "CQLOCAL LAB:"' in lab
assert 'await write("STOP", true)' in lab
assert 'recurrenceE2EPassed: true, // LAB test path ONLY' in lab
assert 'completedTurnWitness(latest)' in lab
assert 'No corroborated terminal reply before deadline' in lab
assert 'Current user turn disappeared or was superseded' in lab
assert 'LAB paused during uncertain reply' in lab
assert 'lastTurnHasCompletionAction=' in lab
assert 'button[data-testid="send-button"]' in lab
assert 'document.querySelectorAll("form[data-chatgpt-composer]").length === 1' in lab
assert "composer().querySelectorAll" not in lab
assert 'location.pathname' in lab
assert 'new chat not yet persisted' in (root / '.nix/firefox/README.md').read_text()
assert 'send.addEventListener("click", () => form.requestSubmit())' in e2e_text
assert 'toggleSend();' in e2e_text
assert 'firefox.ChatQueueLocalBuild' not in make.split('all:',1)[1].split('\n',1)[0]
assert 'firefox.RecurrenceTest' in make.split('test: ',1)[1].split('\n',1)[0]
assert 'No vendor contact' in readme
assert 'PAUSED / NO_GO' in readme
print('PASS: Firefox defaults, no Picom, signed ChatQueue and non-sending selector shim')

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
assert 'No vendor contact' in readme
assert 'PAUSED / NO_GO' in readme
print('PASS: Firefox defaults, no Picom, signed ChatQueue and non-sending selector shim')

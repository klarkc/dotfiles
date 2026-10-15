#!/usr/bin/env python3
"""Run pinned SearXNG via Nix WITHOUT installing packages into the user profile."""
import os
from pathlib import Path
import secrets
import subprocess
import sys

SOURCE = Path(__file__).resolve()
ROOT = SOURCE.parents[3]
SETTINGS = SOURCE.with_name('searxng-settings.yml')
EXPR = f'(builtins.getFlake "{ROOT}").inputs.nixpkgs.legacyPackages.x86_64-linux.searxng'


def main():
    if not (ROOT / 'flake.nix').is_file():
        sys.exit('Expected the dotfiles flake in ' + str(ROOT))
    try:
        result = subprocess.run(['/nix/var/nix/profiles/default/bin/nix', 'build', '--no-link', '--print-out-paths', '--impure', '--expr', EXPR], check=True, text=True, capture_output=True, timeout=120)
        path = Path(result.stdout.strip().splitlines()[-1]) / 'bin' / 'searxng-run'
    except (subprocess.SubprocessError, IndexError) as exc:
        sys.exit(f'Unable to resolve pinned SearXNG: {exc}')
    if not path.is_file():
        sys.exit('SearXNG package missing: ' + str(path))
    os.environ['SEARXNG_SETTINGS_PATH'] = str(SETTINGS)
    os.environ['SEARXNG_PORT'] = '8980'
    os.environ['SEARXNG_BIND_ADDRESS'] = '127.0.0.1'
    os.environ['SEARXNG_SECRET'] = secrets.token_hex(32)
    os.execv(str(path), [str(path)])


if __name__ == '__main__':
    main()

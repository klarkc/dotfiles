# Upstream Dependency Maintenance

## Overview

This system provides automated monitoring of upstream dependencies for the dotfiles project. It implements the cadence proposal approved via DT-080/DT-096.

## Dependency Tiers

| Tier | Frequency | Criteria | Dependencies |
|------|-----------|----------|--------------|
| Critical | Weekly | Core engine, foundational tooling, base system | Fusion, skills.sh, nixpkgs-unstable |
| Moderate | Monthly | Build/formatting tooling, daily-use disruptive tools | flake-parts, nix-fast-build, treefmt-nix, Tmux, Alacritty, Vim |
| Low | Quarterly | Nice-to-have, non-critical tools | Lemurs, Nord, Papirus, crush, codex, lumen, Sunshine, etc. |

## How It Works

### deps-check Script

Location: `.local/bin/deps-check`

Usage:
```bash
# Human-readable output
deps-check

# Machine-readable JSON output (for automation)
deps-check --json

# Quiet mode (no stdout, only logging)
deps-check --quiet
```

The script:
1. Clones each git-tracked dependency (shallow, depth=1)
2. Extracts latest commit date and version tag
3. Outputs summary by tier
4. Logs results to `~/.cache/deps-check.log`

### Systemd Timer

A systemd timer runs the check weekly:
```bash
# Enable the timer
systemctl --user enable --now deps-check.timer

# Check timer status
systemctl --user status deps-check.timer

# View logs
journalctl --user-unit deps-check.service -f
```

## Maintenance Workflow

1. The timer runs `deps-check` weekly
2. Results are logged to `~/.cache/deps-check.log`
3. For critical tier, review the log weekly and bump versions in `flake.lock` if needed
4. For moderate tier, review monthly via `deps-check --json`
5. For low tier, review quarterly

## Manual Trigger

```bash
deps-check
deps-check --json
```

## Future Enhancements

- Compare against previously stored versions for delta reporting
- Auto-generate PRs for dependency updates
- Integrate with CI to fail on outdated critical deps
- Add notification on significant version changes

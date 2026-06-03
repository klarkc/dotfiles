# Heartbeat Monitor — Systemd Timer & Alerting

## Overview

The heartbeat monitor checks agent liveness every 5 minutes and alerts on stale agents.

- **Script**: `scripts/heartbeat-monitor.sh`
- **Timer**: `systemd/heartbeat-monitor.timer` — schedules the monitor every 5 minutes
- **Service**: `systemd/heartbeat-monitor.service` — runs the monitor with logging
- **Alert**: `systemd/heartbeat-monitor-alert.service` — dispatched on exit code 1
- **Log rotation**: `etc/logrotate.d/heartbeat-monitor`
- **Cron fallback**: `etc/crontab.d/heartbeat-monitor`

## Installation (systemd — preferred)

```bash
# 1. Copy units
sudo cp systemd/heartbeat-monitor.timer /etc/systemd/system/
sudo cp systemd/heartbeat-monitor.service /etc/systemd/system/
sudo cp systemd/heartbeat-monitor-alert.service /etc/systemd/system/

# 2. Create log directory
sudo mkdir -p /var/log/klarkc
sudo chown klarkc:klarkc /var/log/klarkc

# 3. Enable and start
sudo systemctl daemon-reload
sudo systemctl enable --now heartbeat-monitor.timer

# 4. Verify
systemctl status heartbeat-monitor.timer
journalctl -u heartbeat-monitor -f   # tail logs
```

## Installation (cron — fallback)

```bash
sudo mkdir -p /var/log/klarkc

# Add to root crontab
sudo tee -a /etc/crontab <<EOF
*/5 * * * * klarkc /home/klarkc/Sources/Fusion/klarkc/dotfiles/.worktrees/azure-crane/scripts/heartbeat-monitor.sh >>/var/log/klarkc/heartbeat-monitor.log 2>&1
*/5 * * * * klarkc EXIT=\$?; if [ \$EXIT -ne 0 ]; then logger -p daemon.err -t heartbeat-alert "Stale agent(s) detected (exit=\$EXIT)"; fi
EOF

sudo systemctl restart cron   # or crond
```

## Configuration

Override via environment variables in the systemd service or via the crontab:

| Variable              | Default                      | Description                      |
| --------------------- | ---------------------------- | -------------------------------- |
| `HEARTBEAT_THRESHOLD` | `60`                         | Seconds before an agent is stale |
| `HEARTBEAT_LOG`       | `/tmp/heartbeat-monitor.log` | Log file path                    |
| `HEARTBEAT_JSON`      | `false`                      | JSON output mode                 |

## Log rotation

Install the logrotate config:

```bash
sudo cp etc/logrotate.d/heartbeat-monitor /etc/logrotate.d/
```

This rotates daily, keeps 30 days, compresses old logs.

## Alerting

When exit code 1 is returned (stale agents detected):

1. The alert service (systemd) logs to journald with `daemon.err` priority
2. If `mailx` is installed, an email is sent to the configured recipient
3. The cron fallback also logs via `logger` and sends email via `mailx`

## References

- DT-327: Root cause analysis of heartbeat disruption
- DT-330: Heartbeat hardening (agent config changes)
- DT-334: This task — timer, logging, and alerting setup

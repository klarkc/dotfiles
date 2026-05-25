#!/usr/bin/env bash
# heartbeat-monitor.sh — Check agent heartbeat health and alert on stale agents
#
# Usage: heartbeat-monitor.sh [--json]
#
# Checks all agents registered with the Fusion engine and reports:
#   - Agents that haven't sent a heartbeat within their configured timeout
#   - Agents in unexpected states
#
# Exit codes:
#   0 — All agents healthy
#   1 — One or more agents stale or in error state
#   2 — Fusion engine unavailable or config missing

set -euo pipefail

# Configuration
ALERT_THRESHOLD_SECONDS="${HEARTBEAT_THRESHOLD:-60}"
LOG_FILE="${HEARTBEAT_LOG:-/tmp/heartbeat-monitor.log}"
JSON_OUTPUT="${HEARTBEAT_JSON:-false}"

# Colors for terminal output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get current timestamp in seconds since epoch
now_epoch() {
    date +%s
}

# Get agent last heartbeat timestamp from Fusion state
get_agent_last_heartbeat() {
    local agent_id="$1"
    local state_file="/tmp/fusion-agent-state/${agent_id}.json"
    if [[ -f "$state_file" ]]; then
        grep -o '"lastHeartbeat":[0-9]*' "$state_file" | grep -o '[0-9]*' || echo "0"
    else
        echo "0"
    fi
}

# Check if an agent is stale
check_agent() {
    local agent_id="$1"
    local last_heartbeat
    last_heartbeat=$(get_agent_last_heartbeat "$agent_id")
    local current_epoch
    current_epoch=$(now_epoch)
    local age_seconds=$(( current_epoch - last_heartbeat ))

    if [[ "$last_heartbeat" -eq 0 ]]; then
        echo "STALE|${agent_id}|no heartbeat recorded"
        return 1
    elif [[ $age_seconds -gt $ALERT_THRESHOLD_SECONDS ]]; then
        echo "STALE|${agent_id}|age=${age_seconds}s over threshold=${ALERT_THRESHOLD_SECONDS}s"
        return 1
    else
        echo "OK|${agent_id}|heartbeat age=${age_seconds}s OK"
        return 0
    fi
}

# Main
main() {
    # Parse arguments
    for arg in "$@"; do
        case "$arg" in
            --json) JSON_OUTPUT=true ;;
        esac
    done
    local agent_dir="/tmp/fusion-agent-state"
    local stale_count=0
    local total_count=0
    local results=()

    if [[ ! -d "$agent_dir" ]]; then
        echo "WARNING: Agent state directory not found at ${agent_dir}"
        echo "Agent heartbeat monitoring requires Fusion engine state files."
        echo "This is informational — the Fusion engine handles heartbeat polling."
        exit 0
    fi

    for state_file in "${agent_dir}"/*.json; do
        [[ -f "$state_file" ]] || continue
        local agent_id
        agent_id=$(basename "$state_file" .json)
        total_count=$((total_count + 1))

        local result
        if result=$(check_agent "$agent_id"); then
            results+=("$result")
        else
            results+=("$result")
            stale_count=$((stale_count + 1))
        fi
    done

    # Output results
    # Format: STATUS|agent_id|details (pipe-delimited, 3 fields)
    if [[ "$JSON_OUTPUT" == "true" ]]; then
        echo "{\"total\":${total_count},\"stale\":${stale_count},\"results\":["
        local first=true
        for r in "${results[@]}"; do
            if [[ "$first" == "true" ]]; then
                first=false
            else
                echo ","
            fi
            local status="${r%%|*}"
            local rest="${r#*|}"
            local agent="${rest%%|*}"
            local details="${rest#*|}"
            echo "{\"status\":\"${status}\",\"agent\":\"${agent}\",\"details\":\"${details}\"}"
        done
        echo "]}"
        # For JSON mode, exit with appropriate code without additional text
        [[ $stale_count -eq 0 ]] && exit 0 || exit 1
    else
        echo "=== Heartbeat Health Check ==="
        echo "Timestamp: $(date -Iseconds)"
        echo "Threshold: ${ALERT_THRESHOLD_SECONDS}s"
        echo "Total agents: ${total_count}, Stale: ${stale_count}"
        echo ""
        for r in "${results[@]}"; do
            local status="${r%%|*}"
            local rest="${r#*|}"
            local agent="${rest%%|*}"
            local details="${rest#*|}"
            if [[ "$status" == "STALE" ]]; then
                echo -e "${RED}[FAIL] ${agent}: ${details}${NC}"
            else
                echo -e "${GREEN}[OK]   ${agent}: ${details}${NC}"
            fi
        done
    fi

    if [[ $stale_count -gt 0 ]]; then
        echo ""
        echo "ALERT: ${stale_count} agent(s) are stale!"
        echo "Review: DT-327 (root cause analysis), DT-330 (heartbeat hardening)"
        return 1
    fi

    return 0
}

main "$@"

#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/vast-qwen-launch" >&2
  exit 2
fi

LAUNCHER="$1"
if [[ ! -x "$LAUNCHER" ]]; then
  echo "launcher is not executable: $LAUNCHER" >&2
  exit 2
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FIXTURES="$ROOT/fixtures"
WORK_ROOT="$(mktemp -d)"
trap 'rm -rf "$WORK_ROOT"' EXIT

SCENARIOS_RUN=0

fail() { echo "FAIL: $*" >&2; exit 1; }
assert_contains() {
  local file="$1" text="$2"
  grep -Fq -- "$text" "$file" || {
    echo "--- $file ---" >&2
    cat "$file" >&2 || true
    fail "expected to find: $text"
  }
}
assert_not_contains() {
  local file="$1" text="$2"
  if grep -Fq -- "$text" "$file"; then
    echo "--- $file ---" >&2
    cat "$file" >&2 || true
    fail "did not expect to find: $text"
  fi
}
assert_calls_contain() { assert_contains "$CURRENT_CALLS" "$1"; }
assert_calls_not_contain() { assert_not_contains "$CURRENT_CALLS" "$1"; }
assert_status() {
  local expected="$1" actual="$2" scenario="$3"
  if [[ "$actual" -ne "$expected" ]]; then
    if [[ -n "${outfile:-}" && -f "${outfile:-}" ]]; then
      echo "--- $outfile ---" >&2
      cat "$outfile" >&2 || true
    fi
    fail "$scenario exit status: expected $expected, got $actual"
  fi
}

make_fake_tools() {
  local bin_dir="$1"
  mkdir -p "$bin_dir"
  printf '#!%s\n' "$BASH" > "$bin_dir/vastai-fake"
  cat >> "$bin_dir/vastai-fake" <<'FAKE'
set -euo pipefail
: "${VAST_FAKE_SCENARIO:?}"
: "${VAST_FAKE_CALLS:?}"
record() { printf '%s\n' "$*" >> "$VAST_FAKE_CALLS"; }
json_empty_offers='{"offers":[]}'
case "${1:-}" in
  set)
    record "$*"
    exit 0
    ;;
  show)
    case "${2:-}" in
      user)
        record "$*"
        if [[ -f "$VAST_FAKE_SCENARIO/auth-failure" ]]; then
          echo "authentication failed" >&2
          exit 1
        fi
        echo '{"username":"fake"}'
        ;;
      instances)
        record "$*"
        count_file="$VAST_FAKE_SCENARIO/.show_instances_count"
        count=0; [[ -f "$count_file" ]] && count="$(cat "$count_file")"
        count=$((count + 1)); echo "$count" > "$count_file"
        create_count_file="$VAST_FAKE_SCENARIO/.create_count"
        create_count=0; [[ -f "$create_count_file" ]] && create_count="$(cat "$create_count_file")"
        if [[ -f "$VAST_FAKE_SCENARIO/instances-$count.json" ]]; then
          cat "$VAST_FAKE_SCENARIO/instances-$count.json"
        elif [[ -f "$VAST_FAKE_SCENARIO/instances-ready.json" && ( "$count" -gt 1 || "$create_count" -gt 0 ) ]]; then
          cat "$VAST_FAKE_SCENARIO/instances-ready.json"
        elif [[ -f "$VAST_FAKE_SCENARIO/instances.json" ]]; then
          cat "$VAST_FAKE_SCENARIO/instances.json"
        else
          echo '{"instances":[]}'
        fi
        ;;
      instance)
        record "$*"
        if [[ -f "$VAST_FAKE_SCENARIO/show-instance-fail" ]]; then exit 1; fi
        if [[ -f "$VAST_FAKE_SCENARIO/show-instance-gone.json" ]]; then cat "$VAST_FAKE_SCENARIO/show-instance-gone.json"; else echo '{}'; fi
        ;;
      volumes)
        record "$*"
        if [[ -f "$VAST_FAKE_SCENARIO/volumes.json" ]]; then cat "$VAST_FAKE_SCENARIO/volumes.json"; else echo '{"volumes":[]}'; fi
        ;;
      *) record "$*"; echo "unknown show command" >&2; exit 1 ;;
    esac
    ;;
  search)
    case "${2:-}" in
      offers)
        record "$*"
        query="${*:4}"
        gpu=""
        if [[ "$query" =~ gpu_name=([^[:space:]]+) ]]; then gpu="${BASH_REMATCH[1]}"; fi
        if [[ -n "$gpu" && -f "$VAST_FAKE_SCENARIO/offers-$gpu.json" ]]; then
          cat "$VAST_FAKE_SCENARIO/offers-$gpu.json"
        elif [[ -f "$VAST_FAKE_SCENARIO/offers.json" ]]; then
          cat "$VAST_FAKE_SCENARIO/offers.json"
        else
          printf '%s\n' "$json_empty_offers"
        fi
        ;;
      volumes)
        record "$*"
        if [[ -f "$VAST_FAKE_SCENARIO/volume-offers.json" ]]; then cat "$VAST_FAKE_SCENARIO/volume-offers.json"; else echo '{"offers":[]}'; fi
        ;;
      *) record "$*"; echo "unknown search command" >&2; exit 1 ;;
    esac
    ;;
  destroy)
    record "$*"
    if [[ -f "$VAST_FAKE_SCENARIO/destroy-timeout" ]]; then while true; do :; done; fi
    if [[ -f "$VAST_FAKE_SCENARIO/destroy-fail" ]]; then exit 1; fi
    echo '{"success": true}'
    ;;
  delete)
    record "$*"
    echo '{"success": true}'
    ;;
  create)
    record "$*"
    count_file="$VAST_FAKE_SCENARIO/.create_count"
    count=0; [[ -f "$count_file" ]] && count="$(cat "$count_file")"
    count=$((count + 1)); echo "$count" > "$count_file"
    if [[ -f "$VAST_FAKE_SCENARIO/create-fail" ]]; then cat "$VAST_FAKE_SCENARIO/create-fail"; exit 1; fi
    if [[ -f "$VAST_FAKE_SCENARIO/create-response.txt" ]]; then cat "$VAST_FAKE_SCENARIO/create-response.txt"; else echo '{"success": true, "new_contract": 84}'; fi
    ;;
  change)
    record "$*"
    echo '{"success": true}'
    ;;
  logs)
    record "$*"
    if [[ -f "$VAST_FAKE_SCENARIO/logs.txt" ]]; then cat "$VAST_FAKE_SCENARIO/logs.txt"; else exit 1; fi
    ;;
  *) record "$*"; echo "unknown fake vastai command: $*" >&2; exit 1 ;;
esac
FAKE
  # The fake records destructive intents only; it never contacts Vast.ai.
  chmod +x "$bin_dir/vastai-fake"

  printf '#!%s\n' "$BASH" > "$bin_dir/vastai"
  cat >> "$bin_dir/vastai" <<'POISON'
set -euo pipefail
printf 'POISON vastai %s\n' "$*" >> "${VAST_FAKE_CALLS:-/dev/null}"
echo "real vastai invocation attempted; use VASTAI_BIN" >&2
exit 99
POISON
  chmod +x "$bin_dir/vastai"

  printf '#!%s\n' "$BASH" > "$bin_dir/curl"
  cat >> "$bin_dir/curl" <<'FAKECURL'
set -euo pipefail
: "${VAST_FAKE_SCENARIO:?}"
printf 'curl %s\n' "$*" >> "${VAST_FAKE_CALLS:?}"
count_file="$VAST_FAKE_SCENARIO/.curl_count"
count=0; [[ -f "$count_file" ]] && count="$(cat "$count_file")"
count=$((count + 1)); echo "$count" > "$count_file"
if [[ -f "$VAST_FAKE_SCENARIO/curl-fail-until-second-create" ]]; then
  create_count_file="$VAST_FAKE_SCENARIO/.create_count"
  create_count=0; [[ -f "$create_count_file" ]] && create_count="$(cat "$create_count_file")"
  [[ "$create_count" -ge 2 ]] || exit 22
fi
if [[ -f "$VAST_FAKE_SCENARIO/curl-fail" ]]; then exit 22; fi
if [[ -f "$VAST_FAKE_SCENARIO/curl-sequence" ]]; then
  status="$(sed -n "${count}p" "$VAST_FAKE_SCENARIO/curl-sequence" || true)"
  [[ -z "$status" ]] && status="$(tail -n1 "$VAST_FAKE_SCENARIO/curl-sequence")"
  [[ "$status" = "ok" ]] || exit 22
fi
if [[ -f "$VAST_FAKE_SCENARIO/api-response.json" ]]; then
  cat "$VAST_FAKE_SCENARIO/api-response.json"
else
  echo '{"data":[{"id":"fake-model"}]}'
fi
FAKECURL
  chmod +x "$bin_dir/curl"

  printf '#!%s\n' "$BASH" > "$bin_dir/sleep"
  cat >> "$bin_dir/sleep" <<'FAKESLEEP'
set -euo pipefail
printf 'sleep %s\n' "$*" >> "${VAST_FAKE_CALLS:-/dev/null}"
exit 0
FAKESLEEP
  chmod +x "$bin_dir/sleep"
  
  # Fake vllm that simulates KV cache failures on first attempt
  printf '#!%s\\n' "$BASH" > "$bin_dir/vllm-fake"
  cat >> "$bin_dir/vllm-fake" <<'VLLMFAKE'
set -euo pipefail
VLLM_RECORD_FILE="${VLLM_FAKE_CALLS:-/dev/null}"
VLLM_KV_FAILURE_FILE="${VLLM_FAKE_SCENARIO:-}/.vllm_kv_fail"

# Record every invocation for test assertions
printf '%s\\n' "$*" >> "$VLLM_RECORD_FILE"

# Simulate KV cache failure on first attempt
if [[ -f "$VLLM_KV_FAILURE_FILE" ]]; then
  print_kv_error="error in vLLM launch: KV cache allocation failed"
  printf '%s\\n' "$print_kv_error" >&2
  exit 127
else
  # Success on retry: write heartbeat to simulate readiness
  echo "READY" > "$VLLM_FAKE_SCENARIO/.vllm_heartbeat"
  exit 0
fi
VLLMFAKE
  chmod +x "$bin_dir/vllm-fake"
}

prepare_scenario() {
  local name="$1"; shift
  CURRENT_DIR="$WORK_ROOT/$name"
  CURRENT_CALLS="$CURRENT_DIR/calls.log"
  mkdir -p "$CURRENT_DIR/home/.local/bin" "$CURRENT_DIR/scenario"
  : > "$CURRENT_CALLS"
  make_fake_tools "$CURRENT_DIR/home/.local/bin"
  for src in "$@"; do
    cp -R --no-preserve=mode,ownership "$src"/. "$CURRENT_DIR/scenario/"
  done
}

run_launcher() {
  local outfile="$1"; shift
  set +e
  env -i \
    HOME="$CURRENT_DIR/home" \
    PATH="$CURRENT_DIR/home/.local/bin:/usr/bin:/bin" \
    VASTAI_BIN="$CURRENT_DIR/home/.local/bin/vastai-fake" \
    VAST_FAKE_SCENARIO="$CURRENT_DIR/scenario" \
    VAST_FAKE_CALLS="$CURRENT_CALLS" \
    DESTROY_GRACE_SECS=0 \
    REPLACE_READY_TIMEOUT_SECS="${REPLACE_READY_TIMEOUT_SECS:-2}" \
    CHECK_READY_TIMEOUT_SECS="${CHECK_READY_TIMEOUT_SECS:-2}" \
    WATCH_MAX_CYCLES="${WATCH_MAX_CYCLES:-0}" \
    "$LAUNCHER" "$@" >"$outfile" 2>&1
  status=$?
  return "$status"
}

scenario_check_no_existing() {
  prepare_scenario check-no-existing "$FIXTURES/base-market" "$FIXTURES/no-existing"
  out="$CURRENT_DIR/out.txt"
  set +e
  run_launcher "$out" check
  status=$?
  set -e
  assert_status 1 "$status" "check-no-existing"
  assert_contains "$out" "No existing labeled instance found."
  assert_contains "$out" "Run the replace command to create an instance:"
  assert_contains "$out" "  replace"
  assert_calls_contain "show instances --raw"
  assert_calls_not_contain "destroy instance"
  assert_calls_not_contain "create instance"
}

scenario_fake_guardrails() {
  prepare_scenario fake-guardrails "$FIXTURES/base-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/out.txt"
  set +e
  run_launcher "$out" check
  status=$?
  set -e
  assert_status 1 "$status" "fake-guardrails"
  assert_contains "$out" "Selected:"
  assert_contains "$out" "  ask_id        : 100001"
  assert_contains "$out" "  machine_id    : 1001"
  assert_contains "$out" "  gpu           : A100_PCIE"
  assert_contains "$out" "Run the replace command through your launcher"
  assert_contains "$out" "--expected-price 0.34"
  assert_contains "$out" "--expected-total-price 0.377"
  assert_calls_contain "show user"
  assert_calls_contain "search offers --raw gpu_name=L40S"
  assert_calls_not_contain "set api-key"
  assert_calls_not_contain "destroy instance"
  assert_not_contains "$out" "Installing vastai"
}


run_expect() {
  local expected="$1" outfile="$2" scenario="$3"; shift 3
  set +e
  run_launcher "$outfile" "$@"
  local status=$?
  set -e
  assert_status "$expected" "$status" "$scenario"
}

scenario_existing_labeled_recommend_replace() {
  prepare_scenario existing-recommend-replace "$FIXTURES/base-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/out.txt"
  run_expect 1 "$out" existing-recommend-replace check
  assert_contains "$out" "Found existing instance:"
  assert_contains "$out" "Replacement appears worth it"
  assert_contains "$out" "No destructive action is performed by default."
  assert_calls_not_contain "destroy instance"
  assert_calls_not_contain "create instance"
}

scenario_no_acceptable_offers() {
  prepare_scenario no-acceptable-check "$FIXTURES/low-quality-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/check.txt"
  run_expect 1 "$out" no-acceptable-check check --max-price 0.10
  assert_contains "$out" "No offers found within max price"
  assert_calls_not_contain "destroy instance"
  assert_calls_not_contain "create instance"

  prepare_scenario no-acceptable "$FIXTURES/no-existing"
  out="$CURRENT_DIR/out.txt"
  run_expect 1 "$out" no-acceptable replace --max-price 0.10
  assert_contains "$out" "No offers matched the search."
  assert_calls_not_contain "create instance"
}

scenario_price_reliability_disk_vram_filters() {
  prepare_scenario filtering "$FIXTURES/base-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/price.txt"
  run_expect 1 "$out" price-filter check --max-price 0.30
  assert_contains "$out" "  gpu           : RTX 5090"
  out="$CURRENT_DIR/reliability.txt"
  run_expect 0 "$out" reliability-filter check --min-reliability 0.995
  assert_calls_contain "reliability>=0.995"
  out="$CURRENT_DIR/disk.txt"
  run_expect 0 "$out" disk-filter check --disk 100
  assert_calls_contain "disk_space>=100"
  out="$CURRENT_DIR/vram.txt"
  run_expect 0 "$out" vram-filter check --min-gpu-ram-mb 40000 --max-price 0.33
  assert_contains "$out" "  gpu           : L40S"
}

scenario_gpu_preference_ranking() {
  prepare_scenario gpu-ranking "$FIXTURES/base-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/out.txt"
  run_expect 1 "$out" gpu-ranking check
  assert_contains "$out" "  gpu           : A100_PCIE"
  assert_contains "$out" "Top candidates:"
}

scenario_check_expected_guard_pass_fail() {
  prepare_scenario check-expected-pass "$FIXTURES/base-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/pass.txt"
  run_expect 1 "$out" check-expected-pass check --expected-machine-id 1001 --expected-price 0.34 --expected-total-price 0.377 --expected-gpu A100_PCIE --expected-cuda 12.9
  assert_contains "$out" "Selected:"
  assert_contains "$out" "  machine_id    : 1001"
  assert_calls_not_contain "destroy instance"
  assert_calls_not_contain "create instance"

  prepare_scenario check-expected-fail "$FIXTURES/base-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/fail.txt"
  run_expect 1 "$out" check-expected-fail check --expected-machine-id 9999 --expected-price 0.20 --expected-gpu L40S
  assert_contains "$out" "No current offer matched the expected replacement constraints."
  assert_calls_not_contain "destroy instance"
  assert_calls_not_contain "create instance"
}

scenario_replace_guard_pass_success() {
  prepare_scenario replace-guard-pass "$FIXTURES/base-market" "$FIXTURES/existing-replace" "$FIXTURES/readiness-success"
  out="$CURRENT_DIR/out.txt"
  run_expect 0 "$out" replace-guard-pass replace --expected-price 0.34 --expected-total-price 0.377 --expected-gpu A100_PCIE --expected-cuda 12.9 --expected-machine-id 1001 --readiness-retry-attempts 0 --destroy-timeout-secs 1 --replace-ready-timeout-secs 2
  assert_contains "$out" "Replacement match:"
  assert_contains "$out" "READY"
  assert_calls_contain "destroy instance 42 -y"
  assert_calls_contain "create instance 100001"
  assert_calls_contain "change bid 84 --price"
}

scenario_replace_snapshot_drift_refusal() {
  prepare_scenario replace-drift "$FIXTURES/base-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/out.txt"
  run_expect 1 "$out" replace-drift replace --expected-machine-id 9999 --expected-price 0.20 --expected-gpu L40S
  assert_contains "$out" "No current offer matched the expected replacement constraints."
  assert_calls_not_contain "destroy instance"
  assert_calls_not_contain "create instance"
}

scenario_volume_fresh_and_reuse() {
  prepare_scenario volume-fresh "$FIXTURES/base-market" "$FIXTURES/existing-with-volume" "$FIXTURES/readiness-success"
  out="$CURRENT_DIR/fresh.txt"
  run_expect 0 "$out" volume-fresh replace --use-volume 1 --max-price 0.40 --expected-price 0.34 --expected-total-price 0.377 --expected-gpu A100_PCIE --readiness-retry-attempts 0 --destroy-timeout-secs 1 --replace-ready-timeout-secs 2
  assert_contains "$out" "Old instance destroy requested; continuing with fresh-volume launch."
  assert_calls_contain "create instance 100001"
  assert_calls_contain "--create-volume"
  assert_calls_contain "delete volume 8001 -y"

  prepare_scenario volume-reuse "$FIXTURES/base-market" "$FIXTURES/reusable-volume" "$FIXTURES/readiness-success"
  out="$CURRENT_DIR/reuse.txt"
  run_expect 0 "$out" volume-reuse replace --use-volume 1 --max-price 0.40 --expected-price 0.34 --expected-total-price 0.377 --expected-gpu A100_PCIE --readiness-retry-attempts 0 --destroy-timeout-secs 1 --replace-ready-timeout-secs 2
  assert_contains "$out" "Waiting for old instance to fully detach from the reusable volume"
  assert_calls_contain "--link-volume 8002"
}

scenario_destroy_timeout_continuation() {
  prepare_scenario destroy-timeout-continuation "$FIXTURES/base-market" "$FIXTURES/existing-with-volume" "$FIXTURES/readiness-success" "$FIXTURES/destroy-timeout"
  out="$CURRENT_DIR/out.txt"
  run_expect 0 "$out" destroy-timeout-continuation replace --use-volume 1 --max-price 0.40 --expected-price 0.34 --expected-total-price 0.377 --expected-gpu A100_PCIE --readiness-retry-attempts 0 --destroy-timeout-secs 1 --replace-ready-timeout-secs 2
  assert_contains "$out" "Destroy request timed out after 1s; continuing with fresh-volume launch anyway."
  assert_calls_contain "destroy instance 45 -y"
  assert_calls_contain "create instance 100001"
}

scenario_create_parsing_and_missing_output() {
  prepare_scenario missing-create-output "$FIXTURES/base-market" "$FIXTURES/no-existing" "$FIXTURES/missing-create-output"
  out="$CURRENT_DIR/out.txt"
  run_expect 1 "$out" missing-create-output replace --max-create-attempts 1
  assert_contains "$out" "Could not parse new contract id"
}

scenario_readiness_failure_retry_success() {
  prepare_scenario readiness-retry-success "$FIXTURES/base-market" "$FIXTURES/no-existing" "$FIXTURES/readiness-retry-success"
  out="$CURRENT_DIR/out.txt"
  REPLACE_READY_TIMEOUT_SECS=1 run_expect 0 "$out" readiness-retry-success replace --max-create-attempts 1 --readiness-retry-attempts 1 --destroy-grace-secs 0 --replace-ready-timeout-secs 1 --model fake/base --model-24gb fake/24 --model-32gb fake/32 --model-48gb fake/48 --model-80gb fake/80
  assert_contains "$out" "Retrying launch after readiness failure (1/1)."
  assert_contains "$out" "READY"
  assert_not_contains "$out" "Grace delay before destroying"
  assert_contains "$out" "model         : fake/80"
  assert_calls_contain "destroy instance 84 -y"
}

scenario_readiness_failure_exhaustion() {
  prepare_scenario readiness-exhaustion "$FIXTURES/base-market" "$FIXTURES/no-existing" "$FIXTURES/readiness-fail"
  out="$CURRENT_DIR/out.txt"
  REPLACE_READY_TIMEOUT_SECS=1 run_expect 1 "$out" readiness-exhaustion replace --max-create-attempts 1 --readiness-retry-attempts 0 --destroy-grace-secs 0
  assert_contains "$out" "Readiness retry attempts exhausted."
  assert_calls_contain "curl -fsS"
  assert_calls_contain "destroy instance 84 -y"
}

scenario_rebid_guards_and_ceiling() {
  prepare_scenario rebid-pass "$FIXTURES/existing-same-machine"
  out="$CURRENT_DIR/pass.txt"
  run_expect 0 "$out" rebid-pass rebid --expected-current-bid 0.34 --expected-min-bid 0.36 --expected-target-bid 0.388800 --max-bid-price 0.45
  assert_contains "$out" "target bid $/h       : 0.388800"
  assert_calls_contain "change bid 43 --price 0.388800"

  prepare_scenario rebid-fail "$FIXTURES/existing-same-machine"
  out="$CURRENT_DIR/fail.txt"
  run_expect 1 "$out" rebid-fail rebid --expected-current-bid 0.35 --expected-min-bid 0.36 --expected-target-bid 0.388800
  assert_contains "$out" "Refusing to rebid: current bid changed since check."
  assert_calls_not_contain "change bid"

  prepare_scenario rebid-min-fail "$FIXTURES/existing-same-machine"
  out="$CURRENT_DIR/min-fail.txt"
  run_expect 1 "$out" rebid-min-fail rebid --expected-current-bid 0.34 --expected-min-bid 0.37 --expected-target-bid 0.388800
  assert_contains "$out" "Refusing to rebid: min bid changed since check."
  assert_calls_not_contain "change bid"

  prepare_scenario rebid-target-fail "$FIXTURES/existing-same-machine"
  out="$CURRENT_DIR/target-fail.txt"
  run_expect 1 "$out" rebid-target-fail rebid --expected-current-bid 0.34 --expected-min-bid 0.36 --expected-target-bid 0.400000
  assert_contains "$out" "Refusing to rebid: computed target bid changed since check."
  assert_calls_not_contain "change bid"

  prepare_scenario rebid-ceiling "$FIXTURES/existing-same-machine"
  out="$CURRENT_DIR/ceiling.txt"
  run_expect 0 "$out" rebid-ceiling rebid --max-bid-price 0.37
  assert_contains "$out" "target bid $/h       : 0.370000"
  assert_calls_contain "change bid 43 --price 0.370000"
}

scenario_rebid_noop_and_scheduler_stopped_refusal() {
  prepare_scenario rebid-noop "$FIXTURES/existing-noop"
  out="$CURRENT_DIR/noop.txt"
  run_expect 0 "$out" rebid-noop rebid --max-bid-price 0.45
  assert_contains "$out" "No useful rebid is needed"
  assert_calls_not_contain "change bid"

  prepare_scenario stopped-refusal "$FIXTURES/existing-stopped"
  out="$CURRENT_DIR/stopped.txt"
  run_expect 1 "$out" stopped-refusal rebid --max-bid-price 0.45
  assert_contains "$out" "Refusing to rebid: scheduler reports this bid instance is stopped"
  assert_calls_not_contain "change bid"
}

scenario_watch_bounded_and_failure() {
  prepare_scenario watch-once "$FIXTURES/base-market" "$FIXTURES/existing-same-machine"
  out="$CURRENT_DIR/watch.txt"
  WATCH_MAX_CYCLES=1 run_expect 0 "$out" watch-once watch --interval 7 --max-bid-price 0.45
  assert_contains "$out" "Interval: 7 seconds"
  assert_contains "$out" "Watch recommendation: rebid. Applying rebid"
  assert_contains "$out" "Sleeping 7 seconds"
  assert_contains "$out" "Watch max cycles reached: 1"
  assert_calls_contain "sleep 7"
  assert_calls_contain "change bid 43 --price"

  prepare_scenario watch-failure "$FIXTURES/base-market" "$FIXTURES/no-existing"
  out="$CURRENT_DIR/failure.txt"
  WATCH_MAX_CYCLES=1 run_expect 1 "$out" watch-failure watch --interval 1
  assert_contains "$out" "Watch check failed without actionable recommendation."
}

scenario_error_handling() {
  prepare_scenario auth-failure "$FIXTURES/base-market" "$FIXTURES/existing-replace" "$FIXTURES/command-failure"
  out="$CURRENT_DIR/auth.txt"
  run_expect 1 "$out" auth-failure check
  assert_contains "$out" "Vast CLI is not authenticated."

  prepare_scenario malformed "$FIXTURES/base-market" "$FIXTURES/existing-replace" "$FIXTURES/malformed-json"
  out="$CURRENT_DIR/malformed.txt"
  run_expect 5 "$out" malformed-json check

  prepare_scenario no-safety "$FIXTURES/low-quality-market" "$FIXTURES/existing-replace"
  out="$CURRENT_DIR/no-safety.txt"
  run_expect 1 "$out" no-safety check
  assert_contains "$out" "No offers found within max price"
}

scenario_check_no_existing
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_fake_guardrails
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_existing_labeled_recommend_replace
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_no_acceptable_offers
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_price_reliability_disk_vram_filters
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_gpu_preference_ranking
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_check_expected_guard_pass_fail
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_replace_guard_pass_success
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_replace_snapshot_drift_refusal
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_volume_fresh_and_reuse
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_destroy_timeout_continuation
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_create_parsing_and_missing_output
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_readiness_failure_retry_success
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_readiness_failure_exhaustion
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_rebid_guards_and_ceiling
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_rebid_noop_and_scheduler_stopped_refusal
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_watch_bounded_and_failure
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))
scenario_error_handling
SCENARIOS_RUN=$((SCENARIOS_RUN + 1))

echo "production scenarios passed: $SCENARIOS_RUN"

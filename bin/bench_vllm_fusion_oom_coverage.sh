#!/usr/bin/env bash
set -euo pipefail

BASE_URL="${BASE_URL:-http://127.0.0.1:8000/v1}"
MODEL="${MODEL:-qwen3.6-35b-a3b}"
API_KEY="${VLLM_API_KEY:-${API_KEY:-}}"
BENCH_PY="${BENCH_PY:-$HOME/Downloads/bench_vllm_chat.py}"
ROOT="${BENCH_ROOT:-$HOME/.cache/vllm-nix-qwen36/benchmarks}"
RUN_ID="$(date +%Y%m%d-%H%M%S)-fusion-oom-coverage"
OUT_DIR="$ROOT/$RUN_ID"
mkdir -p "$OUT_DIR"

redact() {
  sed -E \
    -e 's/(Authorization: Bearer )[A-Za-z0-9._~+\/=:-]+/\1<REDACTED>/g' \
    -e 's/(--api-key[ =])[^ ]+/\1<REDACTED>/g' \
    -e 's/(VLLM_API_KEY=)[^[:space:]]+/\1<REDACTED>/g' \
    -e 's/(api_key.: \[.)[^]]+/\1<REDACTED>/g' \
    -e 's/(api_key.: \[.)[^, ]+([,\]])/\1<REDACTED>\2/g'
}

json_error() {
  local name="$1" prompt="$2" max_tokens="$3" rc="$4" err_path="$5"
  python3 - "$name" "$prompt" "$max_tokens" "$rc" "$err_path" <<'PY'
import json, sys, pathlib
name, prompt, max_tokens, rc, err_path = sys.argv[1:]
err = pathlib.Path(err_path).read_text(errors="replace")
print(json.dumps({
  "name": name,
  "prompt_tokens_requested": int(prompt),
  "completion_tokens_requested": int(max_tokens),
  "returncode": int(rc),
  "error_tail": err[-4000:],
}, indent=2))
PY
}

run_case() {
  local name="$1" prompt="$2" max_tokens="$3"
  local outfile="$OUT_DIR/${name}.json"
  echo "[bench] $name prompt=$prompt max_tokens=$max_tokens"
  set +e
  python3 "$BENCH_PY" \
    --base-url "$BASE_URL" \
    --model "$MODEL" \
    --api-key "$API_KEY" \
    --prompt-tokens "$prompt" \
    --max-tokens "$max_tokens" \
    > "$outfile" 2>"$OUT_DIR/${name}.stderr"
  local rc=$?
  set -e
  if [ "$rc" -ne 0 ]; then
    echo "[bench] $name exited rc=$rc"
    json_error "$name" "$prompt" "$max_tokens" "$rc" "$OUT_DIR/${name}.stderr" > "$outfile"
  fi
  cat "$outfile" | redact
  echo
}

run_parallel2() {
  local name="$1" prompt="$2" max_tokens="$3"
  echo "[bench] $name parallel=2 prompt=$prompt max_tokens=$max_tokens"
  set +e
  python3 "$BENCH_PY" --base-url "$BASE_URL" --model "$MODEL" --api-key "$API_KEY" --prompt-tokens "$prompt" --max-tokens "$max_tokens" > "$OUT_DIR/${name}-a.json" 2>"$OUT_DIR/${name}-a.stderr" &
  local p1=$!
  python3 "$BENCH_PY" --base-url "$BASE_URL" --model "$MODEL" --api-key "$API_KEY" --prompt-tokens "$prompt" --max-tokens "$max_tokens" > "$OUT_DIR/${name}-b.json" 2>"$OUT_DIR/${name}-b.stderr" &
  local p2=$!
  wait "$p1"; local rc1=$?
  wait "$p2"; local rc2=$?
  set -e
  for suffix in a b; do
    local rc="$rc1"
    [ "$suffix" = "b" ] && rc="$rc2"
    local outfile="$OUT_DIR/${name}-${suffix}.json"
    if [ "$rc" -ne 0 ]; then
      json_error "$name-$suffix" "$prompt" "$max_tokens" "$rc" "$OUT_DIR/${name}-${suffix}.stderr" > "$outfile"
    fi
    cat "$outfile" | redact
    echo
  done
}

capture_logs() {
  local active_raw
  systemctl --user status vllm.service --no-pager -l 2>&1 | redact > "$OUT_DIR/systemctl-status-vllm.txt" || true
  systemctl --user show vllm.service 2>&1 | redact > "$OUT_DIR/systemctl-show-vllm.txt" || true

  active_raw="$(systemctl --user show vllm.service -p ActiveEnterTimestamp --value 2>/dev/null || true)"
  if [ -n "$active_raw" ]; then
    journalctl --user -u vllm.service --since "$active_raw" --no-pager 2>/dev/null | redact > "$OUT_DIR/vllm-session-since-restart.log" || true
  fi

  # Always save a robust fallback because journalctl --since can be locale-sensitive.
  journalctl --user -u vllm.service -n 4000 --no-pager 2>/dev/null | redact > "$OUT_DIR/vllm-last-4000.log" || true

  if [ ! -s "$OUT_DIR/vllm-session-since-restart.log" ] || grep -q -- "-- No entries --" "$OUT_DIR/vllm-session-since-restart.log"; then
    cp "$OUT_DIR/vllm-last-4000.log" "$OUT_DIR/vllm-session-since-restart.log" || true
  fi

  {
    echo "===== vLLM key lines ====="
    grep -E "Started vLLM server|Using vLLM install spec|host=.*max_model_len|gpu_memory_utilization|max_num_batched_tokens|GPU KV cache size|Maximum concurrency|Avg prompt throughput|Avg generation throughput|SpecDecoding metrics|CUDA out of memory|out of memory|EngineCore encountered|BadRequestError|maximum context length|num_running_reqs|prompt_token_ids_len|max_tokens" "$OUT_DIR/vllm-session-since-restart.log" | tail -200 || true
  } > "$OUT_DIR/vllm-key-lines.txt"
}

{
  echo "===== VLLM FUSION OOM COVERAGE BENCH ====="
  echo "date=$(date -Is)"
  echo "base_url=$BASE_URL"
  echo "model=$MODEL"
  echo "out_dir=$OUT_DIR"
  echo

  echo "===== Health ====="
  curl -fsS -H "Authorization: Bearer $API_KEY" "$BASE_URL/models" | redact || true
  echo

  echo "===== Quick sanity ====="
  run_case "single-1k-128" 1024 128
  run_case "single-4k-128" 4096 128

  echo "===== Fusion failure-pattern coverage ====="
  echo "# Important: approximates the Fusion crash pattern: about 14k prompt plus 4096 output."
  run_case "single-14k-4096-fusion-risk" 14000 4096

  echo "===== Safer Fusion envelope coverage ====="
  run_case "single-14k-2048-safe-output" 14000 2048
  run_case "single-16k-2048-fusion-limit" 16000 2048

  echo "===== Parallel realistic Fusion coverage ====="
  run_parallel2 "parallel-8k-1024" 8000 1024

  if [ "${RUN_STRESS:-0}" = "1" ]; then
    echo "===== Optional stress coverage ====="
    run_parallel2 "parallel-12k-1024" 12000 1024
    run_case "single-20k-1024" 20000 1024
  fi

  capture_logs
  cat "$OUT_DIR/vllm-key-lines.txt"

  echo
  echo "===== Artifacts ====="
  tar -C "$OUT_DIR" -czf "$OUT_DIR.tar.gz" .
  cp "$OUT_DIR.tar.gz" "$HOME/Downloads/bench.tar.gz"
  echo "out_dir=$OUT_DIR"
  echo "tar=$OUT_DIR.tar.gz"
  echo "upload=$HOME/Downloads/bench.tar.gz"
  echo
  echo "===== What to paste/upload ====="
  echo "Upload ~/Downloads/bench.tar.gz"
  echo "Also paste this report if upload is inconvenient."
} | tee "$OUT_DIR/report.txt"

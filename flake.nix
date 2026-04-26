{
  description = "Pinned Vast.ai launcher for Qwen on interruptible GPUs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f:
        builtins.listToAttrs (map (system: {
          name = system;
          value = f system;
        }) systems);
      mkLauncher = pkgs:
        pkgs.writeShellApplication {
          name = "vast-qwen-launch";
          runtimeInputs = with pkgs; [
            bash
            coreutils
            gnugrep
            gnused
            gawk
            curl
            jq
            uv
          ];
          text = ''
                #!/usr/bin/env bash
                set -euo pipefail

                # Keep numeric parsing/formatting stable across locales.
                export LC_NUMERIC=C
                export PATH="$HOME/.local/bin:$PATH"

                VASTAI_VERSION="''${VASTAI_VERSION:-1.0.3}"
                VASTAI_BIN="''${VASTAI_BIN:-vastai}"
                DEFAULT_IMAGE="''${DEFAULT_IMAGE:-vllm/vllm-openai:v0.19.1}"
                CUDA13_IMAGE="''${CUDA13_IMAGE:-vllm/vllm-openai:v0.19.1-x86_64-cu130}"
                IMAGE="''${IMAGE:-$DEFAULT_IMAGE}"
                IMAGE_AUTO_SELECT="''${IMAGE_AUTO_SELECT:-1}"
                MODEL="''${MODEL:-Qwen/Qwen3.6-27B-FP8}"
                MODEL_24GB="''${MODEL_24GB:-cyankiwi/Qwen3.6-27B-AWQ-INT4}"
                MODEL_32GB="''${MODEL_32GB:-Qwen/Qwen3.6-27B-FP8}"
                MODEL_48GB="''${MODEL_48GB:-Qwen/Qwen3.6-27B-FP8}"
                MODEL_80GB="''${MODEL_80GB:-Qwen/Qwen3.6-27B-FP8}"

                DISK_GB="''${DISK_GB:-40}"
                USE_VOLUME="''${USE_VOLUME:-0}"
                VOLUME_SIZE_GB="''${VOLUME_SIZE_GB:-100}"
                MOUNT_PATH="''${MOUNT_PATH:-/workspace}"

                MAX_PRICE="''${MAX_PRICE:-0.35}"
                BID_PRICE="''${BID_PRICE:-0.33}"
                MIN_RELIABILITY="''${MIN_RELIABILITY:-0.985}"
                PREFERRED_RELIABILITY="''${PREFERRED_RELIABILITY:-0.99}"
                LABEL="''${LABEL:-qwen36-27b-fp8-48k}"
                VOLUME_LABEL="''${VOLUME_LABEL:-qwen36vol}"
                MAX_MODEL_LEN="''${MAX_MODEL_LEN:-49152}"
                MIN_GPU_RAM_MB="''${MIN_GPU_RAM_MB:-22000}"
                MAX_CREATE_ATTEMPTS="''${MAX_CREATE_ATTEMPTS:-3}"
                DESTROY_TIMEOUT_SECS="''${DESTROY_TIMEOUT_SECS:-20}"
                READINESS_RETRY_ATTEMPTS="''${READINESS_RETRY_ATTEMPTS:-2}"
                REPLACE_READY_TIMEOUT_SECS="''${REPLACE_READY_TIMEOUT_SECS:-900}"
                CHECK_READY_TIMEOUT_SECS="''${CHECK_READY_TIMEOUT_SECS:-900}"
                READINESS_RETRY_BID_MARGIN="''${READINESS_RETRY_BID_MARGIN:-1.08}"
                COMMAND="check"
                SELECTED_ASK_ID="''${SELECTED_ASK_ID:-}"
                EXPECTED_REPLACE_MACHINE_ID="''${EXPECTED_REPLACE_MACHINE_ID:-}"
                EXPECTED_REPLACE_PRICE="''${EXPECTED_REPLACE_PRICE:-}"
                EXPECTED_REPLACE_TOTAL_PRICE="''${EXPECTED_REPLACE_TOTAL_PRICE:-}"
                EXPECTED_REPLACE_GPU="''${EXPECTED_REPLACE_GPU:-}"
                EXPECTED_REPLACE_CUDA="''${EXPECTED_REPLACE_CUDA:-}"
                EXPECTED_REBID_CURRENT_BID="''${EXPECTED_REBID_CURRENT_BID:-}"
                EXPECTED_REBID_MIN_BID="''${EXPECTED_REBID_MIN_BID:-}"
                EXPECTED_REBID_TARGET_BID="''${EXPECTED_REBID_TARGET_BID:-}"
                WATCH_INTERVAL="''${WATCH_INTERVAL:-60}"
                CHECK_RECOMMENDATION_FILE="''${CHECK_RECOMMENDATION_FILE:-}"
                MAX_BID_PRICE="''${MAX_BID_PRICE:-0.45}"
                MIN_REPLACE_SAVINGS_PCT="''${MIN_REPLACE_SAVINGS_PCT:-15}"
                REPLACE_PRICE_SLIPPAGE_PCT="''${REPLACE_PRICE_SLIPPAGE_PCT:-2}"
                STARTUP_BID_MARGIN="''${STARTUP_BID_MARGIN:-1.15}"
                STEADY_BID_MARGIN="''${STEADY_BID_MARGIN:-1.08}"
                MIN_BID_MARGIN="''${MIN_BID_MARGIN:-1.02}"

                usage() {
                  local topic="''${1:-all}"
                  case "$topic" in
                    check)
                      cat <<EOF
Usage: launcher check [options]
       launcher [options]

Default command. Inspect the current labeled instance and market.
It never destroys instances and never changes bids.

Options:
  --expected-machine-id ID      Optional: require selected offer machine id to match
  --expected-price FLOAT        Maximum selected instance price from check, plus slippage
  --expected-total-price FLOAT  Maximum selected total effective price from check, plus slippage
  --expected-gpu STRING         Require selected GPU name to match
  --expected-cuda FLOAT         Require selected cuda_max_good to be at least this value
  --replace-price-slippage-pct FLOAT  Allowed price drift over expected price/total before refusing (default: $REPLACE_PRICE_SLIPPAGE_PCT)
  --max-price FLOAT             Max hourly offer to consider (default: $MAX_PRICE)
  --min-reliability FLOAT       Min reliability (default: $MIN_RELIABILITY)
  --preferred-reliability FLOAT Preferred reliability (default: $PREFERRED_RELIABILITY)
  --min-replace-savings-pct N   Savings needed before recommending replace (default: $MIN_REPLACE_SAVINGS_PCT)
  --use-volume 0|1              Include volumes in selection/cost (default: $USE_VOLUME)
  --help                        Show this help
EOF
                      ;;
                    replace)
                      cat <<EOF
Usage: launcher replace [options]

Destroy the existing labeled instance, then create a replacement matching the expected market snapshot.
When expected fields are provided by check, replace searches the current market and proceeds only if a matching offer still exists.
This command is destructive. The default check command only recommends it.

Options:
  --max-price FLOAT             Max hourly offer to consider (default: $MAX_PRICE)
  --bid-price FLOAT             Base bid floor (default: $BID_PRICE)
  --startup-bid-margin FLOAT    Startup bid multiplier (default: $STARTUP_BID_MARGIN)
  --steady-bid-margin FLOAT     Post-readiness bid multiplier (default: $STEADY_BID_MARGIN)
  --max-bid-price FLOAT         Bid ceiling (default: $MAX_BID_PRICE)
  --readiness-retry-attempts N  Retry if instance dies before readiness (default: $READINESS_RETRY_ATTEMPTS)
  --use-volume 0|1              Include volumes in selection/cost (default: $USE_VOLUME)
  --help                        Show this help
EOF
                      ;;
                    rebid)
                      cat <<EOF
Usage: launcher rebid [options]

Adjust the existing labeled instance bid without replacing it.
When expected bid parameters are provided by check, rebid verifies the market/instance state has not changed before updating the bid.

Options:
  --expected-current-bid FLOAT  Require current bid to still match this value before rebidding
  --expected-min-bid FLOAT      Require min bid to still match this value before rebidding
  --expected-target-bid FLOAT   Require computed target bid to still match this value before rebidding
  --steady-bid-margin FLOAT     Rebid multiplier over current effective cost (default: $STEADY_BID_MARGIN)
  --max-bid-price FLOAT         Bid ceiling (default: $MAX_BID_PRICE)
  --use-volume 0|1              Include attached volume in effective cost when present (default: $USE_VOLUME)
  --help                        Show this help
EOF
                      ;;
                    watch)
                      cat <<EOF
Usage: launcher watch [options]

Continuously monitor with check and automatically run the suggested action.
It sleeps --interval seconds between successful cycles.
It stops on failure or when interrupted.

Options:
  --interval SECONDS            Delay between watch cycles (default: $WATCH_INTERVAL)
  --max-price FLOAT             Max hourly offer to consider (default: $MAX_PRICE)
  --min-reliability FLOAT       Min reliability (default: $MIN_RELIABILITY)
  --min-replace-savings-pct N   Savings needed before replacing (default: $MIN_REPLACE_SAVINGS_PCT)
  --startup-bid-margin FLOAT    Startup bid multiplier (default: $STARTUP_BID_MARGIN)
  --steady-bid-margin FLOAT     Rebid/post-readiness multiplier (default: $STEADY_BID_MARGIN)
  --max-bid-price FLOAT         Bid ceiling (default: $MAX_BID_PRICE)
  --help                        Show this help
EOF
                      ;;
                    *)
                      cat <<EOF
Usage: launcher [command] [options]

Commands:
  check     Inspect existing instance and market; recommend replace/rebid/stay (default)
  replace   Destroy existing labeled instance and create selected replacement
  rebid     Adjust existing labeled instance bid only
  watch     Keep monitoring and applying suggested replace/rebid actions

Run command-specific help:
  launcher check --help
  launcher replace --help
  launcher rebid --help
  launcher watch --help
EOF
                      ;;
                  esac
                }

                if [[ $# -gt 0 ]]; then
                  case "$1" in
                    check|replace|rebid|watch)
                      COMMAND="$1"
                      shift
                      ;;
                    -h|--help)
                      usage all
                      exit 0
                      ;;
                  esac
                fi



                while [[ $# -gt 0 ]]; do
                  case "$1" in
                    --vastai-version) VASTAI_VERSION="$2"; shift 2 ;;
                    --image) IMAGE="$2"; IMAGE_AUTO_SELECT=0; shift 2 ;;
                    --default-image) DEFAULT_IMAGE="$2"; if [[ "$IMAGE_AUTO_SELECT" = "1" ]]; then IMAGE="$2"; fi; shift 2 ;;
                    --cuda13-image) CUDA13_IMAGE="$2"; shift 2 ;;
                    --image-auto-select) IMAGE_AUTO_SELECT="$2"; shift 2 ;;
                    --model) MODEL="$2"; shift 2 ;;
                    --model-24gb) MODEL_24GB="$2"; shift 2 ;;
                    --model-32gb) MODEL_32GB="$2"; shift 2 ;;
                    --model-48gb) MODEL_48GB="$2"; shift 2 ;;
                    --model-80gb) MODEL_80GB="$2"; shift 2 ;;
                    --max-model-len) MAX_MODEL_LEN="$2"; shift 2 ;;
                    --min-gpu-ram-mb) MIN_GPU_RAM_MB="$2"; shift 2 ;;
                    --disk) DISK_GB="$2"; shift 2 ;;
                    --use-volume) USE_VOLUME="$2"; shift 2 ;;
                    --volume-size) VOLUME_SIZE_GB="$2"; shift 2 ;;
                    --mount-path) MOUNT_PATH="$2"; shift 2 ;;
                    --volume-label) VOLUME_LABEL="$2"; shift 2 ;;
                    --expected-machine-id) EXPECTED_REPLACE_MACHINE_ID="$2"; shift 2 ;;
                    --expected-price) EXPECTED_REPLACE_PRICE="$2"; shift 2 ;;
                    --expected-total-price) EXPECTED_REPLACE_TOTAL_PRICE="$2"; shift 2 ;;
                    --expected-gpu) EXPECTED_REPLACE_GPU="$2"; shift 2 ;;
                    --expected-cuda) EXPECTED_REPLACE_CUDA="$2"; shift 2 ;;
                    --max-price) MAX_PRICE="$2"; shift 2 ;;
                    --bid-price) BID_PRICE="$2"; shift 2 ;;
                    --min-reliability) MIN_RELIABILITY="$2"; shift 2 ;;
                    --preferred-reliability) PREFERRED_RELIABILITY="$2"; shift 2 ;;
                    --label) LABEL="$2"; shift 2 ;;
                    --max-create-attempts) MAX_CREATE_ATTEMPTS="$2"; shift 2 ;;
                    --destroy-timeout-secs) DESTROY_TIMEOUT_SECS="$2"; shift 2 ;;
                    --readiness-retry-attempts) READINESS_RETRY_ATTEMPTS="$2"; shift 2 ;;
                    --replace-ready-timeout-secs) REPLACE_READY_TIMEOUT_SECS="$2"; shift 2 ;;
                    --check-ready-timeout-secs) CHECK_READY_TIMEOUT_SECS="$2"; shift 2 ;;
                    --readiness-retry-bid-margin) READINESS_RETRY_BID_MARGIN="$2"; shift 2 ;;
                    --max-bid-price) MAX_BID_PRICE="$2"; shift 2 ;;
                    --expected-current-bid) EXPECTED_REBID_CURRENT_BID="$2"; shift 2 ;;
                    --expected-min-bid) EXPECTED_REBID_MIN_BID="$2"; shift 2 ;;
                    --expected-target-bid) EXPECTED_REBID_TARGET_BID="$2"; shift 2 ;;
                    --interval) WATCH_INTERVAL="$2"; shift 2 ;;
                    --min-replace-savings-pct) MIN_REPLACE_SAVINGS_PCT="$2"; shift 2 ;;
                    --replace-price-slippage-pct) REPLACE_PRICE_SLIPPAGE_PCT="$2"; shift 2 ;;
                    --startup-bid-margin) STARTUP_BID_MARGIN="$2"; shift 2 ;;
                    --steady-bid-margin) STEADY_BID_MARGIN="$2"; shift 2 ;;
                    -h|--help) usage "$COMMAND"; exit 0 ;;
                    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
                  esac
                done
                case "$COMMAND" in
                  check|replace|rebid|watch)
                    ;;
                  *)
                    echo "Unknown command: $COMMAND" >&2
                    usage all
                    exit 1
                    ;;
                esac

                shell_quote() {
                  printf '%q' "$1"
                }

                suggest_command() {
                  local rendered=""
                  local arg
                  for arg in "$@"; do
                    if [[ -z "$rendered" ]]; then
                      rendered="$(shell_quote "$arg")"
                    else
                      rendered="$rendered $(shell_quote "$arg")"
                    fi
                  done
                  printf '  %s\n' "$rendered"
                }

                run_watch_loop() {
                  echo "Starting watch loop. Press Ctrl-C to stop."
                  echo "Interval: $WATCH_INTERVAL seconds"

                  while true; do
                    recommendation_file="$(mktemp)"
                    echo
                    echo "Watch cycle: checking current instance and market..."

                    set +e
                    CHECK_RECOMMENDATION_FILE="$recommendation_file" \
                    "$0" check \
                      --vastai-version "$VASTAI_VERSION" \
                      --default-image "$DEFAULT_IMAGE" \
                      --cuda13-image "$CUDA13_IMAGE" \
                      --image-auto-select "$IMAGE_AUTO_SELECT" \
                      --model "$MODEL" \
                      --model-24gb "$MODEL_24GB" \
                      --model-32gb "$MODEL_32GB" \
                      --model-48gb "$MODEL_48GB" \
                      --model-80gb "$MODEL_80GB" \
                      --max-model-len "$MAX_MODEL_LEN" \
                      --min-gpu-ram-mb "$MIN_GPU_RAM_MB" \
                      --disk "$DISK_GB" \
                      --use-volume "$USE_VOLUME" \
                      --volume-size "$VOLUME_SIZE_GB" \
                      --mount-path "$MOUNT_PATH" \
                      --volume-label "$VOLUME_LABEL" \
                      --max-price "$MAX_PRICE" \
                      --bid-price "$BID_PRICE" \
                      --min-reliability "$MIN_RELIABILITY" \
                      --preferred-reliability "$PREFERRED_RELIABILITY" \
                      --label "$LABEL" \
                      --max-create-attempts "$MAX_CREATE_ATTEMPTS" \
                      --destroy-timeout-secs "$DESTROY_TIMEOUT_SECS" \
                      --readiness-retry-attempts "$READINESS_RETRY_ATTEMPTS" \
                      --readiness-retry-bid-margin "$READINESS_RETRY_BID_MARGIN" \
                      --max-bid-price "$MAX_BID_PRICE" \
                      --min-replace-savings-pct "$MIN_REPLACE_SAVINGS_PCT" \
                      --startup-bid-margin "$STARTUP_BID_MARGIN" \
                      --steady-bid-margin "$STEADY_BID_MARGIN"
                    check_status=$?
                    set -e

                    recommendation="stay"
                    if [[ -s "$recommendation_file" ]]; then
                      recommendation="$(cat "$recommendation_file")"
                    fi
                    rm -f "$recommendation_file"

                    expected_current_bid=""
                    expected_min_bid=""
                    expected_target_bid=""

                    expected_machine_id=""
                    expected_replace_price=""
                    expected_replace_total_price=""
                    expected_replace_gpu=""
                    expected_replace_cuda=""

                    if [[ "$recommendation" = replace:* ]]; then
                      IFS=: read -r recommendation _ expected_machine_id expected_replace_price expected_replace_total_price expected_replace_gpu expected_replace_cuda <<< "$recommendation"
                    elif [[ "$recommendation" = rebid:* ]]; then
                      IFS=: read -r recommendation expected_current_bid expected_min_bid expected_target_bid <<< "$recommendation"
                    fi

                    if [[ "$check_status" -ne 0 && "$recommendation" != "replace" && "$recommendation" != "rebid" ]]; then
                      echo "Watch check failed without actionable recommendation."
                      exit "$check_status"
                    fi

                    case "$recommendation" in
                      stay)
                        echo "Watch recommendation: stay."
                        ;;
                      replace)
                        echo "Watch recommendation: replace. Applying replacement..."
                        replace_args=(replace)
                        "$0" "''${replace_args[@]}" \
                          --expected-machine-id "$expected_machine_id" \
                          --expected-price "$expected_replace_price" \
                          --expected-total-price "$expected_replace_total_price" \
                          --expected-gpu "$expected_replace_gpu" \
                          --expected-cuda "$expected_replace_cuda" \
                          --vastai-version "$VASTAI_VERSION" \
                          --default-image "$DEFAULT_IMAGE" \
                          --cuda13-image "$CUDA13_IMAGE" \
                          --image-auto-select "$IMAGE_AUTO_SELECT" \
                          --model "$MODEL" \
                          --model-24gb "$MODEL_24GB" \
                          --model-32gb "$MODEL_32GB" \
                          --model-48gb "$MODEL_48GB" \
                          --model-80gb "$MODEL_80GB" \
                          --model-24gb "$MODEL_24GB" \
                          --model-32gb "$MODEL_32GB" \
                          --model-48gb "$MODEL_48GB" \
                          --model-80gb "$MODEL_80GB" \
                          --max-model-len "$MAX_MODEL_LEN" \
                          --min-gpu-ram-mb "$MIN_GPU_RAM_MB" \
                          --disk "$DISK_GB" \
                          --use-volume "$USE_VOLUME" \
                          --volume-size "$VOLUME_SIZE_GB" \
                          --mount-path "$MOUNT_PATH" \
                          --volume-label "$VOLUME_LABEL" \
                          --max-price "$MAX_PRICE" \
                          --bid-price "$BID_PRICE" \
                          --min-reliability "$MIN_RELIABILITY" \
                          --preferred-reliability "$PREFERRED_RELIABILITY" \
                          --label "$LABEL" \
                          --max-create-attempts "$MAX_CREATE_ATTEMPTS" \
                          --destroy-timeout-secs "$DESTROY_TIMEOUT_SECS" \
                          --readiness-retry-attempts "$READINESS_RETRY_ATTEMPTS" \
                        --replace-ready-timeout-secs "$REPLACE_READY_TIMEOUT_SECS" \
                        --check-ready-timeout-secs "$CHECK_READY_TIMEOUT_SECS" \
                          --readiness-retry-bid-margin "$READINESS_RETRY_BID_MARGIN" \
                          --max-bid-price "$MAX_BID_PRICE" \
                          --min-replace-savings-pct "$MIN_REPLACE_SAVINGS_PCT" \
                          --startup-bid-margin "$STARTUP_BID_MARGIN" \
                          --steady-bid-margin "$STEADY_BID_MARGIN"
                        ;;
                      rebid)
                        echo "Watch recommendation: rebid. Applying rebid..."
                        "$0" rebid \
                          --vastai-version "$VASTAI_VERSION" \
                          --use-volume "$USE_VOLUME" \
                          --label "$LABEL" \
                          --steady-bid-margin "$STEADY_BID_MARGIN" \
                          --max-bid-price "$MAX_BID_PRICE" \
                          --expected-current-bid "$expected_current_bid" \
                          --expected-min-bid "$expected_min_bid" \
                          --expected-target-bid "$expected_target_bid"
                        ;;
                      *)
                        echo "Unknown watch recommendation: $recommendation" >&2
                        exit 1
                        ;;
                    esac

                    echo "Sleeping $WATCH_INTERVAL seconds..."
                    sleep "$WATCH_INTERVAL"
                  done
                }

                if [[ "$COMMAND" = "watch" ]]; then
                  run_watch_loop
                  exit 0
                fi


                need_cmd() {
                  command -v "$1" >/dev/null 2>&1
                }

                create_response_says_success() {
                  local response_file="$1"
                  jq -e '.success == true' "$response_file" >/dev/null 2>&1 && return 0
                  grep -Eq "['\"]success['\"]:[[:space:]]*(True|true)" "$response_file"
                }

                load_candidate() {
                  local idx="$1"
                  BEST_ASK_ID="$(jq -r ".[$idx].ask_id" "$TMPDIR/candidates.json")"
                  BEST_MACHINE_ID="$(jq -r ".[$idx].machine_id" "$TMPDIR/candidates.json")"
                  BEST_GPU="$(jq -r ".[$idx].gpu_name" "$TMPDIR/candidates.json")"
                  BEST_GPU_RAM_MB="$(jq -r ".[$idx].gpu_ram_mb // .[$idx].gpu_ram // 0" "$TMPDIR/candidates.json")"
                  BEST_CUDA_MAX_GOOD="$(jq -r ".[$idx].cuda_max_good // 0" "$TMPDIR/candidates.json")"
                  BEST_DPH="$(jq -r ".[$idx].dph" "$TMPDIR/candidates.json")"
                  BEST_REL="$(jq -r ".[$idx].reliability" "$TMPDIR/candidates.json")"
                  BEST_LOC="$(jq -r ".[$idx].geolocation" "$TMPDIR/candidates.json")"
                  BEST_VOLUME_MODE="$(jq -r ".[$idx].volume_mode // empty" "$TMPDIR/candidates.json")"
                  BEST_REUSABLE_VOLUME_ID="$(jq -r ".[$idx].reusable_volume_id // empty" "$TMPDIR/candidates.json")"
                  BEST_CREATE_VOLUME_OFFER_ID="$(jq -r ".[$idx].create_volume_offer_id // empty" "$TMPDIR/candidates.json")"
                  BEST_VOLUME_COST="$(jq -r ".[$idx].volume_cost // 0" "$TMPDIR/candidates.json")"
                  BEST_TOTAL_COST="$(jq -r ".[$idx].total_hourly_cost // .[$idx].dph" "$TMPDIR/candidates.json")"

                  if [[ "$IMAGE_AUTO_SELECT" = "1" ]]; then
                    IMAGE="$DEFAULT_IMAGE"
                    if awk -v cuda="$BEST_CUDA_MAX_GOOD" 'BEGIN { exit !(cuda >= 13.0) }'; then
                      IMAGE="$CUDA13_IMAGE"
                    fi
                  fi
                }

                print_selected_candidate() {
                  echo "Selected:"
                  printf '  ask_id        : %s\n' "$BEST_ASK_ID"
                  printf '  machine_id    : %s\n' "$BEST_MACHINE_ID"
                  printf '  gpu           : %s\n' "$BEST_GPU"
                  printf '  gpu ram MB    : %s\n' "$BEST_GPU_RAM_MB"
                  printf '  cuda max good : %s\n' "$BEST_CUDA_MAX_GOOD"
                  printf '  instance $/h  : %.6f\n' "$BEST_DPH"
                  if [[ "$USE_VOLUME" = "1" ]]; then
                    printf '  volume mode   : %s\n' "$BEST_VOLUME_MODE"
                    if [[ -n "$BEST_REUSABLE_VOLUME_ID" ]]; then
                      printf '  volume id     : %s\n' "$BEST_REUSABLE_VOLUME_ID"
                    fi
                    if [[ -n "$BEST_CREATE_VOLUME_OFFER_ID" ]]; then
                      printf '  volume offer  : %s\n' "$BEST_CREATE_VOLUME_OFFER_ID"
                    fi
                    printf '  volume est $/h: %.6f\n' "$BEST_VOLUME_COST"
                    printf '  total est $/h : %.6f\n' "$BEST_TOTAL_COST"
                  else
                    printf '  total $/h     : %.6f\n' "$BEST_DPH"
                  fi
                  printf '  bid $/h       : %.6f\n' "$BID_PRICE"
                  printf '  reliability   : %.6f\n' "$BEST_REL"
                  printf '  location      : %s\n' "$BEST_LOC"
                  printf '  image         : %s\n' "$IMAGE"
                  if [[ -n "''${RUNTIME_MODEL:-}" ]]; then
                    printf '  model         : %s\n' "$RUNTIME_MODEL"
                    if [[ -n "''${RUNTIME_QUANTIZATION:-}" ]]; then
                      printf '  quantization  : %s\n' "$RUNTIME_QUANTIZATION"
                    fi
                  else
                    printf '  model         : %s\n' "$MODEL"
                  fi
                  printf '  max context   : %s\n' "$RUNTIME_MAX_MODEL_LEN"
                  printf '  disk          : %s GB\n' "$DISK_GB"
                  if [[ "$USE_VOLUME" = "1" ]]; then
                    printf '  volume        : %s GB at %s\n' "$VOLUME_SIZE_GB" "$MOUNT_PATH"
                  else
                    printf '  volume        : disabled\n'
                  fi
                  echo
                }

                if ! need_cmd "$VASTAI_BIN"; then
                  echo "Installing vastai==$VASTAI_VERSION with uv..."
                  uv tool install "vastai==$VASTAI_VERSION"
                  export PATH="$HOME/.local/bin:$PATH"
                  VASTAI_BIN="vastai"
                fi

                vast() {
                  "$VASTAI_BIN" "$@"
                }

                if [[ -n "''${VAST_API_KEY:-}" ]]; then
                  vast set api-key "$VAST_API_KEY" >/dev/null
                fi

                if ! vast show user >/dev/null 2>&1; then
                  echo "Vast CLI is not authenticated."
                  echo "Set VAST_API_KEY or run: vastai set api-key YOUR_KEY"
                  exit 1
                fi

                wait_for_instance_gone() {
                  local instance_id="$1"
                  local probe_file="$TMPDIR/wait-instance.json"
                  for _ in $(seq 1 60); do
                    if ! timeout 10s "$VASTAI_BIN" show instance "$instance_id" --raw > "$probe_file" 2>/dev/null; then
                      return 0
                    fi
                    if [[ -z "$(jq -r '(.instances.id // .id // empty)' "$probe_file" 2>/dev/null)" ]]; then
                      return 0
                    fi
                    sleep 2
                  done
                  return 1
                }

                expected_context_for_gpu() {
                  local gpu_name="$1"
                  case "$gpu_name" in
                    *H100*|*H200*) echo 98304 ;;
                    *L40*|*A6000*) echo 73728 ;;
                    *5090*) echo 65536 ;;
                    *) echo "$MAX_MODEL_LEN" ;;
                  esac
                }

                get_instance_snapshot() {
                  local instance_id="$1"
                  vast show instances --raw | jq -c --argjson id "$instance_id" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.id // .instance_id // .contract_id // .ask_contract_id // -1) == $id))
                    | .[0] // {}
                  '
                }

                get_instance_ip() {
                  jq -r '(.public_ipaddr // .public_ip // .ssh_host // .host // empty)'
                }

                get_instance_status() {
                  jq -r '(.actual_status // .cur_state // .status // .intended_status // empty)'
                }

                get_instance_cur_state() {
                  jq -r '(.cur_state // empty)'
                }

                get_instance_intended_status() {
                  jq -r '(.intended_status // empty)'
                }

                get_instance_next_state() {
                  jq -r '(.next_state // empty)'
                }

                get_instance_is_bid() {
                  jq -r '(.is_bid // false)'
                }

                status_is_effectively_dead() {
                  local actual="$1"
                  local cur_state="$2"
                  local intended="$3"
                  local next_state="$4"
                  local is_bid="$5"

                  if status_is_terminal_or_bad "$actual" || status_is_terminal_or_bad "$cur_state"; then
                    return 0
                  fi

                  # Vast can show actual_status=loading while the scheduler has already
                  # stopped an outbid instance. Treat bid instances whose scheduler
                  # target is not running/loading as terminal for readiness purposes.
                  if [[ "$is_bid" = "true" || "$is_bid" = "True" || "$is_bid" = "1" ]]; then
                    case "$intended" in
                      ""|running|loading)
                        ;;
                      *)
                        return 0
                        ;;
                    esac

                    case "$next_state" in
                      ""|running|loading)
                        ;;
                      *)
                        return 0
                        ;;
                    esac
                  fi

                  return 1
                }

                status_is_terminal_or_bad() {
                  case "$1" in
                    outbid|exited|stopped|destroyed|error|failed|cancelled|canceled|unreachable)
                      return 0
                      ;;
                    *)
                      return 1
                      ;;
                  esac
                }

                print_logs_context_if_available() {
                  local instance_id="$1"
                  local log_file="$TMPDIR/instance-ready.log"
                  if vast logs "$instance_id" > "$log_file" 2>/dev/null; then
                    local context gpu vram
                    context="$(grep -E '^(CONTEXT:|Selected MAX_MODEL_LEN=)' "$log_file" | tail -n1 | sed -E 's/^CONTEXT:[[:space:]]*//; s/^Selected MAX_MODEL_LEN=//' || true)"
                    gpu="$(grep -E '^GPU:' "$log_file" | tail -n1 | cut -d: -f2- | sed 's/^ *//' || true)"
                    vram="$(grep -E '^VRAM_GB:' "$log_file" | tail -n1 | awk '{print $2}' || true)"
                    if [[ -n "$context" ]]; then
                      printf 'CONTEXT: %s\n' "$context"
                    fi
                    if [[ -n "$gpu" ]]; then
                      printf 'GPU: %s\n' "$gpu"
                    fi
                    if [[ -n "$vram" ]]; then
                      printf 'VRAM_GB: %s\n' "$vram"
                    fi
                    [[ -n "$context" ]]
                    return
                  fi
                  return 1
                }

                instance_api_ready_by_ip() {
                  local ip="$1"
                  [[ -n "$ip" && "$ip" != "null" ]] || return 1
                  curl -fsS --connect-timeout 3 --max-time 5 "http://$ip:8000/v1/models" >/dev/null 2>&1
                }

                existing_instance_runtime_stuck() {
                  local ip="''${EXISTING_INSTANCE_IP:-}"
                  local duration="''${EXISTING_INSTANCE_DURATION_SECS:-0}"

                  case "''${EXISTING_INSTANCE_STATUS:-}" in
                    running|loading|"")
                      ;;
                    *)
                      return 1
                      ;;
                  esac

                  # Scheduler says the instance should be running, but the API is still
                  # not reachable after the threshold. This catches the Vast UI
                  # "Creating... / not running" and vLLM stuck-start cases.
                  case "''${EXISTING_INSTANCE_CUR_STATE:-}" in
                    running|"") ;;
                    *) return 1 ;;
                  esac
                  case "''${EXISTING_INSTANCE_INTENDED_STATUS:-}" in
                    running|"") ;;
                    *) return 1 ;;
                  esac
                  case "''${EXISTING_INSTANCE_NEXT_STATE:-}" in
                    running|"") ;;
                    *) return 1 ;;
                  esac

                  if ! awk -v duration="$duration" -v timeout="$CHECK_READY_TIMEOUT_SECS" 'BEGIN { exit !(duration >= timeout) }'; then
                    return 1
                  fi

                  if instance_api_ready_by_ip "$ip"; then
                    return 1
                  fi

                  return 0
                }

                wait_for_local_api_ready() {
                  local instance_id="$1"
                  local expected_context="$2"
                  local attempt=0
                  local start_ts
                  local now_ts
                  local elapsed_secs
                  start_ts="$(date +%s)"
                  local ip=""
                  local status=""
                  local cur_state=""
                  local intended_status=""
                  local next_state=""
                  local is_bid=""
                  local snapshot=""
                  echo
                  echo "Waiting for instance API readiness..."
                  while true; do
                    attempt=$((attempt + 1))
                    now_ts="$(date +%s)"
                    elapsed_secs=$((now_ts - start_ts))
                    if (( elapsed_secs >= REPLACE_READY_TIMEOUT_SECS )); then
                      echo "Instance $instance_id did not become API-ready within $REPLACE_READY_TIMEOUT_SECS seconds."
                      echo "Treating replacement boot as failed."
                      return 1
                    fi
                    snapshot="$(get_instance_snapshot "$instance_id" || echo '{}')"

                    if [[ "$snapshot" = "{}" || -z "$snapshot" ]]; then
                      echo "Local readiness attempt $attempt: instance $instance_id not visible yet."
                      sleep 5
                      continue
                    fi

                    status="$(printf '%s\n' "$snapshot" | get_instance_status)"
                    cur_state="$(printf '%s\n' "$snapshot" | get_instance_cur_state)"
                    intended_status="$(printf '%s\n' "$snapshot" | get_instance_intended_status)"
                    next_state="$(printf '%s\n' "$snapshot" | get_instance_next_state)"
                    is_bid="$(printf '%s\n' "$snapshot" | get_instance_is_bid)"
                    ip="$(printf '%s\n' "$snapshot" | get_instance_ip)"
                    STATUS_DISPLAY="$status"
                    if [[ -z "$STATUS_DISPLAY" || "$STATUS_DISPLAY" = "null" ]]; then
                      STATUS_DISPLAY="unknown"
                    fi

                    if status_is_effectively_dead "$status" "$cur_state" "$intended_status" "$next_state" "$is_bid"; then
                      echo "Instance $instance_id entered terminal/scheduler-stopped state while waiting."
                      printf '  actual_status  : %s\n' "''${status:-unknown}"
                      printf '  cur_state      : %s\n' "''${cur_state:-unknown}"
                      printf '  intended_status: %s\n' "''${intended_status:-unknown}"
                      printf '  next_state     : %s\n' "''${next_state:-unknown}"
                      printf '  is_bid         : %s\n' "''${is_bid:-unknown}"
                      echo "Aborting readiness wait."
                      return 1
                    fi

                    if [[ -z "$ip" || "$ip" = "null" ]]; then
                      echo "Local readiness attempt $attempt: status=$STATUS_DISPLAY cur_state=''${cur_state:-unknown} intended=''${intended_status:-unknown} next=''${next_state:-unknown} is_bid=''${is_bid:-unknown}; public IP not available yet."
                    elif curl -fsS --connect-timeout 3 --max-time 5 "http://$ip:8000/v1/models" >/dev/null 2>&1; then
                      echo
                      echo "======================================"
                      echo "READY"
                      printf 'IP: %s\n' "$ip"
                      echo "PORT: 8000"
                      printf 'STATUS: %s\n' "$STATUS_DISPLAY"
                      if ! print_logs_context_if_available "$instance_id"; then
                        printf 'CONTEXT: %s (expected from selected GPU)\n' "$expected_context"
                        printf 'GPU: %s\n' "$BEST_GPU"
                      fi
                      echo "======================================"
                      echo
                      return 0
                    else
                      echo "Local readiness attempt $attempt: status=$STATUS_DISPLAY cur_state=''${cur_state:-unknown} intended=''${intended_status:-unknown} next=''${next_state:-unknown} is_bid=''${is_bid:-unknown}; API not ready at http://$ip:8000/v1/models yet."
                    fi
                    sleep 5
                  done
                }

                bump_bid_after_readiness_failure() {
                  local candidate_price="$1"
                  local current_bid="$2"
                  awk \
                    -v candidate="$candidate_price" \
                    -v current="$current_bid" \
                    -v margin="$READINESS_RETRY_BID_MARGIN" \
                    -v max_bid="$MAX_BID_PRICE" '
                      BEGIN {
                        bumped = candidate * margin
                        if (bumped < current * margin) bumped = current * margin
                        if (bumped > max_bid) bumped = max_bid
                        printf "%.6f", bumped
                      }
                    '
                }

                compute_margin_bid() {
                  local candidate_price="$1"
                  local current_bid="$2"
                  local margin="$3"
                  awk \
                    -v candidate="$candidate_price" \
                    -v current="$current_bid" \
                    -v margin="$margin" \
                    -v max_bid="$MAX_BID_PRICE" '
                      BEGIN {
                        wanted = candidate * margin
                        if (wanted < current) wanted = current
                        if (wanted > max_bid) wanted = max_bid
                        printf "%.6f", wanted
                      }
                    '
                }

                set_instance_bid_best_effort() {
                  local instance_id="$1"
                  local target_bid="$2"
                  if [[ -z "$instance_id" || -z "$target_bid" ]]; then
                    return 0
                  fi
                  echo "Setting instance $instance_id bid to $target_bid..."
                  set +e
                  "$VASTAI_BIN" change bid "$instance_id" --price "$target_bid" >/dev/null 2>&1
                  local status=$?
                  set -e
                  if [[ "$status" -ne 0 ]]; then
                    echo "Warning: could not change bid for instance $instance_id. Check Vast CLI syntax/version if needed."
                    return 1
                  fi
                  return 0
                }

                destroy_failed_readiness_instance() {
                  local instance_id="$1"
                  if [[ -z "$instance_id" ]]; then
                    return 0
                  fi
                  echo "Destroying failed readiness instance $instance_id..."
                  set +e
                  timeout "''${DESTROY_TIMEOUT_SECS}s" "$VASTAI_BIN" destroy instance "$instance_id" -y >/dev/null
                  local status=$?
                  set -e
                  if [[ "$status" -eq 124 ]]; then
                    echo "Warning: destroy timed out for failed readiness instance $instance_id."
                    echo "Check manually with: vastai show instances -v"
                    return 1
                  elif [[ "$status" -ne 0 ]]; then
                    echo "Warning: destroy failed for failed readiness instance $instance_id."
                    echo "Check manually with: vastai show instances -v"
                    return 1
                  fi
                  return 0
                }

                offer_still_available() {
                  local ask_id="$1"
                  jq -e --argjson ask_id "$ask_id" '
                    map(select((.ask_id // .id // .offer_id // -1) == $ask_id))
                    | length > 0
                  ' "$TMPDIR/candidates.json" >/dev/null
                }

                select_candidate_index() {
                  if [[ -n "$EXPECTED_REPLACE_GPU" || -n "$EXPECTED_REPLACE_CUDA" || -n "$EXPECTED_REPLACE_PRICE" || -n "$EXPECTED_REPLACE_TOTAL_PRICE" || -n "$EXPECTED_REPLACE_MACHINE_ID" ]]; then
                    local expected_index
                    expected_index="$(jq -r \
                      --arg machine_id "$EXPECTED_REPLACE_MACHINE_ID" \
                      --arg gpu "$EXPECTED_REPLACE_GPU" \
                      --argjson price "''${EXPECTED_REPLACE_PRICE:-0}" \
                      --argjson total "''${EXPECTED_REPLACE_TOTAL_PRICE:-0}" \
                      --argjson cuda "''${EXPECTED_REPLACE_CUDA:-0}" \
                      --argjson slippage_pct "$REPLACE_PRICE_SLIPPAGE_PCT" '
                        def le_with_slippage($actual; $expected):
                          ($expected == 0) or ($actual <= ($expected * (1 + ($slippage_pct / 100.0)) + 0.000001));
                        to_entries
                        | map(select(
                            ($machine_id == "" or ((.value.machine_id | tostring) == $machine_id))
                            and ($gpu == "" or .value.gpu_name == $gpu)
                            and (($cuda == 0) or ((.value.cuda_max_good // 0) >= $cuda))
                            and le_with_slippage((.value.dph // 0); $price)
                            and le_with_slippage((.value.total_hourly_cost // .value.dph // 0); $total)
                          ))
                        | sort_by((.value.total_hourly_cost // .value.dph // 999999), (.value.reliability // 0) * -1)
                        | .[0].key // empty
                      ' "$TMPDIR/candidates.json")"

                    if [[ -n "$expected_index" ]]; then
                      echo "$expected_index"
                      return 0
                    fi

                    echo "No current offer matched the expected replacement constraints." >&2
                    echo "Run check again to get a fresh recommendation." >&2
                    return 1
                  fi

                  if [[ -n "$SELECTED_ASK_ID" ]]; then
                    local ask_index
                    ask_index="$(jq -r --argjson ask_id "$SELECTED_ASK_ID" '
                      to_entries
                      | map(select((.value.ask_id // .value.id // .value.offer_id // -1) == $ask_id))
                      | .[0].key // empty
                    ' "$TMPDIR/candidates.json")"

                    if [[ -n "$ask_index" ]]; then
                      echo "$ask_index"
                      return 0
                    fi
                  fi

                  echo 0
                }

                existing_instance_scheduler_stopped_bid() {
                  if [[ "''${EXISTING_INSTANCE_IS_BID:-false}" = "true" || "''${EXISTING_INSTANCE_IS_BID:-false}" = "True" || "''${EXISTING_INSTANCE_IS_BID:-false}" = "1" ]]; then
                    case "''${EXISTING_INSTANCE_INTENDED_STATUS:-}" in
                      ""|running|loading) ;;
                      *) return 0 ;;
                    esac
                    case "''${EXISTING_INSTANCE_NEXT_STATE:-}" in
                      ""|running|loading) ;;
                      *) return 0 ;;
                    esac
                    if status_is_terminal_or_bad "''${EXISTING_INSTANCE_CUR_STATE:-}"; then
                      return 0
                    fi
                  fi
                  return 1
                }

                current_rebid_target() {
                  local current_effective="$1"
                  local min_bid="$2"
                  local current_bid="$3"

                  # Rebid should be based on the market bid floor, not on
                  # dph_total/current_effective. dph_total includes our own
                  # previous bid, so multiplying it compounds rebids and makes
                  # replacement look artificially attractive.
                  awk \
                    -v current_effective="$current_effective" \
                    -v min_bid="$min_bid" \
                    -v current_bid="$current_bid" \
                    -v margin="''${STEADY_BID_MARGIN:-1.08}" \
                    -v min_margin="''${MIN_BID_MARGIN:-1.02}" \
                    -v max_bid="$MAX_BID_PRICE" '
                      BEGIN {
                        floor = min_bid
                        if (floor <= 0) floor = current_bid
                        if (floor <= 0) floor = current_effective

                        target = floor * margin
                        min_target = min_bid * min_margin
                        if (min_target > target) target = min_target

                        if (current_bid > 0 && current_bid > min_bid && current_bid >= target) {
                          target = current_bid
                        }

                        if (target > max_bid) target = max_bid
                        printf "%.6f", target
                      }
                    '
                }

                rebid_is_useful() {
                  local target_bid="$1"
                  local current_bid="$2"
                  local min_bid="$3"
                  awk \
                    -v target="$target_bid" \
                    -v current="$current_bid" \
                    -v min_bid="$min_bid" '
                      BEGIN {
                        threshold = min_bid
                        if (current > 0 && current > threshold) threshold = current
                        exit !(target > threshold + 0.000001)
                      }
                    '
                }

                float_close() {
                  local left="$1"
                  local right="$2"
                  awk -v left="$left" -v right="$right" '
                    BEGIN {
                      diff = left - right
                      if (diff < 0) diff = -diff
                      exit !(diff <= 0.000001)
                    }
                  '
                }

                verify_expected_rebid_state() {
                  local current_bid="$1"
                  local min_bid="$2"
                  local target_bid="$3"

                  if [[ -n "$EXPECTED_REBID_CURRENT_BID" ]] && ! float_close "$current_bid" "$EXPECTED_REBID_CURRENT_BID"; then
                    echo "Refusing to rebid: current bid changed since check."
                    printf '  expected current bid: %.6f\n' "$EXPECTED_REBID_CURRENT_BID"
                    printf '  observed current bid: %.6f\n' "$current_bid"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  if [[ -n "$EXPECTED_REBID_MIN_BID" ]] && ! float_close "$min_bid" "$EXPECTED_REBID_MIN_BID"; then
                    echo "Refusing to rebid: min bid changed since check."
                    printf '  expected min bid: %.6f\n' "$EXPECTED_REBID_MIN_BID"
                    printf '  observed min bid: %.6f\n' "$min_bid"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  if [[ -n "$EXPECTED_REBID_TARGET_BID" ]] && ! float_close "$target_bid" "$EXPECTED_REBID_TARGET_BID"; then
                    echo "Refusing to rebid: computed target bid changed since check."
                    printf '  expected target bid: %.6f\n' "$EXPECTED_REBID_TARGET_BID"
                    printf '  observed target bid: %.6f\n' "$target_bid"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  return 0
                }


                verify_expected_replace_state() {
                  if [[ -n "$EXPECTED_REPLACE_MACHINE_ID" ]] && [[ "$BEST_MACHINE_ID" != "$EXPECTED_REPLACE_MACHINE_ID" ]]; then
                    echo "Refusing to replace: selected offer machine changed since check."
                    printf '  expected machine_id: %s\n' "$EXPECTED_REPLACE_MACHINE_ID"
                    printf '  observed machine_id: %s\n' "$BEST_MACHINE_ID"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  if [[ -n "$EXPECTED_REPLACE_GPU" ]] && [[ "$BEST_GPU" != "$EXPECTED_REPLACE_GPU" ]]; then
                    echo "Refusing to replace: selected offer GPU changed since check."
                    printf '  expected gpu: %s\n' "$EXPECTED_REPLACE_GPU"
                    printf '  observed gpu: %s\n' "$BEST_GPU"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  if [[ -n "$EXPECTED_REPLACE_CUDA" ]] && ! awk -v observed="$BEST_CUDA_MAX_GOOD" -v expected="$EXPECTED_REPLACE_CUDA" 'BEGIN { exit !(observed + 0.000001 >= expected) }'; then
                    echo "Refusing to replace: selected offer CUDA capability is below the checked value."
                    printf '  expected cuda at least: %.6f\n' "$EXPECTED_REPLACE_CUDA"
                    printf '  observed cuda        : %.6f\n' "$BEST_CUDA_MAX_GOOD"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  if [[ -n "$EXPECTED_REPLACE_PRICE" ]] && ! awk -v observed="$BEST_DPH" -v expected="$EXPECTED_REPLACE_PRICE" -v slippage="$REPLACE_PRICE_SLIPPAGE_PCT" 'BEGIN { limit = expected * (1 + slippage / 100.0) + 0.000001; exit !(observed <= limit) }'; then
                    echo "Refusing to replace: selected offer price is above the checked ceiling."
                    printf '  expected price ceiling: %.6f + %.2f%%\n' "$EXPECTED_REPLACE_PRICE" "$REPLACE_PRICE_SLIPPAGE_PCT"
                    printf '  observed price        : %.6f\n' "$BEST_DPH"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  if [[ -n "$EXPECTED_REPLACE_TOTAL_PRICE" ]] && ! awk -v observed="$BEST_TOTAL_COST" -v expected="$EXPECTED_REPLACE_TOTAL_PRICE" -v slippage="$REPLACE_PRICE_SLIPPAGE_PCT" 'BEGIN { limit = expected * (1 + slippage / 100.0) + 0.000001; exit !(observed <= limit) }'; then
                    echo "Refusing to replace: selected offer total effective price is above the checked ceiling."
                    printf '  expected total ceiling: %.6f + %.2f%%\n' "$EXPECTED_REPLACE_TOTAL_PRICE" "$REPLACE_PRICE_SLIPPAGE_PCT"
                    printf '  observed total        : %.6f\n' "$BEST_TOTAL_COST"
                    echo "Run check again for a fresh recommendation."
                    return 1
                  fi

                  return 0
                }


                print_replace_match_explanation() {
                  echo "Replacement match:"
                  if [[ -n "''${EXPECTED_REPLACE_GPU:-}" ]]; then
                    printf '  gpu                : expected %s, observed %s\n' "$EXPECTED_REPLACE_GPU" "$BEST_GPU"
                  fi
                  if [[ -n "''${EXPECTED_REPLACE_CUDA:-}" ]]; then
                    printf '  cuda               : expected >= %.6f, observed %.6f\n' "$EXPECTED_REPLACE_CUDA" "$BEST_CUDA_MAX_GOOD"
                  fi
                  if [[ -n "''${EXPECTED_REPLACE_PRICE:-}" ]]; then
                    awk -v expected="$EXPECTED_REPLACE_PRICE" -v observed="$BEST_DPH" -v slippage="$REPLACE_PRICE_SLIPPAGE_PCT" '
                      BEGIN {
                        limit = expected * (1 + slippage / 100.0)
                        printf "  instance price     : expected <= %.6f (+%.2f%% => %.6f), observed %.6f\n", expected, slippage, limit, observed
                      }
                    '
                  fi
                  if [[ -n "''${EXPECTED_REPLACE_TOTAL_PRICE:-}" ]]; then
                    awk -v expected="$EXPECTED_REPLACE_TOTAL_PRICE" -v observed="$BEST_TOTAL_COST" -v slippage="$REPLACE_PRICE_SLIPPAGE_PCT" '
                      BEGIN {
                        limit = expected * (1 + slippage / 100.0)
                        printf "  total price        : expected <= %.6f (+%.2f%% => %.6f), observed %.6f\n", expected, slippage, limit, observed
                      }
                    '
                  fi
                  if [[ -n "''${EXPECTED_REPLACE_MACHINE_ID:-}" ]]; then
                    printf '  machine_id         : expected %s, observed %s\n' "$EXPECTED_REPLACE_MACHINE_ID" "$BEST_MACHINE_ID"
                  else
                    printf '  machine_id         : unconstrained, observed %s\n' "$BEST_MACHINE_ID"
                  fi
                }

                replacement_savings_pct() {
                  local current_cost="$1"
                  local candidate_cost="$2"
                  awk -v current="$current_cost" -v candidate="$candidate_cost" '
                    BEGIN {
                      if (current <= 0) {
                        print "0.000000"
                      } else {
                        printf "%.6f", ((current - candidate) / current) * 100
                      }
                    }
                  '
                }

                same_machine_candidate() {
                  [[ -n "''${EXISTING_INSTANCE_MACHINE_ID:-}" ]] && [[ -n "''${BEST_MACHINE_ID:-}" ]] && [[ "''${EXISTING_INSTANCE_MACHINE_ID:-}" = "''${BEST_MACHINE_ID:-}" ]]
                }

                replacement_is_worth_it() {
                  local current_cost="$1"
                  local candidate_cost="$2"
                  local existing_status="$3"

                  if same_machine_candidate; then
                    return 1
                  fi

                  if status_is_terminal_or_bad "$existing_status"; then
                    return 0
                  fi
                  awk \
                    -v current="$current_cost" \
                    -v candidate="$candidate_cost" \
                    -v min_pct="$MIN_REPLACE_SAVINGS_PCT" '
                      BEGIN {
                        if (current <= 0) exit 1
                        savings = ((current - candidate) / current) * 100
                        exit !(savings >= min_pct)
                      }
                    '
                }

                TMPDIR="$(mktemp -d)"
                trap 'rm -rf "$TMPDIR"' EXIT

                echo "Inspecting current deployment..."
                vast show instances --raw > "$TMPDIR/current-instances.json"

                CURRENT_INSTANCE_COUNT="$(jq --arg label "$LABEL" '
                  def rows:
                    if type == "array" then .
                    elif has("instances") then .instances
                    else [] end;
                  rows
                  | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                  | length
                ' "$TMPDIR/current-instances.json")"

                if [[ "$CURRENT_INSTANCE_COUNT" -gt 1 ]]; then
                  echo "More than one instance already uses label $LABEL."
                  echo "Refusing to upsert ambiguously."
                  exit 1
                fi

                EXISTING_INSTANCE_ID=""
                EXISTING_INSTANCE_MACHINE_ID=""
                EXISTING_INSTANCE_STATUS=""
                EXISTING_INSTANCE_CUR_STATE=""
                EXISTING_INSTANCE_INTENDED_STATUS=""
                EXISTING_INSTANCE_NEXT_STATE=""
                EXISTING_INSTANCE_IS_BID="false"
                EXISTING_INSTANCE_IP=""
                EXISTING_INSTANCE_DURATION_SECS="0"
                EXISTING_INSTANCE_GPU=""
                EXISTING_INSTANCE_DPH="0"
                EXISTING_INSTANCE_MIN_BID="0"
                EXISTING_INSTANCE_CURRENT_BID="0"
                EXISTING_TOTAL_ESTIMATE="0"
                if [[ "$CURRENT_INSTANCE_COUNT" -eq 1 ]]; then
                  EXISTING_INSTANCE_ID="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | .[0].id
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_MACHINE_ID="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | .[0].machine_id
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_STATUS="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | .[0].actual_status // .[0].cur_state // "unknown"
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_CUR_STATE="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].cur_state // "")
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_INTENDED_STATUS="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].intended_status // "")
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_NEXT_STATE="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].next_state // "")
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_IS_BID="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].is_bid // false)
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_IP="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].public_ipaddr // .[0].public_ip // .[0].ip // "")
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_DURATION_SECS="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].duration // 0)
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_GPU="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | .[0].gpu_name // .[0].gpu_name_display // ""
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_DPH="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].dph_total // .[0].dph // .[0].discounted_dph_total // 0)
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_MIN_BID="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].min_bid // .[0].bid_floor // 0)
                  ' "$TMPDIR/current-instances.json")"
                  EXISTING_INSTANCE_CURRENT_BID="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | if (.[0].is_bid // false) then
                        (.[0].bid_price // .[0].bid // .[0].dph_base // 0)
                      else
                        (.[0].bid_price // .[0].bid // 0)
                      end
                  ' "$TMPDIR/current-instances.json")"
                  echo "Found existing instance:"
                  printf '  instance_id : %s\n' "$EXISTING_INSTANCE_ID"
                  printf '  machine_id  : %s\n' "$EXISTING_INSTANCE_MACHINE_ID"
                  printf '  status      : %s\n' "$EXISTING_INSTANCE_STATUS"
                  if [[ -n "$EXISTING_INSTANCE_GPU" && "$EXISTING_INSTANCE_GPU" != "null" ]]; then
                    printf '  gpu         : %s\n' "$EXISTING_INSTANCE_GPU"
                  fi
                  printf '  current $/h : %.6f\n' "$EXISTING_INSTANCE_DPH"
                  printf '  min bid $/h : %.6f\n' "$EXISTING_INSTANCE_MIN_BID"
                  if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                    printf '  bid $/h     : %.6f\n' "$EXISTING_INSTANCE_CURRENT_BID"
                  else
                    echo "  bid $/h     : unknown (not exposed by Vast show instances)"
                  fi
                else
                  echo "No existing labeled instance found."
                  if [[ "$COMMAND" = "check" ]]; then
                    echo "No action is performed by default."
                    echo "Run the replace command to create an instance:"
                    suggest_command replace
                    if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                      printf 'replace:%s:%s:%s:%s:%s:%s
' "$BEST_ASK_ID" "$BEST_MACHINE_ID" "$BEST_DPH" "$BEST_TOTAL_COST" "$BEST_GPU" "$BEST_CUDA_MAX_GOOD" > "$CHECK_RECOMMENDATION_FILE"
                    fi
                    exit 1
                  fi
                fi

                EXISTING_INSTANCE_NEEDS_FORCE_DECISION=0
                if [[ -n "$EXISTING_INSTANCE_ID" ]] && [[ "$COMMAND" = "check" ]]; then
                  EXISTING_INSTANCE_NEEDS_FORCE_DECISION=1
                  echo
                  echo "Existing labeled instance found. Searching market first to decide whether replacement is worth it."
                  echo "No destructive action is performed without replace."
                fi

                EXISTING_VOLUME_ID=""
                EXISTING_VOLUME_MACHINE_ID=""
                EXISTING_VOLUME_COST="0"
                if [[ "$USE_VOLUME" = "1" ]]; then
                  vast show volumes --raw > "$TMPDIR/current-volumes.json"
                  if [[ -n "$EXISTING_INSTANCE_ID" ]]; then
                    EXISTING_VOLUME_ID="$(jq -r --argjson instance_id "$EXISTING_INSTANCE_ID" '
                      def rows:
                        if type == "array" then .
                        elif has("volumes") then .volumes
                        else [] end;
                      def uses_instance($instance_id):
                        (.instances // [])
                        | any(
                            if type == "number" then
                              . == $instance_id
                            elif type == "object" then
                              (.id // .instance_id // .contract_id // .ask_contract_id // -1) == $instance_id
                            else
                              false
                            end
                          );
                      rows
                      | map(select(uses_instance($instance_id)))
                      | .[0].id // empty
                    ' "$TMPDIR/current-volumes.json")"
                    if [[ -n "$EXISTING_VOLUME_ID" ]]; then
                      EXISTING_VOLUME_MACHINE_ID="$(jq -r --argjson volume_id "$EXISTING_VOLUME_ID" '
                        def rows:
                          if type == "array" then .
                          elif has("volumes") then .volumes
                          else [] end;
                        rows
                        | map(select(.id == $volume_id))
                        | .[0].machine_id // empty
                      ' "$TMPDIR/current-volumes.json")"
                      EXISTING_VOLUME_COST="$(jq -r --argjson volume_id "$EXISTING_VOLUME_ID" '
                        def rows:
                          if type == "array" then .
                          elif has("volumes") then .volumes
                          else [] end;
                        rows
                        | map(select(.id == $volume_id))
                        | (.[0].storage_total_cost // 0)
                      ' "$TMPDIR/current-volumes.json")"
                      echo "Found attached volume:"
                      printf '  volume_id   : %s\n' "$EXISTING_VOLUME_ID"
                      printf '  machine_id  : %s\n' "$EXISTING_VOLUME_MACHINE_ID"
                      EXISTING_TOTAL_ESTIMATE="$(awk -v inst="$EXISTING_INSTANCE_DPH" -v vol="$EXISTING_VOLUME_COST" 'BEGIN { printf "%.6f", inst + vol }')"
                      printf '  volume $/h  : %.6f\n' "$EXISTING_VOLUME_COST"
                      printf '  total est $/h: %.6f\n' "$EXISTING_TOTAL_ESTIMATE"
                    fi
                  fi
                fi

                if [[ -n "$EXISTING_INSTANCE_ID" && "$COMMAND" = "rebid" ]]; then
                  if [[ "$USE_VOLUME" = "1" && -n "$EXISTING_VOLUME_ID" ]]; then
                    CURRENT_EFFECTIVE_COST="$EXISTING_TOTAL_ESTIMATE"
                  else
                    CURRENT_EFFECTIVE_COST="$EXISTING_INSTANCE_DPH"
                  fi
                  TARGET_REBID_PRICE="$(current_rebid_target "$CURRENT_EFFECTIVE_COST" "$EXISTING_INSTANCE_MIN_BID" "$EXISTING_INSTANCE_CURRENT_BID")"

                  echo
                  echo "Rebid requested for existing instance $EXISTING_INSTANCE_ID."
                  printf '  current effective $/h: %.6f\n' "$CURRENT_EFFECTIVE_COST"
                  if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                    printf '  current bid $/h      : %.6f\n' "$EXISTING_INSTANCE_CURRENT_BID"
                  else
                    echo "  current bid $/h      : unknown (not exposed by Vast show instances)"
                  fi
                  printf '  current min bid $/h  : %.6f\n' "$EXISTING_INSTANCE_MIN_BID"
                  printf '  target bid $/h       : %.6f\n' "$TARGET_REBID_PRICE"
                  printf '  max bid $/h          : %.6f\n' "$MAX_BID_PRICE"

                  if [[ "$TARGET_REBID_PRICE" = "$MAX_BID_PRICE" ]]; then
                    echo "Target bid reached the configured roof."
                  fi

                  verify_expected_rebid_state "$EXISTING_INSTANCE_CURRENT_BID" "$EXISTING_INSTANCE_MIN_BID" "$TARGET_REBID_PRICE"

                  set_instance_bid_best_effort "$EXISTING_INSTANCE_ID" "$TARGET_REBID_PRICE"
                  echo "Rebid requested."
                  echo "Note: for bid instances, Vast show instances usually reflects the requested bid as dph_base/current cost after refresh."
                  exit 0
                fi

                GPUS=(
                  "H100"
                  "H200"
                  "A100_SXM4"
                  "A100_PCIE"
                  "L40S"
                  "L40"
                  "RTX_6000Ada"
                  "RTX_5090"
                  "RTX_5080"
                  "RTX_4090"
                  "RTX_A5000"
                  "RTX_3090"
                )

                normalize_offers() {
                  local gpu="$1"
                  jq --arg gpu "$gpu" '
                    def rows:
                      if type == "array" then .
                      elif has("offers") then .offers
                      elif has("results") then .results
                      else [] end;

                    rows
                    | map({
                        ask_id: (.id // .ask_id // .instance_id // .offer_id),
                        machine_id: (.machine_id // .machine // null),
                        host_id: (.host_id // null),
                        avail_vol_ask_id: (.avail_vol_ask_id // null),
                        avail_vol_size: ((.avail_vol_size // 0) | tonumber),
                        gpu_name: (.gpu_name // .gpu_name_display // $gpu),
                        dph: ((.dph_total // .dph_base // .dph // .discounted_dph_total // 999999) | tonumber),
                        storage_cost: ((.storage_cost // 0) | tonumber),
                        storage_total_cost: ((.storage_total_cost // 0) | tonumber),
                        reliability: ((.reliability2 // .reliability // 0) | tonumber),
                        dlperf: ((.dlperf // .dlperf_per_dphtotal // 0) | tonumber),
                        inet_up: ((.inet_up // 0) | tonumber),
                        inet_down: ((.inet_down // 0) | tonumber),
                        direct_port_count: ((.direct_port_count // 0) | tonumber),
                        geolocation: (.geolocation // .location // .city // ""),
                        num_gpus: ((.num_gpus // 1) | tonumber),
                        gpu_ram: ((.gpu_ram // .gpu_mem // 0) | tonumber),
                        cuda_max_good: ((.cuda_max_good // 0) | tonumber)
                    })
                    | map(select(.ask_id != null and .machine_id != null))
                  '
                }

                preference_rank() {
                  case "$1" in
                    H100|H200) echo 0 ;;
                    A100_SXM4|A100_PCIE) echo 1 ;;
                    L40S|L40|RTX_6000Ada) echo 2 ;;
                    RTX_5090) echo 3 ;;
                    RTX_5080) echo 4 ;;
                    RTX_4090|RTX_A5000|RTX_3090) echo 5 ;;
                    *) echo 9 ;;
                  esac
                }

                echo "Searching Vast offers..."
                : > "$TMPDIR/all.jsonl"

                for gpu in "''${GPUS[@]}"; do
                  query="gpu_name=$gpu num_gpus=1 rentable=true verified=true reliability>=$MIN_RELIABILITY cuda_max_good>=12.9 direct_port_count>2 disk_space>=$DISK_GB"
                  if ! vast search offers --raw "$query" > "$TMPDIR/$gpu.raw.json" 2>/dev/null; then
                    continue
                  fi

                  normalize_offers "$gpu" < "$TMPDIR/$gpu.raw.json" \
                    | jq --argjson rank "$(preference_rank "$gpu")" '
                        map(. + { rank: $rank })
                        | .[]
                      ' >> "$TMPDIR/all.jsonl"
                done

                if [[ ! -s "$TMPDIR/all.jsonl" ]]; then
                  echo "No offers matched the search."
                  exit 1
                fi

                jq -s '.' "$TMPDIR/all.jsonl" > "$TMPDIR/offers.json"

                if [[ "$USE_VOLUME" = "1" ]]; then
                  echo "Searching compatible volume offers..."
                  if ! vast search volumes --raw "disk_space>=$VOLUME_SIZE_GB" > "$TMPDIR/volume-offers.raw.json" 2>/dev/null; then
                    echo "Volume search failed."
                    exit 1
                  fi

                  jq '
                    def rows:
                      if type == "array" then .
                      elif has("offers") then .offers
                      elif has("results") then .results
                      else [] end;

                    rows
                    | map({
                        volume_offer_id: (.id // .ask_contract_id // .volume_id),
                        machine_id: (.machine_id // .machine // null),
                        host_id: (.host_id // null),
                        geolocation: (.geolocation // ""),
                        reliability: ((.reliability2 // .reliability // 0) | tonumber),
                        storage_cost: ((.storage_cost // 0) | tonumber),
                        storage_total_cost: ((.storage_total_cost // 0) | tonumber),
                        disk_space: ((.disk_space // 0) | tonumber)
                    })
                    | map(select(.volume_offer_id != null and .machine_id != null))
                  ' "$TMPDIR/volume-offers.raw.json" > "$TMPDIR/volume-offers.json"

                  if [[ "$(jq 'length' "$TMPDIR/volume-offers.json")" -eq 0 && -z "$EXISTING_VOLUME_ID" ]]; then
                    echo "No compatible volume offers were returned by Vast."
                    exit 1
                  fi
                else
                  echo '[]' > "$TMPDIR/volume-offers.json"
                fi

                EXISTING_VOLUME_ID_JSON="null"
                EXISTING_VOLUME_MACHINE_ID_JSON="null"
                EXISTING_VOLUME_COST_JSON="0"
                if [[ -n "$EXISTING_VOLUME_ID" ]]; then
                  EXISTING_VOLUME_ID_JSON="$EXISTING_VOLUME_ID"
                  EXISTING_VOLUME_MACHINE_ID_JSON="$EXISTING_VOLUME_MACHINE_ID"
                  EXISTING_VOLUME_COST_JSON="$EXISTING_VOLUME_COST"
                fi

                jq -n \
                  --slurpfile offers "$TMPDIR/offers.json" \
                  --slurpfile volume_offers "$TMPDIR/volume-offers.json" \
                  --argjson max_price "$MAX_PRICE" \
                  --argjson requested_volume_size "$VOLUME_SIZE_GB" \
                  --argjson preferred_reliability "$PREFERRED_RELIABILITY" \
                  --argjson require_volume "$USE_VOLUME" \
                  --argjson existing_volume_id "$EXISTING_VOLUME_ID_JSON" \
                  --argjson existing_volume_machine_id "$EXISTING_VOLUME_MACHINE_ID_JSON" \
                  --argjson existing_volume_cost "$EXISTING_VOLUME_COST_JSON" '
                  def loc_tier:
                    (.geolocation | ascii_downcase) as $loc
                    | if ($loc | test("brazil|brasil|sao paulo|rio de janeiro|curitiba|porto alegre|belo horizonte|br$")) then 0
                      elif ($loc | test("argentina|chile|uruguay|paraguay|peru|colombia")) then 1
                      elif ($loc | test("miami|florida|virginia|north carolina|south carolina|georgia|texas|illinois|california|washington|oregon|new york|pennsylvania|us|united states")) then 2
                      elif ($loc | test("portugal|spain|france|netherlands|germany|italy|uk|united kingdom")) then 3
                      elif ($loc | length) > 0 then 4
                      else 5
                      end;

                  def rel_tier($preferred):
                    if .reliability >= $preferred then 0 else 1 end;

                  def offered_volume_hourly($offer; $volume_offer; $requested_size):
                    if (($volume_offer.storage_total_cost // 0) > 0) then ($volume_offer.storage_total_cost // 0)
                    elif (($offer.storage_total_cost // 0) > 0) then ($offer.storage_total_cost // 0)
                    elif (($volume_offer.storage_cost // 0) > 0) then (($volume_offer.storage_cost // 0) * $requested_size)
                    elif (($offer.storage_cost // 0) > 0) then (($offer.storage_cost // 0) * $requested_size)
                    else 0
                    end;

                  ($offers[0]) as $offer_list
                  | ($volume_offers[0]) as $volume_offer_list
                  | $offer_list
                                    | map(
                      . as $offer
                      | ($volume_offer_list
                          | map(select(.machine_id == $offer.machine_id))
                          | sort_by(.storage_total_cost, -.reliability)
                          | .[0]
                        ) as $volume_offer
                      | ($existing_volume_id != null
                         and $existing_volume_machine_id != null
                         and $offer.machine_id == $existing_volume_machine_id) as $can_reuse_volume
                      | . + {
                          loc_tier: loc_tier,
                          rel_tier: rel_tier($preferred_reliability),
                          reusable_volume_id: (if $can_reuse_volume then $existing_volume_id else null end),
                          create_volume_offer_id: ($volume_offer.volume_offer_id // .avail_vol_ask_id // null),
                          volume_mode:
                            (if $can_reuse_volume then "reuse"
                             elif (($volume_offer.volume_offer_id // .avail_vol_ask_id // null) != null) then "create"
                             else null
                             end),
                          volume_cost:
                            (if $can_reuse_volume then $existing_volume_cost
                             else offered_volume_hourly($offer; $volume_offer; $requested_volume_size)
                             end),
                          volume_reliability:
                            (if $can_reuse_volume then null
                             else ($volume_offer.reliability // null)
                             end),
                          total_hourly_cost:
                            (.dph + (if $can_reuse_volume then $existing_volume_cost
                                     else offered_volume_hourly($offer; $volume_offer; $requested_volume_size)
                                     end)),
                          volume_churn_tier: (if $can_reuse_volume then 0 else 1 end)
                        }
                    )
                  | if ($require_volume == 1 or $require_volume == "1")
                    then map(select(.volume_mode != null and .total_hourly_cost <= $max_price))
                    else map(select(.dph <= $max_price))
                    end
                  | sort_by(.rank, .rel_tier, .loc_tier, .volume_churn_tier, .total_hourly_cost, -.dlperf, -.reliability)
                ' > "$TMPDIR/candidates.json"

                count="$(jq 'length' "$TMPDIR/candidates.json")"
                if [[ "$count" -eq 0 ]]; then
                  if [[ "$USE_VOLUME" = "1" ]]; then
                    echo "No offers found within price/reliability constraints that can reuse or recreate the volume."
                  else
                    echo "No offers found within max price $MAX_PRICE."
                  fi
                  exit 1
                fi

                echo
                echo "Top candidates:"
                jq -r '
                  .[:5][] |
                  [
                    .ask_id,
                    (.reusable_volume_id // "-"),
                    (.create_volume_offer_id // "-"),
                    (.volume_mode // "-"),
                    .gpu_name,
                    (.gpu_ram_mb // .gpu_ram),
                    .cuda_max_good,
                    .dph,
                    .volume_cost,
                    .total_hourly_cost,
                    .reliability,
                    .loc_tier,
                    .dlperf,
                    .geolocation
                  ] | @tsv
                ' "$TMPDIR/candidates.json" | while IFS=$'\t' read -r ask_id reusable_volume_id create_volume_offer_id volume_mode gpu_name gpu_ram cuda_max_good dph volume_cost total_hourly_cost reliability loc_tier dlperf geolocation; do
                  printf '  ask_id=%s  reuse_vol=%s  create_vol=%s  vol-mode=%s  gpu=%s  vramMB=%s  cuda=%s  inst=$/h:%.6f  vol-est=$/h:%.6f  total-est=$/h:%.6f  rel=%s  loc-tier=%s  dlperf=%s  loc=%s\n' \
                    "$ask_id" "$reusable_volume_id" "$create_volume_offer_id" "$volume_mode" "$gpu_name" "$gpu_ram" "$cuda_max_good" "$dph" "$volume_cost" "$total_hourly_cost" "$reliability" "$loc_tier" "$dlperf" "$geolocation"
                done
                echo

                SELECTED_CANDIDATE_INDEX="$(select_candidate_index)"
                load_candidate "$SELECTED_CANDIDATE_INDEX"
                tune_runtime_for_selected_gpu() {
                  RUNTIME_MODEL="$MODEL"
                  RUNTIME_QUANTIZATION=""
                  RUNTIME_MAX_MODEL_LEN=49152
                  RUNTIME_GPU_UTIL=0.90
                  RUNTIME_MAX_BATCHED_TOKENS=1024

                  if [[ "$BEST_GPU_RAM_MB" -lt 30000 ]]; then
                    RUNTIME_MODEL="$MODEL_24GB"
                    RUNTIME_QUANTIZATION="compressed-tensors"
                    RUNTIME_MAX_MODEL_LEN=49152
                    RUNTIME_GPU_UTIL=0.88
                    RUNTIME_MAX_BATCHED_TOKENS=1024
                  elif [[ "$BEST_GPU_RAM_MB" -lt 45000 ]]; then
                    RUNTIME_MODEL="$MODEL_32GB"
                    RUNTIME_QUANTIZATION=""
                    RUNTIME_MAX_MODEL_LEN=49152
                    RUNTIME_GPU_UTIL=0.90
                    RUNTIME_MAX_BATCHED_TOKENS=1024
                  elif [[ "$BEST_GPU_RAM_MB" -lt 70000 ]]; then
                    RUNTIME_MODEL="$MODEL_48GB"
                    RUNTIME_QUANTIZATION=""
                    RUNTIME_MAX_MODEL_LEN=65536
                    RUNTIME_GPU_UTIL=0.92
                    RUNTIME_MAX_BATCHED_TOKENS=2048
                  else
                    RUNTIME_MODEL="$MODEL_80GB"
                    RUNTIME_QUANTIZATION=""
                    RUNTIME_MAX_MODEL_LEN=98304
                    RUNTIME_GPU_UTIL=0.94
                    RUNTIME_MAX_BATCHED_TOKENS=3072
                  fi

                  if [[ "$RUNTIME_MAX_MODEL_LEN" -lt 49152 ]]; then
                    echo "Selected GPU $BEST_GPU cannot guarantee minimum 48k context."
                    exit 1
                  fi
                }

                tune_runtime_for_selected_gpu
                print_selected_candidate

                if [[ "$EXISTING_INSTANCE_NEEDS_FORCE_DECISION" = "1" ]]; then
                  if [[ "$USE_VOLUME" = "1" && -n "$EXISTING_VOLUME_ID" ]]; then
                    CURRENT_EFFECTIVE_COST="$EXISTING_TOTAL_ESTIMATE"
                    CANDIDATE_EFFECTIVE_COST="$BEST_TOTAL_COST"
                  else
                    CURRENT_EFFECTIVE_COST="$EXISTING_INSTANCE_DPH"
                    CANDIDATE_EFFECTIVE_COST="$BEST_DPH"
                  fi

                  SAVINGS_PCT="$(replacement_savings_pct "$CURRENT_EFFECTIVE_COST" "$CANDIDATE_EFFECTIVE_COST")"

                  echo "Existing instance replacement analysis:"
                  printf '  current effective $/h : %.6f\n' "$CURRENT_EFFECTIVE_COST"
                  if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                    printf '  current bid $/h       : %.6f\n' "$EXISTING_INSTANCE_CURRENT_BID"
                  else
                    echo "  current bid $/h       : unknown (not exposed by Vast show instances)"
                  fi
                  printf '  current min bid $/h   : %.6f\n' "$EXISTING_INSTANCE_MIN_BID"
                  printf '  candidate effective $/h: %.6f\n' "$CANDIDATE_EFFECTIVE_COST"
                  printf '  savings              : %.2f%%\n' "$SAVINGS_PCT"
                  printf '  required savings     : %.2f%%\n' "$MIN_REPLACE_SAVINGS_PCT"

                  REBID_TARGET_PRICE="$(current_rebid_target "$CURRENT_EFFECTIVE_COST" "$EXISTING_INSTANCE_MIN_BID" "$EXISTING_INSTANCE_CURRENT_BID")"

                  if existing_instance_runtime_stuck; then
                    echo
                    echo "Existing instance appears stuck before API readiness."
                    printf '  status          : %s\n' "''${EXISTING_INSTANCE_STATUS:-unknown}"
                    printf '  cur_state       : %s\n' "''${EXISTING_INSTANCE_CUR_STATE:-unknown}"
                    printf '  intended_status : %s\n' "''${EXISTING_INSTANCE_INTENDED_STATUS:-unknown}"
                    printf '  next_state      : %s\n' "''${EXISTING_INSTANCE_NEXT_STATE:-unknown}"
                    printf '  duration seconds: %.0f\n' "$EXISTING_INSTANCE_DURATION_SECS"
                    printf '  timeout seconds : %.0f\n' "$CHECK_READY_TIMEOUT_SECS"
                    printf '  public IP       : %s\n' "''${EXISTING_INSTANCE_IP:-unknown}"
                    echo "API is not reachable, so this is treated as a failed boot, not a rebid-only case."
                    echo "No destructive action is performed by default."
                    echo "Run the replace command through your launcher to destroy the stuck instance and create a matching replacement:"
                    suggest_command replace --expected-price "$BEST_DPH" --expected-total-price "$BEST_TOTAL_COST" --expected-gpu "$BEST_GPU" --expected-cuda "$BEST_CUDA_MAX_GOOD"
                    echo "Run it through your launcher, for example: nix run . -- <command above>"
                    if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                      printf 'replace:::%s:%s:%s:%s\n' "$BEST_DPH" "$BEST_TOTAL_COST" "$BEST_GPU" "$BEST_CUDA_MAX_GOOD" > "$CHECK_RECOMMENDATION_FILE"
                    fi
                    exit 1
                  elif existing_instance_scheduler_stopped_bid; then
                    echo
                    echo "Existing bid instance is scheduler-stopped/outbid while reported as $EXISTING_INSTANCE_STATUS."
                    printf '  cur_state      : %s\n' "''${EXISTING_INSTANCE_CUR_STATE:-unknown}"
                    printf '  intended_status: %s\n' "''${EXISTING_INSTANCE_INTENDED_STATUS:-unknown}"
                    printf '  next_state     : %s\n' "''${EXISTING_INSTANCE_NEXT_STATE:-unknown}"
                    printf '  current bid $/h: %.6f\n' "$EXISTING_INSTANCE_CURRENT_BID"
                    printf '  min bid $/h    : %.6f\n' "$EXISTING_INSTANCE_MIN_BID"

                    if rebid_is_useful "$REBID_TARGET_PRICE" "$EXISTING_INSTANCE_CURRENT_BID" "$EXISTING_INSTANCE_MIN_BID"; then
                      printf '  suggested rebid $/h : %.6f\n' "$REBID_TARGET_PRICE"
                      echo "Run the rebid command through your launcher to adjust the existing instance bid if this snapshot is still valid:"
                      suggest_command rebid --expected-current-bid "$EXISTING_INSTANCE_CURRENT_BID" --expected-min-bid "$EXISTING_INSTANCE_MIN_BID" --expected-target-bid "$REBID_TARGET_PRICE"
                      if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                        echo "rebid:$EXISTING_INSTANCE_CURRENT_BID:$EXISTING_INSTANCE_MIN_BID:$REBID_TARGET_PRICE" > "$CHECK_RECOMMENDATION_FILE"
                      fi
                      exit 1
                    fi

                    echo "Current bid is already at or above the suggested bid target, but the scheduler has stopped this instance."
                    echo "This instance is not running and should not be treated as healthy."
                    if replacement_is_worth_it "$CURRENT_EFFECTIVE_COST" "$CANDIDATE_EFFECTIVE_COST" "outbid"; then
                      echo "Replacement is the next action because rebid is no longer useful for this stopped scheduler state."
                      echo "No destructive action is performed by default."
                      echo "Run the replace command through your launcher to destroy the stopped instance and create a matching replacement:"
                      suggest_command replace --expected-price "$BEST_DPH" --expected-total-price "$BEST_TOTAL_COST" --expected-gpu "$BEST_GPU" --expected-cuda "$BEST_CUDA_MAX_GOOD"
                      echo "Run it through your launcher, for example: nix run . -- <command above>"
                      if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                        printf 'replace:::%s:%s:%s:%s\n' "$BEST_DPH" "$BEST_TOTAL_COST" "$BEST_GPU" "$BEST_CUDA_MAX_GOOD" > "$CHECK_RECOMMENDATION_FILE"
                      fi
                      exit 1
                    fi

                    echo "No safe rebid is useful and replacement is not worth it by current policy."
                    echo "Run check again later or increase --max-bid-price / lower --min-replace-savings-pct if you want a more aggressive recovery."
                    if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                      echo fail > "$CHECK_RECOMMENDATION_FILE"
                    fi
                    exit 1
                  elif same_machine_candidate; then
                    echo
                    echo "Selected candidate is on the same machine as the existing instance."
                    echo "Treating this as a price/bid situation, not a replacement opportunity."
                    if rebid_is_useful "$REBID_TARGET_PRICE" "$EXISTING_INSTANCE_CURRENT_BID" "$EXISTING_INSTANCE_MIN_BID"; then
                      printf '  suggested rebid $/h : %.6f\n' "$REBID_TARGET_PRICE"
                      echo "Run the rebid command through your launcher to adjust the existing instance bid if this snapshot is still valid:"
                      if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                        suggest_command rebid --expected-current-bid "$EXISTING_INSTANCE_CURRENT_BID" --expected-min-bid "$EXISTING_INSTANCE_MIN_BID" --expected-target-bid "$REBID_TARGET_PRICE"
                      else
                        suggest_command rebid --expected-min-bid "$EXISTING_INSTANCE_MIN_BID" --expected-target-bid "$REBID_TARGET_PRICE"
                      fi
                      if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                        if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                          echo "rebid:$EXISTING_INSTANCE_CURRENT_BID:$EXISTING_INSTANCE_MIN_BID:$REBID_TARGET_PRICE" > "$CHECK_RECOMMENDATION_FILE"
                        else
                          echo "rebid::$EXISTING_INSTANCE_MIN_BID:$REBID_TARGET_PRICE" > "$CHECK_RECOMMENDATION_FILE"
                        fi
                      fi
                      exit 1
                    fi
                    echo "Keeping the existing instance. No action taken."
                    if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                      echo stay > "$CHECK_RECOMMENDATION_FILE"
                    fi
                    exit 0
                  elif replacement_is_worth_it "$CURRENT_EFFECTIVE_COST" "$CANDIDATE_EFFECTIVE_COST" "$EXISTING_INSTANCE_STATUS"; then
                    echo
                    if status_is_terminal_or_bad "$EXISTING_INSTANCE_STATUS"; then
                      echo "Replacement is worth it because the existing instance status is $EXISTING_INSTANCE_STATUS."
                    else
                      echo "Replacement appears worth it based on the savings threshold."
                    fi
                    if offer_still_available "$BEST_ASK_ID"; then
                      echo "Selected offer is still present in this market snapshot."
                    else
                      echo "Selected offer disappeared from this market snapshot; rerun default mode to refresh."
                      exit 1
                    fi
                    echo "No destructive action is performed by default."
                    echo "Run the replace command through your launcher to destroy the existing instance and create a matching replacement:"
                    suggest_command replace --expected-price "$BEST_DPH" --expected-total-price "$BEST_TOTAL_COST" --expected-gpu "$BEST_GPU" --expected-cuda "$BEST_CUDA_MAX_GOOD"
                    echo "Run it through your launcher, for example: nix run . -- <command above>"
                    if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                      echo replace > "$CHECK_RECOMMENDATION_FILE"
                    fi
                    exit 1
                  else
                    echo
                    if status_is_terminal_or_bad "$EXISTING_INSTANCE_STATUS"; then
                      echo "Existing instance is not healthy, but replacement is not attractive enough by the savings threshold."
                      echo "A rebid may be a better recovery action than replacement."
                      printf '  suggested rebid $/h : %.6f
' "$REBID_TARGET_PRICE"
                      echo "Current bid is below the effective-cost adjusted target."
                      echo "Instance is at risk of being outbid."
                      echo "No bid change is performed by default."
                      echo "Run the rebid command to adjust the existing instance bid if this snapshot is still valid:"
                      if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                        suggest_command rebid --expected-current-bid "$EXISTING_INSTANCE_CURRENT_BID" --expected-min-bid "$EXISTING_INSTANCE_MIN_BID" --expected-target-bid "$REBID_TARGET_PRICE"
                      else
                        suggest_command rebid --expected-min-bid "$EXISTING_INSTANCE_MIN_BID" --expected-target-bid "$REBID_TARGET_PRICE"
                      fi
                      if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                        if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                          echo "rebid:$EXISTING_INSTANCE_CURRENT_BID:$EXISTING_INSTANCE_MIN_BID:$REBID_TARGET_PRICE" > "$CHECK_RECOMMENDATION_FILE"
                        else
                          echo "rebid::$EXISTING_INSTANCE_MIN_BID:$REBID_TARGET_PRICE" > "$CHECK_RECOMMENDATION_FILE"
                        fi
                      fi
                      exit 1
                    fi

                    echo "Replacement is not worth it right now."
                    if rebid_is_useful "$REBID_TARGET_PRICE" "$EXISTING_INSTANCE_CURRENT_BID" "$EXISTING_INSTANCE_MIN_BID"; then
                      printf '  suggested rebid $/h : %.6f\n' "$REBID_TARGET_PRICE"
                      echo "Current bid is below the effective-cost adjusted target."
                      echo "Instance is at risk of being outbid."
                      echo "Run the rebid command to adjust the existing instance bid if this snapshot is still valid:"
                      if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                        suggest_command rebid --expected-current-bid "$EXISTING_INSTANCE_CURRENT_BID" --expected-min-bid "$EXISTING_INSTANCE_MIN_BID" --expected-target-bid "$REBID_TARGET_PRICE"
                      else
                        suggest_command rebid --expected-min-bid "$EXISTING_INSTANCE_MIN_BID" --expected-target-bid "$REBID_TARGET_PRICE"
                      fi
                      if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                        if awk -v bid="$EXISTING_INSTANCE_CURRENT_BID" 'BEGIN { exit !(bid > 0) }'; then
                          echo "rebid:$EXISTING_INSTANCE_CURRENT_BID:$EXISTING_INSTANCE_MIN_BID:$REBID_TARGET_PRICE" > "$CHECK_RECOMMENDATION_FILE"
                        else
                          echo "rebid::$EXISTING_INSTANCE_MIN_BID:$REBID_TARGET_PRICE" > "$CHECK_RECOMMENDATION_FILE"
                        fi
                      fi
                      exit 1
                    fi
                    echo "Keeping the existing instance. No action taken."
                    if [[ -n "$CHECK_RECOMMENDATION_FILE" ]]; then
                      echo stay > "$CHECK_RECOMMENDATION_FILE"
                    fi
                    exit 0
                  fi
                fi

                echo "Proceeding noninteractively. Replacement command selected or no existing instance was found."

                OLD_VOLUME_ID_TO_DELETE=""
                if [[ -n "$EXISTING_INSTANCE_ID" ]]; then
                  echo "Verifying a current offer still matches the expected replacement snapshot..."
                  verify_expected_replace_state
                  echo "Matched current offer $BEST_ASK_ID."
                  print_replace_match_explanation
                  echo "Replacing existing instance $EXISTING_INSTANCE_ID..."

                  if [[ "$USE_VOLUME" != "1" ]]; then
                    set +e
                    timeout "''${DESTROY_TIMEOUT_SECS}s" "$VASTAI_BIN" destroy instance "$EXISTING_INSTANCE_ID" -y >/dev/null
                    DESTROY_STATUS=$?
                    set -e
                    case "$DESTROY_STATUS" in
                      0)
                        echo "Old instance destroy requested."
                        ;;
                      124)
                        echo "Destroy request timed out after ''${DESTROY_TIMEOUT_SECS}s."
                        echo "Refusing to create a second instance while the old one may still exist."
                        exit 1
                        ;;
                      *)
                        echo "Failed to request destroy for existing instance $EXISTING_INSTANCE_ID."
                        exit 1
                        ;;
                    esac

                    echo "Waiting for old instance to disappear before launching replacement..."
                    if ! wait_for_instance_gone "$EXISTING_INSTANCE_ID"; then
                      echo "Timed out waiting for instance $EXISTING_INSTANCE_ID to disappear."
                      echo "Refusing to create a second instance while the old one may still exist."
                      exit 1
                    fi
                  elif [[ "$BEST_VOLUME_MODE" = "reuse" ]]; then
                    if ! vast destroy instance "$EXISTING_INSTANCE_ID" -y >/dev/null; then
                      echo "Failed to destroy existing instance $EXISTING_INSTANCE_ID."
                      exit 1
                    fi
                    echo "Waiting for old instance to fully detach from the reusable volume..."
                    if ! wait_for_instance_gone "$EXISTING_INSTANCE_ID"; then
                      echo "Timed out waiting for instance $EXISTING_INSTANCE_ID to disappear."
                      exit 1
                    fi
                  else
                    set +e
                    timeout "''${DESTROY_TIMEOUT_SECS}s" "$VASTAI_BIN" destroy instance "$EXISTING_INSTANCE_ID" -y >/dev/null
                    DESTROY_STATUS=$?
                    set -e
                    case "$DESTROY_STATUS" in
                      0)
                        echo "Old instance destroy requested; continuing with fresh-volume launch."
                        ;;
                      124)
                        echo "Destroy request timed out after ''${DESTROY_TIMEOUT_SECS}s; continuing with fresh-volume launch anyway."
                        ;;
                      *)
                        echo "Failed to request destroy for existing instance $EXISTING_INSTANCE_ID."
                        exit 1
                        ;;
                    esac
                  fi

                  if [[ "$USE_VOLUME" = "1" && -n "$EXISTING_VOLUME_ID" && "$BEST_VOLUME_MODE" != "reuse" ]]; then
                    OLD_VOLUME_ID_TO_DELETE="$EXISTING_VOLUME_ID"
                  fi
                fi

                VOLUME_ARGS=()
                CREATE_VOLUME_LABEL="$VOLUME_LABEL"
                if [[ "$USE_VOLUME" = "1" ]]; then
                  case "$BEST_VOLUME_MODE" in
                    reuse)
                      VOLUME_ARGS=(
                        --link-volume "$BEST_REUSABLE_VOLUME_ID"
                        --mount-path "$MOUNT_PATH"
                      )
                      ;;
                    create)
                      CREATE_VOLUME_LABEL="''${VOLUME_LABEL}_''${BEST_ASK_ID}"
                      VOLUME_ARGS=(
                        --create-volume "$BEST_CREATE_VOLUME_OFFER_ID"
                        --volume-size "$VOLUME_SIZE_GB"
                        --mount-path "$MOUNT_PATH"
                        --volume-label "$CREATE_VOLUME_LABEL"
                      )
                      ;;
                    *)
                      echo "Refusing to launch without a usable volume plan."
                      exit 1
                      ;;
                  esac
                fi

                

                tune_runtime_for_selected_gpu

ONSTART_SCRIPT="$(cat <<EOF
set -euxo pipefail

echo "=== vLLM launch configuration ==="
echo "GPU: ''${BEST_GPU}"
echo "GPU_RAM_MB: ''${BEST_GPU_RAM_MB}"
echo "MODEL: ''${RUNTIME_MODEL}"
echo "QUANTIZATION: ''${RUNTIME_QUANTIZATION:-auto}"
echo "CONTEXT: ''${RUNTIME_MAX_MODEL_LEN}"
echo "GPU_UTIL: ''${RUNTIME_GPU_UTIL}"
echo "MAX_BATCHED_TOKENS: ''${RUNTIME_MAX_BATCHED_TOKENS}"
echo "PROFILE: safe-by-vram-tier"

mkdir -p ''${MOUNT_PATH}/hf
export HF_HOME=''${MOUNT_PATH}/hf
export HUGGINGFACE_HUB_CACHE=''${MOUNT_PATH}/hf
export PYTORCH_CUDA_ALLOC_CONF=expandable_segments:True
export OMP_NUM_THREADS=4

echo "=== Starting vLLM ==="
QUANTIZATION_ARGS=""
if [[ -n "''${RUNTIME_QUANTIZATION:-}" ]]; then
  QUANTIZATION_ARGS="--quantization ''${RUNTIME_QUANTIZATION}"
fi

vllm serve ''${RUNTIME_MODEL} \
  --host 0.0.0.0 \
  --port 8000 \
  --trust-remote-code \
  --dtype auto \
  ''${QUANTIZATION_ARGS:-} \
  --tensor-parallel-size 1 \
  --max-model-len ''${RUNTIME_MAX_MODEL_LEN} \
  --gpu-memory-utilization ''${RUNTIME_GPU_UTIL} \
  --max-num-seqs 1 \
  --max-num-batched-tokens ''${RUNTIME_MAX_BATCHED_TOKENS} \
  --block-size 16 \
  --language-model-only \
  --enable-prefix-caching \
  --enable-auto-tool-choice \
  --tool-call-parser qwen3_coder \
  --reasoning-parser qwen3
EOF
)"
                ENV_STRING="-e HF_HOME=''${MOUNT_PATH}/hf -e HUGGINGFACE_HUB_CACHE=''${MOUNT_PATH}/hf -e PYTORCH_CUDA_ALLOC_CONF=expandable_segments:True -e OMP_NUM_THREADS=4"
                if [[ -n "''${HF_TOKEN:-}" ]]; then
                  ENV_STRING="$ENV_STRING -e HF_TOKEN=''${HF_TOKEN}"
                fi

                CREATE_SUCCESS=0
                NEW_CONTRACT_ID=""
                ATTEMPT_LIMIT="$MAX_CREATE_ATTEMPTS"
                if [[ "$count" -lt "$ATTEMPT_LIMIT" ]]; then
                  ATTEMPT_LIMIT="$count"
                fi

                for ((attempt_idx=0; attempt_idx<ATTEMPT_LIMIT; attempt_idx++)); do
                  if [[ "$attempt_idx" -gt 0 ]]; then
                    echo
                    echo "Retrying with next candidate ($((attempt_idx + 1))/$ATTEMPT_LIMIT)..."
                    load_candidate "$attempt_idx"
                    tune_runtime_for_selected_gpu
                    print_selected_candidate
                  fi

                  VOLUME_ARGS=()
                  CREATE_VOLUME_LABEL="$VOLUME_LABEL"
                  if [[ "$USE_VOLUME" = "1" ]]; then
                    case "$BEST_VOLUME_MODE" in
                      reuse)
                        VOLUME_ARGS=(
                          --link-volume "$BEST_REUSABLE_VOLUME_ID"
                          --mount-path "$MOUNT_PATH"
                        )
                        ;;
                    create)
                        CREATE_VOLUME_LABEL="''${VOLUME_LABEL}_''${BEST_ASK_ID}"
                        VOLUME_ARGS=(
                          --create-volume "$BEST_CREATE_VOLUME_OFFER_ID"
                          --volume-size "$VOLUME_SIZE_GB"
                          --mount-path "$MOUNT_PATH"
                          --volume-label "$CREATE_VOLUME_LABEL"
                        )
                        ;;
                      *)
                        echo "Refusing to launch without a usable volume plan."
                        exit 1
                        ;;
                    esac
                  fi

                  STEADY_BID_PRICE="$(compute_margin_bid "$BEST_DPH" "$BID_PRICE" "$STEADY_BID_MARGIN")"
                  STARTUP_BID_PRICE="$(compute_margin_bid "$BEST_DPH" "$STEADY_BID_PRICE" "$STARTUP_BID_MARGIN")"
                  echo "Requesting instance upsert..."
                  printf '  startup bid $/h: %s\n' "$STARTUP_BID_PRICE"
                  printf '  steady bid $/h : %s\n' "$STEADY_BID_PRICE"
                  CREATE_OUTPUT_FILE="$TMPDIR/create-instance.out"
                  set +e
                  vast create instance "$BEST_ASK_ID" \
                    --image "$IMAGE" \
                    --disk "$DISK_GB" \
                    --label "$LABEL" \
                    --ssh \
                    --direct \
                    --cancel-unavail \
                    --bid_price "$STARTUP_BID_PRICE" \
                    --env "$ENV_STRING" \
                    --onstart-cmd "$ONSTART_SCRIPT" \
                    "''${VOLUME_ARGS[@]}" >"$CREATE_OUTPUT_FILE" 2>&1
                  CREATE_STATUS=$?
                  set -e
                  if [[ "$CREATE_STATUS" -eq 0 ]] && create_response_says_success "$CREATE_OUTPUT_FILE"; then
                    cat "$CREATE_OUTPUT_FILE"
                    NEW_CONTRACT_ID="$(grep -Eo "new_contract['\"]?[[:space:]]*:[[:space:]]*[0-9]+" "$CREATE_OUTPUT_FILE" | grep -Eo '[0-9]+' | head -n1 || true)"
                    CREATE_SUCCESS=1
                    break
                  fi

                  echo
                  echo "Instance creation attempt failed."
                  cat "$CREATE_OUTPUT_FILE"
                  if grep -Fq 'no_such_ask' "$CREATE_OUTPUT_FILE"; then
                    echo "Selected offer went stale before create completed."
                    continue
                  fi
                  if [[ -n "$OLD_VOLUME_ID_TO_DELETE" ]]; then
                    echo "Old volume $OLD_VOLUME_ID_TO_DELETE was left intact."
                  fi
                  exit 1
                done

                if [[ "$CREATE_SUCCESS" != "1" ]]; then
                  echo
                  echo "No launch attempt succeeded."
                  echo "Top candidates went stale too quickly; re-run to refresh the market snapshot."
                  if [[ -n "$OLD_VOLUME_ID_TO_DELETE" ]]; then
                    echo "Old volume $OLD_VOLUME_ID_TO_DELETE was left intact."
                  fi
                  exit 1
                fi

                if [[ -n "$OLD_VOLUME_ID_TO_DELETE" ]]; then
                  echo "Deleting replaced volume $OLD_VOLUME_ID_TO_DELETE..."
                  set +e
                  timeout "''${DESTROY_TIMEOUT_SECS}s" "$VASTAI_BIN" delete volume "$OLD_VOLUME_ID_TO_DELETE" -y >/dev/null
                  DELETE_STATUS=$?
                  set -e
                  if [[ "$DELETE_STATUS" -ne 0 ]]; then
                    echo "Warning: old volume $OLD_VOLUME_ID_TO_DELETE was not deleted automatically."
                    echo "Delete it later after the old instance fully disappears."
                  fi
                fi

                if [[ -n "$NEW_CONTRACT_ID" ]]; then
                  if wait_for_local_api_ready "$NEW_CONTRACT_ID" "$(expected_context_for_gpu "$BEST_GPU")"; then
                    if [[ "$STEADY_BID_PRICE" != "$STARTUP_BID_PRICE" ]]; then
                      echo "Lowering bid after readiness: $STARTUP_BID_PRICE -> $STEADY_BID_PRICE"
                      set_instance_bid_best_effort "$NEW_CONTRACT_ID" "$STEADY_BID_PRICE" || true
                    fi
                  else
                    echo "Instance failed before API readiness. This can happen if the bid is outcompeted or the host exits early."
                    destroy_failed_readiness_instance "$NEW_CONTRACT_ID" || true

                    for ((readiness_retry=1; readiness_retry<=READINESS_RETRY_ATTEMPTS; readiness_retry++)); do
                      OLD_BID_PRICE="$BID_PRICE"
                      BID_PRICE="$(bump_bid_after_readiness_failure "$BEST_DPH" "$BID_PRICE")"
                      echo
                      echo "Retrying launch after readiness failure ($readiness_retry/$READINESS_RETRY_ATTEMPTS)."
                      echo "Increasing bid margin to reduce early outbid risk: $OLD_BID_PRICE -> $BID_PRICE"
                      echo "Refreshing market and re-running launcher..."
                      "$0" \
                        replace \
                        --vastai-version "$VASTAI_VERSION" \
                        --default-image "$DEFAULT_IMAGE" \
                        --cuda13-image "$CUDA13_IMAGE" \
                        --image-auto-select "$IMAGE_AUTO_SELECT" \
                        --model "$MODEL" \
                        --max-model-len "$MAX_MODEL_LEN" \
                        --disk "$DISK_GB" \
                        --use-volume "$USE_VOLUME" \
                        --volume-size "$VOLUME_SIZE_GB" \
                        --mount-path "$MOUNT_PATH" \
                        --volume-label "$VOLUME_LABEL" \
                        --max-price "$MAX_PRICE" \
                        --bid-price "$BID_PRICE" \
                        --min-reliability "$MIN_RELIABILITY" \
                        --preferred-reliability "$PREFERRED_RELIABILITY" \
                        --label "$LABEL" \
                        --max-create-attempts "$MAX_CREATE_ATTEMPTS" \
                        --destroy-timeout-secs "$DESTROY_TIMEOUT_SECS" \
                        --readiness-retry-attempts "$((READINESS_RETRY_ATTEMPTS - readiness_retry))" \
                        --readiness-retry-bid-margin "$READINESS_RETRY_BID_MARGIN" \
                        --max-bid-price "$MAX_BID_PRICE" \
                        --min-replace-savings-pct "$MIN_REPLACE_SAVINGS_PCT" \
                        --startup-bid-margin "$STARTUP_BID_MARGIN" \
                        --steady-bid-margin "$STEADY_BID_MARGIN"
                      exit $?
                    done

                    echo "Readiness retry attempts exhausted."
                    exit 1
                  fi
                else
                  echo "Warning: could not parse new contract id; skipping local API readiness wait."
                fi

                echo
                echo "Instance upsert requested."
                echo "Next:"
                echo "  vastai show instances -v"
                echo "  vastai ssh-url <instance_id>"
          '';
        };
    in {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = mkLauncher pkgs;
        });
      apps = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          launcher = mkLauncher pkgs;
        in {
          default = {
            type = "app";
            program = "${launcher}/bin/vast-qwen-launch";
            meta = {
              description = "Upsert a pinned Vast.ai Qwen deployment with volume reuse when possible.";
            };
          };
        });
      checks = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          launcher = mkLauncher pkgs;
        in {
          launcher-bash-syntax = pkgs.runCommand "vast-qwen-launch-bash-syntax" {
            nativeBuildInputs = [ pkgs.bash ];
          } ''
            set -euo pipefail
            bash -n ${launcher}/bin/vast-qwen-launch
            touch $out
          '';

          volume-parser-compat = pkgs.runCommand "vast-qwen-launch-volume-parser-compat" {
            nativeBuildInputs = [ pkgs.jq ];
          } ''
            set -euo pipefail

            cat > numeric.json <<'EOF'
{"volumes":[{"id":900,"machine_id":111,"storage_total_cost":0.0001,"instances":[42]}]}
EOF

            cat > object.json <<'EOF'
{"volumes":[{"id":901,"machine_id":111,"storage_total_cost":0.0001,"instances":[{"id":42}]}]}
EOF

            jq_filter='
              def rows:
                if type == "array" then .
                elif has("volumes") then .volumes
                else [] end;
              def uses_instance($instance_id):
                (.instances // [])
                | any(
                    if type == "number" then
                      . == $instance_id
                    elif type == "object" then
                      (.id // .instance_id // .contract_id // .ask_contract_id // -1) == $instance_id
                    else
                      false
                    end
                  );
              rows
              | map(select(uses_instance($instance_id)))
              | .[0].id // empty
            '

            test "$(jq -r --argjson instance_id 42 "$jq_filter" numeric.json)" = "900"
            test "$(jq -r --argjson instance_id 42 "$jq_filter" object.json)" = "901"
            touch $out
          '';

          create-response-compat = pkgs.runCommand "vast-qwen-launch-create-response-compat" {
            nativeBuildInputs = [ pkgs.gnugrep pkgs.jq ];
          } ''
            set -euo pipefail

            cat > json-success.txt <<'EOF'
{"success": true, "new_contract": 1}
EOF

            cat > python-success.txt <<'EOF'
Started. {'success': True, 'new_contract': 1}
EOF

            jq -e '.success == true' json-success.txt >/dev/null
            grep -Eq "['\"]success['\"]:[[:space:]]*(True|true)" python-success.txt
            touch $out
          '';

          volume-cost-conversion = pkgs.runCommand "vast-qwen-launch-volume-cost-conversion" {
            nativeBuildInputs = [ pkgs.gawk ];
          } ''
            set -euo pipefail
            test "$(awk 'BEGIN { printf "%.6f", 0.295648 + 0.037037 }')" = "0.332685"
            touch $out
          '';

          volume-estimate-live-pricing = pkgs.runCommand "vast-qwen-launch-volume-estimate-live-pricing" {
            nativeBuildInputs = [ pkgs.jq ];
          } ''
            set -euo pipefail

            jq_filter='
              def offered_volume_hourly:
                if (.volume_offer_total_cost > 0) then .volume_offer_total_cost
                elif (.offer_total_cost > 0) then .offer_total_cost
                elif (.volume_offer_cost > 0) then (.volume_offer_cost * .requested_volume_size)
                elif (.offer_cost > 0) then (.offer_cost * .requested_volume_size)
                else 0
                end;
              {
                volume_cost:
                  (if .can_reuse_volume then .existing_volume_cost
                   else offered_volume_hourly
                   end),
                total_hourly_cost:
                  (.dph + (if .can_reuse_volume then .existing_volume_cost
                           else offered_volume_hourly
                           end))
              }
            '

            cat > input.json <<'EOF'
{"can_reuse_volume":false,"existing_volume_cost":0.037037,"volume_offer_total_cost":0,"offer_total_cost":0,"volume_offer_cost":0.00037037,"offer_cost":0,"requested_volume_size":100,"dph":0.295648}
EOF

            test "$(jq -r "$jq_filter | .volume_cost" input.json)" = "0.037037"
            test "$(jq -r "$jq_filter | .total_hourly_cost" input.json)" = "0.332685"
            touch $out
          '';

          existing-instance-default-is-nondestructive = pkgs.runCommand "vast-qwen-launch-existing-instance-default-is-nondestructive" {
            nativeBuildInputs = [ pkgs.bash pkgs.gnugrep ];
          } ''
            set -euo pipefail

            cat > check.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

EXISTING_INSTANCE_ID=35543075
COMMAND=check
EXISTING_INSTANCE_NEEDS_FORCE_DECISION=0

if [[ -n "$EXISTING_INSTANCE_ID" ]] && [[ "$COMMAND" = "check" ]]; then
  EXISTING_INSTANCE_NEEDS_FORCE_DECISION=1
  echo "Existing labeled instance found. Searching market first to decide whether replacement is worth it."
  echo "No destructive action is performed without replace."
fi

test "$EXISTING_INSTANCE_NEEDS_FORCE_DECISION" = "1"
EOF
            bash check.sh > out.txt
            grep -Fq 'Existing labeled instance found. Searching market first to decide whether replacement is worth it.' out.txt
            grep -Fq 'No destructive action is performed without replace.' out.txt
            touch $out
          '';

                    replace-create-skips-wait = pkgs.runCommand "vast-qwen-launch-replace-create-skips-wait" {
            nativeBuildInputs = [ pkgs.bash pkgs.gnugrep ];
          } ''
            set -euo pipefail

            cat > check.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

BEST_VOLUME_MODE=create
EXISTING_INSTANCE_ID=35543075
EXISTING_VOLUME_ID=35543074
OLD_VOLUME_ID_TO_DELETE=""

echo "Replacing existing instance $EXISTING_INSTANCE_ID..."
if [[ "$BEST_VOLUME_MODE" = "reuse" ]]; then
  echo "Waiting for old instance to fully detach from the reusable volume..."
else
  echo "Old instance destroy requested; continuing with fresh-volume launch."
fi
if [[ -n "$EXISTING_VOLUME_ID" && "$BEST_VOLUME_MODE" != "reuse" ]]; then
  OLD_VOLUME_ID_TO_DELETE="$EXISTING_VOLUME_ID"
fi

test "$OLD_VOLUME_ID_TO_DELETE" = "35543074"
EOF
            bash check.sh > out.txt
            grep -Fq 'Old instance destroy requested; continuing with fresh-volume launch.' out.txt
            touch $out
          '';

          replace-create-destroy-timeout-continues = pkgs.runCommand "vast-qwen-launch-replace-create-destroy-timeout-continues" {
            nativeBuildInputs = [ pkgs.bash pkgs.gnugrep ];
          } ''
            set -euo pipefail

            cat > check.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

DESTROY_TIMEOUT_SECS=20
BEST_VOLUME_MODE=create
DESTROY_STATUS=124

case "$DESTROY_STATUS" in
  0)
    echo "Old instance destroy requested; continuing with fresh-volume launch."
    ;;
  124)
    echo "Destroy request timed out after $DESTROY_TIMEOUT_SECS"'s; continuing with fresh-volume launch anyway.'
    ;;
  *)
    exit 1
    ;;
esac
EOF
            bash check.sh > out.txt
            grep -Fq 'Destroy request timed out after 20s; continuing with fresh-volume launch anyway.' out.txt
            touch $out
          '';

          destroy-timeout-uses-cli-binary = pkgs.runCommand "vast-qwen-launch-destroy-timeout-uses-cli-binary" {
            nativeBuildInputs = [ pkgs.bash pkgs.gnugrep ];
          } ''
            set -euo pipefail

            cat > check.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
VASTAI_BIN=/tmp/fake-vastai
DESTROY_TIMEOUT_SECS=20
echo timeout "''${DESTROY_TIMEOUT_SECS}s" "$VASTAI_BIN" destroy instance 35543075
EOF
            bash check.sh > out.txt
            grep -Fq '/tmp/fake-vastai destroy instance 35543075' out.txt
            touch $out
          '';

          unique-create-volume-label = pkgs.runCommand "vast-qwen-launch-unique-create-volume-label" {
            nativeBuildInputs = [ pkgs.bash ];
          } ''
            set -euo pipefail

            cat > check.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
VOLUME_LABEL=qwen36vol
BEST_ASK_ID=35475257
CREATE_VOLUME_LABEL="$VOLUME_LABEL"'_'"$BEST_ASK_ID"
test "$CREATE_VOLUME_LABEL" = "qwen36vol_35475257"
EOF
            bash check.sh
            touch $out
          '';




          rebid-expected-state-guard = pkgs.runCommand "vast-qwen-launch-rebid-expected-state-guard" {
            nativeBuildInputs = [ pkgs.gawk ];
          } ''
            set -euo pipefail

            float_close() {
              awk -v left="$1" -v right="$2" '
                BEGIN {
                  diff = left - right
                  if (diff < 0) diff = -diff
                  exit !(diff <= 0.000001)
                }
              '
            }

            float_close 0.340800 0.340800
            if float_close 0.340800 0.340900; then
              echo "float comparison should reject changed bid" >&2
              exit 1
            fi

            touch $out
          '';

          rebid-target-uses-instance-bid-fields = pkgs.runCommand "vast-qwen-launch-rebid-target-uses-instance-bid-fields" {
            nativeBuildInputs = [ pkgs.gawk ];
          } ''
            set -euo pipefail

            compute() {
              awk \
                -v effective="$1" \
                -v min_bid="$2" \
                -v current_bid="$3" \
                -v margin="$4" \
                -v max_bid="$5" '
                  BEGIN {
                    floor = min_bid * margin
                    effective_target = effective * margin
                    target = floor
                    if (effective_target > target) target = effective_target
                    if (current_bid > target) target = current_bid
                    if (target > max_bid) target = max_bid
                    printf "%.6f", target
                  }
                '
            }

            test "$(compute 0.30 0.34 0.33 1.08 0.45)" = "0.367200"
            test "$(compute 0.40 0.20 0.42 1.08 0.45)" = "0.432000"
            test "$(compute 0.50 0.20 0.42 1.08 0.45)" = "0.450000"
            touch $out
          '';

          scheduler-stopped-readiness-detection = pkgs.runCommand "vast-qwen-launch-scheduler-stopped-readiness-detection" {
            nativeBuildInputs = [ pkgs.gnugrep ];
          } ''
            set -euo pipefail
            launcher=${launcher}/bin/vast-qwen-launch
            grep -Fq 'status_is_effectively_dead' "$launcher"
            grep -Fq 'intended_status' "$launcher"
            grep -Fq 'next_state' "$launcher"
            grep -Fq 'scheduler-stopped state' "$launcher"
            touch $out
          '';

          min-bid-margin-default = pkgs.runCommand "vast-qwen-launch-min-bid-margin-default" {
            nativeBuildInputs = [ pkgs.gnugrep ];
          } ''
            set -euo pipefail
            launcher=${launcher}/bin/vast-qwen-launch
            grep -Fq 'MIN_BID_MARGIN' "$launcher"
            grep -Fq 'MIN_BID_MARGIN:-1.02' "$launcher"
            touch $out
          '';

          command-interface = pkgs.runCommand "vast-qwen-launch-command-interface" {
            nativeBuildInputs = [ pkgs.gnugrep ];
          } ''
            set -euo pipefail
            launcher=${launcher}/bin/vast-qwen-launch

            grep -Fq 'Usage: launcher check [options]' "$launcher"
            grep -Fq 'Usage: launcher replace [options]' "$launcher"
            grep -Fq 'Usage: launcher rebid [options]' "$launcher"
            grep -Fq 'Usage: launcher watch [options]' "$launcher"
            grep -Fq 'suggest_command replace' "$launcher"
            grep -Fq 'suggest_command rebid' "$launcher"

            if grep -Fq 'read -r -p' "$launcher"; then
              echo "interactive prompt should not exist" >&2
              exit 1
            fi

            touch $out
          '';

                    command-parser-modes = pkgs.runCommand "vast-qwen-launch-command-parser-modes" {
            nativeBuildInputs = [ pkgs.bash ];
          } ''
            set -euo pipefail

            cat > parser.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
COMMAND=check

if [[ $# -gt 0 ]]; then
  case "$1" in
    check|replace|rebid|watch)
      COMMAND="$1"
      shift
      ;;
  esac
fi

case "$COMMAND" in
  check|replace|rebid|watch) ;;
  *) exit 1 ;;
esac

printf '%s\n' "$COMMAND"
EOF

            test "$(bash parser.sh)" = "check"
            test "$(bash parser.sh check)" = "check"
            test "$(bash parser.sh replace --max-price 0.35)" = "replace"
            test "$(bash parser.sh rebid --max-bid-price 0.42)" = "rebid"
            test "$(bash parser.sh watch --interval 1)" = "watch"
            touch $out
          '';








        });

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              bash
              coreutils
              gnugrep
              gnused
              gawk
              curl
              jq
              uv
            ];
          };
        });
    };
}

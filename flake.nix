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
    in {
      apps = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = {
            type = "app";
            program = toString (pkgs.writeShellApplication {
              name = "vast-qwen-launch";
              runtimeInputs = with pkgs; [
                bash
                coreutils
                gnugrep
                gnused
                gawk
                jq
                uv
              ];
              text = ''
                #!/usr/bin/env bash
                set -euo pipefail

                export PATH="$HOME/.local/bin:$PATH"

                VASTAI_VERSION="''${VASTAI_VERSION:-1.0.3}"
                IMAGE="''${IMAGE:-vllm/vllm-openai:v0.9.1}"
                MODEL="''${MODEL:-Qwen/Qwen3.6-27B-FP8}"

                DISK_GB="''${DISK_GB:-40}"
                USE_VOLUME="''${USE_VOLUME:-1}"
                VOLUME_SIZE_GB="''${VOLUME_SIZE_GB:-100}"
                MOUNT_PATH="''${MOUNT_PATH:-/workspace}"

                MAX_PRICE="''${MAX_PRICE:-0.35}"
                BID_PRICE="''${BID_PRICE:-0.33}"
                MIN_RELIABILITY="''${MIN_RELIABILITY:-0.985}"
                LABEL="''${LABEL:-qwen36-27b-fp8-48k}"
                MAX_MODEL_LEN="''${MAX_MODEL_LEN:-49152}"

                usage() {
                  cat <<EOF
Usage: nix run . -- [options]

Version pins:
  --vastai-version X.Y.Z     Vast CLI PyPI version (default: $VASTAI_VERSION)
  --image IMAGE:TAG          Docker image tag (default: $IMAGE)

Model/runtime:
  --model HF_MODEL           Hugging Face model (default: $MODEL)
  --max-model-len N          Context length (default: $MAX_MODEL_LEN)

Instance sizing:
  --disk N                   Container disk GB (default: $DISK_GB)
  --use-volume 0|1           Use workspace volume (default: $USE_VOLUME)
  --volume-size N            Workspace volume GB (default: $VOLUME_SIZE_GB)
  --mount-path PATH          Workspace mount path (default: $MOUNT_PATH)

Market controls:
  --max-price FLOAT          Max hourly offer to consider (default: $MAX_PRICE)
  --bid-price FLOAT          Interruptible bid price (default: $BID_PRICE)
  --min-reliability FLOAT    Min reliability, e.g. 0.985 (default: $MIN_RELIABILITY)

Misc:
  --label STRING             Instance label (default: $LABEL)
  -h, --help                 Show this help
EOF
                }

                while [[ $# -gt 0 ]]; do
                  case "$1" in
                    --vastai-version) VASTAI_VERSION="$2"; shift 2 ;;
                    --image) IMAGE="$2"; shift 2 ;;
                    --model) MODEL="$2"; shift 2 ;;
                    --max-model-len) MAX_MODEL_LEN="$2"; shift 2 ;;
                    --disk) DISK_GB="$2"; shift 2 ;;
                    --use-volume) USE_VOLUME="$2"; shift 2 ;;
                    --volume-size) VOLUME_SIZE_GB="$2"; shift 2 ;;
                    --mount-path) MOUNT_PATH="$2"; shift 2 ;;
                    --max-price) MAX_PRICE="$2"; shift 2 ;;
                    --bid-price) BID_PRICE="$2"; shift 2 ;;
                    --min-reliability) MIN_RELIABILITY="$2"; shift 2 ;;
                    --label) LABEL="$2"; shift 2 ;;
                    -h|--help) usage; exit 0 ;;
                    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
                  esac
                done

                need_cmd() {
                  command -v "$1" >/dev/null 2>&1
                }

                if ! need_cmd vastai; then
                  echo "Installing vastai==$VASTAI_VERSION with uv..."
                  uv tool install "vastai==$VASTAI_VERSION"
                  export PATH="$HOME/.local/bin:$PATH"
                fi

                if [[ -n "''${VAST_API_KEY:-}" ]]; then
                  vastai set api-key "$VAST_API_KEY" >/dev/null
                fi

                if ! vastai show user >/dev/null 2>&1; then
                  echo "Vast CLI is not authenticated."
                  echo "Set VAST_API_KEY or run: vastai set api-key YOUR_KEY"
                  exit 1
                fi

                TMPDIR="$(mktemp -d)"
                trap 'rm -rf "$TMPDIR"' EXIT

                GPUS=(
                  "L40S"
                  "RTX_5090"
                  "A100_PCIE"
                  "RTX_4090"
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
                        gpu_name: (.gpu_name // .gpu_name_display // $gpu),
                        dph: ((.dph_total // .dph_base // .dph // .discounted_dph_total // 999999) | tonumber),
                        reliability: ((.reliability2 // .reliability // 0) | tonumber),
                        dlperf: ((.dlperf // .dlperf_per_dphtotal // 0) | tonumber),
                        inet_up: ((.inet_up // 0) | tonumber),
                        inet_down: ((.inet_down // 0) | tonumber),
                        direct_port_count: ((.direct_port_count // 0) | tonumber),
                        geolocation: (.geolocation // .location // .city // ""),
                        num_gpus: ((.num_gpus // 1) | tonumber),
                        gpu_ram: ((.gpu_ram // .gpu_mem // 0) | tonumber)
                    })
                    | map(select(.ask_id != null))
                  '
                }

                preference_rank() {
                  case "$1" in
                    L40S) echo 0 ;;
                    RTX_5090) echo 1 ;;
                    A100_PCIE) echo 2 ;;
                    RTX_4090) echo 3 ;;
                    *) echo 99 ;;
                  esac
                }

                echo "Searching Vast offers..."
                : > "$TMPDIR/all.jsonl"

                for gpu in "''${GPUS[@]}"; do
                  query="gpu_name=$gpu num_gpus=1 rentable=true verified=true reliability>=$MIN_RELIABILITY direct_port_count>2 disk_space>=$DISK_GB"
                  if ! vastai search offers --raw "$query" > "$TMPDIR/$gpu.raw.json" 2>/dev/null; then
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

                jq -s --argjson max_price "$MAX_PRICE" '
                  map(select(.dph <= $max_price))
                  | sort_by(.rank, .dph, -.dlperf, -.reliability)
                ' "$TMPDIR/all.jsonl" > "$TMPDIR/candidates.json"

                count="$(jq 'length' "$TMPDIR/candidates.json")"
                if [[ "$count" -eq 0 ]]; then
                  echo "No offers found within max price $"$MAX_PRICE"."
                  echo "Try increasing --max-price."
                  exit 1
                fi

                echo
                echo "Top candidates:"
                jq -r '
                  .[:5][] |
                  "  ask_id=\(.ask_id)  gpu=\(.gpu_name)  $/h=\(.dph)  rel=\(.reliability)  dlperf=\(.dlperf)  loc=\(.geolocation)"
                ' "$TMPDIR/candidates.json"
                echo

                BEST_ASK_ID="$(jq -r '.[0].ask_id' "$TMPDIR/candidates.json")"
                BEST_GPU="$(jq -r '.[0].gpu_name' "$TMPDIR/candidates.json")"
                BEST_DPH="$(jq -r '.[0].dph' "$TMPDIR/candidates.json")"
                BEST_REL="$(jq -r '.[0].reliability' "$TMPDIR/candidates.json")"
                BEST_LOC="$(jq -r '.[0].geolocation' "$TMPDIR/candidates.json")"

                echo "Selected:"
                echo "  ask_id      : $BEST_ASK_ID"
                echo "  gpu         : $BEST_GPU"
                echo "  offer $/h   : $BEST_DPH"
                echo "  bid $/h     : $BID_PRICE"
                echo "  reliability : $BEST_REL"
                echo "  location    : $BEST_LOC"
                echo "  image       : $IMAGE"
                echo "  model       : $MODEL"
                echo "  max context : $MAX_MODEL_LEN"
                echo "  disk        : $DISK_GB GB"
                if [[ "$USE_VOLUME" = "1" ]]; then
                  echo "  volume      : $VOLUME_SIZE_GB GB at $MOUNT_PATH"
                else
                  echo "  volume      : disabled"
                fi
                echo

                read -r -p "Create this instance? [Y/n] " reply
                reply="''${reply:-Y}"
                case "$reply" in
                  Y|y|"") ;;
                  *) echo "Aborted."; exit 0 ;;
                esac

                VOLUME_ARGS=()
                if [[ "$USE_VOLUME" = "1" ]]; then
                  echo "Searching for a compatible local volume offer..."
                  if vastai search volumes --raw "disk_space>=$VOLUME_SIZE_GB" > "$TMPDIR/volumes.raw.json" 2>/dev/null; then
                    VOLUME_ID="$({
                      jq -r '
                        def rows:
                          if type == "array" then .
                          elif has("offers") then .offers
                          elif has("results") then .results
                          else [] end;
                        rows
                        | map(select((.id // .ask_id // .volume_id) != null))
                        | first
                        | (.id // .ask_id // .volume_id // empty)
                      ' "$TMPDIR/volumes.raw.json"
                    })"
                    if [[ -n "$VOLUME_ID" ]]; then
                      VOLUME_ARGS=(
                        --create-volume "$VOLUME_ID"
                        --volume-size "$VOLUME_SIZE_GB"
                        --mount-path "$MOUNT_PATH"
                        --volume-label "$LABEL-vol"
                      )
                    else
                      echo "No volume offer found. Continuing without volume."
                    fi
                  else
                    echo "Volume search failed. Continuing without volume."
                  fi
                fi

                ONSTART_SCRIPT="$(cat <<EOF
set -euxo pipefail
mkdir -p ${MOUNT_PATH}/hf
export HF_HOME=${MOUNT_PATH}/hf
export HUGGINGFACE_HUB_CACHE=${MOUNT_PATH}/hf
export PYTORCH_CUDA_ALLOC_CONF=expandable_segments:True
export OMP_NUM_THREADS=4
python3 -m vllm.entrypoints.openai.api_server \
  --model ${MODEL} \
  --trust-remote-code \
  --dtype auto \
  --tensor-parallel-size 1 \
  --max-model-len ${MAX_MODEL_LEN} \
  --gpu-memory-utilization 0.94 \
  --max-num-seqs 1 \
  --max-num-batched-tokens 2048 \
  --block-size 16 \
  --language-model-only \
  --enable-prefix-caching \
  --enable-auto-tool-choice \
  --tool-call-parser qwen3_coder \
  --reasoning-parser qwen3 \
  --port 8000
EOF
)"
                ENV_STRING="-e HF_HOME=${MOUNT_PATH}/hf -e HUGGINGFACE_HUB_CACHE=${MOUNT_PATH}/hf -e PYTORCH_CUDA_ALLOC_CONF=expandable_segments:True -e OMP_NUM_THREADS=4"
                if [[ -n "''${HF_TOKEN:-}" ]]; then
                  ENV_STRING="$ENV_STRING -e HF_TOKEN=''${HF_TOKEN}"
                fi

                set -x
                vastai create instance "$BEST_ASK_ID" \
                  --image "$IMAGE" \
                  --disk "$DISK_GB" \
                  --label "$LABEL" \
                  --ssh \
                  --direct \
                  --cancel-unavail \
                  --bid_price "$BID_PRICE" \
                  --env "$ENV_STRING" \
                  --onstart-cmd "$ONSTART_SCRIPT" \
                  "''${VOLUME_ARGS[@]}"
                set +x

                echo
                echo "Instance requested."
                echo "Next:"
                echo "  vastai show instances -v"
                echo "  vastai ssh-url <instance_id>"
              '';
            });
          };
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
              jq
              uv
            ];
          };
        });
    };
}

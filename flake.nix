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

          launcher = pkgs.writeShellApplication {
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

                # Keep numeric parsing/formatting stable across locales.
                export LC_NUMERIC=C
                export PATH="$HOME/.local/bin:$PATH"

                VASTAI_VERSION="''${VASTAI_VERSION:-1.0.3}"
                DEFAULT_IMAGE="''${DEFAULT_IMAGE:-vllm/vllm-openai:v0.19.1}"
                BLACKWELL_IMAGE="''${BLACKWELL_IMAGE:-vllm/vllm-openai:cu130-nightly-968ed02acedf60d9a8128f96cc69a350327a5143}"
                IMAGE="''${IMAGE:-$DEFAULT_IMAGE}"
                IMAGE_AUTO_SELECT="''${IMAGE_AUTO_SELECT:-1}"
                MODEL="''${MODEL:-Qwen/Qwen3.6-27B-FP8}"

                DISK_GB="''${DISK_GB:-40}"
                USE_VOLUME="''${USE_VOLUME:-1}"
                VOLUME_SIZE_GB="''${VOLUME_SIZE_GB:-100}"
                MOUNT_PATH="''${MOUNT_PATH:-/workspace}"

                MAX_PRICE="''${MAX_PRICE:-0.35}"
                BID_PRICE="''${BID_PRICE:-0.33}"
                MIN_RELIABILITY="''${MIN_RELIABILITY:-0.985}"
                PREFERRED_RELIABILITY="''${PREFERRED_RELIABILITY:-0.99}"
                LABEL="''${LABEL:-qwen36-27b-fp8-48k}"
                MAX_MODEL_LEN="''${MAX_MODEL_LEN:-49152}"
                DRY_RUN="''${DRY_RUN:-0}"

                usage() {
                  cat <<EOF
Usage: nix run . -- [options]

Version pins:
  --vastai-version X.Y.Z     Vast CLI PyPI version (default: $VASTAI_VERSION)
  --image IMAGE:TAG          Docker image tag override (default: auto-selected)
  --default-image IMAGE:TAG  Stable default image (default: $DEFAULT_IMAGE)
  --blackwell-image IMAGE:TAG  Image used for RTX 5090 hosts when auto-select is enabled (default: $BLACKWELL_IMAGE)
  --image-auto-select 0|1    Auto-pick a pinned Blackwell image for RTX 5090 hosts (default: $IMAGE_AUTO_SELECT)

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
  --preferred-reliability FLOAT  Prefer offers at or above this reliability when available (default: $PREFERRED_RELIABILITY)

Misc:
  --label STRING             Instance label (default: $LABEL)
  --dry-run                  Show candidate but don't create instance
  -h, --help                 Show this help
EOF
                }

                while [[ $# -gt 0 ]]; do
                  case "$1" in
                    --vastai-version) VASTAI_VERSION="$2"; shift 2 ;;
                    --image) IMAGE="$2"; IMAGE_AUTO_SELECT=0; shift 2 ;;
                    --default-image) DEFAULT_IMAGE="$2"; if [[ "''${IMAGE_AUTO_SELECT}" = "1" ]]; then IMAGE="$2"; fi; shift 2 ;;
                    --blackwell-image) BLACKWELL_IMAGE="$2"; shift 2 ;;
                    --image-auto-select) IMAGE_AUTO_SELECT="$2"; shift 2 ;;
                    --model) MODEL="$2"; shift 2 ;;
                    --max-model-len) MAX_MODEL_LEN="$2"; shift 2 ;;
                    --disk) DISK_GB="$2"; shift 2 ;;
                    --use-volume) USE_VOLUME="$2"; shift 2 ;;
                    --volume-size) VOLUME_SIZE_GB="$2"; shift 2 ;;
                    --mount-path) MOUNT_PATH="$2"; shift 2 ;;
                    --max-price) MAX_PRICE="$2"; shift 2 ;;
                    --bid-price) BID_PRICE="$2"; shift 2 ;;
                    --min-reliability) MIN_RELIABILITY="$2"; shift 2 ;;
                    --preferred-reliability) PREFERRED_RELIABILITY="$2"; shift 2 ;;
                    --label) LABEL="$2"; shift 2 ;;
                    --dry-run) DRY_RUN=1; shift ;;
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
                        machine_id: (.machine_id // .machine // null),
                        host_id: (.host_id // null),
                        avail_vol_ask_id: (.avail_vol_ask_id // null),
                        avail_vol_size: ((.avail_vol_size // 0) | tonumber),
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
                    | map(select(.ask_id != null and .machine_id != null))
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

                jq -s '.' "$TMPDIR/all.jsonl" > "$TMPDIR/offers.json"

                if [[ "$USE_VOLUME" = "1" ]]; then
                  echo "Searching compatible volume offers..."
                  if ! vastai search volumes --raw "disk_space>=$VOLUME_SIZE_GB" > "$TMPDIR/volumes.raw.json" 2>/dev/null; then
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
                        storage_total_cost: ((.storage_total_cost // 0) | tonumber),
                        disk_space: ((.disk_space // 0) | tonumber)
                    })
                    | map(select(.volume_offer_id != null and .machine_id != null))
                  ' "$TMPDIR/volumes.raw.json" > "$TMPDIR/volumes.json"

                  if [[ "$(jq 'length' "$TMPDIR/volumes.json")" -eq 0 ]]; then
                    echo "No compatible volume offers were returned by Vast."
                    exit 1
                  fi
                else
                  echo '[]' > "$TMPDIR/volumes.json"
                fi

                jq -n \
                  --slurpfile offers "$TMPDIR/offers.json" \
                  --slurpfile volumes "$TMPDIR/volumes.json" \
                  --argjson max_price "$MAX_PRICE" \
                  --argjson preferred_reliability "$PREFERRED_RELIABILITY" \
                  --argjson require_volume "$USE_VOLUME" '
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

                  ($offers[0]) as $offer_list
                  | ($volumes[0]) as $volume_list
                  | $offer_list
                  | map(select(.dph <= $max_price))
                  | map(
                      . as $offer
                      | ($volume_list
                          | map(select(.machine_id == $offer.machine_id))
                          | sort_by(.storage_total_cost, -.reliability)
                          | .[0]
                        ) as $volume
                      | . + {
                          loc_tier: loc_tier,
                          rel_tier: rel_tier($preferred_reliability),
                          volume_offer_id: ($volume.volume_offer_id // .avail_vol_ask_id // null),
                          volume_cost: ($volume.storage_total_cost // 0),
                          volume_reliability: ($volume.reliability // null),
                          total_hourly_cost: (.dph + ($volume.storage_total_cost // 0))
                        }
                    )
                  | if ($require_volume == 1 or $require_volume == "1")
                    then map(select(.volume_offer_id != null))
                    else .
                    end
                  | sort_by(.rank, .rel_tier, .loc_tier, .total_hourly_cost, -.dlperf, -.reliability)
                ' > "$TMPDIR/candidates.json"

                count="$(jq 'length' "$TMPDIR/candidates.json")"
                if [[ "$count" -eq 0 ]]; then
                  if [[ "$USE_VOLUME" = "1" ]]; then
                    echo "No offers found within price/reliability constraints that also have a same-machine volume."
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
                    (.volume_offer_id // ""),
                    .gpu_name,
                    .dph,
                    .volume_cost,
                    .total_hourly_cost,
                    .reliability,
                    .loc_tier,
                    .dlperf,
                    .geolocation
                  ] | @tsv
                ' "$TMPDIR/candidates.json" | while IFS=$'\t' read -r ask_id vol_id gpu_name dph volume_cost total_hourly_cost reliability loc_tier dlperf geolocation; do
                  printf '  ask_id=%s  vol_id=%s  gpu=%s  inst=$/h:%.6f  vol=$/h:%.6f  total=$/h:%.6f  rel=%s  loc-tier=%s  dlperf=%s  loc=%s\n' \
                    "$ask_id" "$vol_id" "$gpu_name" "$dph" "$volume_cost" "$total_hourly_cost" "$reliability" "$loc_tier" "$dlperf" "$geolocation"
                done
                echo

                BEST_ASK_ID="$(jq -r '.[0].ask_id' "$TMPDIR/candidates.json")"
                BEST_MACHINE_ID="$(jq -r '.[0].machine_id' "$TMPDIR/candidates.json")"
                BEST_GPU="$(jq -r '.[0].gpu_name' "$TMPDIR/candidates.json")"
                BEST_DPH="$(jq -r '.[0].dph' "$TMPDIR/candidates.json")"
                BEST_REL="$(jq -r '.[0].reliability' "$TMPDIR/candidates.json")"
                BEST_LOC="$(jq -r '.[0].geolocation' "$TMPDIR/candidates.json")"
                BEST_VOLUME_OFFER_ID="$(jq -r '.[0].volume_offer_id // empty' "$TMPDIR/candidates.json")"
                BEST_VOLUME_COST="$(jq -r '.[0].volume_cost // 0' "$TMPDIR/candidates.json")"
                BEST_TOTAL_COST="$(jq -r '.[0].total_hourly_cost // .[0].dph' "$TMPDIR/candidates.json")"

                if [[ "$IMAGE_AUTO_SELECT" = "1" ]]; then
                  IMAGE="$DEFAULT_IMAGE"
                  if [[ "$BEST_GPU" = "RTX 5090" ]]; then
                    IMAGE="$BLACKWELL_IMAGE"
                  fi
                fi

                echo "Selected:"
                printf '  ask_id        : %s\n' "$BEST_ASK_ID"
                printf '  machine_id    : %s\n' "$BEST_MACHINE_ID"
                printf '  gpu           : %s\n' "$BEST_GPU"
                printf '  instance $/h  : %.6f\n' "$BEST_DPH"
                if [[ "$USE_VOLUME" = "1" ]]; then
                  printf '  volume offer  : %s\n' "$BEST_VOLUME_OFFER_ID"
                  printf '  volume $/h    : %.6f\n' "$BEST_VOLUME_COST"
                  printf '  total $/h     : %.6f\n' "$BEST_TOTAL_COST"
                else
                  printf '  total $/h     : %.6f\n' "$BEST_DPH"
                fi
                printf '  bid $/h       : %.6f\n' "$BID_PRICE"
                printf '  reliability   : %.6f\n' "$BEST_REL"
                printf '  location      : %s\n' "$BEST_LOC"
                printf '  image         : %s\n' "$IMAGE"
                printf '  model         : %s\n' "$MODEL"
                printf '  max context   : %s\n' "$MAX_MODEL_LEN"
                printf '  disk          : %s GB\n' "$DISK_GB"
                if [[ "$USE_VOLUME" = "1" ]]; then
                  printf '  volume        : %s GB at %s\n' "$VOLUME_SIZE_GB" "$MOUNT_PATH"
                else
                  printf '  volume        : disabled\n'
                fi
                echo

                if [[ "$DRY_RUN" = "1" ]]; then
                  echo "Dry run mode - no instance will be created."
                  exit 0
                fi

                read -r -p "Create this instance? [Y/n] " reply
                reply="''${reply:-Y}"
                case "$reply" in
                  Y|y|"") ;;
                  *) echo "Aborted."; exit 0 ;;
                esac

                VOLUME_ARGS=()
                if [[ "$USE_VOLUME" = "1" ]]; then
                  if [[ -z "$BEST_VOLUME_OFFER_ID" ]]; then
                    echo "Refusing to launch without a same-machine volume."
                    exit 1
                  fi
                  VOLUME_ARGS=(
                    --create-volume "$BEST_VOLUME_OFFER_ID"
                    --volume-size "$VOLUME_SIZE_GB"
                    --mount-path "$MOUNT_PATH"
                    --volume-label "qwen36vol"
                  )
                fi

                ONSTART_SCRIPT="$(cat <<EOF
set -euxo pipefail
mkdir -p ''${MOUNT_PATH}/hf
export HF_HOME=''${MOUNT_PATH}/hf
export HUGGINGFACE_HUB_CACHE=''${MOUNT_PATH}/hf
export PYTORCH_CUDA_ALLOC_CONF=expandable_segments:True
export OMP_NUM_THREADS=4
vllm serve ''${MODEL} \
  --host 0.0.0.0 \
  --port 8000 \
  --trust-remote-code \
  --dtype auto \
  --tensor-parallel-size 1 \
  --max-model-len ''${MAX_MODEL_LEN} \
  --gpu-memory-utilization 0.94 \
  --max-num-seqs 1 \
  --max-num-batched-tokens 2048 \
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

                echo "Requesting instance creation..."
                if ! vastai create instance "$BEST_ASK_ID" \
                  --image "$IMAGE" \
                  --disk "$DISK_GB" \
                  --label "$LABEL" \
                  --ssh \
                  --direct \
                  --cancel-unavail \
                  --bid_price "$BID_PRICE" \
                  --env "$ENV_STRING" \
                  --onstart-cmd "$ONSTART_SCRIPT" \
                  "''${VOLUME_ARGS[@]}"; then
                  echo
                  echo "Instance creation failed."
                  exit 1
                fi

                echo
                echo "Instance requested."
                echo "Next:"
                echo "  vastai show instances -v"
                echo "  vastai ssh-url <instance_id>"
            '';
          };
        in {
          default = {
            type = "app";
            program = "${launcher}/bin/vast-qwen-launch";
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

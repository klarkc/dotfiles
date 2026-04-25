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
                BLACKWELL_IMAGE="''${BLACKWELL_IMAGE:-vllm/vllm-openai:cu130-nightly}"
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
                VOLUME_LABEL="''${VOLUME_LABEL:-qwen36vol}"
                MAX_MODEL_LEN="''${MAX_MODEL_LEN:-49152}"
                FORCE_REPLACE_ACTIVE="''${FORCE_REPLACE_ACTIVE:-0}"
                MAX_CREATE_ATTEMPTS="''${MAX_CREATE_ATTEMPTS:-3}"
                DESTROY_TIMEOUT_SECS="''${DESTROY_TIMEOUT_SECS:-20}"

                usage() {
                  cat <<EOF
Usage: nix run . -- [options]

Version pins:
  --vastai-version X.Y.Z       Vast CLI PyPI version (default: $VASTAI_VERSION)
  --image IMAGE:TAG            Docker image tag override (default: auto-selected)
  --default-image IMAGE:TAG    Stable default image (default: $DEFAULT_IMAGE)
  --blackwell-image IMAGE:TAG  Image used for RTX 5090 hosts when auto-select is enabled (default: $BLACKWELL_IMAGE)
  --image-auto-select 0|1      Auto-pick a pinned Blackwell image for RTX 5090 hosts (default: $IMAGE_AUTO_SELECT)

Model/runtime:
  --model HF_MODEL             Hugging Face model (default: $MODEL)
  --max-model-len N            Context length (default: $MAX_MODEL_LEN)

Instance sizing:
  --disk N                     Container disk GB (default: $DISK_GB)
  --use-volume 0|1             Use workspace volume (default: $USE_VOLUME)
  --volume-size N              Workspace volume GB (default: $VOLUME_SIZE_GB)
  --mount-path PATH            Workspace mount path (default: $MOUNT_PATH)
  --volume-label STRING        Name to use for newly created volumes (default: $VOLUME_LABEL)

Market controls:
  --max-price FLOAT            Max hourly offer to consider (default: $MAX_PRICE)
  --bid-price FLOAT            Interruptible bid price (default: $BID_PRICE)
  --min-reliability FLOAT      Min reliability, e.g. 0.985 (default: $MIN_RELIABILITY)
  --preferred-reliability FLOAT Prefer offers at or above this reliability when available (default: $PREFERRED_RELIABILITY)

Misc:
  --label STRING               Instance label used for upsert matching (default: $LABEL)
  --force-replace-active 0|1   Replace an existing loading/running instance instead of staying put (default: $FORCE_REPLACE_ACTIVE)
  --max-create-attempts N      Retry the next candidate when an ask goes stale (default: $MAX_CREATE_ATTEMPTS)
  --destroy-timeout-secs N     Timeout for non-blocking destroy requests during fresh-volume replacement (default: $DESTROY_TIMEOUT_SECS)
  -h, --help                   Show this help
EOF
                }

                while [[ $# -gt 0 ]]; do
                  case "$1" in
                    --vastai-version) VASTAI_VERSION="$2"; shift 2 ;;
                    --image) IMAGE="$2"; IMAGE_AUTO_SELECT=0; shift 2 ;;
                    --default-image) DEFAULT_IMAGE="$2"; if [[ "$IMAGE_AUTO_SELECT" = "1" ]]; then IMAGE="$2"; fi; shift 2 ;;
                    --blackwell-image) BLACKWELL_IMAGE="$2"; shift 2 ;;
                    --image-auto-select) IMAGE_AUTO_SELECT="$2"; shift 2 ;;
                    --model) MODEL="$2"; shift 2 ;;
                    --max-model-len) MAX_MODEL_LEN="$2"; shift 2 ;;
                    --disk) DISK_GB="$2"; shift 2 ;;
                    --use-volume) USE_VOLUME="$2"; shift 2 ;;
                    --volume-size) VOLUME_SIZE_GB="$2"; shift 2 ;;
                    --mount-path) MOUNT_PATH="$2"; shift 2 ;;
                    --volume-label) VOLUME_LABEL="$2"; shift 2 ;;
                    --max-price) MAX_PRICE="$2"; shift 2 ;;
                    --bid-price) BID_PRICE="$2"; shift 2 ;;
                    --min-reliability) MIN_RELIABILITY="$2"; shift 2 ;;
                    --preferred-reliability) PREFERRED_RELIABILITY="$2"; shift 2 ;;
                    --label) LABEL="$2"; shift 2 ;;
                    --force-replace-active) FORCE_REPLACE_ACTIVE="$2"; shift 2 ;;
                    --max-create-attempts) MAX_CREATE_ATTEMPTS="$2"; shift 2 ;;
                    --destroy-timeout-secs) DESTROY_TIMEOUT_SECS="$2"; shift 2 ;;
                    -h|--help) usage; exit 0 ;;
                    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
                  esac
                done

                need_cmd() {
                  command -v "$1" >/dev/null 2>&1
                }

                create_response_says_success() {
                  local response_file="$1"
                  jq -e '.success == true' "$response_file" >/dev/null 2>&1 && return 0
                  grep -Eq "['\"]success['\"]:[[:space:]]*(True|true)" "$response_file"
                }

                status_is_activeish() {
                  case "$1" in
                    loading|running|scheduling) return 0 ;;
                    *) return 1 ;;
                  esac
                }

                load_candidate() {
                  local idx="$1"
                  BEST_ASK_ID="$(jq -r ".[$idx].ask_id" "$TMPDIR/candidates.json")"
                  BEST_MACHINE_ID="$(jq -r ".[$idx].machine_id" "$TMPDIR/candidates.json")"
                  BEST_GPU="$(jq -r ".[$idx].gpu_name" "$TMPDIR/candidates.json")"
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
                    if [[ "$BEST_GPU" = "RTX 5090" ]]; then
                      IMAGE="$BLACKWELL_IMAGE"
                    fi
                  fi
                }

                print_selected_candidate() {
                  echo "Selected:"
                  printf '  ask_id        : %s\n' "$BEST_ASK_ID"
                  printf '  machine_id    : %s\n' "$BEST_MACHINE_ID"
                  printf '  gpu           : %s\n' "$BEST_GPU"
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
                  printf '  model         : %s\n' "$MODEL"
                  printf '  max context   : %s\n' "$MAX_MODEL_LEN"
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
                    if ! vast show instance "$instance_id" --raw > "$probe_file" 2>/dev/null; then
                      return 0
                    fi
                    if [[ -z "$(jq -r '(.instances.id // .id // empty)' "$probe_file" 2>/dev/null)" ]]; then
                      return 0
                    fi
                    sleep 2
                  done
                  return 1
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
                EXISTING_INSTANCE_DPH="0"
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
                  EXISTING_INSTANCE_DPH="$(jq -r --arg label "$LABEL" '
                    def rows:
                      if type == "array" then .
                      elif has("instances") then .instances
                      else [] end;
                    rows
                    | map(select((.label // "") == $label and ((.actual_status // .cur_state // "") != "destroyed")))
                    | (.[0].dph_total // .[0].dph // .[0].discounted_dph_total // 0)
                  ' "$TMPDIR/current-instances.json")"
                  echo "Found existing instance:"
                  printf '  instance_id : %s\n' "$EXISTING_INSTANCE_ID"
                  printf '  machine_id  : %s\n' "$EXISTING_INSTANCE_MACHINE_ID"
                  printf '  status      : %s\n' "$EXISTING_INSTANCE_STATUS"
                  printf '  current $/h : %.6f\n' "$EXISTING_INSTANCE_DPH"
                else
                  echo "No existing labeled instance found."
                fi

                if [[ -n "$EXISTING_INSTANCE_ID" ]] && [[ "$FORCE_REPLACE_ACTIVE" != "1" ]] && status_is_activeish "$EXISTING_INSTANCE_STATUS"; then
                  echo
                  echo "Keeping current placement."
                  echo "Existing instance is still $EXISTING_INSTANCE_STATUS, so upsert will not replace it by default."
                  echo "Use --force-replace-active 1 if you really want to tear it down and move now."
                  exit 0
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
                        storage_cost: ((.storage_cost // 0) | tonumber),
                        storage_total_cost: ((.storage_total_cost // 0) | tonumber),
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
                  | map(select(.dph <= $max_price))
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
                    then map(select(.volume_mode != null))
                    else .
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
                    .dph,
                    .volume_cost,
                    .total_hourly_cost,
                    .reliability,
                    .loc_tier,
                    .dlperf,
                    .geolocation
                  ] | @tsv
                ' "$TMPDIR/candidates.json" | while IFS=$'\t' read -r ask_id reusable_volume_id create_volume_offer_id volume_mode gpu_name dph volume_cost total_hourly_cost reliability loc_tier dlperf geolocation; do
                  printf '  ask_id=%s  reuse_vol=%s  create_vol=%s  vol-mode=%s  gpu=%s  inst=$/h:%.6f  vol-est=$/h:%.6f  total-est=$/h:%.6f  rel=%s  loc-tier=%s  dlperf=%s  loc=%s\n' \
                    "$ask_id" "$reusable_volume_id" "$create_volume_offer_id" "$volume_mode" "$gpu_name" "$dph" "$volume_cost" "$total_hourly_cost" "$reliability" "$loc_tier" "$dlperf" "$geolocation"
                done
                echo

                load_candidate 0
                print_selected_candidate

                read -r -p "Proceed with this upsert? [Y/n] " reply
                reply="''${reply:-Y}"
                case "$reply" in
                  Y|y|"") ;;
                  *) echo "Aborted."; exit 0 ;;
                esac

                OLD_VOLUME_ID_TO_DELETE=""
                if [[ -n "$EXISTING_INSTANCE_ID" ]]; then
                  echo "Replacing existing instance $EXISTING_INSTANCE_ID..."
                  if [[ "$BEST_VOLUME_MODE" = "reuse" ]]; then
                    if ! vast destroy instance "$EXISTING_INSTANCE_ID" >/dev/null; then
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
                    timeout "''${DESTROY_TIMEOUT_SECS}s" "$VASTAI_BIN" destroy instance "$EXISTING_INSTANCE_ID" >/dev/null
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
                  if [[ -n "$EXISTING_VOLUME_ID" && "$BEST_VOLUME_MODE" != "reuse" ]]; then
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

                CREATE_SUCCESS=0
                ATTEMPT_LIMIT="$MAX_CREATE_ATTEMPTS"
                if [[ "$count" -lt "$ATTEMPT_LIMIT" ]]; then
                  ATTEMPT_LIMIT="$count"
                fi

                for ((attempt_idx=0; attempt_idx<ATTEMPT_LIMIT; attempt_idx++)); do
                  if [[ "$attempt_idx" -gt 0 ]]; then
                    echo
                    echo "Retrying with next candidate ($((attempt_idx + 1))/$ATTEMPT_LIMIT)..."
                    load_candidate "$attempt_idx"
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

                  echo "Requesting instance upsert..."
                  CREATE_OUTPUT_FILE="$TMPDIR/create-instance.out"
                  set +e
                  vast create instance "$BEST_ASK_ID" \
                    --image "$IMAGE" \
                    --disk "$DISK_GB" \
                    --label "$LABEL" \
                    --ssh \
                    --direct \
                    --cancel-unavail \
                    --bid_price "$BID_PRICE" \
                    --env "$ENV_STRING" \
                    --onstart-cmd "$ONSTART_SCRIPT" \
                    "''${VOLUME_ARGS[@]}" >"$CREATE_OUTPUT_FILE" 2>&1
                  CREATE_STATUS=$?
                  set -e
                  if [[ "$CREATE_STATUS" -eq 0 ]] && create_response_says_success "$CREATE_OUTPUT_FILE"; then
                    cat "$CREATE_OUTPUT_FILE"
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
                  timeout "''${DESTROY_TIMEOUT_SECS}s" "$VASTAI_BIN" delete volume "$OLD_VOLUME_ID_TO_DELETE" >/dev/null
                  DELETE_STATUS=$?
                  set -e
                  if [[ "$DELETE_STATUS" -ne 0 ]]; then
                    echo "Warning: old volume $OLD_VOLUME_ID_TO_DELETE was not deleted automatically."
                    echo "Delete it later after the old instance fully disappears."
                  fi
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

          loading-instance-stays-put = pkgs.runCommand "vast-qwen-launch-loading-instance-stays-put" {
            nativeBuildInputs = [ pkgs.bash pkgs.gnugrep ];
          } ''
            set -euo pipefail

            cat > check.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

status_is_activeish() {
  case "$1" in
    loading|running|scheduling) return 0 ;;
    *) return 1 ;;
  esac
}

EXISTING_INSTANCE_ID=35543075
EXISTING_INSTANCE_STATUS=loading
FORCE_REPLACE_ACTIVE=0

if [[ -n "$EXISTING_INSTANCE_ID" ]] && [[ "$FORCE_REPLACE_ACTIVE" != "1" ]] && status_is_activeish "$EXISTING_INSTANCE_STATUS"; then
  echo "Keeping current placement."
  echo "Existing instance is still $EXISTING_INSTANCE_STATUS, so upsert will not replace it by default."
  echo "Use --force-replace-active 1 if you really want to tear it down and move now."
  exit 0
fi

exit 1
EOF
            bash check.sh > out.txt

            grep -Fq 'Keeping current placement.' out.txt
            grep -Fq 'Use --force-replace-active 1 if you really want to tear it down and move now.' out.txt
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

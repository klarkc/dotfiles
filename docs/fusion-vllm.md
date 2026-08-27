# Fusion + vLLM + Pi runtime notes

This repo runs Fusion through a user `systemd` service and uses local vLLM through Pi's model registry.

## Working provider identity

Use `local-vllm` consistently everywhere:

- `~/.fusion/settings.json`
  - `defaultProvider = local-vllm`
  - `fallbackProvider = local-vllm`
- `~/.fusion/agent/auth.json`
  - key: `local-vllm`
- `~/.pi/agent/models.json`
  - provider: `providers.local-vllm`

Do not use Fusion `customProviders` for this path. In this setup, Pi/Fusion agent execution resolves the configured model through Pi's model registry, not through dashboard custom providers.

## Pi models registry schema

For the bundled Pi version in Fusion 0.31.0, the working schema is object-shaped:

```json
{
  "providers": {
    "local-vllm": {
      "name": "local-vllm",
      "baseUrl": "http://localhost:8000/v1",
      "api": "openai-completions",
      "apiKey": "VLLM_API_KEY",
      "models": [
        {
          "id": "qwen3.6-35b-a3b",
          "name": "Qwen3.6 35B A3B"
        }
      ]
    }
  }
}
```

A list-shaped `providers: [...]` file was tested and did not load through the bundled Pi `ModelRegistry`.

The `apiKey` value should be the environment variable name without a dollar sign:

```json
"apiKey": "VLLM_API_KEY"
```

Do not use:

```json
"apiKey": "$VLLM_API_KEY"
```

That can be treated as a literal string in some config paths.

## Auth file

Keep this entry in `~/.fusion/agent/auth.json`:

```json
{
  "local-vllm": {
    "type": "api_key",
    "key": "VLLM_API_KEY"
  }
}
```

The `vllm-patch-model-defaults` script maintains this file and the Pi registry when `vllm-config` selects a model.

## systemd user environment imports

`fusion.service` and `vllm@.service` use `PassEnvironment`. That means variables are passed only if they already exist in the systemd user manager environment. The old single-model `vllm.service` is obsolete; use `vllm-config` and the target-based `vllm@...service` instances.

Import the required variables from an interactive shell before restarting services:

```bash
systemctl --user import-environment VLLM_API_KEY HF_TOKEN HF_HUB_TOKEN HUGGING_FACE_HUB_TOKEN GITHUB_TOKEN FUSION_DASHBOARD_TOKEN FUSION_DAEMON_TOKEN VAST_API_KEY
```

Minimum for authenticated local vLLM is:

```bash
systemctl --user import-environment VLLM_API_KEY
```

Minimum for Hugging Face gated/private model access is one of:

```bash
systemctl --user import-environment HF_TOKEN
systemctl --user import-environment HF_HUB_TOKEN
systemctl --user import-environment HUGGING_FACE_HUB_TOKEN
```

Check the user manager environment without printing values:

```bash
for var in VLLM_API_KEY HF_TOKEN HF_HUB_TOKEN HUGGING_FACE_HUB_TOKEN GITHUB_TOKEN FUSION_DASHBOARD_TOKEN FUSION_DAEMON_TOKEN VAST_API_KEY; do
  systemctl --user show-environment | grep -q "^${var}=" && echo "$var present" || echo "$var missing"
done
```

Check whether a running service inherited a variable:

```bash
pid="$(systemctl --user show -p MainPID --value fusion.service)"
tr '\0' '\n' < "/proc/$pid/environ" | grep -q '^VLLM_API_KEY=' && echo present || echo missing
```

## vLLM benchmarks and 35B baseline

Use `vllm-benchmark` for maintained target-aware benchmarks. The older `bench-vllm` helper was removed with the legacy single-model `vllm.service` path.

The 35B interactive baseline is tuned for opencode-style local use with frequent compaction. It keeps full context and two concurrent sequences while enabling MTP2 speculative decoding with extra runtime VRAM headroom:

```env
MAX_MODEL_LEN=49152
MAX_NUM_SEQS=2
MAX_NUM_BATCHED_TOKENS=3072
GPU_MEMORY_UTILIZATION=0.93
SPECULATIVE_CONFIG_B64=eyJtZXRob2QiOiJtdHAiLCJudW1fc3BlY3VsYXRpdmVfdG9rZW5zIjoyfQ==
```

The speculative config decodes to:

```json
{"method":"mtp","num_speculative_tokens":2}
```

Benchmark summaries from the tuning session:

| Profile | Status | GPU util | MTP tokens | Context | Max seqs | Batched tokens | Notes |
|---|---:|---:|---:|---:|---:|---:|---|
| 35B no-MTP baseline | success | 0.94 | none | 49152 | 2 | 3072 | Historical baseline; best 48k long-context throughput. |
| 35B MTP2 initial | failed | 0.94 | 2 | 49152 | 2 | 3072 | Medium benchmark failed with HTTP 500 from CUDA OOM / EngineDeadError. |
| 35B MTP2 tuned | success | 0.93 | 2 | 49152 | 2 | 3072 | Default interactive baseline for opencode. |

| Case | no-MTP decode tok/s | tuned MTP2 decode tok/s | no-MTP TPOT ms | tuned MTP2 TPOT ms | tuned MTP2 result |
|---|---:|---:|---:|---:|---|
| Small, 4x 1k input / 32 output / concurrency 2 | 2.25 | 9.65 | 916.05 | 212.5 | faster |
| Medium, 4x 4k input / 64 output / concurrency 2 | 8.34 | 10.72 | 243.14 | 188.84 | faster |
| Long, 2x 48k input / 64 output / concurrency 2 | 1.38 | 1.23 | 1470.91 | 1646.69 | slower |

Tuned MTP2 startup reported `109,320` GPU KV-cache tokens and `2.22x` maximum concurrency for 49,152-token requests. Speculative decoding acceptance was generally healthy during testing, with mean acceptance length around `2.48-2.91` and average draft acceptance around `73.9%-95.5%`.

Reasoning: opencode with frequent compaction is usually dominated by small/medium interactive requests, where tuned MTP2 substantially improves latency. The 48k long-context regression is accepted for the interactive default. If long-context throughput becomes the dominant workload, manually retest the historical no-MTP settings by clearing `SPECULATIVE_CONFIG_B64` and restoring `GPU_MEMORY_UTILIZATION=0.94`.

## Verification

After `git pull` and `vllm-config qwen3.6-35B-a3b`:

```bash
jq '.providers."local-vllm" | {baseUrl, api, apiKey, models: [.models[].id]}' ~/.pi/agent/models.json
jq '."local-vllm"' ~/.fusion/agent/auth.json
```

Expected model listing through Fusion:

```bash
curl -s http://127.0.0.1:4040/api/models \
  | jq '.models[] | select(.provider=="local-vllm")'
```

Expected entries:

- `local-vllm/qwen3.6-35b-a3b`
- `local-vllm/qwen3.6-27b`

## Variables reviewed

Imported variables currently relevant to these services:

- `VLLM_API_KEY`: required when vLLM is started with `--api-key`; Fusion/Pi must resolve this for local inference.
- `HF_TOKEN`, `HF_HUB_TOKEN`, `HUGGING_FACE_HUB_TOKEN`: used by vLLM/Hugging Face model downloads.
- `GITHUB_TOKEN`: passed to Fusion for GitHub-backed workflows.
- `FUSION_DASHBOARD_TOKEN`, `FUSION_DAEMON_TOKEN`: Fusion auth tokens when auth is enabled.
- `VAST_API_KEY`: useful for Vast.ai automation/agent workflows.

Intentionally not imported by default here: cloud LLM provider keys such as `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `OPENROUTER_API_KEY`, `GEMINI_API_KEY`, and `GOOGLE_API_KEY`. Passing those into Fusion can enable cloud providers; add them explicitly only if that is desired.

## Local patches against upstream vLLM

The `vllm-runtime` derivation layers the following patches on top of the
upstream vLLM wheel built from `vllmRequirement` in
`.nix/vllm-runtime.nix`. Each patch documents its purpose and the
condition that lets us remove it.

| Patch | Source | Purpose | Removal condition |
|---|---|---|---|
| `vllm-qwen3_5-embed-uva.patch` | inline repo | Opt-in Qwen token embedding UVA offload via `qwen_embed_offload_gb` (frees ~2.4 GiB of VRAM by pinning `embed_tokens` to host memory). | Upstream ships the same UVA offload for Qwen 3.5/3.6/3.8 token embeddings in a stable release. Verify with `grep -R "qwen_embed_offload_gb" vllm/model_executor/models/qwen3_5.py` returning `0`. |
| LM-head UVA rewrite (inline `installPhase` Python) | `.nix/vllm-runtime.nix:170-237` | Opt-in Qwen LM head UVA offload via `qwen_lm_head_offload_gb` (mirror of the embed patch for `lm_head`). | Same as above; the rewrite uses stable string anchors so it usually survives upstream refactors without touching its needle. |
| `vllm-2_3-bit-autoround-humming.patch` | upstream PR [vllm-project/vllm#52890](https://github.com/vllm-project/vllm/pull/52890), head `040f4f6f3bdff505f7f8bb943c4da9c8ea77baa2` (rebased onto the local tag). Previous form: [vllm-project/vllm#52729](https://github.com/vllm-project/vllm/pull/52729) (closed unmerged). | CUDA inference of AutoRound 2/3-bit checkpoints, e.g. `huggingface.co/Intel/Qwen3.8-27B-bpw2.8-AutoRound` (the local `qwen3.6-27B` target). Upstream Marlin/GPTQ/AWQ kernels only cover 4/8-bit on CUDA, so without this routing low-bit layers would fail to load with `NotImplementedError`. | Upstream PR #52890 merges into a stable vLLM release. Verify with `grep -R CUDA_HUMMING_SUPPORTED_BITS vllm/model_executor/layers/quantization/inc/schemes/inc_wna16_scheme.py` returning `0`. |

### Bumping the vLLM version

When bumping `vllmRequirement` and `version` in `.nix/vllm-runtime.nix` /
`flake.nix`, run, in this order:

1. Re-apply each `---` patch with `patch -p1 --dry-run` against the new
   upstream source to verify offsets still hold:
   ```bash
   for p in .nix/patches/*.patch; do patch -p1 --dry-run -d <staging_dir> < "$p"; done
   ```
   If a patch no longer applies cleanly, regenerate it from the upstream
   PR/branch.
2. Refresh `outputHash` of the wheelhouse. Nix prints the expected hash
   on the first failing build.
3. Rebuild `nix build .#vllm-runtime` until it closes.
4. Run `nix profile upgrade klarkc` so the user profile picks up the
   new derivation.
5. Smoke-test via `.local/bin/vllm-smoke-test` — see AGENTS.md for the
   contract.

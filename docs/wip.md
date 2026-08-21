# WIP handoff — Qwen3.8-27B on vLLM

Context for the next agent. Captured at the end of a session whose goal was to make vLLM usable on the RTX 3060 12 GB and measure decode throughput.

Branch and HEAD

- Repo: klarkc/dotfiles
- Branch: qwen38
- HEAD: b51e412 (perf(qwen3.8): enable decode-only CUDA graphs)
- Working tree at the end of session: clean except for unrelated pre-existing diffs in .config/crush/crush.json, .config/opencode/opencode.json, .fusion/settings.json, .pi/agent/models.json. They were untouched at the user's instruction and should stay untouched.

Goal of the work

Make vLLM genuinely usable on this 12 GB GPU for the user's real workload (switching between parallel opencode agents via herdr, heavy warm reads of a shared prefix) and not just bootable. Order decided:

1. finish and freeze a reliable vLLM baseline
2. find out why decode tok/s looked low
3. optimize vLLM keeping prefix caching excellent
4. only then compare with Escha/SGLang

Hard requirements preserved throughout the session

- MAX_MODEL_LEN = 49152
- MAX_NUM_SEQS = 2
- prefix caching enabled
- CPU/offload allowed
- best usable end-to-end tokens/s and low agent-switch TTFT
- MTP3 NOT enabled
- torch.compile / Inductor NOT enabled
- 35B-A3B profile untouched

What we ended up with — vLLM best (production)

Profile: /home/klarkc/.config/vllm/qwen3.6-27B.env. Backup of the previous shape before this round is at qwen3.6-27B.env.production. The FP32 SSM intermediate shape is at qwen3.6-27B.env.ssm-fp32-backup.

- Model: Intel/Qwen3.8-27B-bpw2.8-AutoRound
- DTYPE bfloat16, MAX_MODEL_LEN 49152, MAX_NUM_SEQS 2, MAX_NUM_BATCHED_TOKENS 2048
- GPU_MEMORY_UTILIZATION 0.94, CPU_OFFLOAD_GB 0
- KV_CACHE_DTYPE int4_per_token_head
- attention_backend TRITON_ATTN
- mamba_ssm_cache_dtype bfloat16
- COMPILATION_CONFIG {"mode":"NONE","cudagraph_mode":"FULL_DECODE_ONLY","cudagraph_capture_sizes":[1,2]}
- Two targeted UVA offloads via VLLM_EXTRA_ARGS: qwen_embed_offload_gb=3.0 and qwen_lm_head_offload_gb=3.0
- DEFAULT_MAX_TOKENS 4096; preserve_thinking=true; qwen3 reasoning parser; qwen3_coder tool parser

Service observed boot metrics (current):

- Model loading took 7.99 GiB
- Available KV cache memory 2.16 GiB
- GPU KV cache size 111,238 tokens (block_size auto 1552 with mamba_ssm_cache_dtype=bfloat16, was 96166 with FP32)
- kv_cache_max_concurrency 2.26x (was 1.96x)
- num_gpu_blocks 86 (was 45)

Cumulative performance vs the first working baseline (FP32 SSM, no graphs):

| Case | metric | FP32 baseline | final | delta |
|------|--------|--------------:|------:|------:|
| small  (1024 in / 32 out) | decode_tps | 4.06 | 4.88 | +20% |
| small  | tpot_ms | 331 | 255 | -23% |
| small  | ttft_s | 5.4 | 5.1 | -6% |
| medium (4096 in / 64 out) | decode_tps | 4.58 | 5.07 | +11% |
| medium | tpot_ms | 351 | 249 | -29% |
| medium | ttft_s | 5.7 | 9.5 | +67% |
| long   (48000 in / 64 out) | decode_tps | 0.32 | 0.55 | +72% |
| long   | tpot_ms | 1408 | 286 | -80% |
| long   | ttft_s | 314 | 212 | -32% |

Prefix-switch (synthetic ~46k prompt, 12 requests):

| Agent   | cold TTFT | warm2 TTFT |
|---------|----------:|-----------:|
| A       | 199 s     | 5.6 s      |
| B       | 181 s     | 6.8 s      |
| C       | 189 s     | 5.1 s      |
| SoloSIG | 196 s     | 3.0 s      |

Internal cache counters confirm the hit path (counters recorded in prefix-switch.log):

- vllm:prefix_cache_hits_total: 431680 -> 647520 (+215840 tokens over the run)
- vllm:prompt_tokens_cached_total: 389120 -> 604960 (+215840 tokens, matches)
- vllm:cache_config_info exposes: mamba_ssm_cache_dtype bfloat16, block_size 1552, kv_cache_max_concurrency 2.26x, kv_cache_size_tokens 111238, num_gpu_blocks 86, mamba_cache_mode align, mamba_block_size 16, prefix_match_unit None

Key findings from this session

1. The lm_head UVA is load-bearing. Removing it (qwen_lm_head_offload_gb=0) causes OOM on this 12 GB GPU even with MAX_MODEL_LEN=16384, MAX_NUM_SEQS=1, GPU_MEMORY_UTILIZATION=0.92. The lm_head cannot be A/B tested against the GPU-resident case because the no-UVA configuration does not boot. This is consistent with the user's "expected" annotation. We cannot isolate whether the lm_head UVA is the cause of the residual low decode TPS at long context. We did not pursue this further because the BF16 SSM experiment alone moved LONG decode from 0.32 to 0.54 tok/s, which is the binding bottleneck on long-code-agent workloads.

2. mamba_ssm_cache_dtype=bfloat16 is the biggest single win in this session. Long-context decode_tps +72%, long-context tpot_ms -80%, KV cache capacity +16%. Block size auto-resized from 3040 to 1552 to accommodate the smaller SSM state. Quality smoke test (Python Fibonacci coding task) produced a correct reasoning-then-code response; no numerical degradation observed at the tested prompt lengths (up to ~46k).

3. Decode-only CUDA graphs with capture sizes [1, 2] (MAX_NUM_SEQS=2) is the second win. Small decode_tps +51% (3.24 -> 4.88), small tpot_ms -37% (405 -> 255). Medium tpot_ms -22%. Long tpot_ms -22%. KV capacity unchanged. torch.compile / Inductor stays disabled (mode=NONE) because Inductor triggered ~2.37 GiB OOM during autotune earlier.

4. Anthropic-valid sign of cache hit on the prefix-switch run: prompt_tokens in the chat response stays at ~45k per request (it is the logical prompt size, not the cache hit signal). The vLLM counters vllm:prefix_cache_hits_total and vllm:prompt_tokens_cached_total are the authoritative cache-hit signals. Both moved together by exactly +215840 tokens over the run.

5. /metrics scrape must be Bearer-token-authenticated; /v1/tokenize is not exposed on this vLLM build (404); /v1/metrics is 404, the real /metrics is at root /metrics. The benchmark harness already handles all three quirks.

Pending tasks (after this hand-off)

In strict order:

A. Verify the vLLM best config is stable under the actual herdr-driven workload. The synthetic prefix-switch shows it works; now drive the real agent switching through the same endpoint and confirm the warm path in production-grade usage.

B. Consider the optional micro-tuning only after the real workload baseline is captured:
   - prefix_match_unit: current default is None. vLLM supports finer-grained cache key matching. Quantized attention block size is 3040 with FP32 SSM or 1552 with BF16 SSM. The user's prompt mentioned a possible --prefix-match-unit 32 experiment. Do not add this until the basic server is stable and the default prefix-cache behavior is benchmarked.
   - BF16 SSM was a clear win. The recurrent state dtype is now BF16, so prefix cache hashes may differ slightly. Re-run prefix-switch to confirm prefix-cache hit count and warm2 TTFT have not regressed since the cudagraph commit.

C. Once A and B are stable, the next big work is the Escha/SGLang comparison, only if the user opts in. The user explicitly does NOT want this started yet. The reason for the order: Escha (EschaLabs/Qwen3.8-27B-Escha-W2) keeps lm_head INT8 in GPU and may avoid the UVA bottleneck the vLLM path cannot measure. Decode TPS alone will not decide the winner; the comparison must use the same prompts and the same cold/warm sequence.

Commits on qwen38 in this session (in order):

- 4f96c21 fix(vllm-runtime): make LM-head UVA rewrite and wrapper executable
- ce388b5 chore(agents): track served qwen3.8-27b model
- ac95098 tmp(benchmark): add prefix-switch subcommand for agent-switch / cache workload
- 3a36a0b tmp(benchmark): finish prefix-switch with /metrics scrape and clamp metadata
- c2ac808 perf(qwen3.8): enable --mamba-ssm-cache-dtype bfloat16
- b51e412 perf(qwen3.8): enable decode-only CUDA graphs

Notes on the tmp(benchmark) commits: the prefix-switch subcommand lives in /home/klarkc/.local/bin/vllm-benchmark. They keep the tmp prefix because the user wants to drop or rename them once the post-experiment follow-ups are closed. The follow-ups called out in those commits:
  - decide whether to broaden the /metrics counter scrape to all vllm:* names (already done; the recording is in place)
  - decide on the prefix-token clamp margin (currently 12 percent)
  - decide whether to merge the legacy SMALL/MEDIUM/LONG sweep with the prefix-switch subcommand (currently separate)
  - verify that max_model_len-aware clamping matches real workload

Things NOT to do in the next session

- Do not re-enable MTP.
- Do not re-enable torch.compile / Inductor.
- Do not switch back to FP8 + Triton for KV cache.
- Do not switch runtime to SGLang, mistral.rs, or llama.cpp.
- Do not add broad decoder CPU offloading.
- Do not change multiple variables at once.
- Do not change MAX_MODEL_LEN below 49152 unless explicitly required by a future experiment.
- Do not commit unrelated config changes (crush/opencode/fusion/pi) without explicit instruction.
- Do not amend, rebase, or rewrite history. Always add a new commit.

Coupled-version-dependency notes (per AGENTS.md)

- vllm-runtime version: 0.24.0-cu130 (Nix store path /nix/store/qcfbzid6v...vllm-runtime). No runtime bump in this session.
- CUDA toolkit: 13.0.x via Nix. No CUDA bump in this session.
- Model: Intel/Qwen3.8-27B-bpw2.8-AutoRound (revision main). No model bump in this session.
- vLLM source PR: pip install -v "vllm @ git+https://github.com/vllm-project/vllm.git@refs/pull/52729/head" pinned to commit 462591a87e13545e7a8a310b7c1f71ba798a6a10. No source bump in this session.

Files outside the repo that matter

- ~/.local/bin/vllm-benchmark: contains the prefix-switch subcommand. Diff is committed at 3a36a0b.
- ~/.local/bin/vllm-serve-pure: launcher; prescribes the runtime command, dumps benchmark.env.
- ~/.local/bin/vllm-patch-model-defaults: pre-start hook that overrides per-model defaults; current shape: qwen3.8-27b, raw_context=49152, client_context=43008, max_tokens=4096, max_num_seqs=2.
- ~/.local/bin/vllm-bench-prefix-switch: not yet created; the prefix-switch subcommand lives inside vllm-benchmark.
- ~/.local/bin/vllm-config: instance launcher. Usage: vllm-config qwen3.6-27B.
- ~/.cache/vllm-qwen3.6-27B/: per-instance cache. benchmark.env is the file the benchmark reads; runtime_command.sh is the actual vllm serve invocation.
- ~/.cache/vllm-benchmarks/<timestamp>-qwen3.6-27B/: per-run artifact. Contains prefix-switch.csv, prefix-switch.json, prefix-switch.summary.env, prefix-switch.metrics.txt, small/medium/long-requests.{json,log,summary.env,exit-code}, journal-vllm.txt, nvidia-smi.txt, processes.txt, runtime_command.sh, and a tar.gz of the whole directory.

Reproduction commands (next agent)

Backend build chain (no changes since this session):

  nix build .#vllm-runtime -L
  nix profile upgrade klarkc

Service control:

  systemctl --user daemon-reload
  vllm-config qwen3.6-27B

Benchmarks (use the same env vars as this session):

  vllm-benchmark --check
  vllm-benchmark
  vllm-benchmark prefix-switch

Workload benchmark (Agent A/B/C with shared prefix) per the user's plan. The harness lives in vllm-benchmark prefix-switch and uses synthetic prompts that mimic data volume and shape. PREFIX_TOKENS=32768 default is automatically clamped against max_model_len with a 12 percent safety margin; the artifact records requested_prefix_tokens, estimated_actual_prefix_tokens, chars_per_token_estimate, and clamp_reason.

Diagnostic that did NOT run

The lm_head A/B diagnostic: production (qwen_lm_head_offload_gb=3.0 with 12 GB) vs lm_head-on-GPU-FP16 (qwen_lm_head_offload_gb=0). The lm_head-on-GPU-FP16 case does not boot on this 12 GB GPU even with MAX_MODEL_LEN=16384, MAX_NUM_SEQS=1, GPU_MEMORY_UTILIZATION=0.92. Each attempt produced OOM during nvrtc JIT after the model loader succeeded with only 28-41 MB free. The lm_head UVA is load-bearing. No further lm_head-only diagnostic is possible without changing the model (e.g. quantizing lm_head to INT8 in the loader, which is not done in this vLLM build). The next agent should not retry this configuration expecting a different result.

What was tried but rejected

- ESCHA / SGLang: explicitly out of scope this session. Reason: user's order is vLLM first, Escha only after the vLLM path is frozen.
- mistral.rs and llama.cpp: ruled out earlier (llama.cpp has prompt-cache/cache-ram approaches but switched between agents can trigger expensive re-prefills with Qwen's hybrid Gated DeltaNet/recurrent state; mistral.rs is interesting but Qwen3.8 / mixed-AutoRound support is less proven).
- rLLM: not an inference engine; ruled out.
- FP8 KV cache with Triton attention: rejected earlier (FP8 KV cache is not supported by the Triton attention backend on SM86; needed SM89+).
- FlashInfer + FP8 KV: works on SM86 but overall memory still did not fit.
- Generic PrefetchOffloader: announced only saved 0.0175 GB of GPU memory after Humming repacking; rejected as a primary solution earlier.
- Rooting multiple decoder layers over PCIe: currently capped; not pursued this session.
- `OMP_NUM_THREADS` / `VLLM_USE_FLASHINFER_SAMPLER=0` already in force; not retuned.

Style / workflow expectations (unchanged)

- One variable per experiment.
- Always record VRAM, model loading memory, KV capacity, max concurrency, decode_tps, tpot_ms, ttft_s, prefill_tps, request_tps, and prefix-cache counters.
- Always distinguish: build/JIT failure, model-load VRAM failure, KV-cache capacity failure, runtime/decode performance failure.
- Never claim a configuration works without log evidence.
- Always commit with the commit SHA and the exact commands to pull/build/test.

End of WIP.

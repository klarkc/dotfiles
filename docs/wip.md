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

## 2026-08-21 follow-up — freeze inference, move to model/agent behavior tuning

The user considers the current vLLM stack usable. Decode throughput is still modest, but the large infrastructure wins are already captured and prefix reuse is excellent. Do not start another broad vLLM tuning campaign now.

Current priority:

1. validate the current production profile in the real herdr/OpenCode workload;
2. optionally run one isolated `prefix_match_unit=32` experiment after the real baseline is captured;
3. otherwise freeze the inference stack;
4. move to per-model/per-agent behavioral profile tuning and evidence-based evaluation;
5. only after that consider Escha/SGLang.

Terminology: the historical "fine tuning" done for Qwen3.6-35B-A3B was inference/model-profile tuning, not weight fine-tuning/LoRA/SFT. Keep that distinction explicit.

### Current behavioral-config asymmetry

The Qwen3.6-35B-A3B entry in OpenCode has explicit behavioral settings including `top_k=20`, `min_p=0`, `presence_penalty=0`, `repetition_penalty=1.1`, and `thinking_token_budget=1536` plus agent-level temperature/top_p values. The Qwen3.8-27B entry currently has essentially only name/context/output limits and therefore falls much closer to the model/template defaults.

`vllm-patch-model-defaults` currently propagates model id/name, base URL, context, max tokens and max_num_seqs, but not a full behavioral profile. The desired design is for `vllm-config <model>` to select both the serving profile and the behavioral defaults for that model, with agent-level overrides layered on top.

Suggested per-model fields to design around (names may change after checking client support):

- MODEL_TEMPERATURE
- MODEL_TOP_P
- MODEL_TOP_K
- MODEL_MIN_P
- MODEL_PRESENCE_PENALTY
- MODEL_REPETITION_PENALTY
- MODEL_REASONING_EFFORT
- MODEL_THINKING_TOKEN_BUDGET
- MODEL_PRESERVE_THINKING

Do not blindly force unsupported fields into every client. Inspect OpenCode/Pi/Crush/Fusion capabilities and only propagate a setting where it is actually supported. Preserve existing unrelated user diffs in those config files.

### Qwen3.8 reasoning evidence gathered from public community testing

Use these findings as hypotheses and external baselines, not as a substitute for our own benchmark.

1. Doğukan Urker / BenchKit, Qwen3.8-27B on an RTX 3060 12 GB:
   - reported 1,083 tasks per reasoning level under the same config;
   - LOW and MEDIUM tied in the aggregate;
   - XHIGH used about 3x the reasoning tokens and looped about 10x more without a meaningful score gain;
   - a Q4 HumanEval+ follow-up reportedly kept the same overthinking pattern (LOW 95.1 vs XHIGH 96.3 on 164 tasks, 1.2 pp difference), so the observation was not only IQ2-specific;
   - sampling profile used: temperature=1.0, top_p=0.95, top_k=20, min_p=0, presence_penalty=0, repetition_penalty=1.0.
   - Source thread: https://x.com/DogukanUrker/status/2089282517010374665
   - Harness: https://github.com/DogukanUrker/BenchKit

2. Alexey Fateev (@superalesha):
   - reported successful long-running coding-agent use of Qwen3.8-27B with vLLM and `reasoning_effort=low`, including a 67-minute 3D-game build with no reported tool-call errors;
   - anecdotal evidence only, useful as a real-agent signal.
   - Source: https://x.com/superalesha/status/2088928621217931567

3. Tom Turney (@no_stp_on_snek), behavioral audit:
   - 141 held-out probes plus a 56-run thinking ablation;
   - LOW vs XHIGH used roughly 2.9x fewer reasoning tokens in LOW, with no genuine XHIGH wins on the matched integrity/code-judgment set and some XHIGH regressions;
   - found long-agent loops could starve their own context through reasoning verbosity;
   - important caveat: LOW was worse on some safety-adjacent cases, so "LOW always" is not a valid universal rule;
   - identified that the pinned official template treats MEDIUM as valid but has no dedicated MEDIUM instruction branch, making MEDIUM effectively neutral/native thinking rather than a literal midpoint;
   - Source: https://x.com/no_stp_on_snek/status/2088375653162717680

4. BlackwellBoy reproducible campaign:
   - fixed-4K reasoning study headline: OFF 68%, LOW 94%, XHIGH 88%, with XHIGH truncating 5/50;
   - one preregistered ProgramBench agent task under otherwise frozen setup: OFF repeat 241/502, LOW 253/502, MEDIUM 308/502, XHIGH 0/502 after an agent-loop failure;
   - MEDIUM was best quality on that one task but slowest wall time, so it is interesting evidence rather than a universal recommendation;
   - repository: https://github.com/Blackwellboy/qwen38-27b-is-not-one-number

The practical interpretation for our workload is:

- XHIGH is the official/default control, but should not be assumed to be the best production setting.
- MEDIUM should be interpreted as neutral/native thinking on the current template and is the leading first candidate for Plan/Design.
- LOW is the leading latency candidate and should be tested especially for Explore/Scout and possibly Build.
- thinking OFF should be evaluated only for trivial transform/summary/title-style roles.

### Behavioral baseline plan

Do not copy the old 35B tuning into Qwen3.8. Start from an externally grounded sampling baseline and vary one reasoning dimension first.

B0 official/default control:
- thinking=true
- reasoning_effort=xhigh
- temperature=1.0
- top_p=0.95
- top_k=20
- min_p=0
- presence_penalty=0
- repetition_penalty=1.0
- preserve_thinking=true

B1 neutral thinking:
- same as B0 except reasoning_effort=medium

B2 fast thinking:
- same as B0 except reasoning_effort=low

B3 no thinking:
- thinking=false
- only for simple roles/tasks; do not use as the default coding-agent arm.

Do not introduce a `thinking_token_budget` for Qwen3.8 in the first comparison. First measure the natural reasoning-token distribution for LOW/MEDIUM/XHIGH. If a hard budget is later useful, derive it from observed P50/P75/P90/P95 reasoning lengths per agent/task rather than guessing a value such as 1536.

Keep `preserve_thinking=true` for the initial experiment because it was intentionally added for agentic coding and interacts with multi-turn continuity/prefix reuse. Any change to it requires a dedicated A/B rather than being bundled with reasoning effort.

### Agent-specific hypothesis before measurement

This is a starting hypothesis only, not a committed default:

- Plan: MEDIUM
- Design: MEDIUM
- Build: MEDIUM vs LOW A/B
- Explore: LOW
- Scout: LOW
- General: LOW vs MEDIUM
- Summary: thinking OFF
- Compaction: thinking OFF
- Title: thinking OFF

### Evaluation strategy

Use two layers.

External/reproducible baseline:
- Prefer BenchKit because it supports OpenAI-compatible/vLLM endpoints and has HumanEval+, MBPP+, IFEval, Aider Polyglot, RULER, loop detection, reasoning traces, timing, JSON/CSV outputs, and an optional Pi coding-agent harness.
- Start with a small deterministic slice to validate harness correctness, then a larger useful slice. Do not spend hours running the full suite before the harness is validated.

Real workload benchmark:
- create/extend an agent-oriented benchmark for Plan/Build/Explore/SoloSIG-like tasks using frozen repo/task snapshots;
- measure task completion, not merely model-token efficiency;
- record at minimum: success/quality score, wall time, TTFT, reasoning tokens, answer tokens, tool calls, failed tool calls, retries, input tokens, cached-input/prefix-hit data where available;
- derive `time_to_success`, `tokens_to_success`, `tool_calls_to_success`, success rate and loop/failure rate.

Do not infer a win simply because LOW emits fewer tokens. A slower/more thoughtful arm can still win if it avoids retries. Conversely, XHIGH should be rejected if it burns tokens/loops without improving task success.

For Plan specifically, the user is currently observing obvious overthinking under Qwen3.8. The first useful A/B is MEDIUM vs LOW, with XHIGH retained as the official/default control. Do not change the production default before collecting at least a small frozen-task baseline.

### Immediate Build handoff

1. Read this WIP and current repo/config state before editing.
2. Preserve the current vLLM inference profile; no MTP/compile/offload/runtime experiments in this task.
3. Design and implement model-specific behavioral defaults that `vllm-config` can propagate to compatible clients, with agent-specific overrides remaining possible.
4. Do not overwrite unrelated user edits in `.config/opencode/opencode.json`, `.config/crush/crush.json`, `.fusion/settings.json`, or `.pi/agent/models.json`; merge surgically.
5. Establish B0/B1/B2 evaluation support before selecting a new Qwen3.8 production reasoning default.
6. Prefer using/leveraging BenchKit for external quality baselines instead of reinventing standard suites; our own harness should focus on the local agent/SoloSIG workload.
7. Keep infrastructure metrics and behavior metrics separate in artifacts so future Escha/SGLang comparisons can run both layers under the same behavioral profile.
8. If implementation requires choosing a client-specific representation for `reasoning_effort`, first verify the client actually forwards it to the OpenAI-compatible API/template. Do not assume a field present for GPT/OpenAI automatically works for local vLLM/Qwen.

End of 2026-08-21 follow-up.

End of WIP.

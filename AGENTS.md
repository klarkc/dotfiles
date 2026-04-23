# Agents Guide: Vast.ai Nix Runner

This repo provides a reproducible Nix-based launcher for creating interruptible Vast.ai GPU instances running Qwen via vLLM.

## What this agent does

- Installs `vastai` CLI (pinned)
- Searches GPU offers (L40S, 5090, A100, 4090)
- Filters by price, reliability, disk, ports
- Selects best candidate
- Launches instance with vLLM OpenAI server

## Requirements

- Nix with flakes enabled
- Vast.ai account + API key
- Optional: HuggingFace token (for gated models)

## Setup

```bash
export VAST_API_KEY=your_key_here
# optional
export HF_TOKEN=your_hf_token
```

## Run

```bash
nix run github:klarkc/vast.ai
```

With custom parameters:

```bash
nix run github:klarkc/vast.ai -- \
  --max-price 0.4 \
  --bid-price 0.35 \
  --model Qwen/Qwen3.6-27B-FP8
```

## Test (dry behavior)

To test safely:

1. Run the command
2. When prompted `Create this instance?`, type `n`
3. Verify:
   - GPU selection
   - pricing filter
   - candidate ranking

This validates logic without spending money.

## Real run checklist

- API key is set
- Price limits are sane
- Disk >= 40GB
- Reliability >= 0.98

Then confirm with `Y`.

## After launch

```bash
vastai show instances -v
vastai ssh-url <instance_id>
```

## API endpoint

Once running:

```
http://<host>:8000/v1
```

Compatible with OpenAI API.

## Notes for agents

- Always prefer interruptible instances (cost optimized)
- Keep bid slightly below max price
- Favor reliability over cheapest GPU
- Use volume for caching models
- Abort if no candidates under price

## Failure modes

- No offers found → increase max price
- Auth failure → check API key
- Model fails → check HF token

## Extending

You can:

- Change model
- Change GPU preference order
- Adjust vLLM flags in `onstart`

---

This repo is designed for automated agents to safely acquire GPU compute with reproducible infra.

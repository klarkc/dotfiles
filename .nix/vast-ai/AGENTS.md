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

## Test and verification

Run the automated non-destructive quality gate before production use:

```bash
nix flake check
```

This includes `checks.<system>.production-scenarios`, a fake-CLI scenario suite that validates check, replace, rebid, watch, volume, readiness, and failure-mode behavior without real credentials, network calls, or Vast.ai instance changes.

For a real account dry check, use the default `check` command only:

```bash
nix run . -- check
```

Review the generated recommendation and guard arguments. Do not run `replace`, `rebid`, or `watch` unless you intend to modify real instances.

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

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

`fusion.service`, `vllm.service`, and `vllm@.service` use `PassEnvironment`. That means variables are passed only if they already exist in the systemd user manager environment.

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

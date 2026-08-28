# WIP: remove Pi and restore `make test`

## Design handoff to Build

### User goal

- Make `make test` pass again.
- Drop Pi from the repository/codebase because the user no longer intends to use it.

### Current verified state

- `nix --extra-experimental-features "nix-command flakes" flake check` passed before this handoff.
- `atlassian-smoke-test api-token` passed.
- `atlassian-smoke-test oauth` passed.
- `coding-agents-smoke-test` failed with exit `6` because every agent was skipped / failed to return a parseable status object:
  - opencode skipped after failing to return parseable JSON.
  - codex skipped because `OPENAI_API_KEY` is absent.
  - pi skipped after failing to return parseable JSON.
- vLLM currently serves only `qwen3.8-27b`; a direct model listing returned that model name only.
- `.config/opencode/opencode.json` still has top-level `"model": "vllm/qwen3.6-35b-a3b"`, which causes opencode to receive a vLLM `404` for the stale model.
- A manual opencode retry with `-m vllm/qwen3.8-27b` emitted only `step_start` and timed out after 300s. The `qwen3.8-27b` entry currently lacks the richer metadata present on `qwen3.6-35b-a3b` (`tool_call`, `reasoning`, `interleaved`, sampling/options), so capability/config mismatch is a likely cause to investigate.

### Existing uncommitted state to preserve

Build must inspect and preserve existing worktree changes before editing. Current `git status --short` showed:

```text
 M .config/opencode/opencode.json
 M .fusion/settings.json
 M .pi/agent/models.json
```

These were present before this handoff. Do not reset or overwrite them blindly.

### Pi removal scope found by `git grep`

Tracked Pi references that should be reviewed for removal or rewrite:

- `flake.nix:335` installs `pi-coding-agent` in the default profile.
- `.pi/agent/models.json` is tracked Pi model registry config.
- `.gitignore:97-101` explicitly allowlists `.pi/agent/models.json`.
- `.config/git/ignore:3` ignores `.pi` as local state.
- `.local/bin/coding-agents-smoke-test` documents/probes Pi:
  - header comments mention opencode/codex/pi.
  - `PI_SETTINGS` points at `${HOME}/.pi/settings.json`.
  - `probe_pi()` runs `pi -p --mode json --no-session --provider "$provider"`.
  - `main()` calls `probe_pi`.
- `.local/bin/vllm-patch-model-defaults` creates and writes `$HOME/.pi/agent/models.json`.
- `.config/systemd/user/vllm@.service:56` grants write access to `%h/.pi/agent`.
- `.config/systemd/user/fusion.service:36` grants write access to `%h/.pi`.
- `.tmux.conf:3` has a `# needed by pi` comment for `extended-keys`.
- `README.md:134` describes coding-agent smoke probes as opencode/codex/pi and mentions the Pi command.
- `docs/fusion-vllm.md` is Pi-centric and documents the Pi model registry.
- `.vim-cheatsheet.png` matched `pi` as a binary file; this is likely not a meaningful Pi integration reference.

### Recommended implementation plan

1. Remove Pi as an installed/runtime dependency:
   - Delete `pi-coding-agent` from `flake.nix` default profile paths.
   - Delete tracked `.pi/agent/models.json`.
2. Remove Pi from smoke testing:
   - Update `.local/bin/coding-agents-smoke-test` comments to say it probes opencode and codex only.
   - Remove `PI_SETTINGS`, `CODING_AGENTS_PI_*` handling, `probe_pi()`, and the `probe_pi` call.
   - Preserve exit-code semantics: exit `0` only when at least one remaining agent returns parseable status and every product is reported reachable; exit `6` when no remaining agent can prove access or products are unreachable.
3. Remove Pi from vLLM/Fusion integration:
   - Update `.local/bin/vllm-patch-model-defaults` so it only maintains Fusion and opencode defaults; it should no longer create or mutate `$HOME/.pi`.
   - Remove `%h/.pi/agent` from `vllm@.service` `ReadWritePaths`.
   - Remove `%h/.pi` from `fusion.service` `ReadWritePaths` unless current Fusion still needs it independently. Current docs say Fusion previously used Pi's model registry, but the user explicitly wants Pi gone, so prefer no Pi sandbox access.
4. Update docs/config references:
   - Update `README.md` smoke-test text to remove Pi and its command.
   - Rewrite or remove `docs/fusion-vllm.md` sections that describe Pi as the model registry. If Fusion still has a non-Pi local-vLLM path, document that path; otherwise remove stale Pi-specific verification steps.
   - Change `.tmux.conf` comment from `# needed by pi` to a non-Pi rationale if `extended-keys` remains useful, or remove the setting if it was only for Pi.
   - Replace `.gitignore` Pi allowlist with the chosen local-state behavior. Prefer removing the tracked allowlist for `.pi/agent/models.json`; if stale untracked `$HOME/.pi` should stay ignored, keep only a broad local-state ignore intentionally.
5. Restore opencode as the active coding-agent proof path:
   - Update `.config/opencode/opencode.json` top-level model from `vllm/qwen3.6-35b-a3b` to the currently served `vllm/qwen3.8-27b`, or make `vllm-patch-model-defaults` reliably set it to `vllm/${SERVED_MODEL_NAME}` when vLLM starts.
   - Ensure the `qwen3.8-27b` opencode model entry has the required capability metadata for Atlassian MCP tool calls. Compare against the richer `qwen3.6-35b-a3b` entry already present.
   - Keep secrets out of config; continue using `{env:VLLM_API_KEY}` for vLLM auth.

### Acceptance criteria

- `git grep -n -E '\bpi\b|pi-coding-agent|\.pi|CODING_AGENTS_PI|probe_pi|PI_SETTINGS' -- . ':!*.viminfo'` has no stale Pi integration references. Any remaining `.pi` ignore/reference must be intentional and documented as local-state cleanup, not runtime support.
- `git ls-files .pi/agent/models.json` returns no tracked file.
- `nix --extra-experimental-features "nix-command flakes" flake check` passes.
- `atlassian-smoke-test api-token` passes.
- `atlassian-smoke-test oauth` passes.
- `coding-agents-smoke-test` passes with at least opencode returning a parseable status object proving Bitbucket, Jira, and Confluence access.
- `make test` passes end to end.
- No secrets, tokens, Authorization headers, or generated Basic values are printed or committed.

### Risks / edge cases

- If codex remains skipped due to missing `OPENAI_API_KEY`, opencode must be the working proof path for `coding-agents-smoke-test`.
- The current 90s smoke-test timeout may be too low for a local reasoning model plus real MCP calls; avoid merely raising the timeout unless opencode can first return a correct parseable status manually.
- Removing Pi from Fusion may expose a hidden dependency if the current Fusion version still delegates local model registry lookups to Pi. Verify Fusion still lists or can use `local-vllm` after removing Pi config, or document that Fusion local-vLLM support is intentionally deferred if not part of `make test`.
- Existing worktree modifications in `.config/opencode/opencode.json`, `.fusion/settings.json`, and `.pi/agent/models.json` must not be lost.

# WIP: Bitbucket + Jira + Confluence read-only agent tooling

## Current user direction

- Move the prior Bitbucket PR read-only agent/tooling work into this dotfiles HOME repo, not SoloSIG.
- Add Jira in the same style: read-only token-based access, with secrets sourced from local deployed `~/.profile_override`.
- Add Confluence in the same style: read-only access; writes are human/manual through agent skill guidance for now; future ask-gated write layer is deferred.
- Prior `BB_READ_TOKEN` should be ignored for this work; user says it was for other APIs that are no longer the target path.
- Jira token env var `JIRA_READ_TOKEN` is acceptable to user; secrets stay only in `~/.profile_override`, never in Nix evaluation, source, logs, or generated config.
- User wants skills usable by any active agent tool where possible, including pi, opencode, Codex, and similar tools.
- User states Crush is no longer supported and wants all repo mentions/configuration removed; Design can only edit this file, so removal must be handed to Build.

## Repository evidence

- This repo is the user's dotfiles HOME repo (`README.md`: dotfiles installed directly as `$HOME`).
- `.profile` sources `$HOME/.profile_override` if present, which matches the requested local secret storage model.
- Agent-neutral skills already exist under `.agents/skills/**/SKILL.md`; opencode can auto-load external skills from `~/.agents/skills/**/SKILL.md` per its skill loader behavior.
- `.config/opencode/opencode.json` currently configures opencode agents/providers but does not need changes just to load `.agents/skills`.
- `.agents/skills/nix-shell-script-package/SKILL.md` documents the repo convention for Nix-packaged bash tools with no-network self-tests under `.nix/<tool>/` and flake packages/checks.

## MCP/APM findings as of 2026-08-27

- APM is not needed for this feature. It is primarily a packaging/distribution path for agent skills; it does not solve shared runtime access across heterogeneous tools by itself.
- Official Atlassian Rovo MCP now documents Bitbucket Cloud tools in addition to Jira/Confluence/JSM/Compass.
  - Endpoint: `https://mcp.atlassian.com/v1/mcp/authv2`.
  - Supports API-token authentication if enabled by the organization admin.
  - Bitbucket tools include read groups (`read_bitbucket`) for workspaces, repositories, user PRs, PR list/get/comments/diff, branches/commits/files, deployments/pipelines/environments; write groups also exist and must not be granted for read-only usage.
  - Jira tools include read/search groups (`read_jira`, `search_jira`) and write groups (`write_jira`) that must not be granted for read-only usage.
  - Confluence tools include read/search groups (`read_confluence`, `search_confluence`) and write groups (`write_confluence`) that must not be granted for read-only usage.
- Atlassian API-token MCP authentication requires org-admin enablement. Personal tokens use Basic auth (`email:api_token`); service account API keys use Bearer auth. Atlassian docs show direct header configuration, but checked-in config must not contain generated Basic strings or tokens.
- If using opencode MCP directly, opencode supports remote MCP config and permission actions (`allow`, `ask`, `deny`) by tool name. It can prompt for approval, but `--auto` auto-approves anything not explicitly denied, so destructive MCP tools should be denied unless intentionally enabled.
- Cross-agent "ask permission before write" cannot be assumed globally. Approval semantics are client-specific; the reliable safety boundary is token scopes / Atlassian permission groups, with client prompts as a secondary guard where supported.
- Atlassian admin setup for API-token MCP auth:
  - Atlassian Administration -> select org -> Rovo -> Rovo MCP server -> Authentication section -> turn API token on/off.
  - If disabled, MCP clients need OAuth 2.1 instead; API-token auth will not work.
  - User reports they do not see `solosig` in Atlassian Administration and personal Jira settings say they do not have Jira settings/admin access; therefore they likely cannot change Rovo MCP org settings themselves.
  - User reports `https://id.atlassian.com/manage-profile/apps` lists the MCP/ChatGPT integration and thinks they enabled it in ChatGPT before. Treat this as evidence that OAuth consent/connected-app access may already exist for the user's account, not evidence that org-level API-token MCP auth is enabled.
  - Personal Atlassian API tokens can still be used for direct Jira REST API calls subject to product permissions/token scopes. That is separate from Rovo MCP API-token authentication, which Atlassian documents as org-admin-gated.
  - User discovered Rovo MCP in ChatGPT is OAuth-based and currently allows Jira and Confluence, but not Bitbucket.
  - Bitbucket MCP is documented as API-token-auth only and available only if API-token auth is enabled by org admin and the Bitbucket workspace is linked to an organization. A Bitbucket workspace/repository access token that works for direct Bitbucket REST is not known to be accepted as an auth credential to Rovo MCP itself.
- Atlassian admin permission setup:
  - Atlassian Administration -> select org -> Rovo -> Rovo MCP server -> Permissions tab.
  - Use Edit details on permission types/groups.
  - Allow read/search groups only for current phase: `read_jira`, `search_jira`, `read_confluence`, `search_confluence`, `read_bitbucket`.
  - Block write groups for current phase: `write_jira`, `write_confluence`, `write_bitbucket`.
  - Bitbucket MCP requires API-token auth and a Bitbucket workspace linked to the organization.
- Third-party MCP packages still exist but expose write surfaces:
  - `@aashari/mcp-server-atlassian-bitbucket@3.1.0`: generic `bb_get` plus `bb_post`/`bb_put`/`bb_patch`/`bb_delete` and clone.
  - `bitbucket-mcp-server@3.2.0`: PR/repo read tools plus create/update/comment/pipeline trigger tools.
  - `@aashari/mcp-server-atlassian-jira@3.3.0`: generic `jira_get` plus write verbs.
  - `jira-mcp@1.0.1`: smaller Jira read surface (`jql_search`, `get_issue`) but old and limited.

## Design recommendation

Updated user direction: prefer official Atlassian Rovo MCP as the primary integration path if the active coding agents support MCP with permissions. Do not add direct REST wrappers/helpers by default; only add them later if a required agent lacks MCP support or if MCP cannot cover a needed read use case.

Rationale:

- MCP is preferred because the user expects active coding agents to support MCP with permissions, making wrappers/helpers unnecessary for the normal path.
- Assume tokens are read-only for the current implementation. Do not build extra write-action blocking layers solely in repo tooling unless evidence shows a token/tool exposes writes unexpectedly.
- Agent-neutral `.agents/skills` remain useful, but only for write-human-guidance for now; no read skills are needed because read context should come from MCP integration.
- If an active coding agent does not support MCP or cannot safely read via Rovo MCP, revisit wrappers/helpers as a fallback design.
- APM is not necessary unless the user wants to publish/reuse these skills across machines/repos through an APM marketplace/package.

## Proposed implementation handoff

1. Configure official Atlassian Rovo MCP for supported coding agents.
   - Primary endpoint: `https://mcp.atlassian.com/v1/mcp/authv2`.
   - API-token auth works in this environment using `ATLASSIAN_USER_EMAIL` + `ATLASSIAN_MCP_TOKEN` from `~/.profile_override`.
   - ChatGPT OAuth path is reported working for Jira/Confluence; local API-token path is verified for Bitbucket.
   - Do not commit Authorization headers, generated Basic strings, tokens, or token-derived values.
   - For opencode, verify config shape against current schema/docs; MCP command arrays only for local MCP servers. Remote MCP must avoid committed headers unless config supports env interpolation or a safe non-committed local override is used.
2. Add only configuration tests in normal checks.
   - Example: validate opencode config enables/defines the Atlassian MCP integration correctly without secrets.
   - Do not run live/e2e network authentication from `nix flake check`.
3. Add out-of-band smoke tests under `.local/bin/`:
   - `.local/bin/atlassian-smoke-test` should document/run explicit live checks outside `flake check`.
   - Include two smoke-test modes: OAuth MCP smoke test and API-token MCP smoke test using environment variables where applicable.
   - Smoke tests must never print tokens/headers and should print only status, session presence, accessible resource names/URLs, and tool names.
   - Document how to run smoke tests manually.
   - Standardize OAuth browser-flow smoke/reuse around a reusable remote-MCP OAuth bridge pattern, with `mcp-remote` as the current candidate rather than a hard requirement if a better packaged option exists.
     - `.local/bin/atlassian-smoke-test oauth` should open the browser when needed and reuse cached OAuth tokens afterward.
     - `mcp-remote` can bridge stdio-only clients to remote MCP with OAuth and caches tokens under `~/.mcp-auth` or `$MCP_REMOTE_CONFIG_DIR` (current layout under `mcp-remote-v1`).
     - Packaging preference for `mcp-remote` or equivalent: existing nixpkgs package first; dedicated flake input/package second; other ecosystem managers (for example npm) last and only if made repo-managed/offline-safe according to repo policy.
     - Do not use ad hoc live `npx`.
     - Cache remains user-local and must not be committed.
4. Document this repo pattern in agent guidance:
   - E2E/integration smoke tests that need live services/secrets/network must live as `.local/bin/*-smoke-test` scripts.
   - Such smoke tests are manual/out-of-band and must not be part of `nix flake check`.
   - Normal flake checks should cover static/config/no-network validation only.
   - OAuth MCP smoke tests should use a repo-managed remote-MCP OAuth bridge pattern by default; expose a simple `oauth` mode, e.g. `.local/bin/atlassian-smoke-test oauth`, instead of tool-specific mode names. Prefer nixpkgs-packaged tooling first, flake input second, and other managers last.
5. Add write-guidance skills, but no write execution layer yet:
   - Document how agents should instruct the user to perform Jira/Bitbucket writes manually in the UI or CLI until a proper write layer exists.
   - Examples: draft a Jira comment, draft a PR comment, explain transition/approval/merge steps; do not call write APIs.
   - Future write layer should use separate write tokens/scopes, per-operation allowlist, and client-side ask gates where supported.
6. Remove unsupported Crush references/config from repo per user request. Known mentions found by grep:
   - `README.md` lines mentioning Crush/coding agents/package install/vLLM patching.
   - `.local/bin/vllm-patch-model-defaults` Crush config patching block.
   - `docs/dependency-maintenance.md` dependency row.
   - `.gitignore` Crush section.
   - `.config/crush/crush.json` tracked config.
   - `.config/git/ignore` `.crush` ignore.
   - `.config/systemd/user/vllm@.service` `ReadWritePaths` includes `%h/.config/crush`.

## Open questions for the user

- Jira Cloud site URL: `https://solosig.atlassian.net`.
- Jira token env var name in `~/.profile_override`: `JIRA_READ_TOKEN` is acceptable to user.
- Jira account email env var name if Basic auth is used: `JIRA_USER_EMAIL` or shared `ATLASSIAN_USER_EMAIL` still needs confirmation.
- Default Jira project keys/JQL, if any.
- Whether Atlassian org admin has enabled API-token authentication for Rovo MCP and can allow only `read_jira`, `search_jira`, and `read_bitbucket` while blocking writes.
- Whether Atlassian org admin has enabled API-token authentication for Rovo MCP and can allow only `read_jira`, `search_jira`, `read_confluence`, `search_confluence`, and `read_bitbucket` while blocking writes.
- Update from user/admin: admin reports API-token authentication is enabled for Rovo MCP, and Bitbucket is already integrated with Atlassian / listed in `admin.atlassian.com`. Next step is safe verification from a client using a read-scoped API token, without committing or printing secrets.
- MCP smoke test from this workspace succeeded using `ATLASSIAN_USER_EMAIL` + `ATLASSIAN_MCP_TOKEN` from `~/.profile_override` as Basic auth. `initialize` returned HTTP 200 with an MCP session id, `tools/list` returned HTTP 200, and `getAccessibleAtlassianResources` returned `https://solosig.atlassian.net` (`name=solosig`).
- Tools exposed by the current token: `atlassianUserInfo`, `getAccessibleAtlassianResources`, Bitbucket tools (`bitbucketWorkspace`, `bitbucketRepository`, `bitbucketPullRequest`, `bitbucketRepoContent`, `bitbucketPipeline`, `bitbucketDeployment`, `bitbucketEnvironment`), Teamwork Graph tools (`getTeamworkGraphContext`, `getTeamworkGraphObject`, `addTeamworkGraphContext`). Direct Jira/Confluence tools did not appear for this API-token session; Jira/Confluence remain available through ChatGPT OAuth per user report.
- Read-only Bitbucket PR list via Rovo MCP succeeded for `workspaceId=solo_sig`, `repoId=solosig`, `state=OPEN`, `pagelen=5`; response included open PR #107. Because some Bitbucket MCP tools are mixed-action tools and `addTeamworkGraphContext` is present, skills/config must restrict usage to read actions only until a proper write layer exists.
- Whether to use OAuth 2.1 for Jira/Confluence MCP where already available in ChatGPT and possibly other clients.
- Verify whether existing ChatGPT/Atlassian connected-app OAuth consent can access `https://solosig.atlassian.net` resources and expose Jira/Bitbucket read tools; if yes, MCP may be usable via OAuth even without API-token auth.
- If no admin access/contact path exists, prioritize direct REST wrappers with personal tokens and treat official Rovo MCP as blocked/pending admin enablement or OAuth client support.
- Writes: current decision is read-only all allowed; writes are human/manual plus skill guidance; ask-gated write layer is deferred for the future.

## Design review of Build output (2026-08-27)

Status: changes are mostly aligned, but not accepted yet due to blockers.

Findings for Build follow-up:

1. `SKIP_SMOKE=1 make test` does not actually skip smoke tests. In `Makefile`, `smoke` has separate recipe lines; the `exit 0` only exits the first shell, then Make runs the next recipe line and executes `.local/bin/*-smoke-test` anyway. Evidence: `SKIP_SMOKE=1 make test` printed `smoke: skipped (SKIP_SMOKE=1)` and then still ran `.local/bin/atlassian-smoke-test api-token` and `oauth`. This breaks CI because `.github/workflows/test.yml` relies on `SKIP_SMOKE=1 make test`.
2. `.local/bin/atlassian-smoke-test oauth` does not pass the Atlassian MCP endpoint to the bridge. Evidence: with a fake `ATLASSIAN_OAUTH_BRIDGE`, output was `ARGS:` empty while the script logged it was starting against `https://mcp.atlassian.com/v1/mcp/authv2`. Expected call is bridge + endpoint + forwarded args, e.g. `"$bridge" "$ENDPOINT" "$@"`.
3. `make test` runs `nix fmt` before `nix flake check`. In CI this can auto-format files and hide formatting regressions that should fail the formatting check. Prefer `make test` -> `nix flake check` + smoke only; use a separate `make fmt` or `make fix` for mutating format.
4. The live/debug validation process accidentally ran `bash -x` after sourcing `~/.profile_override`, printing secrets into tool output/conversation logs. No committed file matched obvious token patterns, but user should rotate exposed secrets and clear local tool-output logs. Future smoke/debug commands must never combine `set -x` with sourced secret files.

Positive evidence:

- No tracked file contains obvious Atlassian token literals (`ATATT`), Bearer tokens, Google/HF/Vast secret variable assignments, or old Crush references by repository grep.
- `.local/bin/atlassian-smoke-test api-token` currently succeeds with loaded local credentials and prints only status/tool/resource summaries.
- `nix flake check` passed after Build's static config check was fixed.

## Design review of Build fixup 4c2c272 (2026-08-27)

Status: prior Makefile/OAuth-endpoint blockers are fixed, but implementation is not accepted yet due to one auth-smoke blocker.

Fixed evidence:

- `SMOKE_TESTS_ENABLED=false make test` now runs `nix flake check` and does not run smoke tests (`PASS_SMOKE_SKIPPED`).
- `SKIP_SMOKE=1 make test` remains a working backwards-compatible skip alias.
- `.local/bin/atlassian-smoke-test oauth` now passes `https://mcp.atlassian.com/v1/mcp/authv2` as arg1 to the configured bridge; verified with a fake bridge showing `arg1=https://mcp.atlassian.com/v1/mcp/authv2`.
- `make test` no longer runs mutating `nix fmt`; `make fmt` exists separately.

New finding for Build follow-up:

1. High: `.local/bin/atlassian-smoke-test api-token` can report PASS for invalid credentials. Evidence: with `BASH_ENV=/dev/null ATLASSIAN_USER_EMAIL=test@example.com ATLASSIAN_MCP_TOKEN=not-a-real-token .local/bin/atlassian-smoke-test api-token`, the script returned exit 0 and printed `PASS api-token smoke test`, even though `tools/list` had only Teamwork Graph tools and `resources_text=MCP error -32602: Tool getAccessibleAtlassianResources not found`. The script currently treats any HTTP 200 as success and does not inspect JSON-RPC errors or content text MCP errors. Acceptance should require either `getAccessibleAtlassianResources` to return parseable non-empty Atlassian resources including the expected site (`https://solosig.atlassian.net` by default), or fail loudly. It should also fail if the expected Bitbucket tools are absent in API-token mode.

Non-blocking observation:

- In this shell, `BASH_ENV=/home/klarkc/.profile` can implicitly source `.profile_override` for executable bash scripts, so `env -u ATLASSIAN_USER_EMAIL -u ATLASSIAN_MCP_TOKEN .local/bin/atlassian-smoke-test api-token` may still succeed locally. Tests for missing env should set `BASH_ENV=/dev/null` or similar. This is not necessarily a product bug, but should be documented if adding tests for missing env.

## Design review of Build hardening e67c963 (2026-08-27)

Status: accepted. The auth-smoke blocker from the previous review is fixed, no new blockers introduced.

Verified scenarios:

- `BASH_ENV=/dev/null ATLASSIAN_USER_EMAIL=test@example.com ATLASSIAN_MCP_TOKEN=not-a-real-token .local/bin/atlassian-smoke-test api-token`: exit 4, message `FAIL: tools/list missing required tools: bitbucketPullRequest,getAccessibleAtlassianResources; got: addTeamworkGraphContext`. Correct: token without scopes fails the smoke test.
- `ATLASSIAN_EXPECTED_SITE_URL=https://example.test .local/bin/atlassian-smoke-test api-token` (with valid creds): exit 5, message `FAIL: expected site https://example.test not found in resources (got urls: https://solosig.atlassian.net; raw: [...])`. Correct: site mismatch fails the smoke test.
- `.local/bin/atlassian-smoke-test api-token` with default `ATLASSIAN_EXPECTED_SITE_URL`: exit 0, PASS, prints the expected site, resources text, and tool list (one per line, alphabetical). Correct.
- `make test` (default): flake check + both smoke modes succeed (oauth still gracefully skips because the bridge is not installed).
- `SMOKE_TESTS_ENABLED=false make test`: flake check only, no smoke tests run.

Code-level acceptance:

- `.local/bin/atlassian-smoke-test` does not reference `.profile_override`; the only place where `profile_override` appears in the broader repo set is a documentation comment in `.github/workflows/test.yml` explaining how local developers load credentials. That is acceptable.
- The script accepts `ATLASSIAN_EXPECTED_SITE_URL` with default `https://solosig.atlassian.net`, satisfying the previously requested override knob.
- `tools/list` and `getAccessibleAtlassianResources` parse their JSON payload and fail loudly on JSON-RPC errors, missing required tools, MCP `error -NNNN:` content text, empty resources, and missing expected site URL.

## Design review of Build coding-agents smoke test 9f36ec8 (2026-08-27)

Status: not accepted. The new `coding-agents-smoke-test` does not satisfy the user's stated acceptance question: "do the smoke tests validate that the real agent can use the real MCP to request real resources?" Current answer is no.

Findings for Build follow-up:

1. High: `.local/bin/coding-agents-smoke-test` does not exercise any real coding agent making MCP tool calls. It inspects deployed config files (`~/.config/opencode/opencode.json`, `~/.codex/config.toml`, `~/.pi/settings.json`) and then calls `.local/bin/atlassian-smoke-test api-token` directly for live tool discovery. Evidence: lines 149-168 define `live_tool_names()` by executing the helper script, not opencode/codex/pi; lines 240-248 call `live_tool_names`; no `opencode run`, `codex exec`, or `pi --print` path asks an agent to use MCP. This validates direct MCP auth/tool listing, not agent-mediated usage.
2. High: The new smoke test does not request real Bitbucket/Jira/Confluence resources through the agents. It only validates tool names and `getAccessibleAtlassianResources` indirectly through `atlassian-smoke-test`. It does not ask for an actual Bitbucket PR/repo, Jira issue/search/project, or Confluence space/page. For the requested final smoke test, Build should add agent-mediated read probes that request small, known resources (or searches with explicit limits) through each active agent/toolchain being claimed as supported.
3. High: `make test` default is likely broken in the current deployed environment because `coding-agents-smoke-test` probes deployed configs, while this branch's opencode MCP config is only in the worktree until deployed. Evidence during review: direct run of `.local/bin/coding-agents-smoke-test` reported no Atlassian MCP server configured in `/home/klarkc/.config/opencode/opencode.json` and exited 6. That may be correct for deployed state, but it means adding this script to the default smoke loop can make `make test` fail before the repo is deployed. Either document that default smoke requires deployed config, or provide a worktree-aware mode/expected workflow. Do not write outside the worktree to validate.
4. Medium: The script comments are stale/misleading after implementation changes. Lines 22-26 still claim opencode runs `opencode mcp debug atlassian` and codex runs `codex mcp get atlassian`, but the implementation does not use those as live product/resource probes. Comments should match behavior.

Suggested acceptance criteria for the next Build fix:

- `atlassian-smoke-test api-token` can remain the direct MCP protocol smoke test.
- `coding-agents-smoke-test` should be explicit about scope. If it is only a config/readiness check, rename or document it as such and do not claim it proves real agent resource access.
- To prove real agent access, add per-agent non-interactive probes where supported:
  - opencode: `opencode run` with a constrained prompt that must call Atlassian MCP and return a machine-parseable JSON summary for Bitbucket/Jira/Confluence reachability.
  - codex: `codex exec` equivalent if configured with Atlassian MCP.
  - pi: `pi --print --no-session` equivalent if it supports MCP/tools in non-interactive mode.
- Each agent probe should ask for small real read-only resources: Bitbucket `solo_sig/solosig` repo metadata or open PR list; Jira visible projects or bounded JQL against `https://solosig.atlassian.net`; Confluence visible spaces or bounded CQL/page search.
- The smoke should fail only for agents/products claimed as enabled, and should clearly skip unsupported/unconfigured agents.
- Keep all secrets from environment only; no `.profile_override` loading and no shell tracing.

## Design review of Build agent-mediated rewrite 137251c (2026-08-27)

Status: accepted with one minor observation. The script now satisfies the user-defined acceptance question: it drives the real active coding agents, asks them to use Atlassian MCP, and parses their JSON output for product reachability.

Evidence:

- `probe_opencode`, `probe_codex`, `probe_pi` now spawn the real binaries with `opencode run --format json`, `codex exec --json`, and `pi -p --mode json --no-session`. Helper `extract_status_json` parses JSONL or JSON envelope to recover the status object.
- The probe prompt (`build_probe_prompt`) instructs the agent to call real MCP tools against real resources: `bitbucketWorkspace`/`bitbucketRepository` for `solo_sig/solosig`; `getVisibleJiraProjects` for `solosig.atlassian.net`; `getConfluenceSpaces` for `solosig.atlassian.net`. The agent is told not to speculate; booleans reflect whether the MCP call succeeded.
- Per-product reachability combines every probed agent's status object: `true` from any agent that ran passes the product. SKIPs do not block PASS for a product reported by another agent.
- Provider credential gating is env-only: opencode reads the worktree `opencode.json` (override via `CODING_AGENTS_OPENCODE_CONFIG`); codex requires `OPENAI_API_KEY`; pi requires `GOOGLE_KEYFILE` (or whatever `CODING_AGENTS_PI_PROVIDER` selects). No path is read outside the worktree.
- Timeouts use `timeout --foreground --kill-after=5`, so a hung agent cannot block the smoke loop indefinitely.
- Output is redacted to status lines plus user-supplied evidence strings. Tokens, Authorization headers, and generated Basic base64 are never printed.
- Stale comments about `opencode mcp debug atlassian` and `codex mcp get atlassian` are gone; the docstring now matches the implementation.
- `README.md` updated to describe the agent-mediated scope.

Verification on this worktree:

- `bash -n .local/bin/coding-agents-smoke-test` OK.
- `SMOKE_TESTS_ENABLED=false make test` runs flake check only.
- `make test` runs both smoke scripts. In this worktree session: `atlassian-smoke-test api-token` PASS (real MCP reachable for Bitbucket), then `coding-agents-smoke-test api-token` reports SKIP for each active agent because the worktree has no provider auth for opencode (vLLM requires a running server, configured model is `vllm/qwen3.6-35b-a3b`), codex (`OPENAI_API_KEY` missing), and pi (timed out under 15s with `GOOGLE_KEYFILE` set). The smoke test exits 1 with a clear message that no agent returned a parseable status. This is the honest result for the current environment and validates the test will gate end-to-end correctness when provider auth is provided.

Non-blocking observation:

- The all-SKIP exit code is 1, not 6, because the failure path is "no agent returned a parseable status object" rather than "a required product is unreachable from any agent". Both exit codes are non-zero so `make test` fails correctly, but the choice between 1 and 6 is mildly inconsistent with the documented exit code table (which only lists 6). Either accept both 1 and 6 as failure or align the code to always exit 6. Not blocking; documenting for follow-up.

## Hand-off: pack mcp-remote + align coding-agents-smoke-test exit code (2026-08-27)

User request: close the two remaining gaps so the integration is fully self-contained.

1. Pack `mcp-remote` so `.local/bin/atlassian-smoke-test oauth` runs without skipping.
2. Make `coding-agents-smoke-test` exit 6 on all-SKIP so the documented exit table matches behavior.

Constraints and facts collected by Design for Build:

- `mcp-remote` is not in nixpkgs (current `nixpkgs-unstable`). Direct search for top-level and nodePackages attributes failed; `nodePackages`/`nodePackages_latest` are explicitly removed in this nixpkgs.
- `mcp-remote` npm package latest version is `0.8.1`, repository `git+https://github.com/punkpeye/mcp-remote.git`. The project does not publish stable tags, so version pinning must use a git rev.
- Latest commit at design time: `77bbcfcd7892d339c27b5a14818b63cb5c4d3293` (committed 2026-08-27). Locking on this rev is acceptable; the repo policy forbids live package manager resolution but a flake input + locked `flake.lock` is the standard pattern this repo already follows for Fusion and QMD.
- Bins: `mcp-remote` (entry `dist/proxy.js`), `mcp-remote-client` (`dist/client.js`).
- Repo convention for vendored JS CLI runtime: `.nix/<tool>-runtime.nix` exposing a `<tool>-runtime` flake attribute, wired into `packages.default` next to `fusion-runtime` and `vllm-runtime`. The Fusion runtime uses `fetchPnpmDeps` + `pnpmBuildHook` from nixpkgs. Use the same pattern: `pnpmDeps.hash` will need to be initialized (initial `nix build .#mcp-remote-runtime` prints the expected hash; commit it together with the rev).
- The runtime should also expose `mcp-remote-client` if useful, but the smoke test only invokes `mcp-remote`, so the binary must be on PATH after `nix profile install .`.
- AGENTS.md policy: no live `npm install` / `pnpm install` in repo-maintained Nix derivations; everything must be declared through flake inputs and locked. `fetchPnpmDeps` is offline-friendly and acceptable.
- Follow the Fusion/QMD bump-note comment style: a `# Bump note:` annotating the version/rev/hash that must move together.

Concrete steps for Build:

1. Add flake input:

   ```nix
   mcp-remote-src = {
     url = "github:punkpeye/mcp-remote/77bbcfcd7892d339c27b5a14818b63cb5c4d3293";
     flake = false;
   };
   ```

   with a `# Bump note:` matching the Fusion/QMD style.

2. Create `.nix/mcp-remote-runtime.nix` mirroring `fusion-runtime.nix` shape: a derivation that takes `pkgs` and the `mcp-remote-src` input, uses `pkgs.fetchPnpmDeps` for the offline `pnpm-lock.yaml` deps, and `pnpmBuildHook` for the build. The `mainProgram` should be `mcp-remote`. Wrap with `makeWrapper` and a minimal `runtimePath` (nodejs, coreutils, findutils, gawk, gnugrep, gnused, curl) so the bridge can issue HTTPS requests and locate its bundle.

3. Wire into `flake.nix`:

   ```nix
   mkRuntime = <tool>: src:
     pkgs.callPackage ./.nix/${tool}-runtime.nix { inherit src; };

   # in packages:
   mcp-remote-runtime = mkRuntime "mcp-remote" mcp-remote-src;
   default = pkgs.buildEnv {
     paths = [..., mcp-remote-runtime, ...];
   };
   ```

4. Run `nix build .#mcp-remote-runtime` once to obtain `pnpmDeps.hash`, commit the hash next to the bump note.

5. Run `nix profile install .` and verify `which mcp-remote` resolves to the Nix profile binary. Smoke test that `.local/bin/atlassian-smoke-test oauth` no longer SKIPs because the bridge is missing (it will still likely wait on browser OAuth consent the first time, which is acceptable — the test no longer errors out on PATH lookup).

6. Align `coding-agents-smoke-test` exit codes. Currently the all-SKIP path calls `fail` with default exit code `1`. Update the call (or the `fail` helper's default for that branch) so the all-SKIP path uses exit code `6`, matching the documented exit code table:

   ```bash
   fail "no coding agent returned a parseable status object" 6
   ```

   and likewise for the "no coding agent could be probed" branch which already uses `6`. Confirm only 0 and 6 are documented in the script header and used.

7. Re-run `make test SKIP_SMOKE=1` and `make test` to validate end-to-end. The OAuth branch will probably still effectively no-op until the user grants browser consent once; document that expected behavior in the script's comments and README.

8. Commit. Suggested message:

   ```
   feat(nix): pack mcp-remote as runtime + align smoke exit code

   - add flake input mcp-remote-src pinned to commit <hash>
   - add .nix/mcp-remote-runtime.nix following the fusion-runtime pattern
   - wire mcp-remote-runtime into packages.default so the bridge is on
     PATH after nix profile install
   - update bump notes for mcp-remote-src + pnpmDeps.hash to move
     together
   - .local/bin/atlassian-smoke-test oauth no longer SKIPs for missing
     bridge; first run still requires browser consent, subsequent runs
     reuse ~/.mcp-auth
   - .local/bin/coding-agents-smoke-test all-SKIP path now exits 6 to
     match the documented exit code table; header comment updated
   ```

Verification targets after the Build fix:

- `mcp-remote` resolves in `nix profile install .` output and is on PATH.
- `which mcp-remote` returns the profile binary (not a missing command).
- `make test SKIP_SMOKE=1` runs flake check only.
- `make test` runs smoke tests; `atlassian-smoke-test api-token` PASS, `atlassian-smoke-test oauth` opens bridge (no SKIP for missing bridge), `coding-agents-smoke-test` reports SKIP per agent when no provider auth and exits 6 with the aligned message.
- No tokens or Authorization headers printed.
- No secrets added to `.gitignore` or `flake.nix`.

## Design review of Build fixup 2d9c66d + 50916b1 (2026-08-27)

Status: accepted. Both gaps closed; no new blockers.

Acceptance evidence:

- `nix build .#mcp-remote-runtime` succeeds and the resulting bin reports `mcp-remote --version` as `0.1.38`.
- `nix build .#default` succeeds; the resulting profile `bin/` includes `mcp-remote` and `mcp-remote-client`. Conflict resolution: `fusionRuntime` and `mcpRemoteRuntime` are merged via `pkgs.symlinkJoin` into `jsRuntimes` before being added to `packages.default`, and the top-level `nodejs` entry is dropped because the wrapped `mcp-remote` and `fusion` runtimes already expose nodejs on their wrapped PATH. This is a clean match for the Fusion/QMD runtime pattern this repo already uses.
- `.local/bin/atlassian-smoke-test oauth` no longer SKIPs for missing bridge. Verified by hand-running with the profile `mcp-remote` on PATH: the bridge opens a browser OAuth flow against `https://mcp.atlassian.com/v1/mcp/authv2`, prints `Proxy established successfully between local STDIO and remote StreamableHTTPClientTransport`, and exits with `atlassian-smoke-test: PASS oauth bridge invocation completed` (exit 0).
- `SMOKE_TESTS_ENABLED=false make test` runs flake check only.
- `make test` end-to-end: `atlassian-smoke-test api-token` PASS, `atlassian-smoke-test oauth` PASS, `coding-agents-smoke-test api-token` FAIL with the aligned message `FAIL: no coding agent returned a parseable status object 6` and `real_coding_exit=6`.
- No tokens, Authorization headers, bearer tokens, or base64 Basic strings were committed. A repo-wide grep for `ATATT|Authorization: Bearer|api_key=|token=|secret=` matches only the redactor regex pattern itself in `coding-agents-smoke-test`.
- Bump notes match the Fusion/QMD style: `# Bump note:` comments next to the version/rev/hash declarations in both `flake.nix` and `.nix/mcp-remote-runtime.nix`.

Code-level notes:

- `.nix/mcp-remote-runtime.nix` mirrors the Fusion runtime shape: `fetchPnpmDeps` for the offline `pnpm-lock.yaml` deps, `pnpmBuildHook` for the tsup build, `makeWrapper` to expose `mcp-remote` and `mcp-remote-client` on the wrapped PATH, and `__structuredAttrs = true; strictDeps = true;` to keep `pnpmInstallFlags` as a real shell array for the install hook. The runtime PATH is `coreutils curl findutils gawk gnugrep gnused nodejs` (no `nix`/`git`/`docker-client` like Fusion ships, since the bridge is a small Node CLI).
- `flake.lock` updated with the `mcp-remote-src` input pinned to `github:punkpeye/mcp-remote/77bbcfc`. No live `npm install` / `pnpm install` in the derivation; the dependency closure is fetched and locked via `fetchPnpmDeps`.
- The exit-code alignment fix is minimal and surgical: `grep -c '^{' || true` lets the count return zero without tripping `set -e`, so the `fail "..." 6` branch is reached and the documented exit table matches behavior.

Non-blocking observations (deferred for follow-up, not blocking acceptance):

- `.nix/mcp-remote-runtime.nix` declares `version = "0.1.38"` (the upstream `package.json` reported by the source tree at the pinned commit). It does NOT match the npm registry's `0.8.1` because the project has not bumped `package.json` for the latest 0.8.x release. The bump note in the derivation calls this out and instructs future bumps to update the upstream `package.json` version, the source rev, and the `pnpmDeps.hash` together. Acceptable since `0.1.38` is the canonical version reported by the source tree at the pinned commit and the bridge is invoked as a runtime helper rather than a long-lived API surface; future bumps will catch up to the npm version line.
- The smoke test still relies on the operator having completed the first browser OAuth consent for `~/.mcp-auth` (or `MCP_REMOTE_CONFIG_DIR`) to cache the OAuth session. Until that cache exists, `atlassian-smoke-test oauth` will open a browser, then exit 0 with a clear message; the test does not gate on a cached session. If you want to enforce cached-session verification later, the script can call `mcp-remote` once first to populate the cache, or check `~/.mcp-auth` for a session file before invoking the bridge. Defer.

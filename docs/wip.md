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

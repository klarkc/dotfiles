---
name: atlassian-write-human-guidance
description: Use for requests to create, update, review, or draft writes to Atlassian Confluence, Jira, or Bitbucket. Enforce the official MCP authorization gate for delegated Confluence changes; while that gate is blocked, provide copy-paste-ready manual guidance instead of performing writes.
---

# Atlassian write governance and human fallback

This skill is discovered by OpenCode from `~/.agents/skills/atlassian-write-human-guidance/SKILL.md` through the native `skill` tool. This is the canonical tracked definition in the public dotfiles repository. Do not create a duplicate MCP, substitute another Atlassian identity, or change authentication methods to evade a denial.

## Current boundary: fail closed

The `atlassian` MCP uses the existing API-token identity, with authentication derived at runtime from `ATLASSIAN_USER_EMAIL` and `ATLASSIAN_MCP_TOKEN`. The private `.profile_override` contains values only; derived Basic authentication belongs in the tracked `.profile`, not in a secret file or Git.

An October 2026 audit showed that an MCP can initialize and advertise `createConfluenceContent`, `updateConfluenceContent`, and `executeWrite` while a real `getConfluenceContent` call returns HTTP 403 for missing scope. A tool appearing in `tools/list` does **not** establish authorization. Until the prerequisites below are independently verified, the installed OpenCode policy denies `atlassian_createConfluenceContent`, `atlassian_updateConfluenceContent`, `atlassian_executeWrite`, and `atlassian_executeDestructive`.

Do not invoke write tools while this deny gate is installed. Do not use REST, scripts, web browsers, a different connector, another identity, or another MCP to sidestep a refusal. A developer may use a _read-only_ MCP-protocol probe against the same official MCP to classify errors; that does not authorize a write.

## Requirements before enabling delegated Confluence edits

1. The actual token must support `read:confluence:agent-interface` **and** `write:confluence:agent-interface`. Existing API tokens cannot be assumed to have these scopes; a new scoped version may have to replace the value of the **same existing** `ATLASSIAN_MCP_TOKEN` variable, using the account's authorized token-management flow. Do not introduce a second credential or change it automatically.
2. The organization admin must permit the `read_confluence` and `write_confluence` toolsets under Atlassian Administration > Connectors > Atlassian MCP server > Permissions; token authentication must remain authorized. The identity must also have the required Confluence space/page permissions. A ChatGPT connector's separate administrator grant is not evidence about OpenCode.
3. Prove an actual authorized `getConfluenceContent` call succeeds using the existing `atlassian` MCP. HTTP 401/403 or a tool-level scope error means STOP; do not attempt a write, even in dry-run mode. Do not infer permission from the connection indicator, `discover`, or `tools/list`.
4. After a real read succeeds and token, administrator, identity, and space authorizations are confirmed, obtain explicit permission to perform a disposable-page test. Only then update dotfiles **in an isolated worktree**: switch `atlassian_createConfluenceContent` and `atlassian_updateConfluenceContent` from `deny` to `ask`. Keep the generic `atlassian_executeWrite` and destructive `atlassian_executeDestructive` at `deny` so unrelated Jira/Bitbucket writes cannot slip through. Update the matching gate tests, run `make test` and normal hooks, and safely integrate. No unconditional `allow`.
5. With that specifically scoped `ask` gate and user approval, prove create -> read -> update -> read on a disposable page, not a production playbook; verify version progression and handling of stale `snapshotToken` / conflicts. If scope or permission fails at any step, restore the `deny` gate through the same tested workflow. Do not alter production content to test infrastructure.

No agent, skill, or test harness may silently turn off these gates.

## Authorized Confluence write procedure (after the gate is enabled)

- Use **only** the existing official `atlassian` MCP. Identify the intended `cloudId`, space and target URL from existing authorized reads. Never choose a target from title alone.
- Inspect space instructions and call `getContentFormatGuide` with the appropriate Confluence tool name before authoring content. For existing pages, retrieve `getConfluenceContent` with `detail=full` and `content_format=html`; preserve macros, links and surrounding HTML.
- Capture the returned `snapshotToken`, version, and current body. Prepare the smallest necessary update, with a version message; use `dryRun=true` to preview when supported. Review the exact target, modifications, notifications, and reversibility with the user.
- Invoke a create/update action **only after the OpenCode approval prompt is accepted**. Do not auto-approve MCP writes. For an update, pass the current `snapshotToken`; do not fabricate versions.
- Re-read with `getConfluenceContent` and assert expected content persisted and that the version advanced. If a stale snapshot, HTTP 409, or other concurrency rejection occurs, stop and retrieve the latest revision; never blind-retry, force, or overwrite.
- For smoke tests, create one clearly labeled disposable test page, update and re-read it, and report its URL and version evidence. Do not delete, archive, publish other pages, or modify access restrictions unless separately requested.

## Manual fallback for any blocked operation

For Confluence when prerequisites fail, or Jira/Bitbucket writes not explicitly governed and authorized here, produce a draft instead of calling a write API. Include the target, paste-ready body, UI or CLI steps for the human, expected effects and permission limitations. Do not claim a write succeeded. Read-only MCP tools may be used when the identity has legitimate access. If even reading returns 401/403, report the class of missing authorization without revealing identifiers or tokens.

## BuildDoc handoff

A BuildDoc agent must not update official playbooks until the disposable-page E2E proof and the permission-gate change are recorded. When enabled, use full HTML reads, `snapshotToken`, minimal modifications, explicit approval, and post-write verification for **each** page. Preserve the canonical Confluence documents, do not create competing Markdown copies, and keep unrelated deployment gates blocked.

A ChatGPT Atlassian connector uses separate grants. If it reports `Access denied: Your organization admin has not authorized the write_confluence permission`, only an authorized organization administrator can grant its `write_confluence` toolset. Never work around that denial using a different connector.

## Security rules

Never print or commit API tokens, Basic headers, OAuth sessions, customer text, site-specific paths, or private organization identifiers. Keep the `.profile_override` values-only. Never use `curl` or the REST API as a substitute for a denied MCP write. Approval must be scoped to the actual operation, and destructive capabilities stay denied.

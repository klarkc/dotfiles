---
name: atlassian-write-human-guidance
description: Use ONLY when the user asks to draft, edit, or compose a write operation against Atlassian Jira, Confluence, or Bitbucket Cloud (comment, issue create/update/transition, page create/update, PR comment, approve/merge guidance). The skill produces copy-paste-ready text and explicit manual instructions for the user to perform the write themselves in the web UI or CLI; it does NOT call any write API.
---

# Atlassian write-human-guidance skill

This skill exists because write operations against Atlassian products are intentionally **not** automated for the user yet. The current integration only supports read operations through Atlassian Rovo MCP, with read-only personal API tokens. Writes must be performed by the human.

When this skill is active, the agent:

- Reads relevant context through Atlassian Rovo MCP (or direct read APIs where MCP is not available).
- Drafts the exact text or action the user wants.
- Tells the user where to paste, click, or run it manually.
- Never calls `POST`, `PUT`, `PATCH`, `DELETE`, or any MCP tool that mutates state.

## When to use

Use this skill ONLY when the user asks to:

- Draft a Jira issue comment, worklog, transition, or update.
- Draft a Jira issue creation (new ticket description, fields summary).
- Draft a Confluence page create/update or footer/inline comment.
- Draft a Bitbucket PR comment, review summary, or approval/merge guidance.
- Plan a sequence of writes the user will perform manually.

Skip this skill when:

- The user only wants to read context (no draft needed).
- The user is using ChatGPT or another agent that has its own OAuth-mediated Atlassian MCP write path (the user has not enabled that yet and prefers manual writes).
- The user explicitly asks the agent to perform the write directly (the proper write layer is not built yet — escalate and confirm before any future ask-gated execution).

## Output contract

For every draft, produce:

1. **Title / summary**: one sentence describing what this draft is for.
2. **Target**: which product + identifier (e.g. `JIRA-123`, page id, PR id, repo slug).
3. **Body / action**: the exact text or step list, ready to paste.
4. **Where to perform it**: the URL or UI step list (web UI) or CLI command (when applicable).
5. **Side effects to confirm**: anything the user should double-check before clicking submit.

Never include:

- Tokens, session ids, or any environment variable values.
- Internal MCP request/response payloads.
- Speculation about write success — the human will execute and confirm.

## Atlassian Rovo MCP tool surface (read context)

For read context the agent should prefer the official Atlassian Rovo MCP integration when the active coding agent supports it. The current token may not expose every tool below; the agent should fall back to whatever read tools are available and report gaps to the user instead of guessing.

- Bitbucket Cloud: `bitbucketWorkspace`, `bitbucketRepository`, `bitbucketPullRequest` (`list` / `get` / `comments` / `diff` only), `bitbucketRepoContent` (`branch.get` / `commit.get` / `files.get` only), `bitbucketPipeline` (read steps only), `bitbucketDeployment` (list/get), `bitbucketEnvironment` (list/get).
- Jira: `getJiraIssue`, `getJiraIssueRemoteIssueLinks`, `getVisibleJiraProjects`, `getTransitionsForJiraIssue`, `searchJiraIssuesUsingJql`.
- Confluence: `getConfluencePage`, `getConfluencePageDescendants`, `getConfluencePageFooterComments`, `getConfluencePageInlineComments`, `getConfluenceCommentChildren`, `getConfluenceSpaces`, `getPagesInConfluenceSpace`, `searchConfluenceUsingCql`.

Any tool with a destructive hint, or whose `action` argument includes `create`, `update`, `delete`, `merge`, `approve`, `comment`, `transition`, `addWorklog`, `addComment`, `run`, `pipeline.run`, etc., must NOT be invoked by this skill.

## Workflow per request

### 1. Read context

Pull only what is needed to draft accurately. For a Jira comment on `JIRA-123`, get the issue first; for a Bitbucket PR review summary, get the PR details, diff, and existing comments.

### 2. Draft

- Match the user's requested tone and length.
- Reference existing context (other comments, related tickets, file paths) where useful.
- For multi-step writes, list steps in order.

### 3. Provide the manual path

- **Jira**: web UI URL is `https://<site>.atlassian.net/browse/<KEY>` for the issue page; the comment box is at the bottom. Mention "Add a comment" vs. the specific transition if the user asked for a transition.
- **Confluence**: page URL is `https://<site>.atlassian.net/wiki/spaces/<SPACE>/pages/<PAGE_ID>/<title>`; mention the "Edit" button or specific comment anchor.
- **Bitbucket**: PR URL is `https://bitbucket.org/<workspace>/<repo>/pull-requests/<id>`; mention "Add comment", "Approve", "Merge" buttons, or specific CLI like `git fetch && git checkout <branch>` for testing locally.

### 4. Surface confirmations

Always note:

- Whether the write is reversible.
- Whether it notifies other people (reviewers, watchers, mentions).
- Whether the user has permission (assume yes unless read context indicates otherwise).

## Failure modes

- If a required read returns 401/403: report the auth/scope issue without printing tokens; tell the user which Atlassian permission group is likely missing.
- If a read returns 429: stop and report the rate limit; do not busy-loop.
- If the user asks for a destructive bulk operation (delete page, decline many PRs, transition many issues): ask the user to confirm one by one.

## What this skill does NOT do

- Does not call Atlassian Rovo MCP write tools.
- Does not shell out to `curl`/`httpie` with write verbs.
- Does not invoke Bitbucket `bb`, `bitbucket-cli`, or any third-party write wrapper.
- Does not store or print tokens, OAuth codes, or session ids.
- Does not run any smoke test or live verification — that is handled by `.local/bin/atlassian-smoke-test` separately.

## Future work

A proper write layer (separate write tokens, per-operation allowlist, client-side ask gates, possibly official Rovo MCP write tools with admin-constrained scopes) is planned but not built yet. Until then, this skill is the only sanctioned path for Atlassian writes.

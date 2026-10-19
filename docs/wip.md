# Technical handoff

## Current baseline

The current `main` baseline (including changes through `main919`) has removed retired coding-agent integrations. Do not restore retired services, configuration formats, packages, or migration steps from older notes. Keep the required `make test` gate and its live smoke-test behavior intact.

The maintained local model and OpenCode configuration are managed by the repository's current scripts and checks. Keep model identity, served-model metadata, and runtime selection consistent. Do not alter model limits, runtime capacity, or smoke timeouts without a specific request and supporting verification.

## Current MCP and browser handoff

- The self-hosted Desktop Commander MCP uses a local loopback backend and a Cloudflare tunnel protected by Access/OAuth. Its operational and security notes are in [`.nix/desktop-commander-mcp.md`](../.nix/desktop-commander-mcp.md).
- The controller's planned `check`, `probe`, and `tunnel` operations read configuration from the exact private environment file `$HOME/.profile_override`. Bootstrap-only API credentials must not enter the runtime. Required generated credential JSON belongs only in `$XDG_RUNTIME_DIR` with directory mode `0700` and file mode `0600`; do not use CLI arguments or persistent sidecar configuration for private values.
- Existing browser automation uses a dedicated Brave profile and Playwright MCP over stdio, isolated from the user's primary browser. See [`.nix/README-opencode-browser.md`](../.nix/README-opencode-browser.md). Enpass pairing is human-supervised; extension loading alone does not prove pairing. Do not read or expose the password manager's vault or master password.
- `.nix/README-enpass-mcp.md` and `.nix/opencode-virtual-browser.md` are not present in this baseline; use the existing browser guide unless those documents are added in a later, separately scoped change.

## Configuration and privacy

All manually maintained private configuration belongs exclusively in `$HOME/.profile_override`; do not create sibling files. Never put real credentials, tenant identifiers, private hostnames, account/resource IDs, local identities, home paths, challenge nonces, process IDs, or private execution metadata in tracked documentation, test fixtures, Nix expressions/store inputs, command arguments, or logs. Use reserved example domains such as `example.com` when documentation needs a URL. Do not expose private deployment values when describing commands or results.

Smoke/challenge/proof files are disposable test data, not configuration. Keep them in a private runtime location, do not document their real contents, and remove them when no longer needed. Tool output and service journals may contain sensitive arguments or results; do not copy them into reports.

## Verification and resumption

For static repository checks, use the project verification entrypoint `make test`; its commit gate requires the full test including live smoke checks. Individual offline contracts and smoke scripts are documented in `README.md`. Public endpoint probing, service control, OAuth flows, and live smoke tests require explicit runtime configuration and network access; do not claim they passed unless they were actually run.

When resuming MCP work, first confirm the relevant variables are present in `$HOME/.profile_override` without printing their values. The controller inputs include:

```text
DESKTOP_COMMANDER_MCP_HOSTNAME
CLOUDFLARE_ZONE_NAME
DESKTOP_COMMANDER_MCP_RESOURCE_NAME
CLOUDFLARE_TUNNEL_ID
CLOUDFLARE_ACCESS_TEAM
CLOUDFLARE_ACCESS_AUDIENCE
CLOUDFLARE_TUNNEL_SECRET
CLOUDFLARE_ACCOUNT_ID
```

Bootstrap may additionally use `CLOUDFLARE_API_TOKEN`, `CLOUDFLARE_ACCESS_EMAIL`, and optional `CLOUDFLARE_ZONE_ID`; checkpoint resource IDs, if used, also belong only in `$HOME/.profile_override`. Never import the administrative API token into the runtime. Inspect existing resources and reject conflicts or policy drift rather than silently replacing them.

For long-running MCP processes, retain the returned process handle and poll for completion; a backend restart loses child processes, which must be started again. A public probe checks transport and OAuth discovery only; it does not complete OAuth or prove a real client tool call. Keep those verification claims distinct.

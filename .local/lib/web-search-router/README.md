# Keyless web search for OpenCode and ZeroClaw

A single loopback HTTP adapter exposes a SearXNG-compatible JSON API at
`http://127.0.0.1:8979/search?q=your+question&format=json`.

## Provider policy (NO API keys)

1. DuckDuckGo public HTML search
2. Bing public HTML search
3. Wikipedia public search API (limited to encyclopedic coverage)
4. **Self-hosted SearXNG** at `http://127.0.0.1:8980` -- **ALWAYS LAST**

All configured providers are keyless; commercial APIs are not implemented.
`SEARCH_ROUTER_PROVIDERS=bing,duckduckgo` may reprioritize **public** engines,
but local SearXNG is appended last regardless of this variable's value.
`SEARCH_ROUTER_SEARXNG_URL` only accepts a distinct localhost HTTP port.

Local calls have no artificial daily/monthly quota or payment requirement.
However, every public web search -- including those performed by local SearXNG --
uses external engines, which may independently return 403, CAPTCHA or 429.
The adapter automatically falls back on these failures, temporarily cools down
blocked providers and caches successful searches for 10 minutes. The endpoint
never invents a result when all providers fail; it returns HTTP 503 instead.

## Integration

- **OpenCode 1.18.x:** `.config/opencode/tools/web_search.ts` exposes `web_search`
  using the shared router. OpenCode reload/new session after deploy.
- **ZeroClaw 0.8.5:** `.zeroclaw/config.toml` uses its native `web_search_tool`
  with `search_provider = "searxng"`, pointing at the router port 8979.
  The fact that the _adapter_ runs on localhost does **not** make SearXNG the
  first search provider. The adapter routes to public engines before local
  SearXNG. ZeroClaw must restart to load the updated config.
- No changes to the `klarkc` Nix profile are needed. SearXNG is fetched from
  the dotfiles flake's pinned nixpkgs using `nix build --no-link` at startup.

## Service installation (after merging the worktree)

The tracked user units are:

- `.config/systemd/user/web-search-searxng.service` (SearXNG port 8980)
- `.config/systemd/user/web-search-router.service` (JSON adapter port 8979)

Start both services only after the files are placed in their final dotfiles
locations under `$HOME` (their `ExecStart` references `%h`). Do not change the
active `zeroclaw.service` or Nix profile during worktree verification.

```bash
systemctl --user daemon-reload
systemctl --user enable --now web-search-searxng.service web-search-router.service
systemctl --user restart zeroclaw.service
curl -fsS http://127.0.0.1:8979/health
curl -fsSG 'http://127.0.0.1:8979/search' --data-urlencode 'q=PostGIS index' --data-urlencode 'format=json'
```

`web-search-router.service` wants (but does not require) the local SearXNG
service, so searches can continue using public engines if the local service
is unavailable. Both services bind to `127.0.0.1` only.

## Verification without modifying the active home checkout

```bash
python3 -m unittest discover -s .local/lib/web-search-router -p 'test_*.py' -v
python3 .local/lib/web-search-router/run_searxng.py # separate shell
python3 .local/lib/web-search-router/router.py serve # separate shell
curl -fsS http://127.0.0.1:8979/health
```

The test-only `e2e_mock_model.py` implements a deterministic OpenAI-compatible
mock on port 8990; it is **not** a production model. E2E tests should direct
isolated OpenCode/ZeroClaw configurations to that mock and verify the returned
tool invocation/output. Do not modify the active config or running daemons
while running those tests.

The router does not log query text. Search queries do leave the machine when
sent to public engines. No API secrets are required or consumed by the router.

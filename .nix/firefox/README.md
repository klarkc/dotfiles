# ChatQueue + Firefox on solo098 (legacy Arch)

`make firefox.Default firefox.ChatQueueInstall` (also part of `make all`) sets Firefox as default and checks
for the signed upstream ChatQueue package. It does **not** bypass Firefox extension
signing, replace the installed XPI, or auto-approve an add-on permission prompt.

1. Confirm ChatQueue 1.0.1 from [Mozilla Add-ons](https://addons.mozilla.org/en-US/firefox/addon/chatqueue-for-firefox/) is installed (currently installed on solo098).
2. Install the signed [Violentmonkey](https://addons.mozilla.org/en-US/firefox/addon/violentmonkey/) extension through Firefox's UI (one-time confirmation).
3. From an interactive graphical shell, run `make firefox.ChatQueueImport`. It temporarily serves **only our userscript** on `127.0.0.1` and opens its installation page in Firefox. Confirm the import in Violentmonkey, return to the terminal and press Enter to stop the server. Enable it only on `chatgpt.com` and reload the test conversation. If Firefox was started outside an interactive desktop, use `.nix/firefox/chatqueue-compat.user.js` to import manually.
4. On a **test** chat, confirm `document.querySelector('form[data-chatgpt-composer] button[type="submit"]')?.id === 'composer-submit-button'`. Do not use a broker agent to test first.
5. Configure two independent jobs with different harmless prompts. Verify each targets its own chat and waits until reply completion. Only then enable one broker heartbeat at a time, keeping historical gates, pulse reconciliation and at-most-once semantics intact.

### Why a userscript rather than a patched XPI?

ChatQueue 1.0.1 has four old ChatGPT send selectors in `page-task.js` and
none match current `form[data-chatgpt-composer] button[type="submit"]`.
Changing `page-task.js` inside the XPI invalidates Mozilla's signature,
and stock Firefox will refuse permanent installation of the altered file.
The upstream ChatQueue is **All Rights Reserved**; do not commit or redistribute
its packaged scripts. This repository includes **only our independent shim**.
Once upstream corrects its selectors, disable the shim and remove the workaround.

The shim never clicks Send. It only adds the missing ID to one unambiguous
composer submit button and refuses to label Stop/Cancel buttons. An extension
or ChatGPT layout update still requires renewed testing. It does not prove
multi-job safety, prevent global browser replays, or turn on the broker.

### Runtime notes

- Firefox may ask to install both signed extensions and the local userscript: Make cannot silently consent on your behalf. Importing the script is an explicit separate target, not a side effect of automated Make bootstrap.
- The script is installed once through Violentmonkey; it persists across browser restarts. `make all` never reimports it or sends prompts, while `make firefox.ChatQueueImport` provides a one-time interactive installer. Do not paste one-off console patches as a production solution.
- Picom is deliberately absent from XMonad's startup for CPU-only display; don't restart it in XRDP.
- `klarkc/os` must port the behavior declaratively to NixOS, with no Makefiles, Arch/AUR dependency or copying of browser profiles.

### Verified on the live Firefox (2026-11-18)

Violentmonkey 2.50.0 was installed from Mozilla Add-ons and the compatibility
userscript 1.0.0 imported through a loopback-only HTTP installer; the transient
HTTP server was stopped after import. A ChatGPT message composer with draft
returned `composer-submit-button` through DevTools after reload.

A **single scheduled, one-shot ChatQueue send** to an isolated test chat was
observed live: ChatQueue sent `Teste automatizado do ChatQueue. Responda apenas
ACK CQ.`; ChatGPT replied `ACK CQ`, and ChatQueue's History recorded `sent`,
with `0` upcoming jobs. The queue was paused after verification, and a separate
old QualityManager test item stayed in `Needs review` (not replayed). No broker
heartbeat was invoked. **Parallel per-tab behavior and end-to-end four-agent
heartbeat loop are still unverified**.

### Bootstrap validation and publication gate (2026-11-20)

This source patch was prepared in the isolated worktree
`fix/solo098-firefox-bootstrap-20261118`. Do not treat the worktree alone
as a published commit or a deployed NixOS configuration.

- OpenCode is fixed at **2.3.0** from a reviewed `opencode-nixpkgs` pin,
  independent of the older primary Nixpkgs input. This includes the
  upstream fix for MCP tools missing from the first model call (#60490).
  The Nix profile **and** `nix develop` use the same wrapped binary, with a
  Nix version contract.
- OpenAI OAuth is discovered through the OpenCode credential metadata on
  newer versions, without printing secret material. The read-only Bitbucket
  MCP E2E completed a real tool call and correlated a real PR ID with the
  model response. Never count a model claim as independent MCP evidence.
- The Desktop Commander smoke supports both historical and new Python MCP
  HTTP transports (`streamablehttp_client`/`streamable_http_client`) and
  result error field names (`isError`/`is_error`). Its isolated loopback
  lifecycle and access-control checks passed.
- The OptMem synthetic tests depend on **exact upstream bytes**, installed
  by `make optmem.PrototypeMemoInstall` (included in `make all`). The download
  is pinned by Git revision and SHA-256 and does not initialize canonical
  memory, activate a writer, or enable a service. Two calendar-dependent
  tests now use fresh synthetic dates within the 30-day retention window.
- `nix develop --command bash -c 'SMOKE_TESTS_ENABLED=true make test'`
  returned **exit 0** on 2026-11-20, including Nix checks, all six smoke
  scripts in both supported modes, and the isolated OptMem acceptance tests.
  Nine production-dependent OptMem tests intentionally skip while OptMem is
  inactive. This is the actual mandatory Git pre-commit test, not an offline
  test with its live dependencies disabled.

The next steps are a normal commit with its hooks (no `--no-verify`), safe
reconciliation of the source checkout, and the permitted publication flow.
None of these tests authorizes the broker's four live heartbeats or a cloud
migration. The Firefox/ChatQueue one-shot test passed previously, but
per-agent cross-tab and reply-completion verification is still pending.

The system SSH config error also requires separate diagnosis: the reported
mode `0777` belongs to a normal symbolic link whose target is mode `0644`;
the observed path/target ownership is `nobody`, not root. Do **not** try to
fix this by chmod of the symlink. Verify root ownership and source packaging
before privileged repair. Preserve Git pre-push gates and history safeguards.

### Operator decision: own all ChatQueue adaptations (2026-11-20)

No vendor contact or external maintenance request. The **source of truth** for
our compatibility fixes and future recurrence logic is `klarkc/dotfiles`, with
backport requirements and readiness recorded in `klarkc/broker`. Transfer to
`klarkc/os` is **future work only**, after its own review and tests. Never copy
the proprietary XPI, Mozilla profile, cookies, or login secrets into Git.

Our own `.nix/firefox/chatqueue-recurrence-core.mjs` now models safe recurrence.
Run `make firefox.RecurrenceTest` for deterministic, offline contract tests.
It requires **360 seconds after independently observed final reply**, not
360 seconds after `sent`; it fails closed on restart, uncertain effects,
identity mismatch, no prior terminal and missing manual authorization.
The pure model is **not connected to ChatQueue or Violentmonkey yet**, and
therefore it does **not** repair the signed extension's background scheduler.
The four production jobs remain **PAUSED / NO_GO**. Do not use the existing
ChatQueue 1.0.1 repeating jobs for broker agents (it resets `afterReply`).

A production adaptation must be original and versioned here, integrated via a
properly signed Firefox extension or the already signed Violentmonkey host,
with E2E evidence for four chats and restart/ambiguity semantics. Do not
repack ChatQueue 1.0.1 or weaken Firefox signature enforcement. No cron,
ChatGPT Tasks or independent unapproved broker scheduler is introduced.

### In-house LAB-only browser integration (2026-11-20)

New original files under klarkc/dotfiles:

- .nix/firefox/chatqueue-local-controller.mjs: durable PREPARED-before-click
  state, independent fresh preflight, exclusive per-conversation browser lock,
  fail-closed REVIEW on post-click uncertainty and six-minute cooldown after
  independently observed assistant completion.
- .nix/firefox/chatqueue-local-browser.js: Violentmonkey adapter with
  site-specific DOM inspection, one-click delivery and reply observation.
- .nix/firefox/build-chatqueue-local.mjs: deterministic local build of
  .nix/firefox/chatqueue-local.user.js, entirely from our own source.
  The generated script is Git-ignored, regenerated locally on demand and
  **not imported automatically**.
- .nix/tests/chatqueue-local-controller.test.mjs and
  .nix/tests/chatqueue-local-browser.test.mjs: node:test contract tests.
- make firefox.ChatQueueLocalBuild regenerates the script;
  make firefox.RecurrenceTest verifies the checked-in output and all tests.

**This is NOT an operational cutover**. The generated userscript is
0.1.0-lab and intentionally has no production/agent enable command:

- It has no live credentials, no external fetch and no ChatQueue XPI changes.
- A new page import sets STOP=true and starts no timer or send.
- Only a deliberate Violentmonkey menu interaction can configure a harmless
  CQLOCAL LAB: message. Other prompts are rejected and LAB runs are capped
  at two occurrences; there is no operational Scheduler GO.
- The signed upstream ChatQueue queue **must remain PAUSED**. The new
  script is not imported into the operational Firefox profile by Make.
- All four permanent broker heartbeats remain PAUSED/NO_GO. The bridge uses
  DOM heuristics, not a proven production-grade terminal signal; it requires
  isolated Firefox E2E with long generations, tabs/locks, restart during
  PREPARED and after click, identity/account/model validation and unknown
  effects before any live use.

We must not call the extension vendor. Any future transition of the scheduler
to a standalone, locally maintained implementation also needs matching broker
governance and explicit staged activation. The three legacy gates remain OFF.
Future klarkc/os port must package this original behavior declaratively,
without copying browser profiles, signing-bypass preferences, or a proprietary
XPI. Never press original ChatQueue 1.0.1 "Resume queue" for broker recurring
jobs: its afterReply=false reset is unchanged.

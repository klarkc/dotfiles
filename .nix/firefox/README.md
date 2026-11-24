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

### Firefox headless local E2E (separate profile, no ChatGPT login)

Use explicit make firefox.ChatQueueLocalE2E from the source root.
This target builds our ignored LAB-only userscript from original source and
uses flake-pinned Nix Geckodriver + Selenium ephemerally. It starts a fresh,
headless Firefox with a temporary profile, hosts only a synthetic conversation
fixture on 127.0.0.1, injects the LAB adapter in memory with a loopback-only
origin and deliberately shorter timing, then closes the browser and server.
It does **not** install into Violentmonkey, load the user's browser profile,
visit ChatGPT, or touch any real agent chat. The original 360-second
production cooldown remains unchanged.

Verified acceptance scenarios in Firefox: two distinct conversation tabs,
independently configured harmless LAB messages, two sends/replies per tab,
next send strictly after observed terminal reply plus the LAB cooldown,
STOP after the second, and reload during an outstanding reply causing
REVIEW with no duplicate send. In the initial headless test, Firefox refused
execCommand("insertText") in a background tab; the original LAB adapter
now uses a constrained, empty-composer DOM input fallback before it fails
closed. This input fallback **must be separately tested against the actual
ChatGPT editor** in a controlled authenticated test chat before deployment.

The headless local E2E proves Firefox behavior against the synthetic page,
not real ChatGPT UI compatibility, account/model binding, signed Violentmonkey
import, or four-agent heartbeat liveness. Never infer broker GO from this
target. No new scheduler or heartbeats are automatically enabled.

### Completion proof: fail closed if a response is only apparently idle

The LAB controller no longer treats a message that is unchanged for six seconds
and has no visible Stop button as sufficient terminal evidence. It now needs
a **new assistant turn with a visible, enabled end-of-turn action**, such as
a turn-scoped Copy button (English/Portuguese). Only the local synthetic fixture
provides the special cq-local-terminal-receipt marker. A missing/ambiguous
action results in REVIEW at the bounded deadline, never a second send.
The next message is also blocked if the exact user turn disappears or another
user message supersedes it, and manual Pause during an uncertain response
moves the run into REVIEW rather than continuing its wait.

The opt-in Status (no send) menu reports only structural facts: composer
presence, a unique send button, Stop visibility, assistant turn count and
whether the most recent assistant turn exposes a completion action.
It does not read/copy chat text, cookies, credentials or model prompts.
**This is still heuristic**: even a Copy button may not be an authoritative
completion signal on the live ChatGPT UI. The actual authenticated page must
be audited read-only before any live test sends or operational cutover.

The loopback Firefox E2E additionally checks a stable assistant message with
no completion receipt (must end REVIEW, no retry) and a manual pause while
awaiting an answer (REVIEW, no retry). The loopback deadline is shortened
only in memory for test speed; the source keeps its original two-hour
terminal-observation bound and the 360-second cooldown.

### Authenticated Firefox read-only DOM audit (2026-11-23)

In the operator-created, empty ChatGPT chat on the primary Firefox profile,
DevTools read-only queries (without typing into or sending from the editor)
confirmed the following:

- Browser route: / (new chat not yet persisted as a /c/<id> conversation).
- Exactly one form[data-chatgpt-composer], with a
  DIV[contenteditable=true][role=textbox] and the Portuguese accessible label
  Pergunte ao ChatGPT.
- Initially zero form buttons with type=submit, zero visible Stop buttons
  and zero assistant turns. Existing form controls are type=button for
  attachment, model selection, dictation and starting voice mode.
- Therefore requiring a send button in targetReady() before composing is
  incorrect. The LAB bridge now checks only the unique empty composer during
  preflight, and resolves a unique send candidate after composing.
  Recognized post-draft candidates are a button with data-testid=send-button
  or type=submit, subject to visibility/disabled checks. We did not assert
  either post-draft selector is present in the real ChatGPT UI, because no
  draft was entered during the read-only audit.

The Firefox loopback E2E fixture now starts with no send button and adds a
button[type=button][data-testid=send-button] only after a text-input event.
This checks the pre-compose/post-compose separation with two browser tabs,
without modifying the signed ChatQueue extension or authenticated page.

Important: the LAB userscript deliberately matches /c/* only. The empty
/ home route is not an eligible LAB target. To validate the authenticated
composer and completion witness, the operator must first manually send a
harmless, non-sensitive message in this separate new chat so it receives a
real /c/<conversation-id> URL. No broker agent tab, heartbeat job or legacy
ChatQueue queue may be used for that manual setup. The following DOM inspection
must again be read-only before any LAB send is attempted; do not infer a GO.

### Authenticated Firefox persisted-chat audit after MANUAL LAB_ACK (2026-11-23)

The operator created a separate LAB chat by manually sending exactly one
harmless "CQLOCAL LAB:" prompt and receiving "LAB_ACK". The main Firefox
profile was then inspected using read-only DevTools DOM queries on this
persisted /c/ conversation. No messages were generated or sent by the LAB
adapter; no user script was installed or activated.

The actual post-reply DOM differs substantially from the empty / home page:

- Initially **two** form[data-chatgpt-composer] elements were observed:
  one invisible with getClientRects().length == 0 and one visible/active
  with an empty editor. At a later read-only check the hidden duplicate
  had disappeared, leaving ONE form. The selector must choose exactly
  one **visible** composer and refuse any ambiguity; it cannot depend on
  a fixed total form count. The empty active composer has no Send button.
- Legacy [data-message-author-role="user" or "assistant"] elements are
  ABSENT. Role headings instead use data-conversation-role ("user" and
  "assistant"), while data-virtualized-turn bounds the visible turn.
  The assistant role heading belongs to a div.group.flex.flex-col that also
  contains one visible Copy action.
- The page contains **two** [data-user-message-bubble] elements with the
  same harmless LAB seed: one is outside the actual turn and one is inside
  it. Do not find user messages globally. The original user was scoped to
  the virtualized turn; the assistant Copy action was scoped to its own
  heading/group in that same turn.
- One copy-turn action is visually present; its actual accessible label is
  "Copiar" (the bridge also supports "Copy"). No Stop action was visible.
  These facts show a completed _manual_ reply, NOT that Copy appears only
  after all kinds of streaming/long-running responses. Its terminal status
  remains a conservative heuristic until a supervised long-reply E2E test.

The original LAB bridge now supports both the legacy loopback markup and
these live role/turn markers, requires a benign **manually completed seed**
before it can arm, chooses only one visible composer, rejects duplicate
or mixed role layouts, and demands that the latest user message and last
assistant reply are in the SAME modern turn before preflight can pass.
The send is still gated on a unique active candidate AFTER drafting and has
not been tested with a filled authenticated editor. The reply check rejects
a Copy action belonging to an earlier/different turn.

The isolated Firefox E2E now emulates two forms, a duplicate non-turn user
bubble, data-virtualized-turn, an assistant role heading, and a localized
Copiar action. Its negative cases require REVIEW without second send if
Copy is absent from the new reply despite being present on the seed, and
prevent the first send when there are two visible composers, no seed, or a
newer unanswered user turn. This is still NOT an authenticated automated
E2E; the script remains SOURCE ONLY, STOP=true, original XPI unchanged,
four permanent agents paused, runtime NO_GO.

### Live authenticated Firefox LAB one-shot and signed Violentmonkey integration (2026-11-24)

On the existing **separate, previously manually seeded** ChatGPT /c/ LAB
conversation, a supervised one-shot exercise ran the **original** local
LAB source against authenticated Firefox DOM. The harness was temporary
and restricted to the LAB URL; it replaced only its browser GM storage
with **sessionStorage for that ephemeral DevTools experiment** and capped
MAX_CYCLES at **1** in its ephemeral copy. No broker identities or four
permanent agent chats were used; no userscript was installed in this first
phase. The actual source in this repository retained its default
MAX_CYCLES=2 and normal GM bridge.

- The first automatic delivery requested ten original Portuguese sections
  on asynchronous system resilience, ending with a synthetic marker.
  The adapter made exactly ONE send, then observed a streamed reply of
  roughly 8,049 characters. The final marker appeared as requested.
  Polling at ~300 ms showed **119 active Stop** samples, **208 Copy**
  samples, **ZERO simultaneous Stop+Copy** on the new reply, and the
  Copy action appeared after Stop had disappeared. The observed interval
  from first Stop to first Copy was about **36 seconds**. The state
  transitioned to **COOLDOWN, cycle=1, STOP=true**, with no replay.
  This is a positive witness for the **tested UI path**, not a formal
  invariant of every future ChatGPT UI state.
- A second ephemeral one-shot asked for 15 sections and was reloaded
  during a visible Stop in **AWAITING_REPLY, cycle=1**. After reload
  the server-side reply eventually finished, but the persisted state was
  still **AWAITING_REPLY** and required reconciliation. Re-injecting
  the source on the **same LAB conversation** ran the boot procedure and
  produced **REVIEW, STOP=true**, **without a repeated send**. Recovery
  preserved the distinction between conversational completion and
  externally verified delivery effects.
- Independently, the operator authorized a temporary import into the
  **already signed Violentmonkey 2.50.0** extension in the operational
  Firefox profile. We generated a temporary copy of the original script
  constrained via a **single exact /c/ @match** and **MAX_CYCLES=1**.
  The install came from a disposable 127.0.0.1 HTTP server, with external
  edits auto-following OFF. No proprietary XPI was repacked or patched;
  the existing selector-compatibility script stayed installed.
- The Violentmonkey menu Status returned **configured=false,
  phase=NONE, STOP=true**, and a unique visible editor with manual seed
  ready. After explicit Configure and Arm actions in that **single LAB
  tab**, the signed host sent exactly ONE harmless
  CQLOCAL LAB: Respond with LAB_ACK. prompt, received LAB_ACK,
  and its own post-reply Status returned **configured=true,
  phase=COOLDOWN, stopped=true, cycles=1, Stop=false,
  lastTurnHasCompletionAction=true**.
- The **temporary** userscript was then deleted via Violentmonkey's
  UI, including the trash; the dashboard readback showed **ONE**
  installed script, the original non-sending compatibility shim. The
  LAB tab was reloaded after uninstall and retained the test exchanges.
  The unsigned temporary HTTP import server was stopped. No lingering
  laboratory activation or timer was left running; the original signed
  ChatQueue 1.0.1 queue remains PAUSED.

### What these tests DO NOT authorize

These findings satisfy a **single isolated authenticated LAB chat**
positive send/long-reply/terminal test, an in-flight reload **fail-closed**
test, and the real signed userscript-host positive path. They **do not**
constitute production readiness for the permanent four-agent Broker:
no authenticated 4-chat background-tab liveness/reconciliation test,
no independent QualityManager sign-off, no proof of model selection or
stable long-term ChatGPT DOM semantics, no cross-account generalization,
no delivery of actual permanent agent prompts, and no approval to turn
on heartbeats/schedulers/operator digest. The ephemeral one-shot harness
was never intended as a production agent scheduler. Do not infer
production GO, and do not resume ChatQueue 1.0.1.

The dotfiles remain locally ahead of origin due to a previously observed
remote write ACL/403; never bypass that permission. The future NixOS
backport in klarkc/os must use versioned original source, signed
userscript hosting, explicit operator activation, independent QA and the
governance gates stored in klarkc/broker. No manual operator typing is
required for further benign isolated tests when a controlled Firefox
session is already available.

### Pause fencing and four-role loopback regression (2026-11-24)

A deterministic Node fixture reproduced the race in the previous LAB
source: a Pause during the durable READY write could be overwritten by
an older Arm, leaving STOP=false and scheduling a timer. The adapter now
persists a distinct pause epoch, binds each arm to that epoch and an
individual arm ID, and checks that lease before scheduling, composing,
clicking, and accepting a terminal reply. Boot and Pause revoke the epoch;
pending attempt IDs are preserved. A stale arm may not publish permission
for a superseded epoch. These are LAB guards, not a production authorization
or a promise of atomic cancellation of a click that has already happened.

Verification uses 36 Node tests, plus opt-in real headless Firefox against
a temporary loopback-only fixture. Four separate synthetic chats, one for
each role, receive exactly two messages and two replies with distinct
role-specific content, cooldown after verified terminal, and STOP.
Opening a second document of the same chat during AWAITING_REPLY
invalidates the earlier lease; configuration/arm are refused, the
original attempt ID survives in REVIEW, and exactly one send remains.
The negative preflight tests now wait for an actual refusal rather than
accepting the initial STOP=true as evidence.

The live authenticated LAB tests above apply to the older source, not
this delta. No permanent agent chat, real account/model identity, or
production bootstrap/gate has been certified by these loopback fixtures.
The userscript remains LAB-only and is not installed by the test target.

### Independent QA remediation: acknowledged Pause versus click

QualityManager's manual read-only review of e8592fb identified an
unserialized Pause/click interval, stale timer publication, and missing
interleaving coverage. A short per-conversation Web Locks fence now
serializes the final click, Pause/boot revocation, arm publication, and
timer creation. The longer conversation lock can span a reply; the
short fence does not. Pause acknowledgement requires durable readback
of its revocation. The final lease/DOM checks and click have no intervening
await. Old timers in another document may still wake, but must fail their
lease checks; this is not a promise to cancel an already executed click.

Two deterministic two-tab real Firefox loopback cases establish both
orders: Pause acknowledged first prevents the click; click first
precedes Pause acknowledgement. Each preserves the original attempt in
REVIEW and never retries. The 38 Node tests include the ARM_ID/STOP writes
and scheduling-read interleavings. All four synthetic role tabs and the
duplicate-document, reload, and refusal cases remain green.

This remediation awaits independent re-review and an authenticated
signed-host test of this exact source. No operational authorization,
production prompt, or scheduler activation is implied.

### Read-only Git policy evidence (not operational authorization)

The standalone source `chatqueue-git-policy.mjs` is an offline-testable,
read-only admission evidence reader. It checks the exact configured origin,
reads both remote HEADs before and after the four pinned policy blobs,
and requires literal flags, the four identities and all three legacy gates
OFF. Missing objects, unavailable reads, drift, paused roles, old queue
permission or a LAB runtime fail closed. It does not fetch or mutate Git,
serve a local endpoint, persist a credential, install, arm, schedule or send
anything; it is deliberately **not** in the LAB userscript bundle.

For an explicitly authorized manual read-only check, supply the absolute
path of the already available broker Git checkout, one permanent role and
the exact trusted SSH/HTTPS origin via `CHATQUEUE_BROKER_ORIGIN`. Example
(synthetic paths only; store real private values in the existing private
configuration, never tracked files):

```sh
CHATQUEUE_BROKER_ORIGIN=git@example.invalid:example/broker.git \
  node .nix/firefox/chatqueue-git-policy.mjs /example/broker coordinator
```

`NO_GO` exits 2; `GIT_POLICY_PASS_ONLY` exits 0 and still has
`runtimeAuthorized: false`. A matching Git snapshot is necessary evidence,
not a manual GO, account/model/route/document or bootstrap proof, a runtime
lease, a proof of completed provider effects, or an atomic guard against
later Git changes. The eventual send path must acquire fresh evidence at
its own admission and final pre-click boundaries, reconcile attempts, and
independently enforce pause/identity. No periodic checker or new scheduler
is created. The existing signed LAB source and all live browser jobs remain
unchanged. Offline tests run with `make firefox.RecurrenceTest`.

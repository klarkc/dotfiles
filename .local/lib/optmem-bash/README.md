# OptMem Bash evidence extraction (inactive)

This tracked module provides a privacy-first Bash-history evidence collector and
a local-vLLM candidate selector with read-only access to the existing canonical
OptMem `wake`, `recall` and `zoom` API. It does **not** replace the original
Bash history, archive raw commands, or auto-enable any unit.

The user authorized **autonomous low-risk Bash-derived decisions without per-item
human approval**, but not remote sync/push, native-memory changes, or operational
Bash configuration changes. Historic G4 ingestion and promotion remain separately
conditioned on source reconciliation, semantic verification, and durability.

## Architecture and trust boundaries

- `bash_optmem_agent.py capture` reads only `~/.bash_history` (or an explicit
  fixture), preserving a cursor, entire-prefix HMAC, command-family features,
  provenance HMACs, and bounded private SQLite aggregates. Complex commands,
  sensitive arguments and unknown command families are discarded before
  inference. A first pass or a rotated file is not presumed to be dated history.
- `bash_optmem_agent.py extract` reads bounded shared OptMem context, resolves
  **vLLM-current from llm-config** even when the main preset is Full GPT,
  uses authenticated loopback only, and defers while the server is occupied.
  A structured model proposal must pass independent gate rules and refer to
  at least six live observations across three observation dates. The model
  never receives unfiltered raw history and cannot execute it.
- Accepted evidence is written only to the **private staging outbox** as
  `READY_FOR_WRITER_NOT_ACTIVATED`. Until remote restore and multi-harness
  acceptance are proven, the unit never writes the canonical `LOG.txt`.
- `writer/` contains an **isolated-only** upstream OptMem writer and
  same-user AF_UNIX IPC implementation with opaque source proofs, two-phase
  receipt recovery, one-writer locking and idempotency. It **intentionally
  refuses** production memory paths; never install it as a production
  authority without a separate reviewed deployment.

## User service templates (not enabled)

- `.config/systemd/user/optmem-bash-capture.{service,timer}`
- `.config/systemd/user/optmem-bash-extract.{service,timer}`

The capture timer is approximately minutely, extraction approximately hourly
when enabled by a subsequent authorized activation. Both apply read-only home,
tight write paths and resource limits. All current units are _declarative only_:
do **not** start, enable, restart related daemons, or alter `.bashrc` as part
of installation. They use the same `/usr/bin/python3` runtime strategy as the
existing tracked `web-search-router.service`; no Nix input/output changed.

The memory input in the extract unit is `~/.optmem/memory`; if it has not
undergone the controlled cutover, the extractor cannot provide broad migration
context. The separate writer's live pipeline is still blocked.

## Verification

From the **feature worktree**:

```sh
python3 -B .local/lib/optmem-bash/test_bash_optmem_agent.py
python3 -B .local/lib/optmem-bash/writer/test_optmem_writer.py
systemd-analyze verify .config/systemd/user/optmem-bash-*.service \
  .config/systemd/user/optmem-bash-*.timer
make test
```

Synthetic tests never import real Bash commands. The isolated writer tests
invoke the upstream `memo` only in randomly created private fixture
directories, never in the canonical OptMem memory. The smoke entrypoint
`.local/bin/optmem-bash-smoke-test` is picked up by the mandatory
`make test` flow.

## Release gates

Before production: complete the **final semantic resync** from OpenCode,
dotfiles Git, and a fresh supported GPT Web export; certify source provenance,
dedup/supersession and semantic `nap` summaries; test the writer's full
multi-harness integration and rollback; verify remote backup and restore under
new permission, or explicitly negotiate a different durability policy. Do not
mistake a read-only source resync for permission to push/sync memory.

Bash `histappend` and inter-shell synchronization are a distinct operational
change and are **not** implemented or enabled here. Extracting knowledge
periodically cannot preserve commands overwritten between scans.

# OptMem stage-only reader MCP

The server exposes only optmem_wake, optmem_recall, and optmem_zoom through
stdio MCP. All three read the upstream OptMem LOG/TREE in the exact G4P
private staging root; there is no independent semantic store. Startup and
each read verify the staging path/owner, complete record alignment, immutable
LOG hash and audited upstream memo hash. All write operations, arbitrary paths,
regex queries, partial wake responses and oversized output fail closed.

OpenCode MCP discovery passed in a private OPENCODE_CONFIG_DIR. ZeroClaw 0.8.5
requires both an MCP server and an explicit agent bundle, for example:

    [mcp_bundles.optmem_only]
    servers = ["optmem_stage"]
    [agents.default]
    mcp_bundles = ["optmem_only"]

With a real ZeroClaw executable and local deterministic mock model, the
negative no-bundle test excludes all MCP tools, and the positive bundle
test registers three tools and receives the actual U01 OptMem recall result.
These tests do not certify real model tool selection or production daemon
integration. The isolated ZeroClaw config uses memory backend none.

Secure local vLLM alias credentials must be passed by process environment,
without writing any token to TOML or Git:
    ZEROCLAW_providers__models__vllm__default__api_key="$VLLM_API_KEY"
A real local-Qwen ZeroClaw agent test timed out at 64 seconds.

No note, nap, forget, config, or import endpoint is exposed. Writer IPC
remains stage-only and canonical activation, native-memory modifications,
remote pushes and sync remain prohibited until release gates are satisfied.

The exploratory isolated OpenCode run with an OpenAI-compatible loopback mock
timed out at 40 seconds. This proves neither failure nor success of OpenCode
tool invocation; only native OpenCode MCP discovery passed. The mock test is
retained as an optional diagnostic, never treated as a mandatory acceptance.

Read-side protocol unit tests are hermetic. The real-stage tests and live agent
tests require preexisting private G4P source files and already installed runtimes;
they are run separately, not inside Nix build derivations.


## G4K: automatic agent admission with deterministic deduplication

The NEW `agent_mcp.py` endpoint exposes the upstream writer's automatic
`optmem_store(memory_key,text)` alongside wake, recall, and zoom, restricted
to isolated synthetic G4K staging. The caller is an **explicitly authorized
harness** chosen at MCP server startup (`--harness opencode` or `zeroclaw`);
the model cannot submit or override its harness identity or any path.

`agent_store.py` reuses the **same** OptMem writer lock, immutable upstream
LOG and provenance ledger as Bash. It has no secondary relevance-judging LLM
or human approval. Agents choose relevance and provide a stable, global
semantic identity (`memory_key`, e.g. `workflow.git.worktrees`).

Uniqueness is enforced at two levels:
- Stable **logical key**: a different text for the same key is rejected as a
  conflict, never silently appended as an additional active memory.
- Normalized **content**: Unicode NFKC/casefold, collapsed whitespace and
  trailing punctuation canonicalization prevent another append under a
  different key or harness. The writer searches *the entire upstream LOG*,
  including preexisting imported records and Bash writes.

The agent receipt sidecar is merely the source-key/request index, **not** a
second semantic-memory store. The shared writer's PREPARED/COMMITTED journal,
serialized FLOCK and idempotent request identity survive concurrent processes,
restarts and injected crashes, without appending a second record.

Guarantee scope: the same key or the same normalized text cannot be appended
twice **through this writer**. Arbitrary paraphrases with different logical
keys cannot be proven semantically identical by deterministic code; agents
should recall/reuse existing keys instead of inventing a second key for the
same fact. A direct file write bypassing this writer is forbidden and detected
by provenance validation, not automatically repaired. Existing historical
duplicates, if any, require a separate migration and are not rewritten here.

Stage-only authorization example (synthetic paths, never production):

    [[mcp.servers]]
    name = "optmem_stage"
    transport = "stdio"
    command = "/usr/bin/python3"
    args = ["-B", "<absolute-agent_mcp.py>", "--harness", "zeroclaw",
            "--memory-dir", "<private-synthetic-memory>",
            "--sidecar", "<private-synthetic-sidecar>"]
    [mcp_bundles.optmem_only]
    servers = ["optmem_stage"]
    [agents.default]
    mcp_bundles = ["optmem_only"]

ZeroClaw actual executable + locally simulated model completed `optmem_store`
in two independent sessions: first STORED, second ALREADY_PRESENT, with one
original OptMem LOG record. The test neither calls GPT Business nor opens
browser/Enpass integrations. The same memory also passes concurrent stdio MCP
from independent OpenCode/ZeroClaw stage-harness processes.

G4J's quarantine-only proposal inbox was retired from the deliverable: an
explicit agent memory no longer needs a separate semantic approval pipeline.
Bash's local inference/observation proof gate remains unchanged.

## G4L: lookup before deliberate storage (stage-only)

`optmem_lookup_key(memory_key)` reads the shared logical-key index, verifies
its referenced upstream LOG record and returns `FOUND` with index and original
text, or `NOT_FOUND`. It does not append, infer semantic similarity, accept
source paths, or expose an alternate memory store. Both harnesses see the same
key namespace. Agents should call this for an existing stable key and use
`optmem_recall` to inspect related text before allocating a new key.
A reused key with contradictory text remains rejected. No second relevance
model or human review is introduced; production activation is unchanged.

MCP transport compatibility: `tools/call` accepts protocol-owned `_meta`
without letting those fields override trusted harness identity, paths, or
writer authority. Unexpected extra transport parameters remain denied.

## G4M: bounded discovery of existing memories (stage-only)

`optmem_search(query)` scans the single verified upstream LOG while holding
the shared agent lock. It returns up to eight lexical matches, with an index,
existing registered logical key (if any), original fact, and bounded aliases.
Previously imported or Bash-only records appear without a logical key; an
agent may bind a key by storing identical original text through the shared
writer, which cannot append a duplicate. No new semantic store is created.
Queries are bounded literals, and incomplete or corrupted source/index data
fail closed. Agents should search first and compare the returned facts before
inventing another key. Lexical discovery helps key reuse but cannot prove
that distinct paraphrases under different keys mean the same thing.
No production writer, Bash timer, live browser or agent config is enabled.

### G4M restore safety

A nonempty stage can only be opened by the MCP adapter when both the OptMem
provenance ledger and agent-key/request SQLite index are present, owned by the
current user and inaccessible to group/other users. A LOG-only restore must
not silently re-register controlled notes as an unverified baseline or erase
existing logical-key bindings. Fresh legacy LOG enrollment needs a separate
explicit, verified migration procedure; this guard does not authorize one.
Restoration tests use disposable copies of the complete memory + sidecar pair.
Remote durability and production cutover remain blocked.

## G4N: coherent local snapshot and staged restore (NOT production)

`python3 -B .local/lib/optmem-bridge/snapshot_stage.py create --memory
<stage>/memory --sidecar <stage>/sidecar --backup <new-stage-snapshot>`
creates a **local, private, bounded, immutable-by-convention** snapshot of the
entire staged upstream memory (LOG, config and TREE files) and the two SQLite
sidecars. The staging and destination paths must be newly named, private paths
under the ignored `writer/synthetic-cases/` directory. The same script accepts
`verify --backup ...` and `restore --backup ... --destination <new-stage>`.
The target restored stage is never an existing one; canonical OptMem, ZeroClaw
native SQLite and production agent configuration are hard-denied.

The capture holds `agent_lock -> writer_lock` (the real OptMem lock order) and
uses the SQLite online backup API for each database, including uncheckpointed
WAL state; it does not copy raw live SQLite database files. All records, keys,
receipts, ownership, and private modes are verified. Pending PREPARED receipts,
corruption, incomplete sidecars, unusual files, symlinks or colliding target
names fail closed. A sealed manifest records counts, sizes and SHA-256 checks;
restores are published via an atomic local directory rename after validation.
The test suite injects concurrent writes, WAL pages, partial backups, damaged
LOG/SQLite and process-crash receipt states.

**Limitations:** an unkeyed SHA-256 manifest detects accidental corruption,
not a malicious party replacing both the files and the manifest. This is a
*local stage coherence proof*, not a remote durable backup or an encrypted,
authenticated restore chain. Noncooperating raw OptMem writes remain prohibited;
the lock protocol does not protect against malicious bypasses. No remote
transport, cloud service, snapshot timer or native memory migration is enabled.

## Production release gate (read-only, fail closed)

`python3 -B .local/lib/optmem-bridge/production_preflight.py`

This intentionally returns exit status **2 / BLOCKED** until all production
cutover gates are verified. It does not install an MCP, change configuration,
write memory, stop agents or trigger a remote backup. It tests provenance of
the approved historical stage, checks the actual canonical LOG state and
release receipt, validates the two production MCP bindings and the
ZeroClaw native writer exclusivity setting, and accounts for every native
SQLite memory with a source-content digest and verified canonical index.

Local synthetic regressions cover failing closed on omitted native rows,
concurrent growth, tampered records, stale historical evidence, missing MCP
bindings and two simultaneous native writers. A mocked fully certified state
can pass the pure gate; the real host cannot pass merely because an existing
G4N staging smoke was green. The acceptance receipt in private state is
operator evidence, not a cryptographic signature or substitute for executing
the two real runtime E2Es. It must only be created after those E2Es pass.

A complete migration of multiline/oversized/sensitive ZeroClaw notes requires
its own verified lossless mapping or securely retained-archive contract before
production go-live. This strict initial gate intentionally does not invent
semantic equality for paraphrased or truncated legacy notes.

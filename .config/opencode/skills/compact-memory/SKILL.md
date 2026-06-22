---
name: compact-memory
description: Compact a writable OpenCode agent's own MEMORY.md when its frontmatter is dirty or bloated. Use for Build and Design memory maintenance: preserve durable active context, remove stale details, keep pointers to authoritative files, and mark memory clean after successful compaction.
compatibility: opencode
---

# Compact Memory

Use this skill only for a writable primary agent's own memory file:

- Build compacts `.agents/build/MEMORY.md`.
- Design compacts `.agents/design/MEMORY.md`.
- Do not compact another agent's memory.
- Do not use this for Plan, Scout, Explore, Summary, Title, or Compaction agents.

## Goal

Rewrite `MEMORY.md` into a bounded working index. It is not a log, transcript, changelog, or scratchpad.

Keep only durable context needed to resume work after compaction, agent switches, or future sessions.

## Process

1. Read the target `MEMORY.md` fully.
2. Preserve useful YAML frontmatter keys such as `memory_version` and `threshold_bytes`.
3. Set frontmatter `status: clean` after a successful compaction.
4. Remove stale dirty fields such as `dirty_reason`, `size_bytes`, and `checked_at`.
5. Rewrite the body instead of appending to it.
6. Merge duplicate notes and delete stale completed-task details.
7. Prefer pointers to authoritative sources over copied detail: files, line references, ADRs, docs, issues, commits, and commands that can be rerun.
8. If memory conflicts with current repository state, tool output, or direct user instruction, trust current evidence and update memory accordingly.

## Target structure

Use this structure unless the existing memory has a clearly better compact structure:

```md
---
memory_version: 1
status: clean
threshold_bytes: 8192
last_compacted_at: <iso timestamp>
---

# <Build or Design> Memory

## Current Focus
- ...

## Durable Constraints
- ...

## Open Threads
- ...

## Handoffs
- ...

## Pointers
- ...
```

## Size discipline

- Aim for under 120 lines and under the configured threshold, usually 8 KB.
- If everything remaining is genuinely durable and the file still exceeds the threshold, keep the important information, set `status: clean`, and mention the unavoidable reason briefly under `Open Threads` or `Pointers`.
- Never create a second historical log inside memory. If historical detail matters, point to commits, ADRs, docs, issues, or `docs/wip.md`.

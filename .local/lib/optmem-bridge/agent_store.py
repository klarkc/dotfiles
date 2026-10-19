#!/usr/bin/env python3
"""Agent-authorized OptMem store: one writer ledger, no second relevance model.

Source/harness authority must be bound by a trusted tool adapter, not model args.
Only synthetic/staging memory directories accepted by the upstream writer.
"""
from __future__ import annotations
from contextlib import contextmanager, closing
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import sqlite3
import sys

WRITER_DIR=Path(__file__).resolve().parent.parent/"optmem-bash"/"writer"
if str(WRITER_DIR) not in sys.path:
    sys.path.insert(0,str(WRITER_DIR))
import optmem_writer_isolated as writer

ALLOWED=frozenset(("opencode","zeroclaw"))
KEY_RE=re.compile(r"[a-z0-9][a-z0-9._/-]{2,89}")
REQUEST_RE=re.compile(r"[a-z0-9][a-z0-9_-]{7,79}")


class Conflict(ValueError):
    """A reused logical key or request ID refers to different contents."""


def normalized_digest(text: str) -> str:
    return writer.checksum(writer.normalized_memory_text(text).encode())


def validate(harness: str, logical_key: str, text: str, request_id: str) -> dict:
    if harness not in ALLOWED:
        raise ValueError("Unsupported trusted tool adapter")
    if not isinstance(logical_key,str) or not KEY_RE.fullmatch(logical_key):
        raise ValueError("A stable, shared semantic key is required")
    if not isinstance(request_id,str) or not REQUEST_RE.fullmatch(request_id):
        raise ValueError("Invalid idempotency request ID")
    digest=normalized_digest(text) if isinstance(text,str) else ""
    context={
        "source":"agent-explicit-store",
        "harness":harness,
        "memory_key":logical_key,
        "normalized_sha256":digest,
    }
    writer.safe_input("agent-memory:"+logical_key,text,context)
    return context


@contextmanager
def agent_lock(sidecar: Path):
    sidecar=writer.private_dir(sidecar)
    path=sidecar/"agent-store.lock"
    if path.is_symlink():
        raise ValueError("Unsafe agent lock path")
    fd=os.open(path,os.O_CREAT|os.O_RDWR|os.O_CLOEXEC,0o600)
    try:
        fcntl.flock(fd,fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd,fcntl.LOCK_UN)
        os.close(fd)


def agent_connection(sidecar: Path) -> sqlite3.Connection:
    sidecar=writer.private_dir(sidecar)
    file=sidecar/"agent-facts.sqlite3"
    if file.is_symlink():
        raise ValueError("Unsafe agent SQLite path")
    if file.exists() and (file.stat().st_mode&0o077 or file.stat().st_uid!=os.getuid()):
        raise ValueError("Insecure agent fact index")
    con=sqlite3.connect(file,timeout=15)
    con.row_factory=sqlite3.Row
    con.execute("PRAGMA synchronous=FULL")
    con.executescript("""
       CREATE TABLE IF NOT EXISTS agent_keys (
          logical_key TEXT PRIMARY KEY,
          normalized_sha256 TEXT NOT NULL,
          memory_index INTEGER NOT NULL,
          first_harness TEXT NOT NULL
       );
       CREATE INDEX IF NOT EXISTS idx_agent_normalized
          ON agent_keys(normalized_sha256);
       CREATE TABLE IF NOT EXISTS agent_requests (
          harness TEXT NOT NULL,
          request_id TEXT NOT NULL,
          logical_key TEXT NOT NULL,
          normalized_sha256 TEXT NOT NULL,
          memory_index INTEGER,
          state TEXT NOT NULL CHECK(state IN ('PREPARED','COMMITTED')),
          PRIMARY KEY(harness,request_id)
       );
    """)
    os.chmod(file,0o600)
    return con


def verified_index(memory: Path, index: int, expected_digest: str) -> None:
    if index<0 or index>=writer.log_count(memory):
        raise RuntimeError("Agent fact index is beyond upstream OptMem log")
    raw=writer.record_at(memory,index).decode("utf-8").rstrip()
    m=re.fullmatch(r"#[0-9]+ 20[0-9]{2}-[0-9]{2}-[0-9]{2} (.+)",raw)
    if m is None or normalized_digest(m.group(1))!=expected_digest:
        raise RuntimeError("Source record drift or mismatched fact index")


def request_store(memory: Path, sidecar: Path, harness: str,
                  logical_key: str, text: str, request_id: str,
                  failpoint: str | None=None) -> dict:
    """Automatically admit one deliberate agent memory or return prior index.

    Two independent adapter processes share a durable lock, writer sidecar and
    upstream LOG. Exact-normalized text + shared logical key are both identities.
    No optional model relevance review. Different text under a reused key is
    refused to avoid silently creating conflicting active memories.
    """
    context=validate(harness,logical_key,text,request_id)
    # Upstream memo.note strips whitespace. The writer's expected record must
    # reflect the exact upstream string or it would misdiagnose a valid write.
    text=text.strip()
    digest=context["normalized_sha256"]
    memory=writer.private_dir(memory)
    sidecar=writer.private_dir(sidecar)
    with agent_lock(sidecar):
        writer.verify(memory,sidecar)
        with closing(agent_connection(sidecar)) as con:
            request=con.execute("SELECT * FROM agent_requests WHERE harness=? AND request_id=?",
                                (harness,request_id)).fetchone()
            if request is not None:
                if request["logical_key"]!=logical_key or request["normalized_sha256"]!=digest:
                    raise Conflict("Idempotency request has different content or key")
                if request["state"]=="COMMITTED":
                    verified_index(memory,request["memory_index"],digest)
                    return {"status":"ALREADY_PRESENT","index":request["memory_index"]}
            else:
                # Do not reserve a rejected conflicting request. Otherwise its
                # PREPARED receipt could permanently poison a valid logical key.
                old_key=con.execute(
                    "SELECT normalized_sha256 FROM agent_keys WHERE logical_key=?",
                    (logical_key,)).fetchone()
                if old_key is not None and old_key["normalized_sha256"]!=digest:
                    raise Conflict("Logical memory key already has a different value")
                earlier=con.execute("""SELECT 1 FROM agent_requests
                    WHERE logical_key=? AND state='PREPARED'
                      AND normalized_sha256!=? LIMIT 1""",(logical_key,digest)).fetchone()
                if earlier:
                    raise Conflict("Pending conflicting source for logical key")
                old_ledger=writer.ledger_connection(sidecar)
                try:
                    bound=old_ledger.execute(
                        "SELECT entry_text FROM bindings WHERE source_key=?",
                        ("agent-memory:"+logical_key,)).fetchone()
                finally:
                    old_ledger.close()
                if bound is not None and normalized_digest(bound["entry_text"])!=digest:
                    raise Conflict("Historical writer binding differs for logical key")
                # Reserve the request BEFORE the upstream append. A crash can
                # never turn an altered replay into a second committed memory.
                with con:
                    con.execute("""INSERT INTO agent_requests
                      (harness,request_id,logical_key,normalized_sha256,memory_index,state)
                      VALUES (?,?,?,?,NULL,'PREPARED')""",
                      (harness,request_id,logical_key,digest))
                if failpoint=="after_request_prepare":
                    raise InterruptedError("Injected failure after durable request reservation")
            conflicting=con.execute("""SELECT 1 FROM agent_requests
                 WHERE logical_key=? AND state='PREPARED'
                   AND normalized_sha256!=? LIMIT 1""",(logical_key,digest)).fetchone()
            if conflicting:
                raise Conflict("Pending conflicting agent store at same logical key")
            row=con.execute("SELECT * FROM agent_keys WHERE logical_key=?",
                            (logical_key,)).fetchone()
            if row is not None:
                if row["normalized_sha256"]!=digest:
                    raise Conflict("Logical memory key already has a different value")
                verified_index(memory,row["memory_index"],digest)
                index=row["memory_index"]
                status="ALREADY_PRESENT"
            else:
                ledger=writer.ledger_connection(sidecar)
                try:
                    existing=ledger.execute("SELECT * FROM bindings WHERE source_key=?",
                                            ("agent-memory:"+logical_key,)).fetchone()
                finally:
                    ledger.close()
                if existing is not None:
                    if normalized_digest(existing["entry_text"])!=digest:
                        raise Conflict("Conflicting historical memory key reservation")
                    # Recover a PREPARED upstream write by its ORIGINAL authenticated
                    # binding, not by a new competing caller's metadata.
                    if existing["state"]=="PREPARED":
                        original_context=json.loads(existing["source_context_json"])
                        result=writer.commit(memory,sidecar,"agent-memory:"+logical_key,
                                             existing["entry_text"],original_context)
                        index=result["index"]
                    else:
                        index=existing["expected_index"]
                    verified_index(memory,index,digest)
                    status="ALREADY_PRESENT"
                else:
                    by_digest=con.execute("SELECT memory_index FROM agent_keys WHERE normalized_sha256=? LIMIT 1",
                                          (digest,)).fetchone()
                    if by_digest is not None:
                        index=by_digest["memory_index"]
                        verified_index(memory,index,digest)
                        status="ALREADY_PRESENT"
                    else:
                        result=writer.commit(memory,sidecar,"agent-memory:"+logical_key,
                                             text,context,failpoint=failpoint if failpoint in ("after_prepare","after_note") else None)
                        index=result["index"]
                        status=("STORED" if result["outcome"]=="COMMITTED"
                                else "ALREADY_PRESENT")
                    if failpoint=="after_writer_before_index":
                        raise InterruptedError("Injected failure before durable agent receipt")
            with con:
                # One transaction binds the logical key and final request
                # receipt. A crash before it will recover from writer bindings
                # or from the canonical content scan, never append again.
                if row is None:
                    con.execute("""INSERT INTO agent_keys
                         (logical_key,normalized_sha256,memory_index,first_harness)
                         VALUES (?,?,?,?)""",(logical_key,digest,index,harness))
                result=con.execute("""UPDATE agent_requests
                   SET memory_index=?, state='COMMITTED'
                   WHERE harness=? AND request_id=? AND state='PREPARED'
                     AND logical_key=? AND normalized_sha256=?""",
                   (index,harness,request_id,logical_key,digest))
                if result.rowcount!=1:
                    raise RuntimeError("Request receipt precondition changed")
        return {"status":status,"index":index}


def lookup_key(memory: Path, sidecar: Path, logical_key: str) -> dict:
    """Lookup a shared logical identity, verifying its upstream record first.

    Return only key, index, and the actual memorized text. Never infer an
    absent key from similar phrases; callers can recall before choosing one.
    """
    if not isinstance(logical_key, str) or not KEY_RE.fullmatch(logical_key):
        raise ValueError("Invalid memory key")
    memory = writer.private_dir(memory)
    sidecar = writer.private_dir(sidecar)
    with agent_lock(sidecar):
        writer.verify(memory, sidecar)
        with closing(agent_connection(sidecar)) as con:
            row = con.execute(
                "SELECT normalized_sha256,memory_index FROM agent_keys WHERE logical_key=?",
                (logical_key,),
            ).fetchone()
            if row is None:
                return {"status": "NOT_FOUND", "memory_key": logical_key}
            index = row["memory_index"]
            verified_index(memory, index, row["normalized_sha256"])
            raw = writer.record_at(memory, index).decode("utf-8").rstrip()
            match = re.fullmatch(r"#[0-9]+ 20[0-9]{2}-[0-9]{2}-[0-9]{2} (.+)", raw)
            if match is None:
                raise RuntimeError("Invalid referenced upstream record")
            return {"status": "FOUND", "memory_key": logical_key,
                    "index": index, "text": match.group(1)}



MAX_SEARCH_ROWS = 8192
MAX_SEARCH_RECORDS = 65536
MAX_SEARCH_RESULTS = 8
SEARCH_RE = re.compile(r"[\w ./-]{2,64}", re.UNICODE)


def search_memories(memory: Path, sidecar: Path, query: str) -> dict:
    """Bounded global discovery backed exclusively by the audited upstream LOG.

    Agent memories expose stable keys; preexisting Bash/imported memories have
    no key until an agent binds identical text via the existing shared writer.
    Discovery is lexical and advisory, not an LLM approval or dedup assertion.
    """
    if (not isinstance(query, str) or not SEARCH_RE.fullmatch(query)
            or len(query.encode("utf-8")) > 128):
        raise ValueError("Invalid memory search term")
    normalized = writer.normalized_memory_text(query)
    words = re.findall(r"\w+", normalized, re.UNICODE)
    if not words:
        raise ValueError("A nonempty literal search term is required")
    memory = writer.private_dir(memory)
    sidecar = writer.private_dir(sidecar)
    with agent_lock(sidecar):
        writer.verify(memory, sidecar)
        count = writer.log_count(memory)
        if count > MAX_SEARCH_RECORDS:
            raise RuntimeError("Too many upstream entries for bounded search")
        with closing(agent_connection(sidecar)) as con:
            rows = con.execute(
                "SELECT logical_key,normalized_sha256,memory_index "
                "FROM agent_keys ORDER BY logical_key LIMIT ?",
                (MAX_SEARCH_ROWS + 1,),
            ).fetchall()
            if len(rows) > MAX_SEARCH_ROWS:
                raise RuntimeError("Too many logical aliases for bounded search")
            by_index: dict[int, list[str]] = {}
            for row in rows:
                index = row["memory_index"]
                verified_index(memory, index, row["normalized_sha256"])
                by_index.setdefault(index, []).append(row["logical_key"])
        candidates = []
        for index in range(count):
            raw = writer.record_at(memory, index).decode("utf-8").rstrip()
            match = re.fullmatch(r"#[0-9]+ 20[0-9]{2}-[0-9]{2}-[0-9]{2} (.+)", raw)
            if match is None:
                raise RuntimeError("Invalid upstream memory record")
            text = match.group(1)
            aliases = by_index.get(index, [])
            key_terms = " ".join(key.replace("/", " ").replace(".", " ")
                                 .replace("-", " ").replace("_", " ")
                                 for key in aliases)
            haystack = writer.normalized_memory_text(text + " " + key_terms)
            if not all(word in haystack for word in words):
                continue
            phrase = " ".join(words)
            key_hits = sum(word in key_terms for word in words)
            score = 10 * (phrase in haystack) + 4 * key_hits + len(words)
            candidates.append((score, index, {
                "index": index, "text": text,
                "memory_key": aliases[0] if aliases else None,
                "alias_count": len(aliases),
                "aliases": aliases[:8],
            }))
        candidates.sort(key=lambda item: (-item[0], item[1]))
        results = [item[2] for item in candidates[:MAX_SEARCH_RESULTS]]
        # Fail closed if a foreign note or drift appeared while scanning; Bash
        # and agent writers serialize separately through the writer ledger.
        writer.verify(memory, sidecar)
        return {"status": "FOUND" if results else "NOT_FOUND",
                "results": results,
                "truncated": len(candidates) > MAX_SEARCH_RESULTS}


def stable_request_id(harness: str, logical_key: str, text: str) -> str:
    """Tool adapter derives replay identity; caller need not invent one."""
    digest=normalized_digest(text)
    return hashlib.sha256((harness+"\0"+logical_key+"\0"+digest).encode()).hexdigest()[:40]

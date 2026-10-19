#!/usr/bin/env python3
"""Crash-recoverable provenance binding for upstream memo: sandbox-only prototype.

Not an OptMem fork. Source of semantic content is memo LOG.txt; SQLite is a
private provenance/outbox sidecar, not another semantic memory store.
"""
from __future__ import annotations
import fcntl
from contextlib import contextmanager
from datetime import date, datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import sqlite3
import subprocess
import sys
import unicodedata

HERE=Path(__file__).resolve().parent
MEMO=Path.home()/".local/state/optmem-prototype-2026-10-08/memo"
MEMO_SHA256="3dc120d01be3115ef6267eab4103e7909fc830d6227b549f20991ba999ee9ffb"
LOG_REC=320
MAX_ENTRY_BYTES=280
RISK=re.compile(r"(?i)(password|private.key|access.token|bearer|authorization|secret|api.key|sk-[A-Za-z0-9]{10,})")
AGENT_RISK=re.compile(r"(?i)\b(?:token|password|secret|bearer|authorization|private.key|api.key)\b")

def checksum(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()

def normalized_memory_text(entry: str) -> str:
    """Deterministic duplicate identity; preserve semantic punctuation."""
    text=unicodedata.normalize("NFKC",entry).casefold()
    text=" ".join(text.split())
    return text.strip(" .,!?:;")

def duplicate_memory_index(memory: Path, entry: str) -> int | None:
    """Find existing exact-normalized text across baseline, Bash and agents.

    Must be called while writer_lock is held, after verifying provenance.
    This linear scan is conservative and handles preexisting raw-log records.
    """
    identity=normalized_memory_text(entry)
    for idx in range(log_count(memory)):
        raw=record_at(memory,idx).decode("utf-8").rstrip()
        m=re.fullmatch(r"#[0-9]+ 20[0-9]{2}-[0-9]{2}-[0-9]{2} (.+)",raw)
        if m is None:
            raise RuntimeError("Malformed historical record; refuse duplicate check")
        if normalized_memory_text(m.group(1))==identity:
            return idx
    return None

def fmt(obj) -> str:
    return json.dumps(obj,ensure_ascii=False,sort_keys=True,separators=(",",":"))

def private_dir(path: Path) -> Path:
    # Never permit an accidental import into ~/.optmem/memory from this prototype.
    if path.is_symlink() or not path.resolve().is_relative_to(HERE):
        raise ValueError("Writer only supports isolated private fixtures")
    path.mkdir(mode=0o700,parents=True,exist_ok=True)
    if path.stat().st_mode & 0o077:
        raise ValueError("Writer directory permissions are unsafe")
    return path

@contextmanager
def writer_lock(sidecar: Path):
    private_dir(sidecar)
    p=sidecar/"writer.lock"
    if p.is_symlink():
        raise ValueError("Unsafe lock symlink")
    fd=os.open(p,os.O_CREAT | os.O_RDWR | os.O_CLOEXEC,0o600)
    try:
        fcntl.flock(fd,fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd,fcntl.LOCK_UN)
        os.close(fd)

def ledger_connection(sidecar: Path):
    private_dir(sidecar)
    db=sidecar/"provenance.sqlite3"
    if db.is_symlink():raise ValueError("Unsafe SQLite symlink")
    con=sqlite3.connect(db,timeout=15)
    con.row_factory=sqlite3.Row
    con.execute("PRAGMA synchronous=FULL")
    con.executescript("""
        CREATE TABLE IF NOT EXISTS bindings (
          source_key TEXT PRIMARY KEY,
          state TEXT NOT NULL CHECK (state IN ('PREPARED','COMMITTED')),
          expected_index INTEGER NOT NULL,
          expected_day TEXT NOT NULL,
          entry_text TEXT NOT NULL,
          entry_sha256 TEXT NOT NULL,
          source_context_json TEXT NOT NULL,
          prepared_utc TEXT NOT NULL,
          committed_utc TEXT
        );
        CREATE UNIQUE INDEX IF NOT EXISTS idx_one_binding_per_index
            ON bindings(expected_index);
        CREATE TABLE IF NOT EXISTS baseline (
          id INTEGER PRIMARY KEY CHECK(id=1),
          initial_log_count INTEGER NOT NULL,
          initial_log_sha256 TEXT NOT NULL,
          registered_utc TEXT NOT NULL
        );
    """)
    os.chmod(db,0o600)
    return con


def enroll_historical_baseline(memory: Path, sidecar: Path,
                               expected_sha256: str, expected_count: int) -> dict:
    """Operator-only, explicit staging enrollment of an existing legacy LOG.

    This is intentionally not exposed through MCP or the writer IPC. The hash
    and count must come from a separately audited historical source manifest.
    It does not grant authority to activate the production memory directory.
    """
    if not isinstance(expected_sha256, str) or not re.fullmatch(r"[a-f0-9]{64}",expected_sha256):
        raise ValueError("An audited legacy LOG digest is required")
    if type(expected_count) is not int or expected_count < 1:
        raise ValueError("An explicit positive record count is required")
    memory=private_dir(memory)
    sidecar=private_dir(sidecar)
    with writer_lock(sidecar):
        logfile=log_path(memory)
        if logfile.stat().st_uid != os.getuid() or logfile.stat().st_mode&0o077:
            raise ValueError("Unsafe legacy OptMem LOG ownership or permissions")
        data=logfile.read_bytes()
        if (len(data) != expected_count * LOG_REC
                or checksum(data) != expected_sha256):
            raise RuntimeError("Historical baseline hash/count differs from approved manifest")
        for index in range(expected_count):
            raw=data[index*LOG_REC:(index+1)*LOG_REC].decode("utf-8").rstrip()
            if re.fullmatch(rf"#{index} 20[0-9]{{2}}-[0-9]{{2}}-[0-9]{{2}} .+",raw) is None:
                raise RuntimeError("Malformed legacy OptMem baseline record")
        con=ledger_connection(sidecar)
        try:
            if (con.execute("SELECT 1 FROM baseline LIMIT 1").fetchone()
                or con.execute("SELECT 1 FROM bindings LIMIT 1").fetchone()):
                raise RuntimeError("Baseline already registered or writer provenance exists")
            with con:
                con.execute("INSERT INTO baseline VALUES (1,?,?,?)",
                    (expected_count,expected_sha256,datetime.now(timezone.utc).isoformat()))
            verify_bindings(memory,con)
        finally:
            con.close()
        return {"baseline_records":expected_count,"baseline_sha256":expected_sha256}


def log_path(memory: Path) -> Path:
    p=memory/"LOG.txt"
    if p.is_symlink() or not p.is_file():
        raise ValueError("Not an initialized OptMem LOG")
    return p

def log_count(memory: Path) -> int:
    p=log_path(memory)
    size=p.stat().st_size
    if size%LOG_REC:
        raise RuntimeError("Partial OptMem record: stop and use upstream recovery")
    return size//LOG_REC

def expected_record(idx: int, day: str, entry: str) -> bytes:
    raw=f"#{idx} {day} {entry}".encode()
    if len(raw)>LOG_REC-1:raise ValueError("Entry too large for fixed-width record")
    return raw+b" "*(LOG_REC-1-len(raw))+b"\n"

def record_at(memory: Path, idx: int) -> bytes:
    with log_path(memory).open("rb") as f:
        f.seek(idx*LOG_REC)
        return f.read(LOG_REC)

def verify_bindings(memory: Path, con: sqlite3.Connection) -> int:
    """An out-of-band append, missing baseline or changed prefix fails closed."""
    n=log_count(memory)
    base=con.execute("SELECT * FROM baseline WHERE id=1").fetchone()
    if base is None:
        if con.execute("SELECT COUNT(*) FROM bindings").fetchone()[0]:
            raise RuntimeError("Missing baseline on nonempty provenance ledger")
        if n:
            # An absent sidecar after restore must NOT silently turn controlled
            # records into an unaudited imported baseline. Nonempty legacy
            # enrollment requires an explicit, hash-pinned operator procedure.
            raise RuntimeError("Nonempty OptMem LOG requires explicit baseline enrollment")
        # Empty newly initialized fixtures can be enrolled automatically.
        blob=log_path(memory).read_bytes()
        if len(blob)!=n*LOG_REC:
            raise RuntimeError("Unable to record consistent initial baseline")
        with con:
            con.execute("INSERT INTO baseline VALUES (1,?,?,?)",
                (n,checksum(blob),datetime.now(timezone.utc).isoformat()))
        base=con.execute("SELECT * FROM baseline WHERE id=1").fetchone()
    baseline_count=base["initial_log_count"]
    with log_path(memory).open("rb") as f:
        prefix=f.read(baseline_count*LOG_REC)
    if len(prefix)!=baseline_count*LOG_REC or checksum(prefix)!=base["initial_log_sha256"]:
        raise RuntimeError("Preexisting OptMem baseline changed")
    bound=list(con.execute("SELECT * FROM bindings ORDER BY expected_index"))
    if [r["expected_index"] for r in bound]!=list(range(baseline_count,baseline_count+len(bound))):
        raise RuntimeError("Non-contiguous controlled OptMem indices")
    pending=[r for r in bound if r["state"]=="PREPARED"]
    if len(pending)>1 or (pending and pending[0]["expected_index"]!=baseline_count+len(bound)-1):
        raise RuntimeError("Ambiguous multiple pending notes")
    committed=0
    for rec in bound:
        if checksum(rec["entry_text"].encode())!=rec["entry_sha256"]:
            raise RuntimeError("Corrupt provenance entry digest")
        if rec["state"]!="COMMITTED":
            continue
        idx=rec["expected_index"]
        if idx>=n:
            raise RuntimeError("Committed provenance exceeds OptMem log")
        want=expected_record(idx,rec["expected_day"],rec["entry_text"])
        if record_at(memory,idx)!=want:
            raise RuntimeError("OptMem record does not match provenance")
        committed+=1
    complete_count=baseline_count+committed
    if pending:
        if n not in (complete_count,complete_count+1):
            raise RuntimeError("Out-of-band OptMem writes around prepared entry")
    elif n!=complete_count:
        raise RuntimeError("Out-of-band OptMem write or truncation: fail closed")
    return committed

def safe_input(source_key: str, entry: str, source_context: dict):
    if isinstance(source_context,dict) and source_context.get("source")=="agent-explicit-store":
        if set(source_context)!={"source","harness","memory_key","normalized_sha256"}:
            raise ValueError("Unexpected agent provenance fields")
        key=source_context["memory_key"]
        if not isinstance(key,str) or not re.fullmatch(r"[a-z0-9][a-z0-9._/-]{2,89}",key):
            raise ValueError("Invalid stable agent memory key")
        if source_key!="agent-memory:"+key:
            raise ValueError("Agent source key differs from stable logical identity")
        if source_context["harness"] not in ("opencode","zeroclaw"):
            raise ValueError("Unrecognized authorized agent")
        if source_context["normalized_sha256"]!=checksum(
                normalized_memory_text(entry).encode()):
            raise ValueError("Agent normalized content digest mismatch")
        if (not isinstance(entry,str) or "\n" in entry or "\r" in entry
                or RISK.search(entry) or AGENT_RISK.search(entry) or not 12<=len(entry.encode())<=MAX_ENTRY_BYTES):
            raise ValueError("Invalid or sensitive agent memory")
        return
    if not re.fullmatch(r"bash-observation:[a-z0-9_-]+(?: [a-z0-9_-]+)?",source_key):
        raise ValueError("Unsupported source key")
    if "\n" in entry or "\r" in entry or RISK.search(entry) or not 10<=len(entry.encode())<=MAX_ENTRY_BYTES:
        raise ValueError("Unsafe entry")
    if not isinstance(source_context,dict) or source_context.get("source")!="bash-sanitized-feature":
        raise ValueError("Missing scoped Bash source provenance")
    proof=source_context.get("observations")
    if not isinstance(proof,list) or not 6<=len(proof)<=10:
        raise ValueError("Expected 6-10 live evidence references")
    days=set()
    for row in proof:
        if not isinstance(row,dict) or not re.fullmatch(r"[a-f0-9]{64}",row.get("command_hmac","")):
            raise ValueError("Missing opaque source fingerprint")
        if not isinstance(row.get("byte_offset"),int) or row["byte_offset"]<0:
            raise ValueError("Invalid source offset")
        day=row.get("observed_day","")
        if not re.fullmatch(r"20[0-9]{2}-[0-9]{2}-[0-9]{2}",day):
            raise ValueError("Missing source observation day")
        days.add(day)
    if len(days)<3:
        raise ValueError("Insufficient independent live observation days")
    if not re.fullmatch(r"[a-f0-9]{64}",source_context.get("evidence_hmac","")):
        raise ValueError("Provenance aggregate HMAC missing")
    if len(fmt(source_context).encode())>12000:
        raise ValueError("Provenance excessive")

def upstream_memo_note(memory: Path, entry: str) -> int:
    if checksum(MEMO.read_bytes())!=MEMO_SHA256:
        raise RuntimeError("Memo backend hash no longer matches audited revision")
    env=os.environ.copy()
    env["MEMORY_DIR"]=str(memory)
    env["PYTHONDONTWRITEBYTECODE"]="1"
    # Do not inherit a Nix/interactive shell umask: upstream memo may create
    # TREE, configuration or lock artifacts. All must remain private.
    result=subprocess.run([sys.executable,"-B",str(MEMO),"note",entry],env=env,
                          capture_output=True,text=True,timeout=20,umask=0o077)
    if result.returncode:
        raise RuntimeError("Upstream note failed; inspect private staging state")
    match=re.search(r"Saved as #([0-9]+)\.",result.stdout)
    if match is None:
        raise RuntimeError("Unrecognized upstream note confirmation")
    return int(match.group(1))

def pending_resolution(memory: Path, con: sqlite3.Connection):
    """Reconcile PREPARED notes after crashes; never silently append or truncate."""
    n=log_count(memory)
    issues=[]
    for rec in con.execute("SELECT * FROM bindings WHERE state='PREPARED' ORDER BY expected_index"):
        idx=rec["expected_index"]
        if idx>=n:
            if idx!=n:
                raise RuntimeError("Source log moved backwards; pending record cannot be replayed")
            issues.append(rec["source_key"])
            continue
        want=expected_record(idx,rec["expected_day"],rec["entry_text"])
        if record_at(memory,idx)!=want:
            raise RuntimeError("Ambiguous out-of-band append at reserved index")
        with con:
            con.execute("UPDATE bindings SET state='COMMITTED', committed_utc=? WHERE source_key=?",
                        (datetime.now(timezone.utc).isoformat(),rec["source_key"]))
    return issues

def commit(memory: Path, sidecar: Path, source_key: str, entry: str,
           source_context: dict, failpoint: str | None=None) -> dict:
    """One sidecar-prepared note with exact-index reconciliation; sandbox only."""
    memory=private_dir(memory)
    sidecar=private_dir(sidecar)
    safe_input(source_key,entry,source_context)
    if checksum(MEMO.read_bytes())!=MEMO_SHA256:
        raise RuntimeError("Memo upstream source changed")
    log_path(memory)
    with writer_lock(sidecar):
        con=ledger_connection(sidecar)
        try:
            verify_bindings(memory,con)
            unresolved=pending_resolution(memory,con)
            if any(key!=source_key for key in unresolved):
                raise RuntimeError("Another prepared write is awaiting resolution")
            row=con.execute("SELECT * FROM bindings WHERE source_key=?",(source_key,)).fetchone()
            if row is not None:
                if row["entry_text"]!=entry or row["source_context_json"]!=fmt(source_context):
                    raise RuntimeError("Same source key conflicts with previous decision")
                if row["state"]=="COMMITTED":
                    verify_bindings(memory,con)
                    return {"outcome":"ALREADY_COMMITTED","index":row["expected_index"]}
                idx=row["expected_index"]
                day=row["expected_day"]
                if day!=date.today().isoformat():
                    raise RuntimeError("Pending note date differs from current local date")
                if log_count(memory)!=idx:
                    raise RuntimeError("Unexpected log movement before retry")
            else:
                if unresolved:
                    raise RuntimeError("Cannot append while another write is unresolved")
                existing_index=duplicate_memory_index(memory,entry)
                if existing_index is not None:
                    return {"outcome":"ALREADY_PRESENT","index":existing_index}
                idx=log_count(memory)
                day=date.today().isoformat()
                with con:
                    con.execute("""INSERT INTO bindings
                        (source_key,state,expected_index,expected_day,entry_text,
                        entry_sha256,source_context_json,prepared_utc)
                        VALUES (?,'PREPARED',?,?,?,?,?,?)""",
                        (source_key,idx,day,entry,checksum(entry.encode()),fmt(source_context),
                         datetime.now(timezone.utc).isoformat()))
            if failpoint=="after_prepare":
                raise InterruptedError("Injected interruption after durable PREPARED")
            assigned=upstream_memo_note(memory,entry)
            if failpoint=="after_note":
                raise InterruptedError("Injected interruption after durable memo append")
            if assigned!=idx or log_count(memory)!=idx+1:
                raise RuntimeError("Unexpected index or concurrent writer: fail closed")
            if record_at(memory,idx)!=expected_record(idx,day,entry):
                raise RuntimeError("Provenance does not match actual OptMem bytes")
            with con:
                con.execute("UPDATE bindings SET state='COMMITTED',committed_utc=? WHERE source_key=?",
                            (datetime.now(timezone.utc).isoformat(),source_key))
            verify_bindings(memory,con)
            return {"outcome":"COMMITTED","index":idx}
        finally:
            con.close()

def verify(memory: Path, sidecar: Path) -> dict:
    memory=private_dir(memory)
    sidecar=private_dir(sidecar)
    with writer_lock(sidecar):
        con=ledger_connection(sidecar)
        try:
            recovered=pending_resolution(memory,con)
            count=verify_bindings(memory,con)
            return {"committed_provenance":count,"unresolved_prepared":len(recovered),
                    "memory_entries":log_count(memory)}
        finally:
            con.close()

def recall_existing(memory: Path, feature: str) -> bool:
    if not re.fullmatch(r"[a-z0-9_-]+(?: [a-z0-9_-]+)?(?: usage)?",feature):
        raise ValueError("Unsafe recall term")
    env=os.environ.copy()
    env["MEMORY_DIR"]=str(memory)
    env["PYTHONDONTWRITEBYTECODE"]="1"
    p=subprocess.run([sys.executable,"-B",str(MEMO),"recall",re.escape(feature)],env=env,
                    text=True,capture_output=True,timeout=12)
    if p.returncode:
        raise RuntimeError("OptMem recall failed: defer write")
    return feature.casefold() in p.stdout.casefold()

def commit_ready_bash_observation(memory: Path, sidecar: Path, source_state: Path,
                                  feature: str, failpoint: str | None=None) -> dict:
    """Read a classified candidate, verify cryptographic source lineage, then commit."""
    if not re.fullmatch(r"[a-z0-9_-]+(?: [a-z0-9_-]+)?(?: usage)?",feature):
        raise ValueError("Unsupported feature")
    source_state=private_dir(source_state)
    db=source_state/"capture.sqlite3"
    keyfile=source_state/"fingerprint.key"
    if db.is_symlink() or keyfile.is_symlink() or not db.is_file() or not keyfile.is_file():
        raise RuntimeError("Missing private evidence storage")
    if keyfile.stat().st_mode&0o077 or db.stat().st_mode&0o077:
        raise RuntimeError("Unsafe evidence storage permissions")
    key=keyfile.read_bytes()
    if len(key)!=32:raise RuntimeError("Invalid source HMAC key")
    # Read source DB strictly read-only: the writer must not change collector state.
    con=sqlite3.connect("file:"+str(db)+"?mode=ro",uri=True,timeout=10)
    con.row_factory=sqlite3.Row
    try:
        row=con.execute("SELECT * FROM decisions WHERE feature=?",(feature,)).fetchone()
        if row is None or row["status"] not in {
                "READY_FOR_WRITER_NOT_ACTIVATED",
                "RECEIPT_ACKNOWLEDGED_TO_ISOLATED_OPT_MEM"}:
            raise RuntimeError("No eligible scoped Bash observation")
        original=json.loads(row["provenance_json"])
        if not isinstance(original,list):raise RuntimeError("Malformed original proof")
        # Validate the exact *frozen* candidate proof, not the current newest
        # ten observations: later Bash activity must never invalidate earlier
        # valid references. Missing/changed/expired proof still fails closed.
        if not 6<=len(original)<=10 or len({r["observed_day"] for r in original})<3:
            raise RuntimeError("Insufficient frozen source-day corroboration")
        seen=set()
        for item in original:
            marker=(item["epoch"],item["byte_offset"],item["command_hmac"],item["observed_day"])
            if marker in seen:
                raise RuntimeError("Duplicate source proof reference")
            seen.add(marker)
            found=con.execute(
                "SELECT 1 FROM observations WHERE feature=? AND source_quality='live_append' "
                "AND epoch=? AND byte_offset=? AND command_hmac=? AND observed_day=? LIMIT 1",
                (feature,*marker)).fetchone()
            if found is None:
                raise RuntimeError("Frozen source proof expired or changed")
        import hmac
        sha=hmac.new(key,fmt({"feature":feature,"proof":original}).encode(),hashlib.sha256).hexdigest()
        if not hmac.compare_digest(sha,row["evidence_hmac"]):
            raise RuntimeError("Source proof HMAC mismatch")
        if row["model_id"] in ("", "unknown"):
            raise RuntimeError("No attributable model decision")
        text=row["statement"]
        model=row["model_id"]
        if f": {feature};" not in text:
            raise RuntimeError("Text no longer matches sanitized feature")
        proof={"source":"bash-sanitized-feature","feature":feature,
            "evidence_hmac":sha,"model_id":model,"rule_id":row["reason_code"],
            "observations":original}
        safe_input("bash-observation:"+feature,text,proof)
    finally:
        con.close()
    # Both stages are safely below the isolated writer's fixture root.
    # The reader duplicate lookup is also performed at admission time by the agent.
    if recall_existing(memory,feature):
        # An existing commit needs to reconcile, not be counted as an independent duplicate.
        if verify(memory,sidecar)["committed_provenance"]>0:
            ledger=ledger_connection(sidecar)
            try:
                existing=ledger.execute("SELECT state FROM bindings WHERE source_key=?",
                                        ("bash-observation:"+feature,)).fetchone()
                if existing:
                    return {"outcome":"ALREADY_COMMITTED"}
            finally:
                ledger.close()
        return {"outcome":"SKIPPED_DUPLICATE_IN_SHARED_MEMORY"}
    return commit(memory,sidecar,"bash-observation:"+feature,text,proof,failpoint)

def _commit_and_acknowledge_locked(memory: Path, sidecar: Path, source_state: Path,
                                   feature: str, failpoint: str | None=None) -> dict:
    """Stage-only at-least-once queue receipt around exactly-once memo append.

    If execution crashes after memo commit but before SQLite receipt, retry
    finds the upstream binding, and finishes the ACK without duplicating data.
    """
    if failpoint not in {None,"after_writer_before_receipt",
                         "after_prepare","after_note"}:
        raise ValueError("Unknown test-only fault injection")
    # Refuse a second memory root when the same source feature has already
    # been acknowledged elsewhere; do this BEFORE any upstream append.
    source_state=private_dir(source_state)
    memory=private_dir(memory)
    queue_db=source_state/"capture.sqlite3"
    if queue_db.is_symlink() or not queue_db.is_file():
        raise RuntimeError("Unsafe or missing source queue")
    prior_db=sqlite3.connect("file:"+str(queue_db)+"?mode=ro",uri=True,timeout=10)
    try:
        table=prior_db.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='writer_receipts'"
        ).fetchone()
        if table:
            prior=prior_db.execute(
                "SELECT memory_root_sha256 FROM writer_receipts WHERE feature=?",
                (feature,)).fetchone()
            if prior and prior[0]!=checksum(str(memory.resolve()).encode()):
                raise RuntimeError("Source was previously materialized in a different memory")
    finally:
        prior_db.close()
    original_failpoint=failpoint if failpoint in {"after_prepare","after_note"} else None
    result=commit_ready_bash_observation(
        memory,sidecar,source_state,feature,failpoint=original_failpoint)
    if result["outcome"] not in {"COMMITTED","ALREADY_COMMITTED"}:
        return result
    if failpoint=="after_writer_before_receipt":
        raise InterruptedError("Injected failure after verified writer commit")
    source_key="bash-observation:"+feature
    memory=private_dir(memory)
    sidecar=private_dir(sidecar)
    source_state=private_dir(source_state)
    with writer_lock(sidecar):
        ledger=ledger_connection(sidecar)
        try:
            verify_bindings(memory,ledger)
            record=ledger.execute(
                "SELECT * FROM bindings WHERE source_key=?",(source_key,)).fetchone()
            if record is None or record["state"]!="COMMITTED":
                raise RuntimeError("Writer receipt has no committed source binding")
            index=record["expected_index"]
            expected=expected_record(index,record["expected_day"],record["entry_text"])
            if record_at(memory,index)!=expected:
                raise RuntimeError("Receipt points to mismatched memory content")
            receipt_sha=checksum(expected)
            memory_identity=checksum(str(memory.resolve()).encode())
            proof_digest=record["entry_sha256"]
        finally:
            ledger.close()
    db=source_state/"capture.sqlite3"
    if db.is_symlink() or not db.is_file():
        raise RuntimeError("Missing private source queue")
    con=sqlite3.connect(db,timeout=15)
    try:
        con.execute("PRAGMA synchronous=FULL")
        with con:
            con.execute("""CREATE TABLE IF NOT EXISTS writer_receipts(
                feature TEXT PRIMARY KEY,
                source_key TEXT NOT NULL,
                log_index INTEGER NOT NULL,
                memory_record_sha256 TEXT NOT NULL,
                memory_root_sha256 TEXT NOT NULL,
                source_entry_sha256 TEXT NOT NULL,
                acknowledged_utc TEXT NOT NULL
            )""")
            prev=con.execute(
                "SELECT * FROM writer_receipts WHERE feature=?",(feature,)).fetchone()
            if prev is not None:
                if (prev[1],prev[2],prev[3],prev[4],prev[5])!=(
                    source_key,index,receipt_sha,memory_identity,proof_digest):
                    raise RuntimeError("Conflicting previous durable outbox receipt")
            else:
                con.execute("""INSERT INTO writer_receipts
                  (feature,source_key,log_index,memory_record_sha256,
                    memory_root_sha256,source_entry_sha256,acknowledged_utc)
                  VALUES (?,?,?,?,?,?,?)""",(
                  feature,source_key,index,receipt_sha,memory_identity,proof_digest,
                  datetime.now(timezone.utc).isoformat()))
            row=con.execute(
                "SELECT status,statement FROM decisions WHERE feature=?",(feature,)).fetchone()
            if row is None or row[0] not in {
                    "READY_FOR_WRITER_NOT_ACTIVATED",
                    "RECEIPT_ACKNOWLEDGED_TO_ISOLATED_OPT_MEM"}:
                raise RuntimeError("Queue state changed unexpectedly before acknowledgment")
            if checksum(row[1].encode())!=proof_digest:
                raise RuntimeError("Source queue statement no longer matches writer receipt")
            con.execute("""UPDATE decisions
              SET status='RECEIPT_ACKNOWLEDGED_TO_ISOLATED_OPT_MEM'
              WHERE feature=?""",(feature,))
    finally:
        con.close()
    return {"outcome":"ALREADY_ACKNOWLEDGED" if result["outcome"]=="ALREADY_COMMITTED"
            else "COMMITTED_AND_ACKNOWLEDGED",
            "index":index,"receipt_sha256":receipt_sha}


@contextmanager
def source_acknowledgment_lock(source_state: Path):
    source_state=private_dir(source_state)
    lock=source_state/"writer-receipt.lock"
    if lock.is_symlink():
        raise ValueError("Unsafe writer receipt lock")
    fd=os.open(lock,os.O_CREAT | os.O_RDWR | os.O_CLOEXEC,0o600)
    try:
        fcntl.flock(fd,fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd,fcntl.LOCK_UN)
        os.close(fd)

def commit_and_acknowledge(memory: Path, sidecar: Path, source_state: Path,
                           feature: str, failpoint: str | None=None) -> dict:
    """Stage-only receipt protocol; serializes source acknowledgments."""
    with source_acknowledgment_lock(source_state):
        return _commit_and_acknowledge_locked(memory,sidecar,source_state,
                                              feature,failpoint)

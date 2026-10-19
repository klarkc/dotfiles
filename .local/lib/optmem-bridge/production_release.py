#!/usr/bin/env python3
"""Build production acceptance only from verified real runtime and canonical evidence."""
from __future__ import annotations
from contextlib import closing
import hashlib
import json
from pathlib import Path
import sqlite3
import subprocess

import agent_store as a
import production_activate as activate
import production_preflight as p
import production_snapshot as snap

HOME=Path.home()
STATE=HOME/".local/state/optmem-production"
MEMORY=HOME/".optmem/memory"
SIDECAR=STATE/"sidecar"
KEY="optmem.production.integration"
TEXT="OptMem production on solo098 uses one shared canonical writer"


def release():
    active=subprocess.run(["systemctl","--user","is-active","--quiet","zeroclaw.service"])
    if active.returncode:
        raise RuntimeError("ZeroClaw daemon not running with production configuration")
    if not (STATE/"ACTIVE.json").is_file():
        raise RuntimeError("Canonical activation marker missing")
    op_proof=STATE/"e2e-opencode/result.json"
    zc_proof=STATE/"e2e-zeroclaw/result.json"
    for proof in (op_proof,zc_proof):
        if not p._secure_file(proof):
            raise RuntimeError("Missing private production E2E evidence")
    op=json.loads(op_proof.read_text())
    zc=json.loads(zc_proof.read_text())
    if not all([op.get("production") is True,
                op.get("server_healthy") is True,
                op.get("provider_request_seen") is True,
                op.get("model_requested_store") is True,
                op.get("tool_result_seen") is True,
                zc.get("production") is True,
                zc.get("zero_claw_exit")==0,
                zc.get("provider_tool_schema_seen") is True,
                zc.get("model_requested_tool") is True,
                zc.get("actual_tool_result_returned") is True,
                zc.get("last_stdout_contains_found") is True]):
        raise RuntimeError("Real model-to-MCP production evidence incomplete")
    # Independent OpenCode/ZeroClaw tool sessions must share one authenticated
    # upstream record and two committed idempotency receipts, across processes.
    target=a.lookup_key(MEMORY,SIDECAR,KEY)
    if target.get("status")!="FOUND" or target.get("text")!=TEXT:
        raise RuntimeError("Expected canonical integration memory not found")
    with closing(sqlite3.connect(f"file:{SIDECAR/'agent-facts.sqlite3'}?mode=ro",uri=True)) as con:
        rows=con.execute("SELECT harness,memory_index,state FROM agent_requests "
                         "WHERE logical_key=? ORDER BY harness",(KEY,)).fetchall()
        if rows!=[("opencode",target["index"],"COMMITTED"),
                 ("zeroclaw",target["index"],"COMMITTED")]:
            raise RuntimeError("Missing either authenticated agent write receipt")
    verified=a.writer.verify(MEMORY,SIDECAR)
    if verified["memory_entries"]<27 or not p._canonical_attestation(HOME):
        raise RuntimeError("Canonical LOG not fully provenance-attested")
    native_count,accounted=p._native_counts(HOME)
    if native_count<88 or accounted!=native_count:
        raise RuntimeError("Native archive accounting missing rows")
    backups=sorted((STATE/"snapshots").glob("prod-acceptance-*"))
    if not backups:
        raise RuntimeError("Production local backup not captured")
    restored=snap.restore_proof(backups[-1])
    if restored["records"]!=verified["memory_entries"]:
        raise RuntimeError("Canonical backup does not match current LOG")
    if subprocess.run(["systemctl","--user","is-active","--quiet",
                       "zeroclaw-private-config.service"]).returncode:
        # The config-generation service can be oneshot/inactive after a success.
        runtime=Path("/run/user")/str(__import__("os").getuid())/"zeroclaw-config/config.toml"
        if not runtime.is_file():
            raise RuntimeError("No installed ZeroClaw runtime config")
    # Evidence includes the existing full LOG as a prefix at release; later
    # legitimate, ledger-attested writes must not make release appear invalid.
    raw=(MEMORY/"LOG.txt").read_bytes()
    evidence={
        "schema":"OptMem-production-acceptance/v1",
        "canonical_record_count":len(raw)//320,
        "canonical_log_sha256":hashlib.sha256(raw).hexdigest(),
        "canonical_backed_up_and_restored":True,
        "durable_writer_test":True,
        "opencode_live_mcp_test":True,
        "zeroclaw_live_mcp_test":True,
        "native_writer_exclusivity_test":True,
    }
    file=STATE/"canonical-acceptance.json"
    activate.write_private(file,evidence)
    result={**p.decide(p.gather(HOME)[0])}
    if result["status"]!="READY":
        file.unlink()
        raise RuntimeError("Production gate rejected deployment")
    return {"status":"READY","canonical_records":len(raw)//320,
            "native_accounted":accounted,
            "mcp_receipts":["opencode","zeroclaw"],
            "snapshot_restored":True}


if __name__=="__main__":
    print(json.dumps(release(),sort_keys=True))

#!/usr/bin/env python3
"""Fail-closed, read-only production cutover gate for the shared OptMem writer.

No password/cookie access, no network operations, no memory mutations, no
service start/stop, no implicit consent or automatic activation.
"""
from __future__ import annotations
import argparse
from contextlib import closing
from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
import json
import re
from pathlib import Path
import sqlite3
import stat
import unicodedata
import tomllib


@dataclass(frozen=True)
class Evidence:
    historical_baseline_verified: bool
    canonical_writer_verified: bool
    canonical_local_recovery_verified: bool
    opencode_mcp_bound_and_tested: bool
    zeroclaw_mcp_bound_and_tested: bool
    legacy_memory_reconciled: bool
    exclusive_writer_authority: bool


CHECKS = {
    'historical_baseline_verified': 'Previously approved source and fixed-size records verify',
    'canonical_writer_verified': 'Real canonical writer has passed a durable append/restart test',
    'canonical_local_recovery_verified': 'Canonical LOG and both sidecars have a tested local restore',
    'opencode_mcp_bound_and_tested': 'OpenCode has a working production OptMem MCP',
    'zeroclaw_mcp_bound_and_tested': 'ZeroClaw has a working production OptMem MCP bundle',
    'legacy_memory_reconciled': 'All native ZeroClaw facts are migrated or explicitly retained',
    'exclusive_writer_authority': 'No other native/foreign memory writer remains active',
}


def decide(evidence: Evidence) -> dict:
    checks = {key:bool(getattr(evidence,key)) for key in CHECKS}
    blocked = [key for key, passed in checks.items() if not passed]
    return {'status': 'READY' if not blocked else 'BLOCKED', 'checks':checks,
            'unmet_gates':blocked, 'production_modified':False}


def _secure_file(path: Path) -> bool:
    try:
        info = path.lstat()
        return stat.S_ISREG(info.st_mode) and not path.is_symlink() and not info.st_mode & 0o077
    except OSError:
        return False


def _source_confirmed(home: Path) -> bool:
    directory = (home/'.local/state/optmem-migration-2026-10-09/global-staging-20261014'
                 '/g2b-persistent-20261014/g4-authorized-local-20261015/optmem-test-memory')
    consent = directory/'CONSENT.json'
    qa = directory/'BUILD-QA.json'
    provenance = directory/'PROVENANCE.jsonl'
    log = directory/'LOG.txt'
    journal = home/'.local/state/optmem-ingest-2026-10-09/events.jsonl'
    if not all(_secure_file(p) for p in (consent, qa, log, provenance, journal)):
        return False
    try:
        options = json.loads(consent.read_text())
        audit = json.loads(qa.read_text())
        original = log.read_bytes()
        records = [original[i*320:(i+1)*320] for i in range(26)]
        if any(re.fullmatch(rb'#'+str(i).encode()+rb' 20[0-9]{2}-[0-9]{2}-[0-9]{2} .+', r.rstrip()) is None
               for i,r in enumerate(records)):
            return False
        return (options.get('retention_user_authorized') is True
                and options.get('approved_semantically_deduplicated_memory_records')==26
                and audit.get('memory_records_after_exact_anchor_dedup')==26
                and len(original)==26*320
                and audit.get('provenance_sha256')==hashlib.sha256(provenance.read_bytes()).hexdigest()
                and audit.get('journal_sha256')==hashlib.sha256(journal.read_bytes()).hexdigest())
    except (OSError,ValueError,TypeError):
        return False


def _normalized(text: str) -> str:
    return ' '.join(unicodedata.normalize('NFKC',text).casefold().split()).strip(' .,!?:;')


def _native_counts(home: Path) -> tuple[int,int]:
    """Compare every native fact to the verified canonical record, never row counts alone.

    This initial strict gate accepts only lossless normalized one-record
    imports. A future audited multi-record or retained-private-archive scheme
    must add its own provenance proof rather than silently passing this gate.
    """
    source=home/'.zeroclaw/data/memory/brain.db'
    if not source.is_file() or source.is_symlink():
        return -1,0
    with closing(sqlite3.connect(source.as_uri()+'?mode=ro',uri=True,timeout=8)) as con:
        con.execute('PRAGMA query_only=ON')
        if con.execute('PRAGMA quick_check').fetchone()[0]!='ok':
            return -1,0
        native=con.execute('SELECT id,content FROM memories').fetchall()
    count=len(native)
    receipts=home/'.local/state/optmem-production/migration-receipts.jsonl'
    log=home/'.optmem/memory/LOG.txt'
    if not _secure_file(receipts) or not _secure_file(log):
        return count,0
    try:
        records=log.read_bytes()
        if len(records)%320:
            return count,0
        rows=[json.loads(line) for line in receipts.read_text().splitlines() if line.strip()]
        keyed={r['native_id_sha256']:r for r in rows}
        if len(keyed)!=count or len(rows)!=count:
            return count,0
        seen=0
        for native_id,text in native:
            id_hash=hashlib.sha256(str(native_id).encode()).hexdigest()
            row=keyed.get(id_hash)
            if not row or row.get('outcome') not in ('imported','deduplicated'):
                return count,0
            if not isinstance(text,str) or row.get('source_content_sha256')!=hashlib.sha256(text.encode()).hexdigest():
                return count,0
            idx=row.get('canonical_index')
            if type(idx) is not int or idx < 0 or idx >= len(records)//320:
                return count,0
            rec=records[idx*320:(idx+1)*320].decode('utf8').rstrip()
            match=re.fullmatch(r'#[0-9]+ 20[0-9]{2}-[0-9]{2}-[0-9]{2} (.+)',rec)
            if match is None or _normalized(match.group(1))!=_normalized(text):
                return count,0
            seen+=1
        return count,seen
    except (UnicodeDecodeError,ValueError,KeyError,TypeError,OSError):
        return count,0


def gather(home: Path) -> tuple[Evidence,dict]:
    canonical=home/'.optmem/memory/LOG.txt'
    meta={'canonical_record_count':0,'legacy_source_count':0,'legacy_accounted_count':0}
    try:
        if canonical.is_file() and not canonical.is_symlink() and canonical.stat().st_size%320==0:
            meta['canonical_record_count']=canonical.stat().st_size//320
    except OSError:
        pass
    historical=_source_confirmed(home)
    try:
        op=json.loads((home/'.config/opencode/opencode.json').read_text())
        zc=tomllib.loads((home/'.zeroclaw/config.toml').read_text())
    except (OSError,ValueError):
        op,zc={},{}
    op_mem=op.get('mcp',{}).get('optmem',{})
    zc_mcp=zc.get('mcp',{})
    zc_bundles=zc.get('mcp_bundles',{})
    names=zc.get('agents',{}).get('default',{}).get('mcp_bundles',[])
    op_bound=bool(op_mem.get('enabled') is True and op_mem.get('type')=='local')
    zc_bound=bool(zc_mcp.get('enabled') is True and 'optmem' in zc_bundles and 'optmem' in names)
    try:
        n,migrated=_native_counts(home)
    except (sqlite3.DatabaseError,OSError):
        n,migrated=-1,0
    meta['legacy_source_count']=n
    meta['legacy_accounted_count']=migrated
    native_readonly=zc.get('memory',{}).get('auto_save') is False
    # Nonempty production evidence must come from independent real MCP calls.
    # No staging/synthetic release receipts are accepted for the actual canonical.
    release=home/'.local/state/optmem-production/canonical-acceptance.json'
    try:
        proof=json.loads(release.read_text()) if _secure_file(release) else {}
    except (OSError,ValueError):
        proof={}
    ready=bool(proof.get('schema')=='OptMem-production-acceptance/v1'
               and proof.get('canonical_log_sha256')==hashlib.sha256(canonical.read_bytes()).hexdigest()
               and proof.get('canonical_backed_up_and_restored') is True
               and proof.get('durable_writer_test') is True
               and proof.get('opencode_live_mcp_test') is True
               and proof.get('zeroclaw_live_mcp_test') is True
               and proof.get('native_writer_exclusivity_test') is True)
    evidence=Evidence(
        historical_baseline_verified=historical,
        canonical_writer_verified=ready and meta['canonical_record_count']>=26,
        canonical_local_recovery_verified=ready and meta['canonical_record_count']>=26,
        opencode_mcp_bound_and_tested=op_bound and ready,
        zeroclaw_mcp_bound_and_tested=zc_bound and ready,
        legacy_memory_reconciled=n>=0 and migrated==n,
        exclusive_writer_authority=native_readonly and ready,
    )
    return evidence,meta


def main():
    cli=argparse.ArgumentParser(description='READ-ONLY OptMem production readiness gate')
    cli.add_argument('--home',type=Path,default=Path.home())
    args=cli.parse_args()
    evidence,meta=gather(args.home)
    result={**decide(evidence),'schema':'OptMem-production-preflight/v1',
            'evaluated_utc':datetime.now(timezone.utc).isoformat(), 'counts':meta}
    print(json.dumps(result,sort_keys=True))
    return 0 if result['status']=='READY' else 2


if __name__=='__main__':
    raise SystemExit(main())

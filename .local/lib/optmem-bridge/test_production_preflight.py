#!/usr/bin/env python3
"""Synthetic-only preflight tests: never touch the active canonical memory."""
from pathlib import Path
from contextlib import closing
import hashlib
import json
import os
import sqlite3
import tempfile
import unittest

import production_preflight as p


def private_write(path: Path, content: bytes):
    path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    fd=os.open(path,os.O_WRONLY|os.O_CREAT|os.O_EXCL,0o600)
    with os.fdopen(fd,'wb') as f:
        f.write(content)
    path.chmod(0o600)


def record(index: int, text: str):
    raw=f'#{index} 2026-10-19 {text}'.encode()
    assert len(raw)<320
    return raw + b' '*(319-len(raw)) + b'\n'


class ProductionPreflightTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory(prefix='optmem-production-fake-')
        self.home=Path(self.temp.name)
        self.home.chmod(0o700)
        self.journal=self.home/'.local/state/optmem-ingest-2026-10-09/events.jsonl'
        self.stage=self.home/'.local/state/optmem-migration-2026-10-09/global-staging-20261014/g2b-persistent-20261014/g4-authorized-local-20261015/optmem-test-memory'
        self.canonical=self.home/'.optmem/memory/LOG.txt'
        self.native=self.home/'.zeroclaw/data/memory/brain.db'
        self.receipts=self.home/'.local/state/optmem-production/migration-receipts.jsonl'
        self.proof=self.home/'.local/state/optmem-production/canonical-acceptance.json'
        private_write(self.journal,b'{"synthetic":true}\n')
        source=record(0,'Synthetic historical evidence')
        private_write(self.stage/'LOG.txt', source + b''.join(
            record(i, f'Synthetic historical evidence {i}') for i in range(1,26)))
        private_write(self.stage/'PROVENANCE.jsonl',b'{"synthetic":true}\n')
        private_write(self.stage/'CONSENT.json', json.dumps({
            'retention_user_authorized':True,
            'approved_semantically_deduplicated_memory_records':26}).encode())
        private_write(self.stage/'BUILD-QA.json',json.dumps({
            'memory_records_after_exact_anchor_dedup':26,
            'provenance_sha256':hashlib.sha256((self.stage/'PROVENANCE.jsonl').read_bytes()).hexdigest(),
            'journal_sha256':hashlib.sha256(self.journal.read_bytes()).hexdigest()}).encode())
        notes=['Keep local backups before every deployment','Reuse logical keys across separate agents']
        private_write(self.canonical,b''.join(record(i,text) for i,text in enumerate(notes)) +
                      b''.join(record(i,f'Example historical fact {i}') for i in range(2,26)))
        self.native.parent.mkdir(mode=0o700,parents=True,exist_ok=True)
        with closing(sqlite3.connect(self.native)) as conn:
            with conn:
                conn.execute('CREATE TABLE memories (id TEXT PRIMARY KEY, content TEXT)')
                conn.executemany('INSERT INTO memories VALUES (?,?)', [(f'id-{i}',text) for i,text in enumerate(notes)])
        self.native.chmod(0o600)
        self.notes=notes
        private_write(self.home/'.config/opencode/opencode.json',json.dumps({
            'mcp':{'optmem':{'type':'local','enabled':True}}}).encode())
        private_write(self.home/'.zeroclaw/config.toml',b'''[memory]
backend = "none"
auto_save = false
hygiene_enabled = false
[mcp]
enabled = true
[mcp_bundles.optmem]
servers = ["optmem"]
[agents.default]
mcp_bundles = ["optmem"]
''')
        self.archive=self.home/'.local/state/optmem-production/legacy-retained'
        self.archive.mkdir(mode=0o700,parents=True,exist_ok=True)
        archived=self.archive/'brain.sqlite3'
        with closing(sqlite3.connect(self.native)) as src:
            with closing(sqlite3.connect(archived)) as dst:
                src.backup(dst)
            cols=[x[1] for x in src.execute('PRAGMA table_info(memories)')]
        archived.chmod(0o600)
        private_write(self.archive/'manifest.json',json.dumps({
            'schema':'OptMem-native-retained-archive/v1',
            'source_rows':len(self.notes),'fields':cols,
            'native_db_sha256':hashlib.sha256(archived.read_bytes()).hexdigest(),
            'restoration_verified':True}).encode())
        sidecar=self.home/'.local/state/optmem-production/sidecar'
        sidecar.mkdir(mode=0o700,parents=True,exist_ok=True)
        self.sidecar=sidecar
        with closing(sqlite3.connect(sidecar/'provenance.sqlite3')) as conn:
            conn.execute('CREATE TABLE baseline (id INTEGER PRIMARY KEY, initial_log_count INTEGER, initial_log_sha256 TEXT)')
            conn.execute('CREATE TABLE bindings (state TEXT,expected_index INTEGER,expected_day TEXT,entry_text TEXT,entry_sha256 TEXT)')
            conn.execute('INSERT INTO baseline VALUES (1,26,?)',(hashlib.sha256(self.canonical.read_bytes()).hexdigest(),))
            conn.commit()
        with closing(sqlite3.connect(sidecar/'agent-facts.sqlite3')) as conn:
            conn.execute('CREATE TABLE agent_keys (logical_key TEXT,normalized_sha256 TEXT,memory_index INTEGER)')
            conn.execute('CREATE TABLE agent_requests (logical_key TEXT,normalized_sha256 TEXT,memory_index INTEGER,state TEXT)')
            conn.commit()
        for path in sidecar.iterdir():path.chmod(0o600)
        self.write_receipts()
        self.write_proof()

    def tearDown(self):
        self.temp.cleanup()

    def write_receipts(self):
        rows=[{'native_id_sha256':hashlib.sha256(f'id-{i}'.encode()).hexdigest(),
               'source_content_sha256':hashlib.sha256(text.encode()).hexdigest(),
               'canonical_index':i,'outcome':'imported'} for i,text in enumerate(self.notes)]
        if self.receipts.exists():self.receipts.unlink()
        private_write(self.receipts,(''.join(json.dumps(r)+'\n' for r in rows)).encode())

    def write_proof(self):
        data={'schema':'OptMem-production-acceptance/v1',
              'canonical_record_count':26,
              'canonical_log_sha256':hashlib.sha256(self.canonical.read_bytes()).hexdigest(),
              'canonical_backed_up_and_restored':True,'durable_writer_test':True,
              'opencode_live_mcp_test':True,'zeroclaw_live_mcp_test':True,
              'native_writer_exclusivity_test':True}
        if self.proof.exists():self.proof.unlink()
        private_write(self.proof,json.dumps(data).encode())

    def test_ready_only_with_every_real_world_gate_represented(self):
        evidence,counts=p.gather(self.home)
        self.assertEqual(counts['canonical_record_count'],26)
        self.assertEqual((counts['legacy_source_count'],counts['legacy_accounted_count']),(2,2))
        self.assertEqual(p.decide(evidence)['status'],'READY')

    def test_retained_row_requires_matching_lossless_archive(self):
        receipt=[json.loads(line) for line in self.receipts.read_text().splitlines()]
        receipt[1].pop('canonical_index')
        receipt[1]['outcome']='retained'
        receipt[1]['archive_sha256']=json.loads((self.archive/'manifest.json').read_text())['native_db_sha256']
        self.receipts.write_text(''.join(json.dumps(row)+'\n' for row in receipt))
        self.assertEqual(p._native_counts(self.home),(2,2))
        (self.archive/'brain.sqlite3').write_bytes(b'corrupt')
        self.assertEqual(p._native_counts(self.home),(2,0))

    def test_attested_prefix_allows_later_committed_writer_notes(self):
        self.canonical.write_bytes(self.canonical.read_bytes()+record(26,'Legitimate post-release memory update'))
        with closing(sqlite3.connect(self.sidecar/'provenance.sqlite3')) as conn:
            text='Legitimate post-release memory update'
            conn.execute('INSERT INTO bindings VALUES (?,?,?,?,?)',
                ('COMMITTED',26,'2026-10-19',text,hashlib.sha256(text.encode()).hexdigest()))
            conn.commit()
        self.assertTrue(p._canonical_attestation(self.home))
        self.assertTrue(p.gather(self.home)[0].canonical_writer_verified)

    def test_untracked_post_release_append_denied(self):
        self.canonical.write_bytes(self.canonical.read_bytes()+record(26,'Unauthorized memory injection'))
        self.assertFalse(p._canonical_attestation(self.home))

    def test_unmigrated_native_rows_fail_closed(self):
        self.receipts.unlink()
        evidence,counts=p.gather(self.home)
        self.assertEqual(counts['legacy_accounted_count'],0)
        self.assertIn('legacy_memory_reconciled',p.decide(evidence)['unmet_gates'])

    def test_mismatched_source_digest_is_not_accounted(self):
        data=[json.loads(line) for line in self.receipts.read_text().splitlines()]
        data[1]['source_content_sha256']='0'*64
        self.receipts.unlink()
        private_write(self.receipts,(''.join(json.dumps(x)+'\n' for x in data)).encode())
        self.assertEqual(p._native_counts(self.home),(2,0))

    def test_future_unreviewed_native_message_blocks_activation(self):
        with closing(sqlite3.connect(self.native)) as conn:
            with conn:
                conn.execute('INSERT INTO memories VALUES (?,?)',('new-message','This was added after the release snapshot'))
        self.assertEqual(p._native_counts(self.home),(3,0))

    def test_tampered_canonical_record_breaks_proof(self):
        with self.canonical.open('r+b') as f:
            f.seek(24)
            f.write(b'X')
        self.assertFalse(p.gather(self.home)[0].canonical_writer_verified)
        self.assertFalse(p.gather(self.home)[0].legacy_memory_reconciled)

    def test_stale_historical_anchors_fail_closed(self):
        with self.journal.open('ab') as f:f.write(b'changed\n')
        self.assertFalse(p.gather(self.home)[0].historical_baseline_verified)

    def test_dual_native_writers_fail(self):
        cfg=self.home/'.zeroclaw/config.toml'
        cfg.write_text(cfg.read_text().replace('auto_save = false','auto_save = true'))
        evidence,_=p.gather(self.home)
        self.assertIn('exclusive_writer_authority',p.decide(evidence)['unmet_gates'])

    def test_missing_opencode_mcp_fails_even_with_accepted_proof(self):
        (self.home/'.config/opencode/opencode.json').write_text('{"mcp":{}}')
        self.assertFalse(p.gather(self.home)[0].opencode_mcp_bound_and_tested)

    def test_empty_canonical_cannot_be_certified(self):
        self.canonical.write_bytes(b'')
        self.write_proof()
        self.assertFalse(p.gather(self.home)[0].canonical_writer_verified)


if __name__=='__main__':
    unittest.main(verbosity=2)

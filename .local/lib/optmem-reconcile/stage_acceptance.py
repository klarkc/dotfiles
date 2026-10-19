#!/usr/bin/env python3
"""Exercise upstream OptMem on disposable private copies; no production writes."""
from __future__ import annotations
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory
from datetime import datetime,timezone

MEMO_SHA="3dc120d01be3115ef6267eab4103e7909fc830d6227b549f20991ba999ee9ffb"

def digest(p):
    return hashlib.sha256(Path(p).read_bytes()).hexdigest()

def memo(memo_path, memory, *args):
    env=os.environ.copy()
    env["MEMORY_DIR"]=str(memory)
    env["PYTHONDONTWRITEBYTECODE"]="1"
    p=subprocess.run([sys.executable,"-B",str(memo_path),*args],
                     env=env,text=True,capture_output=True,timeout=25)
    if p.returncode:
        raise RuntimeError(f"Memo {args[0]} failed: {p.stderr[:220] or p.stdout[:220]}")
    return p.stdout

def tree_manifest(directory):
    result={}
    for path in sorted(directory.rglob('*')):
        if path.is_symlink(): raise RuntimeError('Symlink in staging source')
        if path.is_file():result[str(path.relative_to(directory))]=digest(path)
    return result

def memory_copy(source, dest):
    if dest.exists():
        raise FileExistsError(dest)
    shutil.copytree(source,dest,symlinks=False)

def synthetic_nap(memo_path, folder):
    folder.mkdir(mode=0o700)
    memo(memo_path,folder,"init")
    notes=[
        "Temporary staging harness A uses the read-only memory adapter.",
        "Temporary staging harness B reads the same original log.",
        "Restore rehearsal checks hashes before reading any memory.",
        "Snapshots are isolated and never write the canonical memory."
    ]
    for note in notes:
        memo(memo_path,folder,"note",note)
    summaries={
        "0-1":"Harness A and B read the same isolated original log.",
        "2-3":"Restores check hashes and avoid canonical memory writes.",
        "0-3":"Isolated harness readers and checked restores do not change canonical memory."
    }
    installed=[]
    for _ in range(8):
        pending=memo(memo_path,folder,"nap")
        m=re.search(r"Run:\s+\S+\s+nap\s+(\d+-\d+)\s+",pending)
        if not m:
            if "Nothing left to compress" not in pending:
                raise AssertionError("Missing expected compression request")
            break
        block=m.group(1)
        if block not in summaries:
            raise AssertionError(f"Unknown summary block: {block}")
        memo(memo_path,folder,"nap",block,summaries[block])
        installed.append(block)
    else:
        raise AssertionError("Compression did not converge")
    memo(memo_path,folder,"config","WAKE_LINES=1")
    wake=memo(memo_path,folder,"wake")
    if "You are awake." not in wake or "#0-3" not in wake:
        raise AssertionError("Semantic summary not present in bounded wake")
    if "Temporary staging harness A" not in memo(memo_path,folder,"recall","harness A"):
        raise AssertionError("Underlying original is no longer retrievable")
    zoom=memo(memo_path,folder,"zoom","0-3")
    if "Harness A and B" not in zoom:
        raise AssertionError("Zoom cannot descend from semantic root")
    memo(memo_path,folder,"forget","0-1")
    if "0-1" not in memo(memo_path,folder,"nap"):
        raise AssertionError("Forgotten summary could not be rebuilt")
    return {"blocks":installed,"semantic_summary_wake":True,"recall_original":True,
            "zoom_descends":True,"forget_rebuild_needed":True}

def main():
    parser=argparse.ArgumentParser()
    parser.add_argument("--memo",type=Path,required=True)
    parser.add_argument("--source",type=Path,required=True)
    parser.add_argument("--out",type=Path,required=True)
    a=parser.parse_args()
    src=a.source.resolve()
    canonical=(Path.home()/".optmem/memory").resolve()
    if src==canonical or src.name!="optmem-test-memory" or "g4-authorized-local" not in str(src):
        raise RuntimeError("Only G4 private staging may be used")
    if digest(a.memo)!=MEMO_SHA:
        raise RuntimeError("Audited upstream memo hash changed")
    original=digest(src/"LOG.txt")
    original_tree=tree_manifest(src)
    if (src/"LOG.txt").stat().st_size!=26*320:
        raise RuntimeError("Unexpected G4P staged note count")
    if a.out.exists() or a.out.is_symlink():
        raise FileExistsError("G4H evidence output must be new")
    a.out.mkdir(mode=0o700)
    with TemporaryDirectory(prefix="g4h-acceptance-",dir=a.out) as tmp:
        root=Path(tmp)
        budgets={}
        for name,budget in (("small-reader-simulated",12),("zeroclaw-simulated",48),("opencode-simulated",96)):
            d=root/name
            memory_copy(src,d)
            memo(a.memo,d,"config",f"WAKE_LINES={budget}")
            wake=memo(a.memo,d,"wake")
            if "You are awake." not in wake: raise AssertionError(f"{name} wake incomplete")
            recall=memo(a.memo,d,"recall","U01")
            if "U01" not in recall: raise AssertionError(f"{name} recall failed")
            zoom=memo(a.memo,d,"zoom","0-1")
            if not zoom.strip(): raise AssertionError(f"{name} zoom empty")
            budgets[name]={"wake":True,"recall":True,"zoom":True,
                           "budget":budget,"source_LOG_sha256":digest(d/"LOG.txt")}
        backup=root/"backup"
        restore=root/"restored"
        memory_copy(src,backup)
        memory_copy(backup,restore)
        if tree_manifest(backup)!=original_tree or tree_manifest(restore)!=original_tree:
            raise AssertionError("Local snapshot failed full-file checksum comparison")
        with (restore/"LOG.txt").open("r+b") as f:
            first=f.read(1)
            f.seek(0)
            f.write(bytes([first[0]^1]))
        if tree_manifest(restore)==original_tree:
            raise AssertionError("Injected fixture corruption went undetected")
        shutil.copyfile(backup/"LOG.txt",restore/"LOG.txt")
        if tree_manifest(restore)!=original_tree:
            raise AssertionError("Local fixture recovery did not reconstruct snapshot")
        if not memo(a.memo,restore,"recall","S22").strip().startswith("#"):
            raise AssertionError("Restored memory cannot recall recent record")
        nap=synthetic_nap(a.memo,root/"synthetic")
    if tree_manifest(src)!=original_tree: raise AssertionError("Staging source modified")
    report={
        "schema":"G4H-stage-acceptance/v1",
        "utc":datetime.now(timezone.utc).isoformat(),
        "source_LOG_sha256":original,"record_count":26,"readers":budgets,
        "verified_source_files":len(original_tree),
        "full_tree_manifest_equal_after_restore":True,
        "local_fixture_corruption_detected_and_recovered":True,
        "source_integrity_unchanged":True,
        "local_backup_restored":True,"synthetic_nap":nap,
        "real_harness_dynamic_integration_tested":False,
        "writer_runtime_activated":False,"remote_restore_tested":False,
        "native_memories_changed":False
    }
    target=a.out/"stage-acceptance.json"
    fd=os.open(target,os.O_CREAT|os.O_EXCL|os.O_WRONLY,0o600)
    with os.fdopen(fd,"w") as f:
        json.dump(report,f,indent=2,sort_keys=True)
        f.write("\n")
    print(json.dumps({"stage_acceptance":"PASS","log_records":26,
                      "harness_simulations":3,"synthetic_nap":nap}))
if __name__=="__main__":
    main()

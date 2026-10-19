#!/usr/bin/env python3
"""Read-only provenance census. Never copies raw OpenCode text or admits memories."""
from __future__ import annotations
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import sqlite3
import subprocess

CUTOFF_MS = 1791983430000  # DONE/2 review cutoff, verified in G4F
RISK = re.compile(r"(?i)(?:password|passwd|secret|senha|api.?key|token|bearer|authorization|private.?key|\.profile_override|sk-[a-z0-9_-]{12,}|ghp_[a-z0-9]{12,})")
PREFERENCE = re.compile(r"(?i)\b(?:prefiro|sempre|nunca|never|always|prefer|daqui em diante|a partir de agora|deve ser|must be|only use)\b")
DELEGATED = re.compile(r"(?im)^\s*(?:from (?:build|builddoc|plan|pl[aá]n|research|agent)\s*:)")
REPLAY = re.compile(r"(?i)^(?:continue|continue if you have next steps, or stop and ask for clarification if you are unsure how to proceed\.?|reply with exactly ok\.?)\s*$")

def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()

def source_rows(db: Path, since_ms: int = CUTOFF_MS):
    """Freeze read-only SQLite transaction; read all message text only in RAM."""
    con = sqlite3.connect(db.as_uri() + "?mode=ro", uri=True, timeout=20)
    con.execute("PRAGMA query_only=ON")
    con.execute("BEGIN")
    try:
        for mid, session, created, raw in con.execute(
            "SELECT id,session_id,time_created,data FROM message "
            "WHERE time_created>? ORDER BY time_created,id", (since_ms,)
        ).fetchall():
            try:
                role = json.loads(raw).get("role")
            except (ValueError, TypeError, AttributeError):
                continue
            if role != "user":
                continue
            texts, types = [], []
            for (part,) in con.execute(
                "SELECT data FROM part WHERE message_id=? ORDER BY id", (mid,)
            ):
                try:
                    obj = json.loads(part)
                except (ValueError, TypeError):
                    types.append("unreadable")
                    continue
                kind = obj.get("type") if isinstance(obj, dict) else "unreadable"
                types.append(kind or "unknown")
                if kind == "text" and isinstance(obj.get("text"), str):
                    texts.append(obj["text"])
            full = "\n".join(texts)
            if not full:
                classification = "NON_TEXT_UNREVIEWED"
            elif RISK.search(full):
                classification = "PRIVACY_HOLD"
            elif DELEGATED.search(full):
                classification = "DELEGATED_HANDOFF_NO_USER_AUTHORITY"
            elif REPLAY.fullmatch(full.strip()):
                classification = "EPHEMERAL_REPLAY"
            elif PREFERENCE.search(full):
                classification = "STANDING_DECISION_MANUAL_REVIEW"
            else:
                classification = "TASK_CONTEXT_MANUAL_REVIEW"
            yield {
                "source": "opencode/user-message", "message_id": mid,
                "session_id": session,
                "created_utc": datetime.fromtimestamp(created / 1000, timezone.utc).isoformat(),
                "text_sha256": digest(full.encode("utf-8")),
                "part_types": sorted(set(types)), "text_length": len(full),
                "classification": classification, "source_only": True,
                "promoted": False, "contains_raw_text": False,
            }
    finally:
        con.rollback()
        con.close()

def reconcile(rows: list[dict], previous: list[dict]) -> dict:
    indexed = {r["message_id"]: r for r in previous}
    observed = {r["message_id"]: r for r in rows}
    if len(observed) != len(rows) or len(indexed) != len(previous):
        raise RuntimeError("Duplicate source IDs; reconciliation refused")
    changed = [key for key in indexed.keys() & observed.keys()
               if indexed[key]["text_sha256"] != observed[key]["text_sha256"]]
    missing = sorted(indexed.keys() - observed.keys())
    # Never silently rebase source anchors when a former observation changed.
    if changed or missing:
        raise RuntimeError(
            f"Source drift: {len(changed)} changed text hashes, {len(missing)} missing IDs"
        )
    fresh = [r for r in rows if r["message_id"] not in indexed]
    repeats: dict[str, str] = {}
    for row in rows:
        first = repeats.setdefault(row["text_sha256"], row["message_id"])
        if row["classification"] == "EPHEMERAL_REPLAY" and first != row["message_id"]:
            row["duplicate_of"] = first
    return {
        "prior_index_count": len(indexed), "all_source_messages": len(rows),
        "new_after_prior_index": len(fresh), "new_ids": [r["message_id"] for r in fresh],
        "classification_all": dict(Counter(r["classification"] for r in rows)),
        "classification_new": dict(Counter(r["classification"] for r in fresh)),
        "text_sha256_drift": 0, "missing_previous_sources": 0,
        "auto_admitted": 0, "source_only": True,
    }

def git_delta(repo: Path, prior_head: str, current_head: str) -> list[dict]:
    def git(*args: str) -> str:
        return subprocess.check_output(["git", "-C", str(repo), *args], text=True)
    if subprocess.run(
        ["git", "-C", str(repo), "merge-base", "--is-ancestor", prior_head, current_head],
        check=False, capture_output=True
    ).returncode != 0:
        raise RuntimeError("Git baseline was not an ancestor; fail closed")
    revs = git("rev-list", "--reverse", f"{prior_head}..{current_head}").splitlines()
    output = []
    for rev in revs:
        paths = git("diff-tree", "--root", "--no-commit-id", "--name-only", "-r", rev).splitlines()
        output.append({
            "commit": rev, "paths": paths,
            "path_manifest_sha256": digest("\n".join(paths).encode()),
            "normative_paths": [p for p in paths if p in {"AGENTS.md","README.md"}
                                or p.startswith((".nix/", ".config/systemd/user/"))],
            "verified_by_git": True, "semantic_decision_auto_approved": False,
        })
    return output

def private_output(out: Path, source_rows: list[dict], report: dict, commits: list[dict]):
    if out.is_symlink() or out.exists():
        raise FileExistsError("Use a new private, immutable generation directory")
    if out.parent.is_symlink() or out.parent.stat().st_mode & 0o077:
        raise PermissionError("Private staging parent must be mode 0700")
    out.mkdir(mode=0o700)
    for filename, content in (
        ("opencode-index.jsonl", "".join(json.dumps(x,sort_keys=True)+"\n" for x in source_rows)),
        ("dotfiles-delta.jsonl", "".join(json.dumps(x,sort_keys=True)+"\n" for x in commits)),
        ("status.json", json.dumps(report,sort_keys=True,indent=2)+"\n"),
    ):
        path = out / filename
        fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(fd, "w") as f:
            f.write(content)
            f.flush()
            os.fsync(f.fileno())
    fd = os.open(out, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--db", required=True, type=Path)
    ap.add_argument("--prior", required=True, type=Path)
    ap.add_argument("--repo", required=True, type=Path)
    ap.add_argument("--prior-head", required=True)
    ap.add_argument("--out", required=True, type=Path)
    args = ap.parse_args()
    previous = [json.loads(line) for line in args.prior.read_text().splitlines()]
    rows = list(source_rows(args.db))
    rec = reconcile(rows, previous)
    head = subprocess.check_output(["git", "-C", str(args.repo), "rev-parse", "HEAD"],text=True).strip()
    commits = git_delta(args.repo, args.prior_head, head)
    report = {"schema":"OptMem-G4H-source-only/v1",
              "captured_utc":datetime.now(timezone.utc).isoformat(),
              "current_dotfiles_head":head,"baseline_dotfiles_head":args.prior_head,
              "new_commits":len(commits),"no_gpt_web_access":True,
              "historical_text_exported":False,"canonical_changed":False,
              "source_report":rec}
    private_output(args.out, rows, report, commits)
    print(json.dumps({"g4h": str(args.out), "source":rec, "new_commits":len(commits)}))

if __name__ == "__main__":
    main()

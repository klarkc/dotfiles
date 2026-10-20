#!/usr/bin/env python3
"""Private versioned working-tree replication for OptMem; Git commits are the snapshots."""
from __future__ import annotations

from contextlib import closing, contextmanager
from datetime import datetime, timezone
import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import re
import shlex
import shutil
import sqlite3
import stat
import subprocess
import tempfile
import tarfile
import io
import uuid

import production_operator as po
import production_preflight as gate
import production_snapshot as prod
import snapshot_stage as snap

REMOTE = "git@github.com:klarkc/memory.git"
OWNER = "klarkc/memory"
LEGACY_SCHEMA = "OptMem-private-Git-recovery/v1"
SCHEMA = "OptMem-private-Git-working-tree/v2"
MAX_FILES = 512
MAX_BYTES = 256 * 1024 * 1024
AUX = {
    "legacy/brain.sqlite3": "legacy-retained/brain.sqlite3",
    "legacy/manifest.json": "legacy-retained/manifest.json",
    "legacy/archive-receipts.jsonl": "legacy-retained/migration-receipts.jsonl",
    "metadata/activation.json": "ACTIVE.json",
    "metadata/acceptance.json": "canonical-acceptance.json",
    "metadata/migration-receipts.jsonl": "migration-receipts.jsonl",
}
README = """# OptMem private Git history

This repository stores private, unencrypted canonical OptMem memories and
the retained historical ZeroClaw SQLite database. KEEP IT PRIVATE.

The root canonical/, legacy/, metadata/, and manifest.json files
always represent the latest complete memory checkpoint. Each Git commit
stores its previous version; unchanged memory produces NO NEW COMMIT.
There are no per-run snapshots/<id> directories in current HEAD.

The first backup commit in history used the old snapshots/<id> layout.
Its objects remain accessible through git show e6db6bd:...; the migration
commit changes only the Git working tree and NEVER force-pushes or rewrites
that history. Restore older Git revisions using the dotfiles git_sync.py
restore --revision <commit> command. Restores are always ISOLATED, never
written automatically to the live canonical or ZeroClaw native stores.

This is not end-to-end encrypted and SHA-256 detects corruption, not a
malicious attacker who can replace both bytes and manifests. SQLite and
provenance receipts are verified before push and after clone/restore.
"""



def digest(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as inp:
        for part in iter(lambda: inp.read(131072), b""):
            h.update(part)
    return h.hexdigest()


def canonical_json(obj) -> bytes:
    return json.dumps(obj, sort_keys=True, separators=(",", ":")).encode()


def private_dir(path: Path, create: bool = False) -> Path:
    if create:
        path.mkdir(parents=True, mode=0o700, exist_ok=True)
    st = path.lstat()
    if not stat.S_ISDIR(st.st_mode) or st.st_uid != os.getuid() or st.st_mode & 0o077:
        raise RuntimeError("Unsafe private directory")
    return path


def private_file(path: Path) -> Path:
    snap._safe_regular(path)
    return path


def safe_relative(name: str) -> Path:
    if not isinstance(name, str) or name.startswith("/") or "\\" in name:
        raise RuntimeError("Invalid recovery path")
    parts = name.split("/")
    if any(x in ("", ".", "..", ".git") for x in parts):
        raise RuntimeError("Unsafe recovery path component")
    return Path(*PurePosixPath(name).parts)


def write_once(path: Path, data: bytes) -> None:
    private_dir(path.parent, create=True)
    fd = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "wb") as target:
        target.write(data)
        target.flush()
        os.fsync(target.fileno())


def subprocess_checked(cmd: list[str], env: dict | None = None,
                       timeout: int = 90, check: bool = True):
    result = subprocess.run(cmd, env=env, timeout=timeout, capture_output=True, text=True)
    if check and result.returncode:
        # Do not relay subprocess stdout/stderr containing memory, keys or tokens.
        raise RuntimeError("Git operation failed: " + cmd[0] + " exit=" + str(result.returncode))
    return result


@contextmanager
def exclusive(state: Path):
    private_dir(state, create=True)
    fd = os.open(state / "sync.lock",
                 os.O_CREAT | os.O_RDWR | os.O_CLOEXEC | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "rb") as file:
        fcntl.flock(file.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        try:
            yield
        finally:
            fcntl.flock(file.fileno(), fcntl.LOCK_UN)


def scoped_github_token(home: Path) -> str:
    if os.environ.get("GH_TOKEN"):
        return os.environ["GH_TOKEN"]
    f = private_file(home / ".profile_override")
    for line in f.read_text().splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        try:
            terms = shlex.split(line, comments=True)
        except ValueError:
            continue
        if terms and terms[0] == "export":
            terms = terms[1:]
        if len(terms) == 1 and terms[0].startswith("GH_TOKEN="):
            token = terms[0].partition("=")[2]
            if token and len(token) <= 1024:
                return token
    raise RuntimeError("No GitHub token available to verify private repository")


def github_environment(home: Path, remote: str) -> dict:
    # Git pre-commit hooks export GIT_DIR, GIT_INDEX_FILE, and possibly
    # GIT_CONFIG_COUNT. A nested private backup repository must never
    # inherit any of those pointers to the parent dotfiles repository.
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_") and key != "GH_TOKEN"}
    env["GIT_TERMINAL_PROMPT"] = "0"
    env["GIT_ASKPASS"] = "/usr/bin/false"
    env["GIT_CONFIG_NOSYSTEM"] = "1"
    if remote == REMOTE:
        config = private_file(home / ".ssh/config")
        private_dir(config.parent)
        # solo098 has an unrelated globally misowned OpenSSH include. Reuse
        # only the user's private config and keep host-key checks mandatory.
        env["GIT_SSH_COMMAND"] = (
            "ssh -F " + str(config) +
            " -o BatchMode=yes -o StrictHostKeyChecking=yes -o ConnectTimeout=12")
    return env


class GitMemory:
    def __init__(self, home: Path | None = None, remote: str = REMOTE, verify_private=None):
        self.home = home if home is not None else Path.home()
        self.remote = remote
        self.state = self.home / ".local/state/optmem-git-sync"
        self.repo = self.state / "repo"
        self.recovery = self.state / "recovery"
        self.production = self.home / ".local/state/optmem-production"
        self.host = re.sub("[^a-z0-9-]", "-", os.uname().nodename.lower())[:40]
        self.authorize = verify_private if verify_private is not None else self.verify_github
        self.env = github_environment(self.home, self.remote)

    def verify_github(self) -> None:
        if self.remote != REMOTE:
            raise RuntimeError("Unapproved remote URL")
        gh = self.home / ".nix-profile/bin/gh"
        if not gh.is_file():
            raise RuntimeError("Missing GitHub CLI for private visibility check")
        env = os.environ.copy()
        env["GH_TOKEN"] = scoped_github_token(self.home)
        response = subprocess_checked([str(gh), "api", "repos/" + OWNER], env, timeout=35)
        metadata = json.loads(response.stdout)
        if (metadata.get("full_name", "").lower() != OWNER
                or metadata.get("private") is not True
                or metadata.get("archived") is not False
                or metadata.get("disabled") is not False
                or metadata.get("default_branch") != "main"):
            raise RuntimeError("Refusing sync: remote is not verified private and active")

    def git(self, *args: str, check: bool = True):
        return subprocess_checked(["/usr/bin/git", "-C", str(self.repo), *args],
                                  self.env, check=check)

    def has_ref(self, ref: str) -> bool:
        return self.git("show-ref", "--quiet", "--verify", ref, check=False).returncode == 0

    def update_git(self) -> None:
        self.authorize()
        private_dir(self.state, create=True)
        if not self.repo.exists() and not self.repo.is_symlink():
            self.repo.mkdir(mode=0o700)
            self.git("init", "--quiet", "--initial-branch=main")
            self.git("remote", "add", "origin", self.remote)
        private_dir(self.repo)
        private_dir(self.repo / ".git")
        if self.git("remote", "get-url", "origin").stdout.strip() != self.remote:
            raise RuntimeError("Memory remote URL changed")
        if self.git("symbolic-ref", "--quiet", "--short", "HEAD").stdout.strip() != "main":
            raise RuntimeError("Unexpected memory checkout branch")
        if self.git("status", "--porcelain").stdout.strip():
            raise RuntimeError("Uncommitted memory checkout: refusing automatic reset")
        self.git("fetch", "--no-tags", "origin")
        local = self.has_ref("refs/heads/main")
        remote = self.has_ref("refs/remotes/origin/main")
        if remote and not local:
            self.git("switch", "--quiet", "-C", "main", "origin/main")
        elif remote and local:
            behind = self.git("merge-base", "--is-ancestor", "HEAD", "origin/main",
                              check=False).returncode == 0
            ahead = self.git("merge-base", "--is-ancestor", "origin/main", "HEAD",
                             check=False).returncode == 0
            if behind:
                self.git("merge", "--ff-only", "origin/main")
            elif not ahead:
                # Flat canonical LOG/SQLite must NEVER be automatically merged
                # between two independently active writers or branches.
                raise RuntimeError("Concurrent memory Git histories conflicted: manual reconciliation required")
        if self.git("status", "--porcelain").stdout.strip():
            raise RuntimeError("Git fetch left an unclean checkout")

    def ancillary(self) -> dict[str, Path]:
        return {target: self.production / relative for target, relative in AUX.items()}

    def verified_sources(self) -> dict[str, dict]:
        if gate.decide(gate.gather(self.home)[0])["status"] != "READY":
            raise RuntimeError("OptMem production cutover gate is not READY")
        files = {}
        for name, source in self.ancillary().items():
            private_file(source)
            files[name] = {"sha256": digest(source), "size": source.stat().st_size}
        info = json.loads((self.production / "legacy-retained/manifest.json").read_text())
        if (info.get("schema") != "OptMem-native-retained-archive/v1"
                or info.get("native_db_sha256") != files["legacy/brain.sqlite3"]["sha256"]
                or info.get("restoration_verified") is not True):
            raise RuntimeError("Legacy native memory archive changed")
        with closing(sqlite3.connect(
                f"file:{self.production/'legacy-retained/brain.sqlite3'}?mode=ro&immutable=1",
                uri=True)) as con:
            if (con.execute("PRAGMA integrity_check").fetchone()[0] != "ok"
                    or con.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
                    != info.get("source_rows")):
                raise RuntimeError("Archived native memory failed integrity check")
        return files

    def verify_generation(self, root: Path) -> dict:
        private_dir(root)
        bundle = json.loads(private_file(root / "bundle.json").read_text())
        valid_name = r"[a-z0-9-]{1,40}-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{12}"
        if (bundle.get("schema") != LEGACY_SCHEMA
                or not isinstance(bundle.get("generation"), str)
                or not re.fullmatch(valid_name, bundle["generation"])
                or bundle["generation"] != root.name):
            raise RuntimeError("Unexpected Git recovery generation")
        listed = bundle.get("files")
        if not isinstance(listed, dict) or len(listed) > MAX_FILES:
            raise RuntimeError("Incomplete or oversized recovery manifest")
        mandatory = {"canonical/manifest.json", "canonical/memory/LOG.txt",
                     "canonical/sidecar/provenance.sqlite3",
                     "canonical/sidecar/agent-facts.sqlite3", *AUX}
        if not mandatory.issubset(listed):
            raise RuntimeError("Required recovery files missing")
        used_bytes = 0
        for relative, expected in listed.items():
            source = private_file(root / safe_relative(relative))
            current = {"sha256": digest(source), "size": source.stat().st_size}
            if current != expected:
                raise RuntimeError("Recovery content checksum mismatch")
            used_bytes += current["size"]
            if used_bytes > MAX_BYTES:
                raise RuntimeError("Recovery size exceeds audited bound")
        actual = {p.relative_to(root).as_posix() for p in snap._walk_files(root)}
        if actual != set(listed) | {"bundle.json"}:
            raise RuntimeError("Unlisted files in remote recovery generation")
        canonical = json.loads((root / "canonical/manifest.json").read_text())
        ancillary = {key: listed[key] for key in AUX}
        fingerprint = hashlib.sha256(canonical_json(
            {"canonical": canonical["files"], "ancillary": ancillary})).hexdigest()
        if fingerprint != bundle.get("fingerprint"):
            raise RuntimeError("Inconsistent canonical/legacy fingerprint")
        legacy = json.loads((root / "legacy/manifest.json").read_text())
        if (legacy.get("schema") != "OptMem-native-retained-archive/v1"
                or legacy.get("native_db_sha256") != listed["legacy/brain.sqlite3"]["sha256"]
                or legacy.get("source_rows") != bundle.get("native_rows")):
            raise RuntimeError("Legacy native SQLite archive metadata differs")
        with closing(sqlite3.connect(
                f"file:{root/'legacy/brain.sqlite3'}?mode=ro&immutable=1", uri=True)) as db:
            if (db.execute("PRAGMA integrity_check").fetchone()[0] != "ok"
                    or db.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
                    != legacy["source_rows"]):
                raise RuntimeError("Native SQLite archive integrity check failed")
        private_dir(self.production / "snapshots", create=True)
        proven = prod.restore_proof(root / "canonical")
        state = bundle.get("canonical_state")
        if (not isinstance(state, dict) or proven["records"] != state.get("records")
                or proven["keys"] != state.get("keys")
                or proven["requests"] != state.get("requests")):
            raise RuntimeError("Restored canonical writer state differs")
        return {"generation": bundle["generation"], "records": proven["records"],
                "native_rows": legacy["source_rows"], "files": len(listed),
                "restored": True}

    def scan_private_secrets(self, root: Path) -> None:
        scanner = self.home / ".nix-profile/bin/gitleaks"
        if not scanner.is_file():
            raise RuntimeError("Local secret scanner missing; refuse upload")
        report = subprocess_checked(
            [str(scanner), "dir", "--no-banner", str(root)],
            timeout=60, check=False)
        if report.returncode:
            raise RuntimeError("Secret scan rejected memory snapshot; not uploaded")


    def verify_tree(self, root: Path) -> dict:
        """Verify one complete HEAD tree, not an accumulation of generations."""
        private_dir(root)
        metadata = json.loads(private_file(root / "manifest.json").read_text())
        if metadata.get("schema") != SCHEMA:
            raise RuntimeError("Unknown flat Git memory manifest")
        manifest_files = metadata.get("files")
        if not isinstance(manifest_files, dict) or len(manifest_files) > MAX_FILES:
            raise RuntimeError("Incomplete flat memory file manifest")
        required = {"canonical/manifest.json", "canonical/memory/LOG.txt",
                    "canonical/sidecar/agent-facts.sqlite3",
                    "canonical/sidecar/provenance.sqlite3", *AUX}
        if not required.issubset(manifest_files):
            raise RuntimeError("Missing canonical or legacy data")
        allowed = {"README.md", "manifest.json", "canonical", "legacy", "metadata"}
        if (root / ".git").is_dir():
            private_dir(root / ".git")
            allowed.add(".git")
        if set(child.name for child in root.iterdir()) != allowed:
            raise RuntimeError("Unknown top-level backup entries")
        observed = set()
        used = 0
        for folder in ("canonical", "legacy", "metadata"):
            private_dir(root / folder)
            for file in snap._walk_files(root / folder):
                rel = file.relative_to(root).as_posix()
                private_file(file)
                actual = {"sha256": digest(file), "size": file.stat().st_size}
                if actual != manifest_files.get(rel):
                    raise RuntimeError("Flat memory file hash/size mismatch")
                observed.add(rel)
                used += actual["size"]
                if used > MAX_BYTES:
                    raise RuntimeError("Memory backup larger than audit budget")
        if observed != set(manifest_files):
            raise RuntimeError("Unlisted or missing memory backup files")
        private_file(root / "README.md")
        canonical = json.loads((root / "canonical/manifest.json").read_text())
        if canonical.get("schema") != "OptMem-production-snapshot/v1":
            raise RuntimeError("Invalid embedded OptMem snapshot")
        if canonical.get("files") != {
            key.removeprefix("canonical/"): manifest_files[key]
            for key in manifest_files if key.startswith("canonical/") and key != "canonical/manifest.json"
        }:
            raise RuntimeError("Embedded canonical manifest differs")
        hashes = {key: manifest_files[key] for key in AUX}
        fingerprint = hashlib.sha256(canonical_json(
            {"canonical": canonical["files"], "ancillary": hashes})).hexdigest()
        if fingerprint != metadata.get("fingerprint"):
            raise RuntimeError("Memory fingerprint mismatch")
        legacy = json.loads((root / "legacy/manifest.json").read_text())
        if (legacy.get("schema") != "OptMem-native-retained-archive/v1"
                or legacy.get("native_db_sha256") != hashes["legacy/brain.sqlite3"]["sha256"]
                or legacy.get("source_rows") != metadata.get("native_rows")):
            raise RuntimeError("Native legacy memory metadata mismatch")
        with closing(sqlite3.connect(
                f"file:{root/'legacy/brain.sqlite3'}?mode=ro&immutable=1", uri=True)) as con:
            if (con.execute("PRAGMA integrity_check").fetchone()[0] != "ok"
                    or con.execute("SELECT count(*) FROM memories").fetchone()[0]
                    != legacy["source_rows"]):
                raise RuntimeError("Native legacy memory cannot be restored")
        private_dir(self.production / "snapshots", create=True)
        restored = prod.restore_proof(root / "canonical")
        expected = metadata.get("canonical_state")
        if (not isinstance(expected, dict)
                or restored["records"] != expected.get("records")
                or restored["keys"] != expected.get("keys")
                or restored["requests"] != expected.get("requests")):
            raise RuntimeError("Canonical ledger did not restore")
        return {"status": "VERIFIED", "records": restored["records"],
                "native_rows": legacy["source_rows"],
                "fingerprint": fingerprint, "files": len(observed)}

    def _current_remote(self) -> dict | None:
        if (self.repo / "manifest.json").is_file():
            return self.verify_tree(self.repo)
        if (self.repo / "snapshots").is_dir():
            private_dir(self.repo / "snapshots")
            for directory in (self.repo / "snapshots").iterdir():
                self.verify_generation(directory)
            if not list((self.repo / "snapshots").iterdir()):
                raise RuntimeError("Empty legacy snapshot tree")
            if set(child.name for child in self.repo.iterdir()) != {
                    ".git", "README.md", "snapshots"}:
                raise RuntimeError("Unknown legacy backup top-level entries")
            return {"status": "LEGACY_BACKUP_VERIFIED"}
        if self.has_ref("refs/heads/main"):
            raise RuntimeError("Existing Git backup has no recognized complete state")
        if set(child.name for child in self.repo.iterdir()) != {".git"}:
            raise RuntimeError("Unexpected files in uninitialized Git backup")
        return None

    def require_continuous_history(self, previous: Path, upcoming: Path,
                                   previous_host: str) -> None:
        """Fail closed on writer handover or a stale/rolled-back canonical LOG.

        Git itself preserves earlier history, but blindly committing an older
        canonical copy over HEAD would silently make newer memories disappear
        from current search. Canonical writers never rewrite existing records.
        """
        if previous_host != self.host:
            raise RuntimeError("Memory writer host changed; explicit handover required")
        old_log = private_file(previous / "canonical/memory/LOG.txt")
        new_log = private_file(upcoming / "canonical/memory/LOG.txt")
        if new_log.stat().st_size < old_log.stat().st_size:
            raise RuntimeError("Canonical LOG regressed: refuse silent overwrite")
        with old_log.open("rb") as old, new_log.open("rb") as new:
            for chunk in iter(lambda: old.read(131072), b""):
                if chunk != new.read(len(chunk)):
                    raise RuntimeError("Canonical LOG diverged: manual recovery required")
        for name in ("legacy/brain.sqlite3", "legacy/manifest.json"):
            if digest(private_file(previous / name)) != digest(private_file(upcoming / name)):
                raise RuntimeError("Retained native archive changed without reconciliation")
        old_state = json.loads((previous / "canonical/manifest.json").read_text())["state"]
        new_state = json.loads((upcoming / "canonical/manifest.json").read_text())["state"]
        for field in ("records", "keys", "requests"):
            if type(old_state.get(field)) is not int or type(new_state.get(field)) is not int:
                raise RuntimeError("Canonical snapshot counters invalid")
            if new_state[field] < old_state[field]:
                raise RuntimeError("Canonical provenance ledger regressed")

    def commit_tree(self, sealed: Path, aux: dict[str, dict]) -> dict:
        canonical = json.loads((sealed / "manifest.json").read_text())
        if canonical.get("schema") != "OptMem-production-snapshot/v1":
            raise RuntimeError("Unexpected canonical snapshot manifest")
        fingerprint = hashlib.sha256(canonical_json(
            {"canonical": canonical["files"], "ancillary": aux})).hexdigest()
        old = self._current_remote()
        if old and old.get("fingerprint") == fingerprint:
            return {"status": "UNCHANGED", "records": old["records"]}
        # Construct and verify one complete replacement tree outside Git.
        preparation = self.state / (".pending-current-" + uuid.uuid4().hex)
        preparation.mkdir(mode=0o700)
        try:
            sources = {}
            for file in snap._walk_files(sealed):
                sources["canonical/" + file.relative_to(sealed).as_posix()] = file
            sources.update(self.ancillary())
            listed = {}
            for name, source in sorted(sources.items()):
                recorded = snap._copy_secure(source, preparation / safe_relative(name))
                if name in aux and recorded != aux[name]:
                    raise RuntimeError("Legacy metadata changed during memory capture")
                listed[name] = recorded
            legacy_meta = json.loads((preparation / "legacy/manifest.json").read_text())
            manifest = {"schema": SCHEMA, "created_utc":
                        datetime.now(timezone.utc).isoformat(), "host": self.host,
                        "files": listed, "fingerprint": fingerprint,
                        "canonical_state": canonical["state"],
                        "native_rows": legacy_meta["source_rows"]}
            write_once(preparation / "manifest.json", canonical_json(manifest) + b"\n")
            write_once(preparation / "README.md", README.encode())
            self.verify_tree(preparation)
            if old:
                if old.get("status") == "LEGACY_BACKUP_VERIFIED":
                    candidates = sorted((self.repo / "snapshots").iterdir())
                    previous = candidates[-1]
                    all_hosts = {
                        json.loads((snapshot / "bundle.json").read_text()).get("host")
                        for snapshot in candidates
                    }
                    if all_hosts != {self.host}:
                        raise RuntimeError("Legacy snapshots include foreign writer host")
                    previous_host = self.host
                else:
                    previous = self.repo
                    previous_host = json.loads((previous / "manifest.json").read_text()).get("host")
                self.require_continuous_history(previous, preparation, previous_host)
            self.scan_private_secrets(preparation)
            # A failure beyond this point leaves only a dirty private Git
            # checkout. Its next execution refuses it instead of overwriting.
            if old:
                existing_names = (("snapshots", "README.md") if
                                  old.get("status") == "LEGACY_BACKUP_VERIFIED"
                                  else ("canonical", "legacy", "metadata",
                                        "manifest.json", "README.md"))
                self.git("rm", "--quiet", "-r", "--", *existing_names)
            for file in snap._walk_files(preparation):
                rel = file.relative_to(preparation)
                snap._copy_secure(file, self.repo / rel)
            self.git("add", "-A")
            self.verify_tree(self.repo)
            self.git("-c", "user.name=OptMem Sync", "-c",
                     "user.email=optmem@localhost", "commit", "--quiet",
                     "-m", "backup(optmem): canonical memory update")
            return {"status": "CREATED", "records": canonical["state"]["records"]}
        finally:
            if preparation.exists():
                shutil.rmtree(preparation)

    def push_and_audit(self) -> dict:
        if not self.has_ref("refs/heads/main"):
            raise RuntimeError("No committed backup branch")
        for attempt in range(3):
            result = self.git("push", "origin", "HEAD:refs/heads/main", check=False)
            if result.returncode == 0:
                break
            if attempt == 2:
                raise RuntimeError("Push failed; no force push or Git history replacement")
            self.update_git()  # Raises on divergence, never merges SQLite.
        head = self.git("rev-parse", "HEAD").stdout.strip()
        with tempfile.TemporaryDirectory(prefix="verify-github-", dir=self.state) as tmp:
            checkout = Path(tmp) / "clone"
            subprocess_checked(["/usr/bin/git", "clone", "--quiet", "--depth=1",
                                "--no-tags", "--branch", "main",
                                self.remote, str(checkout)], self.env)
            private_dir(checkout)
            observed_head = subprocess_checked(["/usr/bin/git", "-C", str(checkout),
                                                  "rev-parse", "HEAD"], self.env).stdout.strip()
            if head != observed_head:
                raise RuntimeError("Fresh GitHub clone HEAD differs")
            verified = self.verify_tree(checkout)
        return {"status": "PUSHED_AND_RESTORED", "remote": OWNER,
                "commit": head[:12], "records": verified["records"],
                "native_rows": verified["native_rows"]}

    def sync(self) -> dict:
        with exclusive(self.state):
            self.update_git()
            auxiliary = self.verified_sources()
            private_dir(self.production / "snapshots", create=True)
            tmp = self.production / "snapshots" / ("sync-" + uuid.uuid4().hex[:18])
            try:
                prod.seal(tmp)
                result = self.commit_tree(tmp, auxiliary)
            finally:
                if tmp.exists():
                    shutil.rmtree(tmp)
            if result["status"] == "UNCHANGED":
                remote_matches = (self.has_ref("refs/remotes/origin/main")
                    and self.git("rev-parse", "HEAD").stdout.strip() ==
                        self.git("rev-parse", "origin/main").stdout.strip())
                if remote_matches:
                    return {"status": "UNCHANGED", "remote": OWNER,
                            "records": result["records"]}
            return self.push_and_audit()

    def restore(self, revision: str) -> dict:
        """Recover HEAD or an ancestor Git commit to a NEW isolated directory."""
        if not re.fullmatch(r"[0-9a-f]{7,40}", revision):
            raise ValueError("Restoration requires a commit SHA, not a path")
        with exclusive(self.state):
            self.update_git()
            resolved = self.git("rev-parse", "--verify",
                                revision + "^{commit}").stdout.strip()
            if (self.git("merge-base", "--is-ancestor", resolved, "HEAD",
                         check=False).returncode != 0):
                raise RuntimeError("Unknown or unreachable backup revision")
            private_dir(self.recovery, create=True)
            destination = self.recovery / resolved
            if destination.exists() or destination.is_symlink():
                raise FileExistsError("Revision already restored; no overwrite")
            # Git archive emits only objects from the given historical commit.
            result = subprocess.run(["/usr/bin/git", "-C", str(self.repo),
                                     "archive", "--format=tar", resolved],
                                    capture_output=True, timeout=90, env=self.env)
            if result.returncode != 0 or len(result.stdout) > MAX_BYTES:
                raise RuntimeError("Git archive retrieval invalid or too large")
            staging = self.recovery / (".pending-" + uuid.uuid4().hex)
            staging.mkdir(mode=0o700)
            try:
                observed = set()
                total = 0
                with tarfile.open(fileobj=io.BytesIO(result.stdout), mode="r:") as archive:
                    for member in archive:
                        relative = safe_relative(member.name.rstrip("/"))
                        target = staging / relative
                        if member.isdir():
                            private_dir(target, create=True)
                            continue
                        if not member.isfile() or relative.as_posix() in observed:
                            raise RuntimeError("Nonregular or duplicate Git object")
                        observed.add(relative.as_posix())
                        total += member.size
                        if len(observed) > MAX_FILES or total > MAX_BYTES:
                            raise RuntimeError("Too many/big restored Git objects")
                        reader = archive.extractfile(member)
                        if reader is None:
                            raise RuntimeError("Unreadable archive content")
                        write_once(target, reader.read())
                if (staging / "manifest.json").is_file():
                    proof = self.verify_tree(staging)
                else:
                    legacy = staging / "snapshots"
                    if not legacy.is_dir() or len(list(legacy.iterdir())) != 1:
                        raise RuntimeError("Legacy Git revision is not a single verified snapshot")
                    proof = self.verify_generation(next(legacy.iterdir()))
                staging.rename(destination)
                return {"status": "RECOVERED_ISOLATED", "revision": resolved[:12],
                        "destination": str(destination),
                        "records": proof["records"], "native_rows": proof["native_rows"]}
            finally:
                if staging.exists():
                    shutil.rmtree(staging)

def main():
    cli = argparse.ArgumentParser(description="Private GitHub OptMem backup and restore")
    subs = cli.add_subparsers(dest="command", required=True)
    subs.add_parser("sync")
    restore = subs.add_parser("restore")
    restore.add_argument("--revision", required=True)
    verify = subs.add_parser("verify")
    verify.add_argument("--revision", required=False)
    args = cli.parse_args()
    os.umask(0o077)
    store = GitMemory()
    if args.command == "sync":
        result = store.sync()
    elif args.command == "restore":
        result = store.restore(args.revision)
    else:
        with exclusive(store.state):
            store.update_git()
            if args.revision:
                if not re.fullmatch(r"[0-9a-f]{7,40}", args.revision):
                    raise ValueError("Invalid Git commit")
                full=store.git("rev-parse", "--verify", args.revision+"^{commit}").stdout.strip()
                if store.git("rev-parse", "HEAD").stdout.strip()!=full:
                    raise RuntimeError("Verify old revisions with restore --revision")
            result = store.verify_tree(store.repo)
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()

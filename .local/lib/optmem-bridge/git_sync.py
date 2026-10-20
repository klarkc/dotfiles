#!/usr/bin/env python3
"""Private, fail-closed GitHub snapshots of OptMem. Never pull onto live memory."""
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
import uuid

import production_operator as po
import production_preflight as gate
import production_snapshot as prod
import snapshot_stage as snap

REMOTE = "git@github.com:klarkc/memory.git"
OWNER = "klarkc/memory"
SCHEMA = "OptMem-private-Git-recovery/v1"
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
README = """# OptMem private memory backups

This repository contains private, unencrypted canonical OptMem memory,
SQLite provenance and retained legacy memory. KEEP THE REPOSITORY PRIVATE.
Never copy its history to a public repository or fork.

Each snapshots/<id> directory is a complete, immutable, SHA-256 verified
recovery generation. The live writer is never changed by Git fetch or pull.
The dotfiles git_sync.py restore command copies a verified generation to a
new private recovery folder, and NEVER replaces active memory automatically.

Recovery on another computer requires explicit installation and a quiescent
cutover. Checksums detect corruption, not malicious tampering by somebody
who can rewrite both data and manifests. GitHub SSH transport is verified.
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
                result = self.git("-c", "user.name=OptMem Sync", "-c",
                                  "user.email=optmem@localhost", "merge",
                                  "--no-edit", "--no-ff", "origin/main", check=False)
                if result.returncode:
                    self.git("merge", "--abort", check=False)
                    raise RuntimeError("Concurrent memory Git histories conflicted")
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

    def known_fingerprints(self) -> set[str]:
        snapshots = self.repo / "snapshots"
        if not snapshots.exists():
            return set()
        private_dir(snapshots)
        found = set()
        for root in snapshots.iterdir():
            private_dir(root)
            doc = json.loads(private_file(root / "bundle.json").read_text())
            if doc.get("schema") != SCHEMA:
                raise RuntimeError("Unknown memory snapshot schema")
            if doc.get("host") == self.host:
                found.add(doc["fingerprint"])
        return found

    def verify_generation(self, root: Path) -> dict:
        private_dir(root)
        bundle = json.loads(private_file(root / "bundle.json").read_text())
        valid_name = r"[a-z0-9-]{1,40}-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{12}"
        if (bundle.get("schema") != SCHEMA
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

    def commit_snapshot(self, sealed: Path, aux: dict[str, dict]) -> dict:
        canonical = json.loads((sealed / "manifest.json").read_text())
        if canonical.get("schema") != "OptMem-production-snapshot/v1":
            raise RuntimeError("Unexpected canonical snapshot schema")
        fingerprint = hashlib.sha256(canonical_json(
            {"canonical": canonical["files"], "ancillary": aux})).hexdigest()
        if fingerprint in self.known_fingerprints():
            return {"status": "UNCHANGED", "records": canonical["state"]["records"]}
        generation = (self.host + "-" +
                      datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ") +
                      "-" + uuid.uuid4().hex[:12])
        target = self.repo / "snapshots" / generation
        private_dir(target / "canonical", create=True)
        sources = {}
        for file in snap._walk_files(sealed):
            key = "canonical/" + file.relative_to(sealed).as_posix()
            sources[key] = file
        sources.update(self.ancillary())
        listed = {}
        for key, file in sorted(sources.items()):
            path = target / safe_relative(key)
            copied = snap._copy_secure(file, path)
            if key in aux and copied != aux[key]:
                raise RuntimeError("Legacy metadata changed while preparing backup")
            listed[key] = copied
        native = json.loads((target / "legacy/manifest.json").read_text())
        bundle = {"schema": SCHEMA, "generation": generation, "host": self.host,
                  "created_utc": datetime.now(timezone.utc).isoformat(),
                  "fingerprint": fingerprint, "files": listed,
                  "canonical_state": canonical["state"], "native_rows": native["source_rows"]}
        write_once(target / "bundle.json", canonical_json(bundle) + b"\n")
        self.verify_generation(target)
        self.scan_private_secrets(target)
        if not self.has_ref("refs/heads/main"):
            write_once(self.repo / "README.md", README.encode())
            self.git("add", "--", "README.md")
        self.git("add", "--", "snapshots/" + generation)
        self.git("-c", "user.name=OptMem Sync", "-c",
                 "user.email=optmem@localhost", "commit", "--quiet",
                 "-m", "backup(optmem): " + generation)
        return {"status": "CREATED", "generation": generation,
                "records": canonical["state"]["records"]}

    def push_and_audit(self, generation: str | None) -> dict:
        if not self.has_ref("refs/heads/main"):
            raise RuntimeError("No committed backup branch")
        for attempt in range(3):
            result = self.git("push", "origin", "HEAD:refs/heads/main", check=False)
            if result.returncode == 0:
                break
            if attempt == 2:
                raise RuntimeError("Git push failed after safe retries; local data retained")
            self.update_git()
        head = self.git("rev-parse", "HEAD").stdout.strip()
        if generation is None:
            # Verify newest local-host generation even when no new writes.
            generations = self.local_generations()
            if not generations:
                raise RuntimeError("No backup generation exists to validate")
            generation = generations[-1]
        with tempfile.TemporaryDirectory(prefix="verify-github-", dir=self.state) as tmp:
            checkout = Path(tmp) / "clone"
            subprocess_checked(["/usr/bin/git", "clone", "--quiet", "--depth=1",
                                "--no-tags", "--branch", "main",
                                self.remote, str(checkout)], self.env)
            private_dir(checkout)
            observed_head = subprocess_checked(["/usr/bin/git", "-C", str(checkout),
                                                  "rev-parse", "HEAD"], self.env).stdout.strip()
            if head != observed_head:
                raise RuntimeError("GitHub HEAD differs from pushed local commit")
            verified = self.verify_generation(checkout / "snapshots" / generation)
        return {"status": "PUSHED_AND_RESTORED",
                "remote": OWNER, "commit": head[:12],
                "generation": generation, "records": verified["records"],
                "native_rows": verified["native_rows"]}

    def local_generations(self) -> list[str]:
        directory = self.repo / "snapshots"
        if not directory.exists():
            return []
        private_dir(directory)
        found = []
        for path in directory.iterdir():
            private_dir(path)
            bundle = json.loads(private_file(path / "bundle.json").read_text())
            if bundle.get("host") == self.host:
                found.append(path.name)
        return sorted(found)

    def sync(self) -> dict:
        with exclusive(self.state):
            self.update_git()
            aux = self.verified_sources()
            private_dir(self.production / "snapshots", create=True)
            temporary = self.production / "snapshots" / ("sync-" + uuid.uuid4().hex[:18])
            try:
                prod.seal(temporary)
                created = self.commit_snapshot(temporary, aux)
            finally:
                if temporary.exists():
                    shutil.rmtree(temporary)
            if created["status"] == "UNCHANGED":
                remote_matches = (self.has_ref("refs/remotes/origin/main")
                    and self.git("rev-parse", "HEAD").stdout.strip() ==
                        self.git("rev-parse", "origin/main").stdout.strip())
                if remote_matches:
                    return {"status": "UNCHANGED", "remote": OWNER,
                            "records": created["records"]}
                return self.push_and_audit(None)
            return self.push_and_audit(created["generation"])

    def restore(self, generation: str) -> dict:
        if not re.fullmatch(r"[a-z0-9-]{1,40}-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{12}", generation):
            raise ValueError("Recovery ID rejected")
        with exclusive(self.state):
            self.update_git()
            private_dir(self.recovery, create=True)
            source = self.repo / "snapshots" / generation
            expected = self.verify_generation(source)
            destination = self.recovery / generation
            if destination.exists() or destination.is_symlink():
                raise FileExistsError("Recovery destination already exists")
            staging = self.recovery / (".pending-" + uuid.uuid4().hex)
            staging.mkdir(mode=0o700)
            recovered = staging / generation
            recovered.mkdir(mode=0o700)
            try:
                for path in snap._walk_files(source):
                    snap._copy_secure(path, recovered / path.relative_to(source))
                if self.verify_generation(recovered) != expected:
                    raise RuntimeError("Independent recovery folder is incomplete")
                recovered.rename(destination)
                return {"status": "RECOVERED_ISOLATED", "generation": generation,
                        "destination": str(destination),
                        "records": expected["records"],
                        "native_rows": expected["native_rows"]}
            finally:
                shutil.rmtree(staging)


def main():
    cli = argparse.ArgumentParser(description="Private GitHub OptMem backup and restore")
    subs = cli.add_subparsers(dest="command", required=True)
    subs.add_parser("sync")
    restore = subs.add_parser("restore")
    restore.add_argument("--generation", required=True)
    verify = subs.add_parser("verify")
    verify.add_argument("--generation", required=True)
    args = cli.parse_args()
    os.umask(0o077)
    store = GitMemory()
    if args.command == "sync":
        result = store.sync()
    elif args.command == "restore":
        result = store.restore(args.generation)
    else:
        with exclusive(store.state):
            store.update_git()
            result = store.verify_generation(store.repo / "snapshots" / args.generation)
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()

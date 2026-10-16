#!/usr/bin/env python3
"""Audit tracked snapshots of a verified-public Git repository with Gitleaks."""
import argparse
import json
import os
from pathlib import Path
import re
import secrets
import shutil
import stat
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request


def git_environment():
    return {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}


def run_git(repo, *args, check=False):
    return subprocess.run(["git", "-C", str(repo), *args], stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, check=check, env=git_environment())


def public_metadata(url):
    """Return True only for an API response explicitly confirming public visibility."""
    if url.startswith("git@"):
        host_path = url[4:].split(":", 1)
        if len(host_path) != 2:
            return False
        host, path = host_path
    else:
        parsed = urllib.parse.urlsplit(url)
        if parsed.scheme not in ("https", "ssh") or parsed.password or parsed.query or parsed.fragment:
            return False
        if parsed.username and not (parsed.scheme == "ssh" and parsed.username == "git"):
            return False
        host, path = parsed.hostname or "", parsed.path.strip("/")
    parts = path.removesuffix(".git").split("/")
    if not all(re.fullmatch(r"[A-Za-z0-9_.-]+", part) and part not in (".", "..") for part in parts):
        return False
    if host == "github.com" and len(parts) == 2:
        endpoint = "https://api.github.com/repos/" + "/".join(urllib.parse.quote(p, safe="") for p in parts)
        predicate = lambda d: d.get("private") is False and d.get("visibility") == "public"
    elif host == "gitlab.com" and len(parts) >= 2:
        endpoint = "https://gitlab.com/api/v4/projects/" + urllib.parse.quote("/".join(parts), safe="")
        predicate = lambda d: d.get("visibility") == "public"
    elif host == "bitbucket.org" and len(parts) == 2:
        endpoint = "https://api.bitbucket.org/2.0/repositories/" + "/".join(urllib.parse.quote(p, safe="") for p in parts)
        predicate = lambda d: d.get("is_private") is False
    else:
        return False
    request = urllib.request.Request(endpoint, headers={"Accept": "application/json", "User-Agent": "git-privacy-audit"})
    try:
        with urllib.request.urlopen(request, timeout=8) as response:
            data = json.loads(response.read())
        return isinstance(data, dict) and predicate(data)
    except (OSError, urllib.error.URLError, ValueError, json.JSONDecodeError):
        return False


def verified_public_repo(repo):
    # Only Git metadata is read here. No working-tree paths are inspected before this gate.
    root = run_git(repo, "rev-parse", "--show-toplevel")
    if root.returncode:
        return None
    root_path = Path(os.fsdecode(root.stdout.rstrip(b"\n"))).resolve()
    remote = run_git(root_path, "remote", "get-url", "origin")
    if remote.returncode:
        return None
    url = os.fsdecode(remote.stdout.rstrip(b"\n"))
    return root_path if public_metadata(url) else None


def ignored(repo, path):
    try:
        rel = path.relative_to(repo)
    except ValueError:
        return True
    return run_git(repo, "check-ignore", "-q", "--", str(rel)).returncode == 0


def safe_report(path):
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return None
    if not isinstance(data, list):
        return None
    result = []
    for item in data:
        if not isinstance(item, dict) or not isinstance(item.get("StartLine"), int) or not isinstance(item.get("File"), str):
            return None
        rule = item.get("RuleID", "unknown")
        if not isinstance(rule, str) or not re.fullmatch(r"[a-zA-Z0-9_-]{1,120}", rule):
            rule = "unknown"
        commit = item.get("Commit", "")
        if not isinstance(commit, str) or not re.fullmatch(r"[a-fA-F0-9]{7,64}", commit):
            commit = ""
        result.append({"RuleID": rule, "File": item["File"], "StartLine": item["StartLine"], "Commit": commit[:12]})
    return result


def instruction_policy(content):
    text = content.decode("utf-8", "replace").lower()
    public = any(x in text for x in ("repository is public", "public repository", "repositório é público", "repositorio e publico", "publicly readable"))
    privacy = re.search(
        r"\b(?:never|do not|must not|n[aã]o|jamais)\b[^.\n]{0,180}"
        r"\b(?:commit|publish|expose|disclose|versione|versionar|publique|publicar|exponha|expor|divulgue|divulgar)\b"
        r"[^.\n]{0,180}\b(?:secret\w*|credential\w*|token\w*|password\w*|privat\w*|privad\w*|sens[ií]v\w*|credencia\w*|senha\w*|confidencia\w*)\b",
        text,
    ) is not None
    return public and privacy


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--history", action="store_true")
    parser.add_argument("--report-dir")
    parser.add_argument("--gitleaks", default="gitleaks")
    args = parser.parse_args(argv)
    try:
        repo = verified_public_repo(Path(args.repo).expanduser().absolute())
    except (OSError, ValueError, subprocess.SubprocessError):
        repo = None
    if repo is None:
        print("REFUSED: public repository status could not be verified; no content was read or report created.", file=sys.stderr)
        return 2

    scanner = shutil.which(args.gitleaks) if os.sep not in args.gitleaks else args.gitleaks
    if not scanner or not Path(scanner).is_file() or not os.access(scanner, os.X_OK):
        print("ERROR: approved Gitleaks executable unavailable; no scan performed", file=sys.stderr)
        return 2
    if args.report_dir:
        report = Path(args.report_dir).expanduser().absolute()
    else:
        report = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state")) / "opencode/git-privacy-audit" / secrets.token_hex(12)
    if report.exists() or report.is_symlink():
        print("ERROR: report path must be a new directory", file=sys.stderr)
        return 2
    # Reject symlinked parents and any containing repository without an ignored-path guarantee.
    cur = report.parent
    while cur != cur.parent:
        if cur.is_symlink():
            print("ERROR: symlink in report path", file=sys.stderr)
            return 2
        marker = run_git(cur, "rev-parse", "--show-toplevel")
        if not marker.returncode:
            parent_repo = Path(os.fsdecode(marker.stdout.rstrip(b"\n"))).resolve()
            if not ignored(parent_repo, report):
                print("ERROR: report path is inside a repository and not confirmed ignored", file=sys.stderr)
                return 2
        cur = cur.parent
    os.umask(0o077)
    try:
        report.mkdir(parents=True, mode=0o700)
    except OSError:
        print("ERROR: could not create private report directory", file=sys.stderr)
        return 2
    os.chmod(report, 0o700)
    try:
        listing = run_git(repo, "ls-files", "-z", "--stage")
        if listing.returncode:
            raise RuntimeError
        entries = {}
        for row in listing.stdout.split(b"\0"):
            if not row:
                continue
            meta, name = row.split(b"\t", 1)
            mode, oid, stage = meta.split()
            if stage != b"0":
                raise RuntimeError
            entries[name] = (mode, oid)
        snapshots = []
        for label in ("index", "working"):
            base = report / label
            base.mkdir(mode=0o700)
            for name, (mode, oid) in entries.items():
                if mode == b"160000":
                    continue
                rel = Path(os.fsdecode(name))
                if rel.is_absolute() or ".." in rel.parts:
                    continue
                if mode == b"120000":
                    continue
                indexed = run_git(repo, "cat-file", "blob", oid.decode())
                if indexed.returncode:
                    raise RuntimeError
                content = indexed.stdout
                if label == "working":
                    target = repo
                    unsafe = False
                    for part in rel.parts:
                        target = target / part
                        if target.is_symlink():
                            unsafe = True
                            break
                    if unsafe:
                        continue
                    try:
                        if stat.S_ISREG(target.lstat().st_mode):
                            content = target.read_bytes()
                    except FileNotFoundError:
                        continue
                dest = base / rel
                dest.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
                dest.write_bytes(content)
                os.chmod(dest, 0o600)
            snapshots.append(base)
        config, ignore_file = report / "gitleaks.toml", report / "empty.ignore"
        config.write_text("[extend]\nuseDefault = true\n", encoding="utf-8")
        ignore_file.write_text("", encoding="utf-8")
        for p in (config, ignore_file):
            os.chmod(p, 0o600)
        findings, partial = [], False
        for base in snapshots:
            out = report / (base.name + ".json")
            cmd = [scanner, "dir", str(base), "--redact=100", "--report-format=json", "--report-path", str(out),
                   "--config", str(config), "--ignore-gitleaks-allow", "--gitleaks-ignore-path", str(ignore_file)]
            result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=git_environment())
            (report / (base.name + ".scanner.log")).write_bytes(result.stdout + result.stderr)
            os.chmod(report / (base.name + ".scanner.log"), 0o600)
            parsed = safe_report(out)
            if result.returncode not in (0, 1) or parsed is None:
                partial = True
            else:
                for item in parsed:
                    item["Source"] = "snapshot"
                findings += parsed
        if args.history:
            out = report / "history.json"
            cmd = [scanner, "git", str(repo), "--redact=100", "--report-format=json", "--report-path", str(out),
                   "--config", str(config), "--ignore-gitleaks-allow", "--gitleaks-ignore-path", str(ignore_file),
                   "--log-opts=--all --full-history"]
            result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=git_environment())
            (report / "history.scanner.log").write_bytes(result.stdout + result.stderr)
            os.chmod(report / "history.scanner.log", 0o600)
            parsed = safe_report(out)
            if result.returncode not in (0, 1) or parsed is None:
                partial = True
            else:
                for item in parsed:
                    item["Source"] = "history"
                findings += parsed
        for base in snapshots:
            if not (base / "AGENTS.md").is_file():
                findings.append({"RuleID": "AGENTS_ROOT_MISSING", "File": "AGENTS.md", "StartLine": 0, "Commit": "", "Source": base.name})
            for name in entries:
                rel = Path(os.fsdecode(name))
                if rel.name != "AGENTS.md" or not (base / rel).is_file():
                    continue
                if not instruction_policy((base / rel).read_bytes()):
                    findings.append({"RuleID": "AGENTS_PUBLIC_PRIVACY_POLICY_MISSING", "File": str(rel), "StartLine": 1, "Commit": "", "Source": base.name})
                else:
                    print(json.dumps({"policy": "recommendation-detected", "file": str(rel), "source": base.name, "validation": "heuristic; manual review required"}))
        if partial:
            print("INCONCLUSIVE: scanner failure or malformed report; raw details are private.")
        if not findings and not partial:
            print("No findings reported; this is not proof that the repository is clean.")
        remote_commits = set()
        if args.history:
            refs = run_git(repo, "rev-list", "--remotes")
            if refs.returncode == 0:
                remote_commits = set(refs.stdout.decode("ascii", "ignore").splitlines())
        for item in findings:
            commit = item["Commit"]
            published = "unknown"
            if item.get("Source") == "history" and commit and any(c.startswith(commit) for c in remote_commits):
                published = "yes (cached remote ref)"
            print(json.dumps({**item, "Source": item.get("Source", "policy"), "Published": published}))
        if any(f["RuleID"].startswith("AGENTS_") for f in findings):
            print("AGENTS policy check is heuristic; manual review required. I can propose a correction with your consent.")
        print("Tracked snapshots only; symlinks, submodules, LFS payloads, reflogs and unsupported binary contents may be uncovered.")
        return 2 if partial else (1 if findings else 0)
    except (OSError, RuntimeError, subprocess.SubprocessError):
        print("ERROR: audit failed; results are inconclusive. Details remain private.", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())

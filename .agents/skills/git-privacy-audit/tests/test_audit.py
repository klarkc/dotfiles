import contextlib
import io
import json
import os
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).parents[1] / "scripts"))
import audit


class AuditTests(unittest.TestCase):
    def setUp(self):
        clean_git = patch.dict(os.environ, audit.git_environment(), clear=True)
        clean_git.start()
        self.addCleanup(clean_git.stop)
        self.tmp = tempfile.TemporaryDirectory()
        self.base = Path(self.tmp.name)
        self.repo = self.base / "arbitrary-repository-name"
        self.repo.mkdir()
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        subprocess.run(["git", "-C", str(self.repo), "remote", "add", "origin",
                        "https://github.com/example-org/example-project.git"], check=True)
        self.scanner = self.base / "fake-gitleaks"
        self.scanner.write_text("#!" + sys.executable + "\n" +
            "import json,os,sys\n"
            "from pathlib import Path\n"
            "args=sys.argv[1:]\n"
            "if os.environ.get('FAKE_FAIL'):\n print('SCANNER_SECRET_SENTINEL',file=sys.stderr); raise SystemExit(8)\n"
            "assert '--redact=100' in args and '--ignore-gitleaks-allow' in args and '-C' not in args\n"
            "assert Path(args[args.index('--config')+1]).read_text()=='[extend]\\nuseDefault = true\\n'\n"
            "out=Path(args[args.index('--report-path')+1])\n"
            "if args[0]=='dir':\n"
            " p=Path(args[1])\n"
            " assert p.is_dir()\n"
            " open(os.environ['FAKE_SEEN'],'a').write(args[1]+'\\n')\n"
            " files={str(x.relative_to(p)):x.read_text() for x in p.rglob('*') if x.is_file()}\n"
            " with open(os.environ['FAKE_FILES'],'a') as f: f.write(json.dumps({'snapshot':p.name,'files':files})+'\\n')\n"
            "elif args[0]=='git':\n"
            " assert '--log-opts=--all --full-history' in args and Path(args[1]).is_dir()\n"
            "else: raise SystemExit(3)\n"
            "if os.environ.get('FAKE_MALFORMED'): out.write_text('malformed'); raise SystemExit(0)\n"
            "finding={'RuleID':'generic-api-key','File':'tracked.txt','StartLine':1,'Secret':'SYNTHETIC_SECRET_SENTINEL','Match':'SYNTHETIC_SECRET_SENTINEL','Author':'PRIVATE_AUTHOR_SENTINEL'}\n"
            "json.dump([finding] if os.environ.get('FAKE_FINDING') else [],open(out,'w'))\n")
        self.scanner.chmod(0o700)

    def tearDown(self):
        self.tmp.cleanup()

    def invoke(self, *extra, env=None):
        stdout, stderr = io.StringIO(), io.StringIO()
        with patch.object(audit, "verified_public_repo", return_value=self.repo), \
             contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr), \
             patch.dict(os.environ, env or {}, clear=False):
            code = audit.main(["--repo", str(self.repo), "--gitleaks", str(self.scanner), *extra])
        return code, stdout.getvalue(), stderr.getvalue()

    def test_refusal_precedes_inventory_scanner_and_reports(self):
        report = self.base / "must-not-exist"
        with patch.object(audit, "verified_public_repo", return_value=None), \
             patch.object(audit, "run_git", side_effect=AssertionError("inventory accessed")):
            out, err = io.StringIO(), io.StringIO()
            with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
                code = audit.main(["--repo", str(self.repo), "--report-dir", str(report), "--gitleaks", str(self.scanner)])
        self.assertEqual(code, 2)
        self.assertFalse(report.exists())
        self.assertIn("REFUSED", err.getvalue())

    def test_existing_report_directory_and_symlink_are_refused(self):
        existing = self.base / "existing-report"
        existing.mkdir()
        link = self.base / "report-link"
        link.symlink_to(existing, target_is_directory=True)
        for target in (existing, link):
            out, err = io.StringIO(), io.StringIO()
            with patch.object(audit, "verified_public_repo", return_value=self.repo), \
                 contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
                code = audit.main(["--repo", str(self.repo), "--report-dir", str(target), "--gitleaks", str(self.scanner)])
            self.assertEqual(code, 2)
            self.assertIn("new directory", err.getvalue())

    def test_metadata_api_unknown_private_and_public(self):
        class Response:
            def __init__(self, data): self.data = json.dumps(data).encode()
            def __enter__(self): return self
            def __exit__(self, *args): pass
            def read(self): return self.data
        with patch.object(audit.urllib.request, "urlopen", return_value=Response({"private": True, "visibility": "private"})):
            self.assertFalse(audit.public_metadata("https://github.com/example-org/example-project.git"))
        with patch.object(audit.urllib.request, "urlopen", return_value=Response({"private": False, "visibility": "public"})):
            self.assertTrue(audit.public_metadata("https://github.com/example-org/example-project.git"))
        with patch.object(audit.urllib.request, "urlopen", side_effect=OSError("offline")):
            self.assertFalse(audit.public_metadata("https://github.com/example-org/example-project.git"))

    def test_index_and_working_snapshots_and_private_permissions(self):
        f = self.repo / "tracked.txt"
        f.write_text("INDEX_ONLY_SENTINEL")
        subprocess.run(["git", "-C", str(self.repo), "add", "tracked.txt"], check=True)
        f.write_text("WORKTREE_ONLY_SENTINEL")
        seen, files = self.base / "seen", self.base / "files"
        code, out, err = self.invoke(env={"FAKE_SEEN": str(seen), "FAKE_FILES": str(files),
                                          "XDG_STATE_HOME": str(self.base / "state")})
        self.assertEqual(code, 1, err)
        self.assertIn("AGENTS_ROOT_MISSING", out)
        self.assertEqual(len(seen.read_text().splitlines()), 2)
        captured = {r["snapshot"]: r["files"] for r in map(json.loads, files.read_text().splitlines())}
        self.assertEqual(captured["index"]["tracked.txt"], "INDEX_ONLY_SENTINEL")
        self.assertEqual(captured["working"]["tracked.txt"], "WORKTREE_ONLY_SENTINEL")
        report = next((self.base / "state/opencode/git-privacy-audit").iterdir())
        self.assertEqual(stat.S_IMODE(report.stat().st_mode), 0o700)
        self.assertTrue(all(stat.S_IMODE(p.stat().st_mode) == 0o600 for p in report.iterdir() if p.is_file()))

    def test_symlink_file_and_parent_never_followed(self):
        outside = self.base / "outside-secret"
        outside.write_text("OUTSIDE_SENTINEL")
        (self.repo / "link").symlink_to(outside)
        d = self.repo / "dir"
        d.mkdir()
        (d / "file").write_text("TRACKED_BASE")
        subprocess.run(["git", "-C", str(self.repo), "add", "link", "dir/file"], check=True)
        d.rename(self.repo / "dir-old")
        (self.repo / "dir").symlink_to(self.base, target_is_directory=True)
        seen, files = self.base / "seen", self.base / "files"
        code, out, err = self.invoke(env={"FAKE_SEEN": str(seen), "FAKE_FILES": str(files),
                                          "XDG_STATE_HOME": str(self.base / "state")})
        self.assertEqual(code, 1, err)
        self.assertIn("AGENTS_ROOT_MISSING", out)
        self.assertNotIn("outside-secret", files.read_text())
        self.assertNotIn("OUTSIDE_SENTINEL", files.read_text())
        self.assertNotIn("OUTSIDE_SENTINEL", out + err)

    def test_scanner_failure_secret_never_reaches_console(self):
        code, out, err = self.invoke(env={"FAKE_FAIL": "1", "FAKE_SEEN": str(self.base / "seen"),
                                          "FAKE_FILES": str(self.base / "files"),
                                          "XDG_STATE_HOME": str(self.base / "state")})
        self.assertEqual(code, 2)
        self.assertNotIn("SCANNER_SECRET_SENTINEL", out + err)

    def test_agents_policy_missing_and_heuristic_language(self):
        self.assertFalse(audit.instruction_policy(b"No recommendation here"))
        self.assertTrue(audit.instruction_policy(b"This repository is public. Never commit credentials or private information."))
        self.assertTrue(audit.instruction_policy("Este repositório é público. Não publique credenciais.".encode()))
        self.assertFalse(audit.instruction_policy(b"This repository is public. Publish private information."))
        self.assertFalse(audit.instruction_policy(b"This repository is public."))

    def test_agents_index_and_working_policy_drift_is_detected(self):
        path = self.repo / "AGENTS.md"
        path.write_text("This repository is public. Never commit credentials.")
        subprocess.run(["git", "-C", str(self.repo), "add", "AGENTS.md"], check=True)
        path.write_text("This repository is public. Publish private information.")
        code, out, err = self.invoke(env={"FAKE_SEEN": str(self.base / "seen"),
            "FAKE_FILES": str(self.base / "files"), "XDG_STATE_HOME": str(self.base / "state")})
        self.assertEqual(code, 1, err)
        records = [json.loads(line) for line in out.splitlines() if line.startswith("{")]
        self.assertTrue(any(r.get("policy") == "recommendation-detected" and r.get("source") == "index" for r in records))
        self.assertTrue(any(r.get("RuleID") == "AGENTS_PUBLIC_PRIVACY_POLICY_MISSING" and r.get("Source") == "working" for r in records))

    def test_scanner_findings_never_disclose_values_and_history_flags_work(self):
        code, out, err = self.invoke("--history", env={"FAKE_FINDING": "1",
            "FAKE_SEEN": str(self.base / "seen"), "FAKE_FILES": str(self.base / "files"),
            "XDG_STATE_HOME": str(self.base / "state")})
        self.assertEqual(code, 1, err)
        self.assertIn("generic-api-key", out)
        self.assertNotIn("SYNTHETIC_SECRET_SENTINEL", out + err)
        self.assertNotIn("PRIVATE_AUTHOR_SENTINEL", out + err)

    def test_malformed_scanner_report_fails_closed(self):
        code, out, err = self.invoke(env={"FAKE_MALFORMED": "1", "FAKE_SEEN": str(self.base / "seen"),
            "FAKE_FILES": str(self.base / "files"), "XDG_STATE_HOME": str(self.base / "state")})
        self.assertEqual(code, 2)
        self.assertIn("INCONCLUSIVE", out)
        self.assertNotIn("No findings reported", out)

    def test_default_report_path_inside_another_repo_must_be_ignored(self):
        code, out, err = self.invoke(env={"XDG_STATE_HOME": str(self.repo / "unsafe-state")})
        self.assertEqual(code, 2)
        self.assertIn("not confirmed ignored", err)
        self.assertFalse((self.repo / "unsafe-state").exists())

    def test_private_metadata_refuses_before_any_file_inventory(self):
        class Response:
            def __enter__(self): return self
            def __exit__(self, *args): pass
            def read(self): return b'{"private": true, "visibility": "private"}'
        report = self.base / "refused-report"
        real_git = audit.run_git
        def metadata_only(repo, *args, **kwargs):
            self.assertIn(args[0], ("rev-parse", "remote"))
            return real_git(repo, *args, **kwargs)
        with patch.object(audit.urllib.request, "urlopen", return_value=Response()), \
             patch.object(audit, "run_git", side_effect=metadata_only), \
             contextlib.redirect_stderr(io.StringIO()):
            code = audit.main(["--repo", str(self.repo), "--report-dir", str(report)])
        self.assertEqual(code, 2)
        self.assertFalse(report.exists())

    def test_unsupported_and_credential_origins_make_no_network_request(self):
        origins = ("https://token@github.com/example/project.git", "file:///repo", "/repo",
            "https://internal.example.com/example/project.git", "https://github.com/../project.git")
        with patch.object(audit.urllib.request, "urlopen", side_effect=AssertionError("network called")):
            for origin in origins:
                self.assertFalse(audit.public_metadata(origin))

    def test_inherited_hook_environment_cannot_redirect_audit_repository(self):
        with patch.dict(os.environ, {"GIT_WORK_TREE": ".", "GIT_DIR": "/unrelated-repository",
                                     "GIT_INDEX_FILE": "/unrelated-index"}):
            result = audit.run_git(self.repo, "rev-parse", "--show-toplevel")
        self.assertEqual(result.returncode, 0)
        self.assertEqual(Path(result.stdout.decode().strip()), self.repo)

    def test_malformed_report_is_not_clean_and_flags_are_safe(self):
        self.assertIsNone(audit.safe_report(self.base / "absent"))
        malformed = self.base / "malformed.json"
        malformed.write_text("not-json")
        self.assertIsNone(audit.safe_report(malformed))
        skill = Path(__file__).parents[1] / "SKILL.md"
        text = skill.read_text()
        front = text.split("---", 2)[1]
        self.assertIn("metadata:", front)
        self.assertIn('opencode/slash: "true"', front)


if __name__ == "__main__":
    unittest.main()

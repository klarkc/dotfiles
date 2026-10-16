import os
from pathlib import Path
import secrets
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]


class PrivacyIntegration(unittest.TestCase):
    def setUp(self):
        clean_git = patch.dict(os.environ, {key: value for key, value in os.environ.items()
                                           if not key.startswith("GIT_")}, clear=True)
        clean_git.start()
        self.addCleanup(clean_git.stop)

    def test_global_rule_and_project_rule_request_the_skill(self):
        global_rule = (ROOT / ".config/opencode/AGENTS.md").read_text()
        project_rule = (ROOT / "AGENTS.md").read_text()
        self.assertIn("first modification task", global_rule)
        self.assertIn("git-privacy-audit", global_rule)
        self.assertIn("private and unverified", global_rule)
        self.assertIn("existing pre-commit framework", global_rule)
        self.assertIn("git-privacy-audit", project_rule)
        self.assertIn("This repository is PUBLIC", project_rule)
        self.assertIn("!/.config/opencode/AGENTS.md", (ROOT / ".gitignore").read_text())

    def test_existing_hook_framework_preserves_full_test_and_native_scan(self):
        flake = (ROOT / "flake.nix").read_text()
        self.assertIn("hooks.gitleaks =", flake)
        self.assertIn("gitleaks git --pre-commit --staged --redact=100", flake)
        self.assertIn("--config ${gitleaksDefaultConfig}", flake)
        self.assertIn("--gitleaks-ignore-path ${gitleaksEmptyIgnore}", flake)
        self.assertIn("hooks.make-test =", flake)
        self.assertIn('entry = "bash .local/bin/git-hook-make-test"', flake)
        self.assertIn("pkgs.gitleaks", flake)

    def test_native_gitleaks_checks_the_index_not_the_working_file(self):
        scanner = shutil.which("gitleaks")
        self.assertIsNotNone(scanner, "Run inside nix develop: Gitleaks is a declared dependency")
        with tempfile.TemporaryDirectory() as temp:
            base = Path(temp)
            repo = base / "fixture"
            repo.mkdir()
            subprocess.run(["git", "init", "-q", str(repo)], check=True, capture_output=True)
            config = base / "gitleaks.toml"
            config.write_text("[extend]\nuseDefault = true\n")
            ignore = base / "empty.ignore"
            ignore.write_text("")
            command = [scanner, "git", str(repo), "--pre-commit", "--staged", "--redact=100",
                       "--no-banner", "--log-level=warn", "--ignore-gitleaks-allow",
                       "--config", str(config), "--gitleaks-ignore-path", str(ignore)]
            candidate = "0123456789abcdef" * 4 + secrets.token_hex(16)
            path = repo / "settings.txt"
            path.write_text("safe staged text\n")
            subprocess.run(["git", "-C", str(repo), "add", "settings.txt"], check=True, capture_output=True)
            path.write_text('api_key = "' + candidate + '"\n')
            safe = subprocess.run(command, capture_output=True, text=True)
            self.assertEqual(safe.returncode, 0, "Unstaged fixture must not affect the native staged gate")
            subprocess.run(["git", "-C", str(repo), "add", "settings.txt"], check=True, capture_output=True)
            path.write_text("safe working text\n")
            rejected = subprocess.run(command, capture_output=True, text=True)
            self.assertEqual(rejected.returncode, 1, "Synthetic staged credential must be rejected")
            self.assertTrue(candidate not in rejected.stdout + rejected.stderr,
                            "Native scanner must redact the synthetic fixture value")


if __name__ == "__main__":
    os.umask(0o077)
    unittest.main()

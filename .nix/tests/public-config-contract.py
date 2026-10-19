import ast
import json
from pathlib import Path
import tomllib
import unittest


ROOT = Path(__file__).resolve().parents[2]


class PublicConfiguration(unittest.TestCase):
    def test_remote_deployment_has_no_baked_in_defaults(self):
        tree = ast.parse((ROOT / ".nix/desktop-commander-mcp-cloudflare.py").read_text())
        names = {target.id for node in tree.body if isinstance(node, ast.Assign)
                 for target in node.targets if isinstance(target, ast.Name)}
        self.assertFalse(names & {"DEFAULT_HOSTNAME", "ZONE_NAME", "RESOURCE_NAME", "STATE_DIR"})
        controller = (ROOT / ".nix/desktop-commander-mcp.py").read_text()
        self.assertNotIn('add_argument("--hostname"', controller)
        self.assertNotIn('add_argument("--credentials-file"', controller)
        self.assertIn('PRIVATE["load_profile"](required=RUNTIME_KEYS)', controller)
        self.assertIn("TemporaryDirectory", controller)
        self.assertIn('dir=runtime', controller)

    def test_generated_configuration_never_replaces_the_public_template(self):
        renderer = (ROOT / ".nix/zeroclaw-private-config.py").read_text()
        self.assertIn('values = helper["load_profile"]()', renderer)
        self.assertIn('runtime_dir / "config.toml"', renderer)
        self.assertIn('os.replace(temporary, target)', renderer)
        template = tomllib.loads((ROOT / ".zeroclaw/config.toml").read_text())
        self.assertFalse(template["channels"]["discord"]["default"]["enabled"])
        self.assertEqual(template["channels"]["discord"]["default"]["guild_ids"], [])
        self.assertEqual(template["peer_groups"]["discord_default"]["external_peers"], [])

    def test_application_defaults_do_not_contain_user_history_or_home_paths(self):
        kolu = json.loads((ROOT / ".config/kolu/config.json").read_text())
        self.assertEqual(kolu["activityFeed"]["recentRepos"], [])
        self.assertEqual(kolu.get("session", {}).get("terminals", []), [])
        yay = json.loads((ROOT / ".config/yay/config.json").read_text())
        self.assertNotIn("buildDir", yay)
        settings = (ROOT / ".config/shell_gpt/.sgptrc").read_text()
        self.assertNotIn("ROLE_STORAGE_PATH=", settings)
        self.assertNotIn("OPENAI_FUNCTIONS_PATH=", settings)

    def test_private_config_policy_is_unambiguous(self):
        instructions = (ROOT / "AGENTS.md").read_text()
        self.assertIn("$HOME/.profile_override", instructions)
        self.assertIn("PUBLIC", instructions)
        service = (ROOT / ".config/systemd/user/desktop-commander-mcp-tunnel.service").read_text()
        self.assertIn("ConditionPathExists=%h/.profile_override\n", service)
        zero = (ROOT / ".config/systemd/user/zeroclaw.service").read_text()
        self.assertIn("Requires=llm-environment.service zeroclaw-private-config.service\n", zero)
        self.assertIn("BindReadOnlyPaths=%t/zeroclaw-config/config.toml:%h/.zeroclaw/config.toml\n", zero)
        self.assertIn("ExecStart=%h/.nix-profile/bin/zeroclaw daemon\n", zero)
        self.assertNotIn("--config-dir", zero)  # Keep existing workspace and state directories.
        setup = (ROOT / ".config/systemd/user/zeroclaw-private-config.service").read_text()
        self.assertIn("Type=oneshot\n", setup)
        self.assertIn("AssertPathExists=%h/.profile_override\n", setup)
        self.assertIn("ExecStart=/usr/bin/python3 -B %h/.nix/zeroclaw-private-config.py\n", setup)
        self.assertIn("UMask=0077\n", setup)
        self.assertIn('RUNTIME_DIR_NAME = "zeroclaw-config"', (ROOT / ".nix/zeroclaw-private-config.py").read_text())
        self.assertIn("UnsetEnvironment=ZEROCLAW_risk_profiles__default__allowed_roots", zero)


if __name__ == "__main__":
    unittest.main()

# Enpass personal MCP (read-only)

Source: pinned bitterdev/enpass-mcp 0.2.0. flake.nix locks the Git commit,
and .nix/enpass-mcp-runtime.nix locks the native npm dependency tree. The
profile installs enpass-mcp (user registration CLI) and
enpass-mcp-readonly (the ONLY command connected to OpenCode).

## Onboarding (human-only, no master password in LLM context)

1. Open a terminal on host-example with the OS Secret Service keyring unlocked.
   If DBUS_SESSION_BUS_ADDRESS is missing, ensure the user session bus
   exists and set it to unix:path=/run/user/1000/bus.
2. Run the following directly in the terminal to register the local vault:
   enpass-mcp add-vault personal --path "$HOME/Documentos/Enpass/Vaults/primary/vault.enpassdb"
   This verifies the master passphrase in a hidden local prompt and stores it
   in the Linux Secret Service keyring. Never paste the master passphrase
   into chat, .profile_override, or OpenCode.
3. Run enpass-mcp list-vaults. Confirm "password: stored in keychain".
4. Run opencode mcp list. Expect enpass connected; do not print passwords.
5. Verify reads with explicit OpenCode approvals for item lookup/secret tools.

## Boundaries

- enpass-mcp-readonly explicitly sets ENPASS_MCP_ALLOW_WRITES=0,
  overriding an inherited unsafe value.
- OpenCode denies enpass_* by default and allowlists only specific reads;
  get_item, get_password, and get_otp require interactive approval.
- enpass_export_attachment is blocked (it writes plaintext files).
- create_item, delete_item, sync_pull are not advertised by upstream.
- Write requests route to the enpass-write-human-guidance skill which
  describes human GUI steps; it does not mutate vaults or use write APIs.
- The MCP DOES return plaintext passwords to the model when a sensitive
  read is explicitly approved. This version does not yet support secure
  credential-to-browser fill without model exposure; do not claim otherwise.
- The MCP is stdio-only, no listening port and no remote token. The
  .profile_override should not contain an Enpass master password or token.
- Do not test against the real Enpass vault in CI. Offline smoke tests use a
  temporary empty configuration and enforce write-denial even with an
  inherited ENPASS_MCP_ALLOW_WRITES=1.
- If an upstream version changes, review its tool catalog for new mutators
  and update the deny-by-default policy and smoke tests before bumping.
- This package is a third-party, little-adopted implementation. Perform a
  focused code audit before approving sensitive reads.

## Testing

- nix build .#enpass-mcp-runtime (requires native SQLCipher compilation)
- nix build .#checks.x86_64-linux.enpass-mcp-readonly-contract
- python3 .nix/tests/enpass-mcp-readonly-smoke.py /nix/store/…/bin/enpass-mcp-readonly
- make test inside the isolated worktree / nix develop.

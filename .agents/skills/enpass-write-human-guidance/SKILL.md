---
name: enpass-write-human-guidance
description: Use ONLY when the user asks to create, edit, replace, rotate, or delete credentials or other entries in the personal Enpass password vault. Use the read-only Enpass MCP to look up non-secret metadata if necessary, but produce safe instructions for the human to complete the write in the Enpass desktop UI. Never perform automatic Enpass writes.
---

# Enpass write-human-guidance

The OpenCode Enpass MCP integration is **read-only**. Writes happen in the desktop Enpass GUI by the human, as with atlassian-write-human-guidance, not through an agent or an unrestricted shell command.

## Workflow

1. Determine the exact target: vault, entry title, URL/domain and requested change.
2. For an existing entry, use enpass_list_items for identification. Never read unrelated vault entries. Only call enpass_get_item, enpass_get_password or enpass_get_otp with the user's explicit consent and interactive OpenCode approval; do not include secrets in the final draft.
3. Prepare short Enpass Desktop UI steps. For creation: open Enpass, choose vault > '+' > Login; enter title, correct URL, username, then use Enpass's built-in password generator. For updating: select the entry, choose Edit, change the specific field, then Save. For deletion: explicitly flag irreversibility/recovery/trash and ask before instructing irreversible actions.
4. The user types or pastes passwords inside Enpass; never ask them to send passwords, master passwords, recovery keys, 2FA seeds or tokens in chat.
5. After user confirmation, optionally verify only the presence/metadata via enpass_list_items. Never claim an Enpass write was completed unless the user confirms it.
6. Warn when Enpass cloud sync is failing: a locally saved credential may not be recoverable on other devices. Have the user verify a synced backup before depending on it.

## Non-negotiable boundaries

- Never enable ENPASS_MCP_ALLOW_WRITES, call create_item, delete_item, sync_pull, write to the SQLCipher vault, or use shell/sqlite/http/GUI automation to circumvent the read-only MCP boundary.
- Do not use export_attachment to materialize decrypted secrets.
- Do not print or embed secret values, URLs containing login tokens, vault material, or clipboard values in logs or instructions.
- For an authentication request, use the associated browser's secure autofill/password-manager workflow or an already authorized persistent session, not a plaintext credential passed to the model.
- If the user asks the agent to perform Enpass writes automatically, state that the write layer is deliberately not enabled and supply the manual Enpass steps.

## Output for each write request

Provide the entry title, account URL, the requested field names (not secret values), concise Enpass UI steps, and a follow-up check for saved record and cloud sync.

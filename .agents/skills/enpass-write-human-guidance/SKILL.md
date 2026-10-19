---
name: enpass-write-human-guidance
description: Use when the user asks to create, edit, replace, rotate, or delete credentials in Enpass. Give precise human-only Enpass Desktop instructions, never automate a vault write.
---

# Enpass write-human-guidance

The dotfiles use Enpass Desktop and its official browser extension. OpenCode
has no Enpass vault MCP and no master password access.

When changing a credential:

1. Ask for the target entry name and intended field, not any secret.
2. Explain how to make the change inside Enpass Desktop: choose a vault and
   entry, Edit, change the field in the Enpass GUI, then Save. For a new login,
   select Add > Login, set the title and URL, and use Enpass's own password
   generator. The user performs all writes.
3. Never request, read or log passwords, master passwords, PINs, recovery
   keys, 2FA seeds or decrypted attachments in chat.
4. If the user asks to delete an item, warn about recovery/irreversibility.
5. Confirm sync status before assuming the updated vault is backed up.
6. For browser login use the Enpass official extension, let the user unlock
   via the Enpass UI and then reuse the persistent browser session.

Never use a shell command, SQLCipher update, enpass-mcp, Playwright GUI
automation or a separate tool to change the vault. In particular do not
attempt to bypass the Enpass PIN or the application's timeout policy.

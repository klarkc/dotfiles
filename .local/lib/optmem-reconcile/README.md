# OptMem G4H verification (inactive)

These tools produce local, read-only source audits and isolated acceptance evidence.
They never access GPT Web or enable services. No automatic historical-memory
admission is implemented.

- reconcile.py: reads OpenCode SQLite in a transaction, compares the old hashes,
  inventories all new user-role messages without storing their raw text, and
  lists subsequent dotfiles commits. Treats all candidate decisions as pending.
- test_reconcile.py: regression tests for source drifts, privacy minimization,
  non-text and delegated-handoff cases, and private evidence permissions.
- stage_acceptance.py: checks existing G4P staging notes with three simulated
  read-only budgets (small reader, ZeroClaw, OpenCode), recall, zoom, private backup/restore, and
  synthetic semantic nap/forget, using **disposable copies only**.

The simulated reader identities are NOT evidence of real OpenCode, ZeroClaw
runtime integration; stage writer tests likewise do not grant
production-writer authority. External backup/restore remains blocked by the
user's no-sync/no-push permission boundary.

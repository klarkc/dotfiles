/*
 * Enpass MCP's better-sqlite3-multiple-ciphers native binding calls
 * sqlite3_key/rekey. The nix build sees an upstream SQLite header without
 * those sqlite3mc declarations on some toolchains. Supply only prototypes;
 * these symbols MUST come from the package's bundled sqlite3mc at link time.
 * Bump note: remove this shim if the upstream addon fixes header selection.
 */
#ifdef __cplusplus
#include <sqlite3.h>
extern "C" {
int sqlite3_key(sqlite3 *db, const void *key, int len);
int sqlite3_rekey(sqlite3 *db, const void *key, int len);
}
#endif

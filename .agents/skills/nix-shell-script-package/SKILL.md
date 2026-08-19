# Skill: Nix-packaged shell script with co-located tests

## When to use

Apply when the user asks for any of:

- A new command-line tool written in shell (`bash`, POSIX `sh`)
- A tool that needs runtime inputs from `nixpkgs` (e.g. `lrzip`, `git`, `jq`)
- A tool that must be installable via `nix profile install`
- A tool that comes with a self-test
- A wrapper around a foreign binary (`archive-pack`, `git-lfs`, `kubectl` plugins, ...)

Skip this skill when:

- The tool is implemented in Python/Go/Rust (use the language-specific skill)
- The tool is a one-shot script for personal use (no Nix packaging needed)
- The user explicitly says "skip Nix" or "no packaging"

## Outcome

After applying this skill, you have:

```
.nix/<tool>/
  <tool>.sh          # the main script (regular bash, not a Nix heredoc)
  <tool>-test.sh     # the self-test (also regular bash)
.nix/<tool>.nix      # writeShellApplication wrapper, readFile from .sh files
```

Plus updates to `flake.nix` exposing:

- `packages.<tool>` and `packages.<tool>-test`
- `checks.<tool>-test`

## Workflow

### 1. Lay out the files

Create the directory `.nix/<tool>/` (dot path per `AGENTS.md`).

Write `.nix/<tool>/<tool>.sh` as a **regular bash file**:

- First line: `set -euo pipefail`
- `chmod +x` after writing (so it runs standalone for quick smoke-testing)
- No Nix-heredoc escaping (`''$` becomes `$`, `''${` becomes `${`)
- Use `#!/usr/bin/env bash` if the file should be directly executable

Write `.nix/<tool>/<tool>-test.sh` with the self-test:

- Same `set -euo pipefail` discipline
- Use `mktemp -d` for working directory, `trap 'rm -rf' EXIT` for cleanup
- Each scenario is a discrete sub-block with `log "..."` markers
- Assertions print `FAIL: <reason>` to stderr and exit 1 on miss
- Final `log "PASS"` only when everything passes

### 2. Write the Nix wrapper

In `.nix/<tool>.nix`:

```nix
{
  lib,
  pkgs,
}:

let
  # List every binary the script invokes; runtimeInputs adds them to PATH.
  runtime = with pkgs; [
    coreutils
    gawk
    gnutar
    gzip
    # ...add as needed
  ];

  script = pkgs.writeShellApplication {
    name = "<tool>";
    runtimeInputs = runtime;
    text = builtins.readFile ./<tool>/<tool>.sh;
  };

  test = pkgs.writeShellApplication {
    name = "<tool>-test";
    runtimeInputs = runtime ++ [ script ];
    text = builtins.readFile ./<tool>/<tool>-test.sh;
  };
in
{
  inherit script test;
  inherit runtime;
}
```

Why `builtins.readFile`:

- Lets shellcheck / shfmt / IDE tooling see the `.sh` files
- Removes the need to escape `$` and `${` as `''$` / `''${`
- Keeps the Nix wrapper thin (just name + runtimeInputs + path)

### 3. Wire into `flake.nix`

Add to `packages.default.paths`:

```nix
script
test
```

Add standalone packages:

```nix
packages.<tool> = script;
packages.<tool>-test = test;
```

Add check:

```nix
checks.<tool>-test = pkgs.runCommand "<tool>-test"
  { nativeBuildInputs = [ test ]; }
  ''
    <tool>-test
    touch $out
  '';
```

### 4. Verify with `nix flake check`

```bash
nix --extra-experimental-features "nix-command flakes" fmt
nix --extra-experimental-features "nix-command flakes" flake check
```

Both must pass before commit. `nix fmt` may reformat the `.nix` wrapper, which is
fine; it does not touch the `.sh` files.

### 5. Commit

Files to add:

```bash
git add .nix/<tool>.nix .nix/<tool>/ flake.nix
```

Commit message format (Conventional Commits per `COMMIT_GUIDELINES.md`):

```
feat(<scope>): add <tool> Nix packaging with self-test
```

Or, when adding tests to an existing tool:

```
test(<scope>): add <test-name> subtest for <tool>
```

## Acceptance criteria

Before considering the skill applied:

- [ ] `.sh` files exist on disk and pass `bash -n <file>` (syntax check)
- [ ] `.sh` files are executable (`chmod +x`) and run from a regular shell without Nix
- [ ] `nix flake check` passes (formatting + pre-commit + new check)
- [ ] `nix run .#<tool> -- --help` shows the usage
- [ ] The test file's scenarios each test one observable behavior (file existence,
      exit code, output content, archive size, etc.)
- [ ] At least one subtest covers a regression scenario (something that used to
      work, would fail if a specific implementation detail changes)
- [ ] Commits is on one logical change; no unrelated edits bundled in

## Common pitfalls

- **Forgetting to `git add` the new `.sh` files.** Nix flake requires tracked
  paths; `builtins.readFile` of an untracked file fails the flake check with
  `Path '.nix/<tool>/<tool>.sh' is not tracked by Git`.
- **Bash-only features in scripts that should be POSIX.** `[[ ]]`, `local`,
  arrays, `printf -v`, process substitution `<( )` are bash-only. The
  `runtimeInputs` should include `bash` (or use `bash` as the interpreter).
  `writeShellApplication` defaults to bash, so plain bash is fine.
- **nixfmt choking on shell heredocs inside Nix strings.** The whole point of
  this skill is to avoid that: scripts live in `.sh` files, nixfmt never sees
  their content.
- **Tests asserting on file sizes with tight thresholds.** Use realistic
  thresholds (`< 1.5 × photo_size` rather than `< photo_size`) so test
  fixtures don't break with small format changes.
- **Tests asserting on `--exclude` patterns with loose grep.** Use anchored
  regex (`^\\./sample-b\\.tgz$`) instead of substring (`grep "sample-b"`), or
  split into two greps so each pattern's intent is explicit.

## Reference implementation

The first iteration of this pattern is `.nix/archive-pack/` and
`.nix/backup-tools.nix` in this repo (commit `677e32b`). Use it as a working
example:

- `.nix/archive-pack/archive-pack.sh` (the main pack tool)
- `.nix/archive-pack/archive-pack-test.sh` (the self-test with 8 subtests)
- `.nix/backup-tools.nix` (the Nix wrapper using `builtins.readFile`)
- `flake.nix` exposes `packages.archive-pack`, `packages.archive-pack-test`,
  `checks.archive-pack-test`

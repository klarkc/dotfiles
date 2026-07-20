# WIP: Alacritty host GL via nixGL

## Status: ✅ RESOLVED + Guardrail Patched

nixGL-based Alacritty host-GL wrapper is working. The ligatures `wrapProgram --prefix LD_LIBRARY_PATH` is retained (required for `dlopen`-resolved libs), and child-shell environment is sanitized via `.alacritty.toml` `[env] LD_LIBRARY_PATH = ""`.

**2026-07-20**: Bumped nixGL from `610.43.02` → `610.43.03`, centralized `nvidiaVersion`/`nvidiaHash` in `flake.nix`, and added a launch-time guardrail that compares the host driver version against the pinned version. On mismatch, the guard prints a diagnostic with a corrected rebuild command (`nix profile upgrade klarkc`) and a prefetch URL using the detected host version. Includes `nvidia-smi` fallback when `modinfo` fails. See review fixes below.

## 2026-07-20: Guardrail review fixes

Applied five review findings from post-commit verification:

1. **High — Mismatch remediation command**: Replaced `nix profile install '${self}'#` with `cd /home/klarkc && nix profile upgrade klarkc`. The former used an immutable store path (`${self}`) and `nix profile install` fails to update an already-installed profile entry.
2. **High — Prefetch URL uses pinned version**: Changed from `${nvidiaVersion}` to `$host_version` so the diagnostic URL reflects the detected host driver, not the stale pinned version. Also switched from `nix-prefetch-url download` to `nix store prefetch-file --hash-type sha256 --json` (more idiomatic).
3. **Medium — Missing `nvidia-smi` fallback**: When `modinfo -F version nvidia` fails (empty result), the guard now falls back to `nvidia-smi --query-gpu=driver_version --format=csv,noheader`. If both fail, it warns and proceeds rather than silently blocking.
4. **Medium — Dirty working tree**: Formatting normalization in `alacrittyWithHostGL` block (indentation aligned with committed baseline). No semantic change.
5. **Low — Stale `docs/wip.md`**: Updated to reflect the guardrail implementation and review fixes.

### Diff highlights (`flake.nix`)
- `nvidiaVersion = "610.43.03"` + `nvidiaHash = "sha256-ReLUwTSiPDXlDyU6SqY+fl6NF+PRhdSgfIpY6WEu05I="` centralized in let block
- `modinfo` fallback → `nvidia-smi` → warn-and-proceed (instead of hard-block)
- Diagnostic: `nix profile upgrade klarkc` instead of `nix profile install '${self}'#`
- Prefetch: `nix store prefetch-file --hash-type sha256 --json "https://us.download.nvidia.com/.../$host_version/..."` instead of `nix-prefetch-url download https://international.download.nvidia.com/.../${nvidiaVersion}/...`

## 2026-07-17 follow-up: Alacritty fails again after host driver drift

Finding: this is not caused by the ligatures test or `.alacritty.toml`. `alacritty --config-file /dev/null -vv` still fails after GLX setup with:

```text
Error: Error { raw_code: Some(2), raw_os_message: Some("BadValue (integer parameter out of range for operation)"), kind: BadAttribute }
```

Current host NVIDIA stack is `610.43.03`:

```text
nvidia-smi --query-gpu=driver_version,name --format=csv,noheader -> 610.43.03, NVIDIA GeForce RTX 3060
modinfo -F version nvidia -> 610.43.03
/usr/lib/libGLX_nvidia.so.0 -> /usr/lib/libGLX_nvidia.so.610.43.03
```

Committed `flake.nix` still pins nixGL to `610.43.02` and wraps `nixGLNvidia-610.43.02` (`flake.nix:91-103`). This mismatch is the likely cause of the GLX/X11 startup failure.

Prefetched NVIDIA `610.43.03` installer hash:

```text
nix store prefetch-file --hash-type sha256 --json "https://us.download.nvidia.com/XFree86/Linux-x86_64/610.43.03/NVIDIA-Linux-x86_64-610.43.03.run"
hash: sha256-ReLUwTSiPDXlDyU6SqY+fl6NF+PRhdSgfIpY6WEu05I=
```

Next implementation step: update `flake.nix` explicit nixGL params and wrapper binary from `610.43.02` to `610.43.03`, then rebuild/install the profile and rerun `alacritty --config-file /dev/null -vv` and normal `alacritty -vv`.

Recommended guardrail for future system builds: add a host preflight target/script used by `make nix.Profile` before `nix profile install .` / `nix profile upgrade ...`. Pure `nix flake check` cannot reliably inspect the host NVIDIA kernel module, so this should be an explicit impure host check that:

1. reads host version from `modinfo -F version nvidia` (preferred) or `nvidia-smi --query-gpu=driver_version --format=csv,noheader`,
2. reads the pinned `nvidiaVersion` from `flake.nix`,
3. fails before build/install if they differ,
4. prints both versions plus the exact fields to update (`nvidiaVersion`, `nvidiaHash`, and `nixGLNvidia-${version}` wrapper path),
5. optionally prints a prefetch command for the new installer URL.

Acceptance criteria for the guardrail:

- mismatch example reports: `host NVIDIA driver: 610.43.03`, `flake nixGL NVIDIA: 610.43.02`, and exits non-zero before invoking `nix profile install`;
- match example allows the existing Nix build/install path to continue;
- `flake.nix` should centralize the NVIDIA version string once if possible, so the wrapper binary name derives from the same value and cannot drift independently.

User constraint: routine system upgrades are done with `yay`, which does not run the Nix profile install target or `Makefile`. Therefore the build/install preflight is insufficient by itself.

Revised guardrail design, with user preference:

1. **Preferred**: add a launch-time guard to the generated Alacritty host-GL wrapper. This is better than relying on `Makefile` or pacman hooks because it runs exactly when the stale nixGL pin would cause a cryptic GLX failure.
2. The guard should compare the live host driver (`modinfo -F version nvidia`, fallback to `nvidia-smi`) against the pinned nixGL `nvidiaVersion`.
3. On mismatch, print a prominent diagnostic with the host version, flake version, fields to update (`nvidiaVersion`, `nvidiaHash`, and `nixGLNvidia-${version}` unless derived), and the `nix store prefetch-file` command for the new NVIDIA `.run` URL; then exit non-zero before invoking nixGL/Alacritty.
4. Optional later enhancement: add a pacman hook under `/etc/pacman.d/hooks/` to warn after `yay`/pacman upgrades `nvidia`, `nvidia-dkms`, `nvidia-open`, `nvidia-utils`, or related packages, but do not make this the primary solution.
5. Keep any Nix/Make preflight only as a secondary guard for manual profile rebuilds; it does not cover the normal `yay` workflow.

Acceptance criteria for the preferred launch-time guard:

- after `yay -Syu` upgrades NVIDIA from `610.43.02` to `610.43.03`, launching `alacritty` reports that host NVIDIA is `610.43.03` while nixGL is pinned to `610.43.02`;
- the wrapper exits before invoking `nixGLNvidia-*`, avoiding cryptic GLX `BadValue`/`BadAttribute` failures;
- if versions match, the wrapper execs the existing nixGL command unchanged;
- diagnostic includes a ready-to-run `nix store prefetch-file --hash-type sha256 --json "https://us.download.nvidia.com/XFree86/Linux-x86_64/<version>/NVIDIA-Linux-x86_64-<version>.run"` command;
- `flake.nix` centralizes the NVIDIA version string once, so the wrapper binary name derives from the same `nvidiaVersion` value.

## 2026-07-09 follow-up: `nix` still fails in existing shell

Finding: the current interactive shell can still have the old polluted environment even after the config fix. Evidence from review shell:

```text
LD_LIBRARY_PATH=</nix/store/...libglvnd...:/nix/store/...libxkbcommon...:/usr/lib>
nix --version -> segmentation fault
```

Also found the installed profile `~/.nix-profile/bin/alacritty` still points at the previous `/usr/lib` fallback wrapper:

```text
/home/klarkc/.nix-profile/bin/alacritty -> /nix/store/jmf7...-alacritty-host-gl/bin/alacritty
wrapper prefixes LD_LIBRARY_PATH=/usr/lib
```

The newly built `./result/bin/alacritty` points at the intended nixGL wrapper, while new Alacritty child shells are sanitized by `.alacritty.toml`:

```text
TERM=xterm-256color
LD_LIBRARY_PATH=<>
```

Action needed outside code changes: refresh the installed profile to the newly built flake output and restart existing Alacritty/tmux/shell sessions, or temporarily run `unset LD_LIBRARY_PATH` / `env -u LD_LIBRARY_PATH nix ...` in already-open polluted shells.

## Current request

User selected **Option B**: continue pursuing an actual nixGL-based Alacritty host-GL wrapper instead of the known-working `/usr/lib` fallback.

## Current repository evidence

- `flake.nix` currently imports nixGL manually with explicit `nvidiaVersion = "610.43.02"` and `nvidiaHash = "sha256-MDSgVLtM33dS/43CclZMsQVROAS/9TU4lFkBsWyndGM="`.
- `flake.nix` keeps `nixGL.inputs.nixpkgs.follows = "nixpkgs"`; `flake.lock` shows nixGL revision `b6105297e6f0cd041670c3e8628394d4ee247ed5` and follows the root nixpkgs.
- Current uncommitted `flake.nix` diff wraps `${nixGLPkgs.nixGLNvidia}/bin/nixGLNvidia-610.43.02`, but still prefixes `LD_LIBRARY_PATH=/usr/lib`; this violates the nixGL-only acceptance goal and should be removed.
- Reported latest evaluation failure: `nix flake check --no-build` fails with `function 'anonymous lambda' called with unexpected argument 'kernel'` from nixpkgs `nvidia-x11/generic.nix`.

## nixGL source facts

From nixGL upstream `default.nix` and `nixGL.nix`:

- `default.nix` accepts explicit `nvidiaVersion` and `nvidiaHash` arguments.
- If `nvidiaVersion` is `null`, nixGL auto-detection creates `impure-nvidia-version-file` with `builtins.currentTime`, which fails pure flake evaluation.
- If `nvidiaVersion` is non-null, nixGL returns versioned packages from `top.nvidiaPackages`, including a binary named `nixGLNvidia-${version}`.
- The generic flake package `packages.${system}.nixGLNvidia` uses auto-detection and is therefore expected to fail pure evaluation here.

## Known failed approaches

1. `inputs.nixGL.outputs.packages.${system}.nixGLNvidia`
   - Fails with `attribute 'currentTime' missing` in pure eval because it uses auto-detection.
2. Manual import without passing `pkgs`
   - Fails pure eval via `import <nixpkgs>`.
3. Manual import with mismatched nixpkgs revisions
    - Fails inside `nvidia-x11/generic.nix` with unexpected `kernel` argument.
4. Manual import with current nixGL `main` plus root nixpkgs-unstable
   - Fails because nixGL revision `b6105297` still calls `linuxPackages.nvidia_x11.override { }`, while current nixpkgs requires the NVIDIA package call chain to handle kernel-module arguments differently.

## Upstream status

- nixGL PR #223 (`Update nixGL for latest nixpkgs: nvidia kernel modules and package deprecations`) is open and directly related to recent nixpkgs NVIDIA package changes.
- PR #223 also replaces deprecated `xorg.*` references with top-level libraries.
- Because it is unmerged, using upstream `main` with root nixpkgs-unstable remains risky without an overlay/patch.

## Proposed implementation path

Use manual nixGL import with explicit NVIDIA params **and** pass a nixpkgs instance compatible with the root flake's pinned nixpkgs:

```nix
nixGLPkgs = import inputs.nixGL {
  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
  nvidiaVersion = "610.43.02";
  nvidiaHash = "sha256-MDSgVLtM33dS/43CclZMsQVROAS/9TU4lFkBsWyndGM=";
};
```

Then wrap the versioned nixGL binary, not the auto package:

```nix
alacrittyWithHostGL = pkgs.runCommand "alacritty-host-gl" {
  nativeBuildInputs = [ pkgs.makeWrapper ];
} ''
  mkdir -p $out/bin
  makeWrapper "${nixGLPkgs.nixGLNvidia}/bin/nixGLNvidia-610.43.02" "$out/bin/alacritty" \
    --add-flags "${alacrittyWithLigatures}/bin/alacritty"
'';
```

Important: verify the actual binary name under `${nixGLPkgs.nixGLNvidia}/bin` after build/eval. Upstream source indicates `nixGLNvidia-${version}` for explicit imports.

Recommended next step: do **not** add another raw `/usr/lib` fallback. Instead, test a minimal nixGL source patch/overlay based on upstream PR #223 plus any additional local fix required for the current nixpkgs NVIDIA `kernel` API. If that patch is too invasive, prefer temporarily reverting to the already-known-working `/usr/lib` wrapper only as an explicit fallback decision.

## Lock/input design

Preferred first attempt:

- Restore `nixGL.inputs.nixpkgs.follows = "nixpkgs"` in `flake.nix`.
- Regenerate/restore `flake.lock` so root `nixpkgs` remains the project nixpkgs and nixGL follows it.
- The explicit `nvidiaVersion` should avoid the `builtins.currentTime` auto-detection path even with follows enabled.

If the `kernel` argument mismatch persists, the root nixpkgs revision may be incompatible with nixGL's current `linuxPackages.nvidia_x11` override pattern. In that case, either:

- pin nixGL to a revision compatible with the project's nixpkgs, or
- pin root nixpkgs to a revision where nixGL's NVIDIA package build works, after checking broader package impact.

Do not leave a separate unused nixGL nixpkgs node in `flake.lock` unless nixGL is actually isolated and used successfully.

## Acceptance criteria

| Criterion | Status |
|-----------|--------|
| No `/usr/lib` GL fallback in `alacrittyWithHostGL` | ✅ Removed |
| `alacrittyWithHostGL` runs `nixGLNvidia-610.43.02` | ✅ Verified |
| `packages.default` includes `alacrittyWithHostGL` | ✅ Unchanged |
| `packages.alacritty` remains `alacrittyWithLigatures` | ✅ Unchanged |
| `nix flake check --no-build` passes | ✅ Passes |
| `nix build '.#'` passes | ✅ Builds (~5 min) |
| `nix profile install '.#'` succeeds | ✅ Installed |
| `alacritty -vv` — NVIDIA GL renderer | ✅ RTX 3060, OpenGL 3.3.0 NVIDIA 610.43.02 |
| Ligatures wrapper retains `LD_LIBRARY_PATH` for `dlopen` | ✅ Retained (RPATH insufficient) |
| Child shell has clean `LD_LIBRARY_PATH` | ✅ Set to `""` in `.alacritty.toml` |
| `.alacritty.toml` `[env]` has both `TERM` and `LD_LIBRARY_PATH` | ✅ Restored |

## Changes made

1. **Pinned nixGL to PR #223 HEAD** (`e0fbb55ff50d0f1fc7b55f34035dfb04f199a2fb`) via `git+https://github.com/nix-community/nixGL?ref=refs/pull/223/head` — resolves the `unexpected argument 'kernel'` breakage from nixpkgs NVIDIA API changes.
2. **Removed `--prefix LD_LIBRARY_PATH : "/usr/lib"`** from `alacrittyWithHostGL` — nixGL now fully supplies the GL library path; no host-fallback needed.
3. **Updated `flake.lock`** — nixGL input updated from `github:nix-community/nixGL/b610529` → `git+.../e0fbb55`.
4. **Retained ligatures `wrapProgram --prefix LD_LIBRARY_PATH`** — required because `xkbcommon-dl` uses `dlopen()` which ignores ELF RPATH.
5. **Added `LD_LIBRARY_PATH = ""`** to `[env]` in `.alacritty.toml` — sanitizes child-shell environment.

## Resolution: wrapper retained, child-shell sanitized

Final approach: keep the ligatures `wrapProgram` (necessary because `xkbcommon-dl` uses `dlopen`, which ignores ELF RPATH) and sanitize child shells via `.alacritty.toml` `[env] LD_LIBRARY_PATH = ""`.

Verification results:
- `env -u LD_LIBRARY_PATH result/bin/alacritty -vv -e true` — launches cleanly, OpenGL 3.3.0 NVIDIA 610.43.02, no config errors
- `env -u LD_LIBRARY_PATH result/bin/alacritty -e 'printenv LD_LIBRARY_PATH'` — child shell reports empty `LD_LIBRARY_PATH`
- `nix flake check --no-build` — passes
- `nix build '.#'` — passes

### Why wrapper stays (vs RPATH/patchelf)

`xkbcommon-dl` loads `libxkbcommon-x11.so` via `dlopen()`. Unlike direct ELF linking, `dlopen()` does **not** consult RPATH — it only searches standard system paths and `LD_LIBRARY_PATH`. Therefore RPATH cannot substitute for the wrapper's `--prefix LD_LIBRARY_PATH`. The correct design is:

1. **Alacritty process**: gets `LD_LIBRARY_PATH` from `wrapProgram` (allows `dlopen` to find runtime libs)
2. **Child shells**: get `LD_LIBRARY_PATH = ""` from `.alacritty.toml` `[env]` (prevents leakage into tmux, nix, devenv, etc.)

## Changes made (final)

1. **Kept** `wrapProgram --prefix LD_LIBRARY_PATH` in `.nix/alacritty-ligatures.nix` — required for `dlopen`-resolved libs.
2. **Added** `LD_LIBRARY_PATH = ""` to `[env]` in `.alacritty.toml` — sanitizes child-shell environment.
3. **Fixed** duplicate `LD_LIBRARY_PATH` entry in `.alacritty.toml` (was listed twice, caused config parse error).
4. **Restored** `TERM = "xterm-256color"` in `.alacritty.toml` `[env]` — was accidentally replaced by `LD_LIBRARY_PATH = ""` in step 2.

## Review: inherited `LD_LIBRARY_PATH` regression

The nixGL build/runtime work above fixed the `/usr/lib` fallback in `flake.nix`, but the current Alacritty design still has a separate environment-leak problem that can break Determinate Nix.

### Findings

1. **Critical — Alacritty ligatures wrapper exported `LD_LIBRARY_PATH` into every shell spawned by Alacritty.**
   - ~~Evidence~~: `.nix/alacritty-ligatures.nix:26-27` wrapped `$out/bin/alacritty` with:
     ```nix
     wrapProgram "$out/bin/alacritty" \
       --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath alacrittyDeps.runtimeLibs}"
     ```
   - ~~Because Alacritty launches the user shell/PTY, this wrapper environment was inherited by tmux, shells, `nix`, `devenv`, etc.~~
   - ~~This matched the reported failing environment containing Alacritty runtime libs and `/usr/lib`, and the reported workaround `env -u LD_LIBRARY_PATH nix --version`.~~
   - **Resolved**: Wrapper prefix removed in round 2.

2. **Critical — nixGL also uses `LD_LIBRARY_PATH`, so removing the ligatures wrapper prefix alone was insufficient.**
   - ~~Evidence~~: `flake.nix:102-103` ran Alacritty through `${nixGLPkgs.nixGLNvidia}/bin/nixGLNvidia-610.43.02`.
   - ~~Evidence~~: nixGL's NVIDIA wrapper sets `LD_LIBRARY_PATH` for the Alacritty process so GL/NVIDIA libraries resolve. That environment was still inherited by child shells unless Alacritty's child environment was explicitly sanitized.
   - ~~Therefore, wrapper-level scoping could not fully solve this by itself: once Alacritty had `LD_LIBRARY_PATH`, normal process inheritance passed it to spawned shell commands.~~
   - **Resolved**: Added `LD_LIBRARY_PATH = ""` to `[env]` in `.alacritty.toml` to sanitize child-shell environment.

3. **Medium — vLLM appears scoped for systemd use, but direct wrappers still export `LD_LIBRARY_PATH` for the vLLM process.**
   - Evidence: `.config/systemd/user/vllm@.service:36` has `UnsetEnvironment=LD_LIBRARY_PATH LD_PRELOAD` before `ExecStart=%h/.local/bin/vllm-serve-pure`.
   - Evidence: `.local/bin/vllm-serve-pure:68` exports `LD_LIBRARY_PATH` immediately before launching vLLM, scoped to that service/process tree.
   - Evidence: `.nix/vllm-runtime.nix:107-113` creates a `vllm` wrapper that exports `LD_LIBRARY_PATH` for direct invocations. That does not mutate the parent interactive shell, but it should not be sourced or used as a login/session wrapper.

### Recommended smallest design

1. **Keep the Alacritty ligatures `wrapProgram --prefix LD_LIBRARY_PATH`** in `.nix/alacritty-ligatures.nix`.
    - `xkbcommon-dl` uses `dlopen()` to load `libxkbcommon-x11.so`, which does **not** consult ELF RPATH.
    - The wrapper's `--prefix LD_LIBRARY_PATH` is the only reliable mechanism to supply runtime libs to `dlopen`.
    - **Done in final round**.

2. **Sanitize Alacritty's spawned-shell environment.**
    - Added `LD_LIBRARY_PATH = ""` to `[env]` in `.alacritty.toml`.
    - This prevents the Alacritty process's `LD_LIBRARY_PATH` from leaking into child shells (tmux, nix, devenv, etc.).
    - **Done in final round**.

3. **Do not attempt RPATH/patchelf as a substitute for the wrapper.**
    - `dlopen()` ignores RPATH; patchelf would not resolve `libxkbcommon-x11.so` at runtime.
    - Verified empirically: removing the wrapper without RPATH replacement causes `xkbcommon-dl` panic.

### Follow-up acceptance criteria

- `flake.nix` keeps the nixGL-only Alacritty host-GL wrapper and still has no raw `/usr/lib` fallback.
- `.nix/alacritty-ligatures.nix` retains `wrapProgram --prefix LD_LIBRARY_PATH` (required for `dlopen`-resolved libs).
- `.alacritty.toml` has `[env] LD_LIBRARY_PATH = ""` to sanitize child shells.
- `.alacritty.toml` also has `TERM = "xterm-256color"` restored (regression fix).
- Alacritty still launches with ligatures and NVIDIA GL via nixGL.
- Inside a new Alacritty-launched shell:
  - `printf '%s\n' "${LD_LIBRARY_PATH-}"` is empty — verified.
  - `nix --version` works without `env -u LD_LIBRARY_PATH` — verified.
  - `ldd /nix/var/nix/profiles/default/bin/nix` resolves Nix dependencies from `/nix/store`, not from `/usr/lib` except unavoidable loader/system mappings.
- `nix flake check --no-build` passes — verified.
- `nix build '.#'` passes — verified.
- vLLM services still include `UnsetEnvironment=LD_LIBRARY_PATH LD_PRELOAD`, and no vLLM wrapper is sourced into the user session.

## Risks

- NVIDIA driver pin `610.43.02` must be updated whenever host driver changes.
- `nvidiaHash` is specific to the downloaded NVIDIA `.run` installer.
- nixGL's NVIDIA packaging may lag nixpkgs changes; expect possible `linuxPackages.nvidia_x11` API mismatch.
- Using `nixGL` can significantly increase closure/build time because it builds/downloads driver libs.

# Tier 1 (Critical) Dependency Review Report
**Date:** 2026-05-22  
**Review Period:** Since last review  
**Next Review:** 2026-05-29 (weekly)  

---

## Executive Summary

3 critical dependencies reviewed. **2 of 3 have findings requiring attention:**
- Fusion: ✅ Clean — v0.32.0 is current
- skills.sh: ⚠️ Repository not accessible at configured URL  
- nixpkgs-unstable: ⚠️ 5 days of divergence from current pin

---

## 1. Fusion (@runfusion/fusion)

| Field | Value |
|-------|-------|
| Current Pin | v0.32.0 (npm) |
| Latest Release | v0.32.0 (2026-05-20) |
| Latest Commit | 97e6a0c (2026-05-21) |
| Divergence | None — pin is current |

**Assessment: ✅ CLEAN**  
No new releases since v0.32.0. The latest commit (97e6a0c) is from May 21 but has not been tagged as a new release. Current pin is valid.

**Action:** No update required.

---

## 2. skills.sh

| Field | Value |
|-------|-------|
| Configured URL | https://github.com/skills/sh |
| HTTP Status | 404 — Page Not Found |
| npm Registry | Not found |
| Clone Status | Failed |

**Assessment: ⚠️ REPOSITORY NOT FOUND**  
The configured URL https://github.com/skills/sh returns a 404. The repository either:
- Has been renamed/moved
- Is private/unavailable
- Never existed at this URL

**Impact:** Cannot perform automated update checks. The deps-check script fails silently on this dependency.

**Recommended Actions:**
1. Verify the correct repository URL (may be `skills-sh/skills-sh` based on GitHub search)
2. Update the deps-check config if URL has changed
3. Consider if this dependency is still needed or if it's been discontinued

---

## 3. nixpkgs-unstable

| Field | Value |
|-------|-------|
| Current Pin | `5a51fe22` (May 16, 2026) |
| Latest `nixos-unstable-small` | `f63936d7` (May 21, 2026) |
| Divergence | 5 days, ~100+ commits |
| Open Security Issues | None (GitHub) |

**Assessment: ⚠️ DIVERGENCE DETECTED**  
The flake is 5 days behind `nixos-unstable-small`. This is within normal drift range for a weekly-checked dependency but warrants evaluation.

**Risk Analysis:**
- No known security vulnerabilities in open nixpkgs issues
- The divergence is gradual (NixOS updates continuously)
- No breaking changes expected in 5 days of drift

**Recommended Action:**  
Schedule a pin update within the next 2-3 days to bring the flake current with nixos-unstable-small. This is a low-risk update.

---

## Findings Summary

| Dependency | Status | Action Required |
|------------|--------|-----------------|
| Fusion | ✅ Clean | None |
| skills.sh | ⚠️ Repo 404 | Investigate correct URL |
| nixpkgs | ⚠️ 5-day drift | Plan pin update |

---

## Next Review Date
**2026-05-29** (weekly cadence per Tier 1 requirements)

---

## Notes
- The deps-check script has a known issue on NixOS with read-only /home filesystem (mitigated by /tmp fallback)
- Clone failures for some moderate/low tier deps (flake-parts, Papirus, yazi, taffybar) suggest these repos may be private or removed

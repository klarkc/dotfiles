# servidor-hj-mesmo

## Determinate Nix: pinned distribution and one-time migration

- **Only Determinate Nix** is supported on `solo098`; do not install the Arch `nix` package. The guest bootstrap uses the official [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer/releases/tag/v3.24.2), release `v3.24.2`, **x86_64-linux binary SHA256** `22ed14ee1b24d100d388f1fc245e3062bd5982433d159cffa6dec34c0eb15dab`. It verifies the checksum before running the binary. The binary installs Determinate Nix and the multi-user daemon.
- Determinate Nix owns `/etc/nix/nix.conf`; custom options belong in `/etc/nix/nix.custom.conf`. Do not append `experimental-features` to the installer-managed configuration. Bootstrap requires `/nix/receipt.json`, `/nix/store` and a `nix --version` that explicitly identifies **Determinate Nix**.
- **Fail closed on mixed installations:** bootstrap refuses `pacman -Q nix` and refuses unknown partial `/nix` or `/etc/nix/nix.conf`. It never deletes an existing store or silently replaces a Nix installation.
- **Recovery from the first guest bootstrap (2026-11-12):** the initial CI-pinned `a67ade2d` script installed Arch's `nix` package and failed at `nix profile install .#solo098-gce` with missing `/nix/store`, after XRDP/Brave/Enpass were installed. The correct recovery is to **inspect** the existing pacman package, unit, config and any store content before explicitly removing only the Arch package; do not delete `/nix` or `/etc/nix` blindly. A new CI-green guest-bootstrap commit needs a controlled fast-forward of the guest's dotfiles checkout **and** reconciliation of GCE metadata `solo098-dotfiles-sha` to that exact commit. Never rerun the old `/tmp` bootstrap while migrating.
- On guest, the installer needs to run with root privileges and can prompt for confirmation. After installing, bootstrap sources the Determinate daemon profile for user `klarkc`, then uses `nix profile add .#solo098-gce`. This is separate from the long-term `klarkc/os` NixOS module.

Independent Arch Linux workstation built from the 2026-10-20 dotfiles snapshot.
The `klarkc/os` **plano-legado** remains frozen.

## Two separate access paths

- **ChatGPT:** self-hosted Desktop Commander `solo098` via Cloudflare Access/OAuth, `cloudflared` and local MCP proxy. No paid Desktop Commander Remote service.
- **Owner on Windows:** `gcloud.cmd` -> IAP tunnel on TCP 3389 -> `mstsc` -> XRDP/XMonad. No Cloudflare on the RDP path, no public VM IPv4.

## Cost and configuration

- VM candidate: `e2-highmem-4`, 4 vCPU/32 GiB RAM, in `southamerica-east1-b`.
- Persistent boot disk: 256 GiB `pd-standard` (still charged while stopped).
- Ceiling: R$560/month, working-hours usage **only**. Account-specific 2026-11-11 reference: ~R$93.72/month for the 256 GiB disk and ~R$1.75 per running VM hour; 176h yields ~R$401.92 for VM + disk only. Cloud NAT/IP, image/staging storage, taxes and internet egress incur additional charges. Re-check billing prices before provisioning. BusinessManager cost review is in `klarkc/broker/inbox/platformmanager/`.
- Preserve non-GPU features: PipeWire, XMonad, Taffybar, picom (`xrender` for RDP), feh, scrot/satty, i3lock (manual), Haskell LSP, Gvim, git-lfs, keyd and all other dotfiles. Leave Sunshine and CUDA/NVIDIA auto-start disabled.

## Preflight — read-only

The official Arch Linux `Arch-Linux-x86_64-cloudimg-20261101.620452.qcow2` image is signed. The public signing key is in `arch-boxes-key.asc` (fingerprint `1B9A16984A4E8CB448712D2AE0B78BF4326C6F8F`). The preflight verifies the small signed checksum, VM availability and existing resources **without** creating any GCE resource.

```bash
git clone --depth 1 --branch servidor-hj-mesmo https://github.com/klarkc/dotfiles.git
cd dotfiles
bash .nix/servidor-hj-mesmo/preflight.sh
```

## Full image verification on Cloud Shell (no GCE charges)

After installing `qemu-utils` in Cloud Shell, run the exact same signed-image checks and local QCOW2 -> sparse RAW -> oldgnu tar.gz conversion **without** creating GCE resources:

```bash
bash .nix/servidor-hj-mesmo/provision-gce.sh --verify-only
```

The command requires a clean checkout and green `Test` CI at its exact SHA. It downloads the official immutable November image (576,697,856 bytes), displays curl progress, detects stalled transfers (under 64 KiB/s for 60 seconds), and attempts multiple Arch mirrors (Fastly, geo and Arizona). A partial public QCOW2 is cached in private `/tmp/solo098-arch-cache-$(id -u)` **for resuming if interrupted** in the same Cloud Shell VM, and is verified against the signed SHA256 before use. The cache is ephemeral and is **not** in `/home`. Conversion artifacts are deleted at exit; the public QCOW2 cache remains to avoid re-downloading for a later `--apply`. The operation checks signature, QCOW2, sparse RAW, tar archive and temporary free space. It does not provision a VM, image, Cloud NAT, bucket or firewall.

## Cloud Shell provisioning (billable)

Only run after `Test` CI is green at the _exact_ branch SHA, an up-to-date provider cost quote and reviewing the bootstrap. The tool prompts for `CRIAR_SOLO098_256` before any paid resource creation.

```bash
bash .nix/servidor-hj-mesmo/provision-gce.sh --apply
```

It checks the signed checksum, downloads/verifies the full image, converts to sparse raw disk and imports a custom GCE image, then creates a private VPC, Cloud NAT, IAP-only firewalls and the 256 GiB VM. The official Arch image has `cloud-init`, not the Google guest agent. Actual GCE boot, `cloud-init`, Btrfs root expansion and SSH login through IAP **were verified 2026-11-12**. XRDP/XMonad, Brave/Enpass UI and Desktop Commander end-to-end acceptance remain pending. If the boot or cloud-init fails, stop and investigate; do not guess credentials.

The Cloud Shell records the exact CI-tested dotfiles SHA in non-secret VM metadata (`solo098-dotfiles-sha`). Guest bootstrap refuses a newer or divergent checkout, preventing installation of untested branch changes. If the branch advances after provisioning, stop and reconcile explicitly rather than bypassing the SHA guard.

The Cloud Shell creates a local SSH key; the only material inserted into guest `user-data` is its public key. The XRDP login password remains locked until the operator configures it via SSH/IAP. Cloudflare credentials stay in the guest private `~/.profile_override`, never Git or cloud metadata.

## Guest bootstrap and acceptance

After boot, check the serial console, tunnel port 22 through IAP, log in as `klarkc`, and set a private password with `sudo passwd klarkc`. Then review/run:

```bash
sudo bash ~/.nix/servidor-hj-mesmo/guest-bootstrap.sh
```

This installs the Arch/Xorg/XRDP foundation, enables the self-hosted MCP runtime, and preserves the dotfiles tree. It is a **first-run bootstrap, not yet E2E verified**. AUR package build can fail on upstream changes. `guest-bootstrap.sh` verifies pinned source revisions of `xrdp`, `xorgxrdp` and `brave-bin` and refuses unexpectedly modified source. Never bypass pin validation, CI or firewall controls. Enable the Cloudflare tunnel only after placing privately maintained values in `~/.profile_override`.

Acceptance: real RDP XMonad login and reconnect, non-GPU terminal/window manager, real `Desktop_Commander_solo098` tool call, VM stop/start retaining disk and costs within the agreed budget.

## Enpass X11 — required for SoloSIG

- The standard Enpass AUR packages tracked in November 2026 use the Wayland build `6.12.6.2258` and are not suitable for an X11-only XRDP/XMonad desktop. Enpass officially maintains the `x11` APT channel; the local Arch `enpass-x11/PKGBUILD` extracts the vendor's `6.12.6.2255` Debian package and verifies its pinned SHA-256 (`716fbae54d52ccd807a6bd53f4cf2d359f6c7f0892ad8491f3a1bf5d8d37b099`).
- `guest-bootstrap.sh` builds this package as `klarkc` using `makepkg` and installs it with `pacman` without configuring APT. The existing XMonad startup command `/opt/enpass/Enpass -minimize` is retained.
- Package provenance: https://apt.enpass.io/dists/x11/main/binary-amd64/Packages; https://help.enpass.io/personal/latest/all/enpass-apps-installation. A vendor update requires manually reviewing the X11 package index, the new digest and compatibility before changing the pin.
- **Runtime acceptance still pending:** verify the application opens inside a real X11 RDP session, the operator can unlock/sync a private vault, and the Brave extension's native bridge works. Do not put credentials or vault data in Git, cloud metadata or CI logs.
- **Brave is also P0:** guest-bootstrap now builds reviewed `brave-bin` 1.99.56 from an exact AUR revision, alongside pinned `xrdp`/`xorgxrdp` sources. The AUR pin check fails closed if remote HEAD changes; an update requires fresh PKGBUILD review. **This is not yet runtime-tested.** Provisioning/desktop productivity is not accepted until both applications are usable.

## Windows control and VM shutdown

The canonical PowerShell 5.1+ script is `.nix/servidor-hj-mesmo/solo098-control.ps1`. Running it or the `Workstation GCE` shortcut checks the VM: **RUNNING connects immediately** through IAP/RDP; **TERMINATED opens a GUI confirmation** that accepts after five seconds unless Cancel is clicked. The existing `-Desligar` switch remains as an external manual fallback to stop VM then remove Cloud NAT. Windows local port 13389 must be unused; no public VM IP or Cloudflare is required for RDP. Closing RDP does **not** stop the VM. The new GUI flow still requires Windows E2E.

Use `systemctl poweroff` inside Linux only after saving work. **A systemd shutdown hook cannot guarantee NAT deletion** after kernel panic, interrupted shutdown or network failure. The VM shutdown -> GCE TERMINATED behavior is not yet proven for this Arch/cloud-init image without Google guest agent; verify GCE status externally. Cloud NAT must be reconciled externally, and disks/images/staging can incur charges after shutdown. An independent Cloud Scheduler/Cloud Run cleanup/alert design is proposed but **not deployed or authorized**. See [RUNBOOK.md](./RUNBOOK.md) for manual fallback and resource recovery.

## Cloudflare and self-hosted Desktop Commander

The guest bootstrap completed on 2026-11-12 and `nix --version` reports `nix (Determinate Nix 3.24.2) 2.35.3`; `zeroclaw --version` reports 0.8.5. XRDP services, XMonad, Brave and Enpass binaries were verified. Desktop Commander remote `solo098` currently appears **Offline**, though the VM is RUNNING. The MCP runtime and Cloudflare tunnel user-service unit files exist in this repository, but Cloudflare credentials and OAuth enrollment were *not* provisioned by guest bootstrap. A direct MCP diagnostics call failed; do not claim successful end-to-end Cloudflare connectivity. Read-only SSH diagnostics and safe enrollment requirements are documented in [RUNBOOK.md](./RUNBOOK.md). The private `~/.profile_override` must never be published.


For first-time provisioning, interrupted creation, signed-image validation, SSH/IAP, rescue procedures and Windows usage, read [RUNBOOK.md](./RUNBOOK.md).

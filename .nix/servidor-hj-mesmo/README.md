# servidor-hj-mesmo

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

## Cloud Shell provisioning (billable)

Only run after `Test` CI is green at the _exact_ branch SHA, an up-to-date provider cost quote and reviewing the bootstrap. The tool prompts for `CRIAR_SOLO098_256` before any paid resource creation.

```bash
bash .nix/servidor-hj-mesmo/provision-gce.sh --apply
```

It checks the signed checksum, downloads/verifies the full image, converts to sparse raw disk and imports a custom GCE image, then creates a private VPC, Cloud NAT, IAP-only firewalls and the 256 GiB VM. The official Arch image has `cloud-init`, not the Google guest agent. The image and guest login are **not yet validated on GCE**. If the boot or cloud-init fails, stop and investigate; do not guess credentials.

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

## Windows control

The Windows `solo098-control.ps1` is distributed separately. It uses `gcloud.cmd` to start/stop the VM and creates a local IAP tunnel for `mstsc`. Closing RDP does not stop the GCE instance. No Cloudflare is involved in that connection.

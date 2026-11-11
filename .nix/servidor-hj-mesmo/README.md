# servidor-hj-mesmo

Independent, historical October 2026 dotfiles base for the new solo098 GCE VM.
Frozen klarkc/os plano-legado is not modified.

## Access
ChatGPT: self-hosted Desktop Commander solo098 -> Cloudflare Access/OAuth -> cloudflared -> local MCP proxy.
Owner on Windows: gcloud.cmd -> IAP TCP 3389 -> XRDP/XMonad. Cloudflare is NOT used for XRDP.

## Cost and safety
Proposed e2-highmem-4, 4 vCPU and 32 GiB RAM, 512 GiB pd-standard.
Target R$560/month; disk is billed when stopped. Cost review: klarkc/broker inbox/platformmanager (2026-11-11).
Do not open public IP/3389, do not store tunnel tokens in Git or VM metadata.
Preserve non-GPU dotfiles features; disable only GPU-specific startup.

## Read-only Google Cloud Shell preflight
git clone --depth 1 --branch servidor-hj-mesmo https://github.com/klarkc/dotfiles.git
cd dotfiles
bash .nix/servidor-hj-mesmo/preflight.sh

Only provision after CI green, verifying GCE image freshness and real costs.
Upstream Arch GCE image builder lists its last prebuilt image in 2022; do not silently use a stale OS image.
Cloud provider connector in this conversation cannot create the required 512 GiB no-public-IP instance.
Windows control, guest setup, Cloudflare secret enrollment and real RDP/MCP acceptance remain separate pending steps.

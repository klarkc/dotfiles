#!/usr/bin/env bash
# --verify-only: full local QCOW2 signature/checksum/RAW/tar test, NO GCE writes.
# --apply: the same local verification plus explicit consent before paid creation.
set -euo pipefail
mode="${1:-}"
if [[ $# != 1 || ($mode != --verify-only && $mode != --apply) ]]; then
  echo "Usage: bash $0 --verify-only | --apply" >&2
  exit 2
fi
root="$(cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
project=argon-surf-472319-b0
region=southamerica-east1
zone=southamerica-east1-b
vm=solo098
image_name=solo098-arch-20261101
image_file=Arch-Linux-x86_64-cloudimg-20261101.620452.qcow2
mirror=https://geo.mirror.pkgbuild.com/images/latest
vpc=solo098-vpc
subnet=solo098-subnet
router=solo098-router
nat=solo098-nat
gcs_bucket=argon-surf-472319-b0-solo098-image-staging
for t in gcloud git curl gpg qemu-img tar sha256sum python3 ssh-keygen; do
  command -v "$t" >/dev/null || {
    echo "Missing $t. Install it before provisioning." >&2
    exit 2
  }
done
[[ "$(git branch --show-current)" == servidor-hj-mesmo ]] || {
  echo 'Unexpected branch.' >&2
  exit 1
}
git diff --quiet && git diff --cached --quiet || {
  echo 'Dirty tree: stop.' >&2
  exit 1
}
sha="$(git rev-parse HEAD)"
tmp="$(mktemp -d)"
trap 'rm -rf -- "$tmp"' EXIT
# Fail closed: exact SHA must have a completed successful Test workflow.
curl --fail --silent --show-error --location \
  "https://api.github.com/repos/klarkc/dotfiles/actions/workflows/test.yml/runs?head_sha=$sha&event=push&per_page=30" \
  -o "$tmp/ci.json"
python3 - "$tmp/ci.json" "$sha" <<'PY'
import json,sys
data=json.load(open(sys.argv[1], encoding="utf-8"))
sha=sys.argv[2]
ok=any(
    r.get("head_sha")==sha and r.get("head_branch")=="servidor-hj-mesmo"
    and r.get("conclusion")=="success" and r.get("status")=="completed"
    for r in data.get("workflow_runs",[])
)
if not ok: raise SystemExit("BLOCKED: exact commit lacks green mandatory Test CI")
print("PASS: required Test CI green at exact commit",sha)
PY
bash "$root/.nix/servidor-hj-mesmo/preflight.sh"
if [[ $mode == --apply ]] && gcloud compute instances describe "$vm" --project="$project" --zone="$zone" >/dev/null 2>&1; then
  echo "BLOCKED: VM $vm already exists. Refusing to mutate it." >&2
  exit 1
fi
echo 'Plan: e2-highmem-4 (4 vCPU, 32 GiB), 256 GiB pd-standard.'
echo 'Budget ceiling R$560/month, 176 working hours reference; NOT 24/7.'
echo '2026-11-11 account-rate reference: ~R$401.92 at 176 VM hours + 256 GiB pd-standard.'
echo 'This excludes Cloud NAT/IP, egress, custom image, staging storage, taxes and existing spend.'
if [[ $mode == --verify-only ]]; then
  echo 'VERIFY-ONLY: local QCOW2/RAW/tar validation; no GCE resources will be created.'
else
  echo 'WARNING: --apply may create billable image, staging bucket, NAT and VM.'
  echo 'A failed boot may require manual cleanup; the guest environment is not yet proven.'
fi
mkdir -m 0700 "$tmp/gnupg"
export GNUPGHOME="$tmp/gnupg"
gpg --batch --import "$root/.nix/servidor-hj-mesmo/arch-boxes-key.asc" >/dev/null 2>&1
for extension in SHA256 SHA256.sig sig; do
  curl --fail --silent --show-error --location --retry 3 \
    "$mirror/$image_file.$extension" -o "$tmp/$image_file.$extension"
done
image_exists=false
if [[ $mode == --apply ]] && gcloud compute images describe "$image_name" --project="$project" >/dev/null 2>&1; then
  image_exists=true
fi
if [[ $image_exists == false ]]; then
  curl --fail --silent --show-error --location --retry 3 \
    "$mirror/$image_file" -o "$tmp/$image_file"
  gpg --batch --verify "$tmp/$image_file.sig" "$tmp/$image_file" >/dev/null 2>&1 || {
    echo 'BLOCKED: image PGP signature invalid' >&2
    exit 1
  }
  (cd "$tmp" && sha256sum --check "$image_file.SHA256")
  qemu-img check -f qcow2 "$tmp/$image_file"
  qemu-img info -f qcow2 "$tmp/$image_file"
  qemu-img info -f qcow2 --output=json "$tmp/$image_file" >"$tmp/qcow-info.json"
  # Fail before conversion when /tmp cannot fit the worst-case RAW and tar.
  # The official Arch cloud image starts at 2 GiB; boot disk expands to 256 GiB in GCE.
  python3 - "$tmp/qcow-info.json" "$tmp" "$tmp/$image_file" <<'PY'
import json, os, shutil, sys
with open(sys.argv[1], encoding="utf-8") as f:
    virtual = int(json.load(f)["virtual-size"])
gib = 1024 ** 3
if virtual < gib or virtual % gib:
    raise SystemExit("BLOCKED: virtual disk size is not a whole GiB")
free = shutil.disk_usage(sys.argv[2]).free
# Conservative reservation: original QCOW2 plus two full RAW copies and 1 GiB.
need = 2 * virtual + os.path.getsize(sys.argv[3]) + gib
print(f"QCOW2 virtual disk: {virtual / gib:.2f} GiB; temporary free: {free / gib:.2f} GiB")
print(f"Conservative free-space requirement: {need / gib:.2f} GiB")
if free < need:
    raise SystemExit("BLOCKED: insufficient temporary disk space; no GCE writes")
PY
  qemu-img convert -p -f qcow2 -O raw -S 4k "$tmp/$image_file" "$tmp/disk.raw"
  tar --format=oldgnu -Sczf "$tmp/disk.tar.gz" -C "$tmp" disk.raw
  tar -tzf "$tmp/disk.tar.gz" | grep -Fx 'disk.raw'
  du -h "$tmp/$image_file" "$tmp/disk.raw" "$tmp/disk.tar.gz"
  if [[ $mode == --verify-only ]]; then
    echo 'PASS: signed official QCOW2, SHA256, qemu-img, sparse RAW and oldgnu tar.gz verified locally.'
    echo 'PASS: no GCE resources created or modified; temporary files will be removed.'
    exit 0
  fi
  echo 'Local image verification complete. Subsequent steps may incur GCE charges.'
  read -r -p 'To create paid resources, type CRIAR_SOLO098_256: ' confirmed
  [[ $confirmed == CRIAR_SOLO098_256 ]] || {
    echo 'No GCE writes performed.'
    exit 1
  }
  if ! gcloud storage buckets describe "gs://$gcs_bucket" --project="$project" >/dev/null 2>&1; then
    gcloud storage buckets create "gs://$gcs_bucket" \
      --project="$project" --location="$region" --uniform-bucket-level-access \
      --public-access-prevention=enforced
  fi
  gcloud storage cp "$tmp/disk.tar.gz" "gs://$gcs_bucket/disk.tar.gz"
  gcloud compute images create "$image_name" --project="$project" \
    --source-uri="gs://$gcs_bucket/disk.tar.gz" \
    --family=solo098-arch --storage-location="$region" \
    --labels=plan=servidor-hj-mesmo
else
  # Explicit consent is required even when resuming after an existing image.
  echo 'Existing custom image found; its provenance still requires operator review.'
  read -r -p 'To continue with paid resources, type CRIAR_SOLO098_256: ' confirmed
  [[ $confirmed == CRIAR_SOLO098_256 ]] || {
    echo 'No GCE writes performed.'
    exit 1
  }
fi
# Dedicated VPC with IAP-only SSH and RDP; Cloud NAT permits outbound package updates.
if ! gcloud compute networks describe "$vpc" --project="$project" >/dev/null 2>&1; then
  gcloud compute networks create "$vpc" --project="$project" --subnet-mode=custom
fi
if ! gcloud compute networks subnets describe "$subnet" --project="$project" --region="$region" >/dev/null 2>&1; then
  gcloud compute networks subnets create "$subnet" --project="$project" --region="$region" \
    --network="$vpc" --range=10.146.0.0/24 --enable-private-ip-google-access
fi
if ! gcloud compute routers describe "$router" --project="$project" --region="$region" >/dev/null 2>&1; then
  gcloud compute routers create "$router" --project="$project" --region="$region" --network="$vpc"
fi
if ! gcloud compute routers nats describe "$nat" --project="$project" --router="$router" --region="$region" >/dev/null 2>&1; then
  gcloud compute routers nats create "$nat" --project="$project" --router="$router" \
    --region="$region" --auto-allocate-nat-external-ips --nat-custom-subnet-ip-ranges="$subnet"
fi
for port in 22 3389; do
  if [[ $port == 22 ]]; then
    rule=solo098-iap-ssh
  else
    rule=solo098-iap-rdp
  fi
  if ! gcloud compute firewall-rules describe "$rule" --project="$project" >/dev/null 2>&1; then
    gcloud compute firewall-rules create "$rule" --project="$project" \
      --network="$vpc" --direction=INGRESS --source-ranges=35.235.240.0/20 \
      --target-tags=solo098-iap --allow="tcp:$port"
  fi
done
# Local key never leaves the operator device. Only the public key is put in user-data.
ssh_dir="$HOME/.ssh"
mkdir -p "$ssh_dir"
chmod 700 "$ssh_dir"
ssh_key="$ssh_dir/solo098-cloudshell"
if [[ ! -s "$ssh_key.pub" ]]; then
  ssh-keygen -q -t ed25519 -N '' -f "$ssh_key" -C 'solo098-cloudshell'
fi
public_key="$(cat "$ssh_key.pub")"
cat >"$tmp/user-data" <<EOF
#cloud-config
users:
  - default
  - name: klarkc
    groups: [wheel]
    shell: /bin/bash
    lock_passwd: true
    sudo:
      - "ALL=(ALL) ALL"
      - "ALL=(ALL) NOPASSWD: /usr/bin/passwd klarkc"
    ssh_authorized_keys:
      - $public_key
EOF
gcloud compute instances create "$vm" --project="$project" --zone="$zone" \
  --machine-type=e2-highmem-4 --image="$image_name" \
  --boot-disk-size=256GB --boot-disk-type=pd-standard --no-boot-disk-auto-delete \
  --network="$vpc" --subnet="$subnet" --no-address --no-service-account --no-scopes \
  --tags=solo098-iap --labels=plan=servidor-hj-mesmo \
  --metadata=enable-oslogin=FALSE,block-project-ssh-keys=TRUE,solo098-dotfiles-sha="$sha" \
  --metadata-from-file="user-data=$tmp/user-data"
echo 'VM created. Cloud-init first boot and SSH via IAP must now be tested.'
echo "Cloud Shell private SSH key remains at $ssh_key (do not share)."
echo "To reach SSH, start: gcloud compute start-iap-tunnel $vm 22 --zone=$zone --project=$project --local-host-port=localhost:10022"
echo "Then: ssh -i $ssh_key -p 10022 klarkc@127.0.0.1"
echo 'Set XRDP login password with: sudo passwd klarkc (never in chat/metadata).'
echo 'Then install guest packages via the separately versioned guest-bootstrap.sh.'

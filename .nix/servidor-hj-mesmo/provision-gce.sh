#!/usr/bin/env bash
# One-time Cloud Shell import/provision. Paid resources: requires --apply and confirmation.
set -euo pipefail
if [[ "$#" != 1 || "$1" != --apply ]]; then
  echo "Usage: bash $0 --apply (requires GCE billing and prints a final confirmation)" >&2
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
  command -v "$t" >/dev/null || { echo "Missing $t. Install it before provisioning." >&2; exit 2; }
done
[[ "$(git branch --show-current)" == servidor-hj-mesmo ]] || { echo 'Unexpected branch.' >&2; exit 1; }
git diff --quiet && git diff --cached --quiet || { echo 'Dirty tree: stop.' >&2; exit 1; }
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
if gcloud compute instances describe "$vm" --project="$project" --zone="$zone" >/dev/null 2>&1; then
  echo "BLOCKED: VM $vm already exists. Refusing to mutate it." >&2
  exit 1
fi
echo 'Billable plan: e2-highmem-4 (4 vCPU, 32 GiB), 512 GiB pd-standard.'
echo 'Target R$560/month at controlled working hours; NOT a 24/7 quote.'
echo 'Disk, imported image, Cloud NAT, staging bucket and egress can incur charges.'
echo 'Guest environment is not yet proven on GCE. A failed boot may require manual cleanup.'
read -r -p 'To create paid resources, type CRIAR_SOLO098_512: ' confirmed
[[ "$confirmed" == CRIAR_SOLO098_512 ]] || { echo 'No changes.'; exit 1; }
mkdir -m 0700 "$tmp/gnupg"
export GNUPGHOME="$tmp/gnupg"
gpg --batch --import "$root/.nix/servidor-hj-mesmo/arch-boxes-key.asc" >/dev/null 2>&1
for extension in SHA256 SHA256.sig sig; do
  curl --fail --silent --show-error --location --retry 3 \
    "$mirror/$image_file.$extension" -o "$tmp/$image_file.$extension"
done
if ! gcloud compute images describe "$image_name" --project="$project" >/dev/null 2>&1; then
  curl --fail --silent --show-error --location --retry 3 \
    "$mirror/$image_file" -o "$tmp/$image_file"
  gpg --batch --verify "$tmp/$image_file.sig" "$tmp/$image_file" >/dev/null 2>&1 || {
    echo 'BLOCKED: image PGP signature invalid' >&2; exit 1;
  }
  (cd "$tmp" && sha256sum --check "$image_file.SHA256")
  qemu-img info -f qcow2 "$tmp/$image_file"
  qemu-img convert -p -f qcow2 -O raw -S 4k "$tmp/$image_file" "$tmp/disk.raw"
  tar -Sczf "$tmp/disk.tar.gz" -C "$tmp" disk.raw
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
  if [[ "$port" == 22 ]]; then rule=solo098-iap-ssh; else rule=solo098-iap-rdp; fi
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
cat > "$tmp/user-data" <<EOF
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
  --boot-disk-size=512GB --boot-disk-type=pd-standard --no-boot-disk-auto-delete \
  --network="$vpc" --subnet="$subnet" --no-address --no-service-account --no-scopes \
  --tags=solo098-iap --labels=plan=servidor-hj-mesmo \
  --metadata=enable-oslogin=FALSE,block-project-ssh-keys=TRUE \
  --metadata-from-file="user-data=$tmp/user-data"
echo 'VM created. Cloud-init first boot and SSH via IAP must now be tested.'
echo "Cloud Shell private SSH key remains at $ssh_key (do not share)."
echo "To reach SSH, start: gcloud compute start-iap-tunnel $vm 22 --zone=$zone --project=$project --local-host-port=localhost:10022"
echo "Then: ssh -i $ssh_key -p 10022 klarkc@127.0.0.1"
echo 'Set XRDP login password with: sudo passwd klarkc (never in chat/metadata).'
echo 'Then install guest packages via the separately versioned guest-bootstrap.sh.'

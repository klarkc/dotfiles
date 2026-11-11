#!/usr/bin/env bash
# READ ONLY: official Arch image signed checksum and existing GCE resources.
set -euo pipefail
project=argon-surf-472319-b0
zone=southamerica-east1-b
region=southamerica-east1
image=Arch-Linux-x86_64-cloudimg-20261101.620452.qcow2
mirror=https://geo.mirror.pkgbuild.com/images/latest
key=1B9A16984A4E8CB448712D2AE0B78BF4326C6F8F
here="$(cd -- "$(dirname -- "$0")" && pwd)"
for tool in gcloud gpg curl awk; do
  command -v "$tool" >/dev/null || {
    echo "Missing $tool" >&2
    exit 2
  }
done
tmp="$(mktemp -d)"
trap 'rm -rf -- "$tmp"' EXIT
mkdir -m 0700 "$tmp/gnupg"
export GNUPGHOME="$tmp/gnupg"
fingerprint="$(gpg --show-keys --with-colons "$here/arch-boxes-key.asc" 2>/dev/null | awk -F: '$1=="fpr" {print $10; exit}')"
[[ "$fingerprint" == "$key" ]] || {
  echo 'FAIL: Arch signing key fingerprint mismatch' >&2
  exit 1
}
gpg --batch --import "$here/arch-boxes-key.asc" >/dev/null 2>&1
for ext in SHA256 SHA256.sig; do
  curl --fail --silent --show-error --location --retry 3 "$mirror/$image.$ext" -o "$tmp/$image.$ext"
done
gpg --batch --verify "$tmp/$image.SHA256.sig" "$tmp/$image.SHA256" >/dev/null 2>&1 || {
  echo 'FAIL: official signed checksum verification failed' >&2
  exit 1
}
grep -Fq "$image" "$tmp/$image.SHA256" || {
  echo 'FAIL: image name absent from checksum' >&2
  exit 1
}
echo "PASS: official Arch 2026-11-01 signed checksum (key $key)"
echo 'NOTE: full image bytes and detached signature MUST be verified before import.'
gcloud compute machine-types describe e2-highmem-4 --project="$project" --zone="$zone" --format='table(name,guestCpus,memoryMb)'
gcloud compute instances list --project="$project" --filter="zone:($zone)" --format='table(name,status)'
gcloud compute disks list --project="$project" --filter="zone:($zone)" --format='table(name,sizeGb,status)'
gcloud compute networks list --project="$project" --format='table(name,subnetMode)'
gcloud compute routers list --project="$project" --filter="region:($region)" --format='table(name,region.basename())'
echo 'PASS: no cloud resources created or modified.'

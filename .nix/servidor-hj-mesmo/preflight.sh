#!/usr/bin/env bash
# Read-only Cloud Shell checks before any paid provisioning.
set -euo pipefail
project=argon-surf-472319-b0
zone=southamerica-east1-b
region=southamerica-east1
command -v gcloud >/dev/null || { echo 'Missing gcloud' >&2; exit 2; }
command -v python3 >/dev/null || { echo 'Missing Python 3' >&2; exit 2; }
echo 'GCE instance type:'
gcloud compute machine-types describe e2-highmem-4 --project="$project" --zone="$zone" --format='table(name,guestCpus,memoryMb)'
echo 'GCE Arch image:'
created="$(gcloud compute images describe-from-family arch --project=arch-linux-gce --format='value(creationTimestamp)' || true)"
[[ -n "$created" ]] || { echo 'BLOCKED: Arch image unavailable' >&2; exit 1; }
IMAGE_CREATED="$created" python3 - <<'PY'
from datetime import datetime, timezone
from os import environ
created=datetime.fromisoformat(environ['IMAGE_CREATED'].replace('Z','+00:00'))
age=(datetime.now(timezone.utc)-created).days
print('Image age:',age,'days')
if age>180: raise SystemExit('BLOCKED: stale image; create new image or review safe full upgrade')
PY
echo 'Existing VMs/disks:'
gcloud compute instances list --project="$project" --filter="zone:($zone)" --format='table(name,status)'
gcloud compute disks list --project="$project" --filter="zone:($zone)" --format='table(name,sizeGb,status)'
echo 'Networks/routers:'
gcloud compute networks list --project="$project" --format='table(name,subnetMode)'
gcloud compute routers list --project="$project" --filter="region:($region)" --format='table(name,region.basename())'
echo 'PASS: read-only GCE preflight completed.'

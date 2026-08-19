set -euo pipefail

work="$(mktemp -d -t archive-pack-test.XXXXXX)"
trap 'rm -rf "$work"' EXIT

backup_dir="$work/backup"
mkdir -p "$backup_dir"

log() { printf '[test %s] %s\n' "$(date +%H:%M:%S)" "$*"; }

log "Building synthetic backup at $backup_dir"

# Type variety: tar.gz, zip, plain file, directory
mkdir -p "$work/src_a" "$work/src_b"
printf 'hello a\n' >"$work/src_a/file1.txt"
printf 'shared content\n' >"$work/src_a/shared.txt"
printf 'unique b\n' >"$work/src_b/file2.txt"
printf 'shared content\n' >"$work/src_b/shared.txt"

(cd "$work/src_a" && tar -czf "$backup_dir/sample-a.tgz" .)
(cd "$work/src_b" && tar -czf "$backup_dir/sample-b.tgz" .)
rm -f "$backup_dir/sample-c.zip"
(cd "$work/src_a" && zip -qr "$backup_dir/sample-c.zip" .)
printf 'plain note\n' >"$backup_dir/notes.txt"
mkdir -p "$backup_dir/docs"
printf 'readme\n' >"$backup_dir/docs/readme.txt"

log "First archive-pack (no prev archive)"
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1

test -f "$backup_dir/archive.lrz" || {
  echo "FAIL: missing archive.lrz" >&2
  exit 1
}

log "Validate first archive with lrzip -t"
lrzip -t "$backup_dir/archive.lrz"

log "Validate first archive contents"
lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - >"$work/list1.txt"
cat "$work/list1.txt"

grep -q "file1.txt" "$work/list1.txt" || {
  echo "FAIL: file1.txt missing" >&2
  exit 1
}
grep -q "file2.txt" "$work/list1.txt" || {
  echo "FAIL: file2.txt missing" >&2
  exit 1
}
grep -q "notes.txt" "$work/list1.txt" || {
  echo "FAIL: notes.txt missing" >&2
  exit 1
}
grep -q "docs/readme.txt" "$work/list1.txt" || {
  echo "FAIL: docs/readme.txt missing" >&2
  exit 1
}

log "Simulate append-only: remove source, re-pack, old file should remain"
rm -f "$backup_dir/sample-a.tgz"
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1

lrzip -t "$backup_dir/archive.lrz"
lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - >"$work/list2.txt"

grep -q "file1.txt" "$work/list2.txt" || {
  echo "FAIL: file1.txt missing after re-pack (append-only broken)" >&2
  exit 1
}
grep -q "file2.txt" "$work/list2.txt" || {
  echo "FAIL: file2.txt missing after re-pack" >&2
  exit 1
}

log "Test --exclude"
rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
  "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
(cd "$work/src_a" && tar -czf "$backup_dir/sample-a.tgz" .)
(cd "$work/src_b" && tar -czf "$backup_dir/sample-b.tgz" .)
(cd "$work/src_a" && zip -qr "$backup_dir/sample-c.zip" .)
printf 'plain note\n' >"$backup_dir/notes.txt"
mkdir -p "$backup_dir/docs"
printf 'readme\n' >"$backup_dir/docs/readme.txt"

ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 \
  --exclude 'sample-b.tgz'

lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - >"$work/list3.txt"
# Tight assertion: forbid the literal archive file and its extracted prefix.
# Two greps so each pattern has its own anchoring (single regex with
# alternation gets ambiguous when one alternative needs prefix-match and the
# other needs exact-match).
if grep -qE "^\\./sample-b\\.tgz$" "$work/list3.txt" ||
  grep -qE '^\./sample-b-extract/' "$work/list3.txt"; then
  echo "FAIL: sample-b should have been excluded" >&2
  exit 1
fi

log "Test append-only preserves files inside directories that exist in both prev and new"
rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
  "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
mkdir -p "$backup_dir/docs"
printf 'readme\n' >"$backup_dir/docs/readme.txt"
printf 'old file\n' >"$backup_dir/docs/keep-me.txt"

log "First pack: seeds archive with docs/keep-me.txt and docs/readme.txt"
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1

log "Simulate user removing keep-me.txt from source and adding newer.txt"
rm "$backup_dir/docs/keep-me.txt"
printf 'newer file\n' >"$backup_dir/docs/newer.txt"

log "Re-pack: keep-me.txt must survive in archive even though removed from source"
log "(this guards the append-only invariant for files inside shared dirs)"
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1

lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - >"$work/list4.txt"

grep -q "^\\./docs/keep-me\\.txt$" "$work/list4.txt" ||
  {
    echo "FAIL: docs/keep-me.txt missing after re-pack (append-only regression)" >&2
    exit 1
  }
grep -q "^\\./docs/newer\\.txt$" "$work/list4.txt" ||
  {
    echo "FAIL: docs/newer.txt missing after re-pack" >&2
    exit 1
  }
grep -q "^\\./docs/readme\\.txt$" "$work/list4.txt" ||
  {
    echo "FAIL: docs/readme.txt missing after re-pack" >&2
    exit 1
  }

log "Test dedup across archives (same photo in tar.gz and zip)"
rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
  "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
mkdir -p "$work/dup_a" "$work/dup_b"
# Random bytes (incompressible, like real jpg/mp4).
dd if=/dev/urandom of="$work/dup_a/photo.bin" bs=1M count=2 status=none
cp "$work/dup_a/photo.bin" "$work/dup_b/photo.bin"
# Sanity: files are byte-identical
cmp -s "$work/dup_a/photo.bin" "$work/dup_b/photo.bin" ||
  {
    echo "FAIL: dedup test setup broken (files differ)" >&2
    exit 1
  }
# Each copy goes into a different archive type
(cd "$work/dup_a" && tar -czf "$backup_dir/dup-takeout.tgz" .)
(cd "$work/dup_b" && zip -qr "$backup_dir/dup-photos.zip" .)
photo_size=$(stat -c%s "$work/dup_a/photo.bin")
log "Each archive holds a $photo_size-byte random payload"

ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1

archive_size=$(stat -c%s "$backup_dir/archive.lrz")
# With dedup: archive ~= photo_size + ~10 KB tar/lrzip overhead
# Without dedup: archive ~= 2 * photo_size + overhead
threshold=$((photo_size + photo_size / 2))
log "archive=$archive_size photo=$photo_size threshold=$threshold"
if [ "$archive_size" -ge "$threshold" ]; then
  echo "FAIL: dedup not working. archive=$archive_size bytes >= threshold=$threshold bytes (1.5x photo). Without dedup archive would be ~$((photo_size * 2)) bytes." >&2
  exit 1
fi

log "PASS"

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

log "Test --keep-archives keeps original archive alongside extracted contents"
rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
  "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
mkdir -p "$work/src_keep"
printf 'kept archive content\n' >"$work/src_keep/keep.txt"
(cd "$work/src_keep" && tar -czf "$backup_dir/keep-sample.tgz" .)
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 --keep-archives
lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - >"$work/list_keep.txt"
grep -q "keep-sample-extract/keep.txt" "$work/list_keep.txt" ||
  {
    echo "FAIL: --keep-archives missing extracted dir" >&2
    exit 1
  }
grep -q "keep-sample.tgz" "$work/list_keep.txt" ||
  {
    echo "FAIL: --keep-archives missing original archive" >&2
    exit 1
  }

log "Test --dry-run does not modify archive"
size_before=$(stat -c%s "$backup_dir/archive.lrz")
mtime_before=$(stat -c%Y "$backup_dir/archive.lrz")
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 --dry-run >/dev/null 2>&1
size_after=$(stat -c%s "$backup_dir/archive.lrz")
mtime_after=$(stat -c%Y "$backup_dir/archive.lrz")
if [ "$size_before" != "$size_after" ] || [ "$mtime_before" != "$mtime_after" ]; then
  echo "FAIL: --dry-run modified archive (size $size_before->$size_after, mtime $mtime_before->$mtime_after)" >&2
  exit 1
fi

log "Test --clean-temp removes stage/ and logs/ but preserves archive"
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 --clean-temp
[ ! -d "$backup_dir/stage" ] ||
  {
    echo "FAIL: --clean-temp did not remove stage/" >&2
    exit 1
  }
[ ! -d "$backup_dir/logs" ] ||
  {
    echo "FAIL: --clean-temp did not remove logs/" >&2
    exit 1
  }
[ -f "$backup_dir/archive.lrz" ] ||
  {
    echo "FAIL: --clean-temp removed archive.lrz" >&2
    exit 1
  }

log "Test --clean-source removes sources and preserves archive + excludes"
rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
  "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
mkdir -p "$work/src_clean"
printf 'clean me\n' >"$work/src_clean/data.txt"
(cd "$work/src_clean" && tar -czf "$backup_dir/clean-sample.tgz" .)
test -f "$backup_dir/clean-sample.tgz" ||
  {
    echo "FAIL: setup (clean-sample.tgz not created)" >&2
    exit 1
  }
ec=0
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 --clean-source || ec=$?
[ "$ec" = "0" ] || {
  echo "FAIL: --clean-source exited with $ec" >&2
  exit 1
}
[ ! -f "$backup_dir/clean-sample.tgz" ] ||
  {
    echo "FAIL: --clean-source did not remove clean-sample.tgz" >&2
    exit 1
  }
[ -f "$backup_dir/archive.lrz" ] ||
  {
    echo "FAIL: --clean-source removed archive.lrz" >&2
    exit 1
  }
[ -f "$backup_dir/archive.lrz.SUMMARY.txt" ] ||
  {
    echo "FAIL: --clean-source removed archive.lrz.SUMMARY.txt" >&2
    exit 1
  }

log "Test --skip-source-integrity bypasses corrupt-source check"
rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
  "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
printf 'this is not a real tar.gz\n' >"$backup_dir/corrupt.tgz"
ec=0
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 || ec=$?
[ "$ec" = "2" ] || {
  echo "FAIL: corrupt source should exit 2, got $ec" >&2
  exit 1
}
[ ! -f "$backup_dir/archive.lrz" ] ||
  {
    echo "FAIL: corrupt source produced archive.lrz" >&2
    exit 1
  }
ec=0
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 --skip-source-integrity || ec=$?
[ "$ec" = "0" ] ||
  {
    echo "FAIL: --skip-source-integrity should exit 0, got $ec" >&2
    exit 1
  }
rm -f "$backup_dir/corrupt.tgz"

log "Test --verify on existing archive exits 0"
ec=0
ARCHIVE_PACK_DIR="$backup_dir" archive-pack --verify || ec=$?
[ "$ec" = "0" ] || {
  echo "FAIL: --verify on valid archive exited $ec" >&2
  exit 1
}

log "Test --verify on missing archive exits non-zero"
rm -f "$backup_dir/archive.lrz"
ec=0
ARCHIVE_PACK_DIR="$backup_dir" archive-pack --verify >/dev/null 2>&1 || ec=$?
[ "$ec" != "0" ] || {
  echo "FAIL: --verify on missing archive should fail, exited $ec" >&2
  exit 1
}

log "Test --help exits 0 and prints usage"
ec=0
ARCHIVE_PACK_DIR="$backup_dir" archive-pack --help || ec=$?
[ "$ec" = "0" ] || {
  echo "FAIL: --help exited with $ec" >&2
  exit 1
}

log "Test --retain 1 keeps only one snapshot"
rm -rf "$backup_dir"/archive-*.lrz "$backup_dir/archive.lrz" "$backup_dir/archive.lrz.SUMMARY.txt" \
  "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest"
mkdir -p "$work/src_r1" "$work/src_r2" "$work/src_r3"
printf 'one\n' >"$work/src_r1/a.txt"
printf 'two\n' >"$work/src_r2/b.txt"
printf 'three\n' >"$work/src_r3/c.txt"
(cd "$work/src_r1" && tar -czf "$backup_dir/r1.tgz" .)
ARCHIVE_PACK_DIR="$backup_dir" archive-pack --threads 2 --maxram 40 --window 5 --level 1
sleep 1
(cd "$work/src_r2" && tar -czf "$backup_dir/r2.tgz" .)
ARCHIVE_PACK_DIR="$backup_dir" archive-pack --threads 2 --maxram 40 --window 5 --level 1
sleep 1
(cd "$work/src_r3" && tar -czf "$backup_dir/r3.tgz" .)
ARCHIVE_PACK_DIR="$backup_dir" archive-pack --threads 2 --maxram 40 --window 5 --level 1 --retain 1
snap_count=$(find "$backup_dir" -maxdepth 1 -name 'archive-*.lrz' | wc -l)
[ "$snap_count" = "1" ] || {
  echo "FAIL: --retain 1 left $snap_count snapshots, expected 1" >&2
  exit 1
}

log "Test --retain 0 disables snapshotting"
rm -rf "$backup_dir"/archive-*.lrz "$backup_dir/archive.lrz" "$backup_dir/archive.lrz.SUMMARY.txt" \
  "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest"
mkdir -p "$work/src_r0"
printf 'zero\n' >"$work/src_r0/x.txt"
(cd "$work/src_r0" && tar -czf "$backup_dir/r0.tgz" .)
ARCHIVE_PACK_DIR="$backup_dir" archive-pack --threads 2 --maxram 40 --window 5 --level 1
sleep 1
ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
  --threads 2 --maxram 40 --window 5 --level 1 --retain 0
snap_count=$(find "$backup_dir" -maxdepth 1 -name 'archive-*.lrz' | wc -l)
[ "$snap_count" = "0" ] || {
  echo "FAIL: --retain 0 created $snap_count snapshots, expected 0" >&2
  exit 1
}

log "PASS"

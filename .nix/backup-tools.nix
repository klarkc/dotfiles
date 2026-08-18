{
  lib,
  pkgs,
}:

let
  backupRuntime = with pkgs; [
    coreutils
    findutils
    gawk
    gnugrep
    gnused
    gnutar
    gzip
    lrzip
    procps
    unzip
    util-linux
    zip
  ];

  packScript = pkgs.writeShellApplication {
    name = "google-takeout-pack";
    runtimeInputs = backupRuntime;
    text = ''
      set -euo pipefail

      backup_dir="''${GOOGLE_TAKEOUT_BACKUP_DIR:-$HOME/.backup}"
      stage_dir="$backup_dir/.stage"
      logs_dir="$backup_dir/.logs"
      manifest_dir="$backup_dir/.manifest"
      output="$backup_dir/all-clean.lrz"

      threads=8
      maxram=80
      window=20
      level=6
      skip_source_integrity=0
      clean_temp=0
      clean_source=0
      dry_run=0

      usage() {
        cat <<USAGE
      Usage: google-takeout-pack [options]

      Options:
        --threads N          Number of lrzip threads (default: 8)
        --maxram N           Maximum RAM for lrzip in hundreds of MB (default: 80)
        --window N           Maximum lrzip window in hundreds of MB (default: 20)
        --level N            Compression level 1-9 (default: 6)
        --skip-source-integrity   Skip integrity check on source archives
        --clean-temp         Remove staging and logs after a successful pack
        --clean-source       Remove source archives after a successful pack and integrity test
        --dry-run            Print what would happen without changes
        -h, --help           Show this help

      Environment:
        GOOGLE_TAKEOUT_BACKUP_DIR  Override backup directory (default: ~/.backup)

      Notes:
        - --clean-source is a no-op unless the resulting all-clean.lrz passes lrzip -t.
        - The script never deletes the final all-clean.lrz.
      USAGE
      }

      log() {
        printf '[%s] %s\n' "$(date -Iseconds)" "$*"
      }

      while [ $# -gt 0 ]; do
        case "$1" in
          --threads)
            threads="$2"
            shift 2
            ;;
          --maxram)
            maxram="$2"
            shift 2
            ;;
          --window)
            window="$2"
            shift 2
            ;;
          --level)
            level="$2"
            shift 2
            ;;
          --skip-source-integrity)
            skip_source_integrity=1
            shift
            ;;
          --clean-temp)
            clean_temp=1
            shift
            ;;
          --clean-source)
            clean_source=1
            shift
            ;;
          --dry-run)
            dry_run=1
            shift
            ;;
          -h|--help)
            usage
            exit 0
            ;;
          *)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
        esac
      done

      if [ ! -d "$backup_dir" ]; then
        echo "Backup directory not found: $backup_dir" >&2
        exit 1
      fi

      log "Backup dir: $backup_dir"
      log "Threads=$threads maxram=$maxram window=$window level=$level dry_run=$dry_run clean_temp=$clean_temp clean_source=$clean_source"

      run() {
        if [ "$dry_run" = 1 ]; then
          printf '[dry-run] %s\n' "$*"
        else
          "$@"
        fi
      }

      mkdir -p "$logs_dir" "$manifest_dir"
      integrity_log="$logs_dir/source-integrity.txt"
      pack_log="$logs_dir/pack.log"
      summary="$backup_dir/all-clean.lrz.SUMMARY.txt"

      shopt -s nullglob

      log "Step 1/5: source integrity"
      {
        echo "=== SOURCE INTEGRITY $(date -Iseconds) ==="
        for f in "$backup_dir"/takeout-*.tgz; do
          [ -e "$f" ] || continue
          printf '%s\tgzip+tartest: ' "$(basename "$f")"
          if gzip -t "$f" >/dev/null 2>&1 && tar -tf "$f" >/dev/null 2>&1; then
            echo OK
          else
            echo FAIL
          fi
        done
        for f in "$backup_dir"/Photos*.zip; do
          [ -e "$f" ] || continue
          printf '%s\tunzip-test: ' "$(basename "$f")"
          if unzip -tq "$f" >/dev/null 2>&1; then
            echo OK
          else
            echo FAIL
          fi
        done
        echo "=== END $(date -Iseconds) ==="
      } > "$integrity_log"
      cat "$integrity_log"

      if [ "$skip_source_integrity" = 0 ] && grep -q FAIL "$integrity_log"; then
        log "Source integrity failures detected. Refusing to continue. See $integrity_log"
        exit 2
      fi

      log "Step 2/5: extract sources"
      if [ "$dry_run" = 0 ]; then
        rm -rf "$stage_dir"
        mkdir -p "$stage_dir"/{takeout,photos,loose,misc}
      fi

      for f in "$backup_dir"/takeout-*.tgz; do
        [ -e "$f" ] || continue
        base="$(basename "$f")"
        name="''${base%.tgz}"
        target="$stage_dir/takeout/$name"
        log "extract takeout: $base -> $target"
        run mkdir -p "$target"
        run tar -xzf "$f" -C "$target"
      done

      for f in "$backup_dir"/Photos*.zip; do
        [ -e "$f" ] || continue
        base="$(basename "$f")"
        name="''${base%.zip}"
        target="$stage_dir/photos/$name"
        log "extract photos: $base -> $target"
        run mkdir -p "$target"
        run unzip -q "$f" -d "$target"
      done

      for f in "$backup_dir"/VID_*.mp4; do
        [ -e "$f" ] || continue
        log "loose video: $(basename "$f")"
        run cp "$f" "$stage_dir/loose/"
      done

      for f in "$backup_dir"/pipelineLog-*.txt; do
        [ -e "$f" ] || continue
        log "misc log: $(basename "$f")"
        run mkdir -p "$stage_dir/misc"
        run cp "$f" "$stage_dir/misc/"
      done

      file_count="$(find "$stage_dir" -type f 2>/dev/null | wc -l)"
      stage_bytes="$(du -sb "$stage_dir" 2>/dev/null | awk '{print $1}')"
      log "Stage ready: $file_count files, $stage_bytes bytes"

      log "Step 3/5: pack with lrzip"
      {
        echo "=== PACK $(date -Iseconds) ==="
        echo "Stage files: $file_count"
        echo "Stage size: $stage_bytes bytes"
        echo "Output: $output"
        echo "lrzip args: -m $maxram -w $window -L $level -p $threads"
      } > "$pack_log"

      if [ "$dry_run" = 0 ]; then
        if ! ( cd "$stage_dir" && tar -cf - . ) \
             | lrzip -m "$maxram" -w "$window" -L "$level" -p "$threads" -o "$output" 2>>"$pack_log"; then
          echo "lrzip failed; see $pack_log" >&2
          exit 3
        fi
      fi

      log "Step 4/5: validate output"
      if [ "$dry_run" = 0 ]; then
        if ! lrzip -t "$output" 2>>"$pack_log"; then
          echo "lrzip -t failed; see $pack_log" >&2
          exit 4
        fi
        if ! lrzip -d -o - "$output" 2>/dev/null | tar -tf - > "$manifest_dir/file-list.txt"; then
          echo "tar -tf failed on decompressed stream" >&2
          exit 4
        fi
        entries="$(wc -l < "$manifest_dir/file-list.txt")"
        log "Archive list: $entries entries"
      else
        entries=0
      fi

      log "Step 5/5: write summary"
      output_size="$(stat -c%s "$output" 2>/dev/null || echo 0)"
      {
        echo "Backup: $backup_dir"
        echo "Generated: $(date -Iseconds)"
        echo "Files staged: $file_count"
        echo "Entries in archive: $entries"
        echo "Stage bytes: $stage_bytes"
        echo "Output size: $output_size bytes"
        echo "Compression ratio: $(awk -v a="$stage_bytes" -v b="$output_size" 'BEGIN{ if(a>0){printf "%.2f%%", (1-b/a)*100}else{print "n/a"} }')"
        echo "lrzip flags: -m $maxram -w $window -L $level -p $threads"
      } > "$summary"
      cat "$summary"

      if [ "$clean_temp" = 1 ] && [ "$dry_run" = 0 ]; then
        log "Cleaning temporary staging and logs"
        run rm -rf "$stage_dir" "$logs_dir"
      fi

      if [ "$clean_source" = 1 ] && [ "$dry_run" = 0 ]; then
        if lrzip -t "$output" >/dev/null 2>&1; then
          log "Cleaning source archives (output validated)"
          run rm -f "$backup_dir"/takeout-*.tgz
          run rm -f "$backup_dir"/Photos*.zip
          run rm -f "$backup_dir"/VID_*.mp4
          run rm -f "$backup_dir"/pipelineLog-*.txt
        else
          echo "Refusing to clean source: lrzip -t failed on $output" >&2
          exit 5
        fi
      fi

      log "Done"
    '';
  };

  testScript = pkgs.writeShellApplication {
    name = "google-takeout-pack-test";
    runtimeInputs = backupRuntime ++ [ packScript ];
    text = ''
      set -euo pipefail

      work="$(mktemp -d -t google-takeout-pack-test.XXXXXX)"
      trap 'rm -rf "$work"' EXIT

      backup_dir="$work/backup"
      mkdir -p "$backup_dir/.stage" 2>/dev/null || true
      mkdir -p "$backup_dir"

      log() { printf '[test %s] %s\n' "$(date +%H:%M:%S)" "$*"; }

      log "Building synthetic backup at $backup_dir"

      mkdir -p "$work/src_a" "$work/src_b"
      printf 'hello a\n' > "$work/src_a/file1.txt"
      printf 'duplicate content\n' > "$work/src_a/dup.txt"
      printf 'unique b\n' > "$work/src_b/file2.txt"
      printf 'duplicate content\n' > "$work/src_b/dup.txt"

      ( cd "$work/src_a" && tar -czf "$backup_dir/takeout-test-a.tgz" . )
      ( cd "$work/src_b" && tar -czf "$backup_dir/takeout-test-b.tgz" . )

      rm -f "$backup_dir/Photos1.zip"
      ( cd "$work/src_a" && zip -qr "$backup_dir/Photos1.zip" . )

      log "Running google-takeout-pack against synthetic backup"
      GOOGLE_TAKEOUT_BACKUP_DIR="$backup_dir" google-takeout-pack \
        --threads 2 --maxram 40 --window 5 --level 1

      test -f "$backup_dir/all-clean.lrz" || { echo "FAIL: missing all-clean.lrz" >&2; exit 1; }

      log "Validating with lrzip -t"
      lrzip -t "$backup_dir/all-clean.lrz"

      log "Validating contents"
      lrzip -d -o - "$backup_dir/all-clean.lrz" | tar -tf - > "$work/list.txt"
      cat "$work/list.txt"

      grep -q "file1.txt" "$work/list.txt" || { echo "FAIL: file1.txt missing" >&2; exit 1; }
      grep -q "file2.txt" "$work/list.txt" || { echo "FAIL: file2.txt missing" >&2; exit 1; }

      log "PASS"
    '';
  };
in
{
  inherit packScript testScript;
  backupRuntime = backupRuntime;
}

{
  lib,
  pkgs,
}:

let
  backupRuntime = with pkgs; [
    bzip2
    coreutils
    file
    findutils
    gawk
    gnugrep
    gnused
    gnutar
    gzip
    lrzip
    p7zip
    procps
    rsync
    unzip
    util-linux
    xz
    zip
    zstd
  ];

  packScript = pkgs.writeShellApplication {
    name = "archive-pack";
    runtimeInputs = backupRuntime;
    text = ''
      set -euo pipefail

      backup_dir="''${ARCHIVE_PACK_DIR:-$HOME/.backup}"
      stage_dir="$backup_dir/stage"
      logs_dir="$backup_dir/logs"
      manifest_dir="$backup_dir/manifest"
      output="$backup_dir/archive.lrz"
      summary="$backup_dir/archive.lrz.SUMMARY.txt"

      threads=8
      maxram=80
      window=20
      level=6
      skip_source_integrity=0
      clean_temp=0
      clean_source=0
      keep_archives=0
      dry_run=0
      retain=5
      retain_days=0
      exclude_patterns=()

      default_excludes=(
        "stage"
        "logs"
        "manifest"
        "archive.lrz"
        "archive.lrz.SUMMARY.txt"
        "archive-*.lrz"
        ".gitignore"
        "README.md"
      )

      usage() {
        cat <<USAGE
      Usage: archive-pack [options]

      Packs every file/folder in $backup_dir into a single lrzip-compressed
      archive with append-only semantics. Re-pack merges new sources with
      the previous archive: removed sources stay, modified sources update,
      and lrzip dedups identical content.

      Append-only semantics:
        - A file or directory removed from $backup_dir stays in the archive.
        - A file replaced in $backup_dir (same name, new content) replaces the
          previous version in the archive ("newer wins").
        - A file renamed in $backup_dir appears under both old and new paths;
          lrzip dedups identical content if the bytes match.

      Options:
        --threads N            Number of lrzip threads (default: 8)
        --maxram N             Maximum RAM for lrzip in hundreds of MB (default: 80)
        --window N             Maximum lrzip window in hundreds of MB (default: 20)
        --level N              Compression level 1-9 (default: 6)
        --skip-source-integrity   Skip per-file integrity check before packing
        --keep-archives        Also preserve original archive files (.tar.*, .zip, .7z)
                               alongside their extracted contents
        --clean-temp           Remove stage/logs/manifest after a successful pack
        --clean-source         Remove top-level sources after a successful pack and
                               integrity test. Excludes (see --exclude) are preserved.
        --exclude GLOB         Extra path glob to skip (repeatable). On top of the
                               built-in excludes (stage, logs, manifest, archive.lrz,
                               archive.lrz.SUMMARY.txt, archive-*.lrz, .gitignore,
                               README.md).
        --retain N             Keep the last N datestamped snapshots (default: 5).
                               0 disables snapshot retention.
        --retain-days N        Drop snapshots older than N days (default: 0, off).
                               0 disables age-based retention.
        --dry-run              Print actions without writing the archive
        --verify               Only run 'lrzip -t' on the existing archive and exit
        -h, --help             Show this help

      Environment:
        ARCHIVE_PACK_DIR   Override backup directory (default: ~/.backup)
      USAGE
      }

      log() {
        printf '[%s] %s\n' "$(date -Iseconds)" "$*"
      }

      match_excluded() {
        local name="$1"
        local glob_pat
        for glob_pat in "''${default_excludes[@]}" "''${exclude_patterns[@]}"; do
          # shellcheck disable=SC2053
          [[ "$name" == $glob_pat ]] && return 0
        done
        return 1
      }

      classify() {
        # echoes: tar | zip | 7z | regular | dir
        local f="$1"
        if [ -d "$f" ]; then
          echo dir
          return
        fi
        case "$f" in
          *.tar|*.tar.gz|*.tgz|*.tar.bz2|*.tbz2|*.tar.xz|*.txz|*.tar.zst|*.tzst|*.tar.lz|*.tlz)
            echo tar ;;
          *.zip) echo zip ;;
          *.7z) echo 7z ;;
          *) echo regular ;;
        esac
      }

      extract_archive() {
        # $1 = archive file, $2 = destination dir
        local f="$1" dest="$2"
        mkdir -p "$dest"
        case "$f" in
          *.tar.gz|*.tgz)    tar -xzf "$f" -C "$dest" ;;
          *.tar.bz2|*.tbz2)  tar -xjf "$f" -C "$dest" ;;
          *.tar.xz|*.txz)    tar -xJf "$f" -C "$dest" ;;
          *.tar.zst|*.tzst)  tar --zstd -xf "$f" -C "$dest" ;;
          *.tar.lz|*.tlz)    tar --lzip -xf "$f" -C "$dest" ;;
          *.tar)             tar -xf "$f" -C "$dest" ;;
          *.zip)             unzip -q "$f" -d "$dest" ;;
          *.7z)              7z x -y -o"$dest" "$f" >/dev/null ;;  # 7z: -o<path> with no space
          *)                 return 1 ;;
        esac
      }

      test_archive() {
        # returns 0 if archive is OK, 1 otherwise
        local f="$1"
        case "$f" in
          *.tar.gz|*.tgz)
            gzip -t "$f" >/dev/null 2>&1 || return 1
            tar -tf "$f" >/dev/null 2>&1 || return 1
            ;;
          *.tar.bz2|*.tbz2)
            bzip2 -t "$f" >/dev/null 2>&1 || return 1
            tar -tf "$f" >/dev/null 2>&1 || return 1
            ;;
          *.tar.xz|*.txz)
            xz -t "$f" >/dev/null 2>&1 || return 1
            tar -tf "$f" >/dev/null 2>&1 || return 1
            ;;
          *.tar.zst|*.tzst)
            zstd -q -t "$f" >/dev/null 2>&1 || return 1
            tar -tf "$f" >/dev/null 2>&1 || return 1
            ;;
          *.tar.lz|*.tlz)
            tar -tf "$f" >/dev/null 2>&1 || return 1
            ;;
          *.tar)
            tar -tf "$f" >/dev/null 2>&1 || return 1
            ;;
          *.zip)
            unzip -tq "$f" >/dev/null 2>&1 || return 1
            ;;
          *.7z)
            7z t "$f" >/dev/null 2>&1 || return 1
            ;;
          *)
            return 1
            ;;
        esac
      }

      stage_basename() {
        # $1 = original file path, returns basename to use in staging dir
        local base="$1"
        # strip leading path
        base="$(basename "$base")"
        case "$base" in
          *.tar.gz) echo "''${base%.tar.gz}-extract" ;;
          *.tgz)    echo "''${base%.tgz}-extract" ;;
          *.tar.bz2) echo "''${base%.tar.bz2}-extract" ;;
          *.tbz2)   echo "''${base%.tbz2}-extract" ;;
          *.tar.xz) echo "''${base%.tar.xz}-extract" ;;
          *.txz)    echo "''${base%.txz}-extract" ;;
          *.tar.zst) echo "''${base%.tar.zst}-extract" ;;
          *.tzst)   echo "''${base%.tzst}-extract" ;;
          *.tar.lz) echo "''${base%.tar.lz}-extract" ;;
          *.tlz)    echo "''${base%.tlz}-extract" ;;
          *.tar)    echo "''${base%.tar}-extract" ;;
          *.zip)    echo "''${base%.zip}-extract" ;;
          *.7z)     echo "''${base%.7z}-extract" ;;
          *)        echo "$base" ;;
        esac
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
          --keep-archives)
            keep_archives=1
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
          --exclude)
            exclude_patterns+=("$2")
            shift 2
            ;;
          --retain)
            retain="$2"
            shift 2
            ;;
          --retain-days)
            retain_days="$2"
            shift 2
            ;;
          --dry-run)
            dry_run=1
            shift
            ;;
          --verify)
            if [ ! -f "$output" ]; then
              echo "No archive at $output" >&2
              exit 1
            fi
            log "Verifying $output"
            lrzip -t "$output"
            log "OK"
            exit 0
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
      log "Options: threads=$threads maxram=$maxram window=$window level=$level dry_run=$dry_run clean_temp=$clean_temp clean_source=$clean_source keep_archives=$keep_archives retain=$retain retain_days=$retain_days"
      log "Extra excludes: ''${exclude_patterns[*]:-<none>}"

      run() {
        if [ "$dry_run" = 1 ]; then
          printf '[dry-run] %s\n' "$*"
        else
          "$@"
        fi
      }

      # Verify-only short circuit
      if [ -n "''${VERIFY_ONLY:-}" ]; then
        if [ ! -f "$output" ]; then
          echo "No archive at $output" >&2
          exit 1
        fi
        lrzip -t "$output"
        exit 0
      fi

      mkdir -p "$logs_dir" "$manifest_dir" "$stage_dir"
      integrity_log="$logs_dir/source-integrity.txt"
      pack_log="$logs_dir/pack.log"
      : > "$pack_log"

      # Pre-clean staging (we rebuild from scratch on every run)
      if [ "$dry_run" = 0 ]; then
        find "$stage_dir" -mindepth 1 -delete 2>/dev/null || rm -rf "$stage_dir"
      fi

      # -----------------------------------------------------------------
      # Step 1: integrity check on all candidate archive inputs
      # -----------------------------------------------------------------
      log "Step 1/5: source integrity"
      {
        echo "=== SOURCE INTEGRITY $(date -Iseconds) ==="
        shopt -s nullglob dotglob
        for f in "$backup_dir"/*; do
          [ -e "$f" ] || continue
          name="$(basename "$f")"
          match_excluded "$name" && continue
          kind="$(classify "$f")"
          case "$kind" in
            tar|zip|7z)
              printf '%s\t%s-test: ' "$name" "$kind"
              if test_archive "$f"; then
                echo OK
              else
                echo FAIL
              fi
              ;;
            *)
              # regular files and directories: nothing to test
              printf '%s\t(type=%s, no integrity test)\n' "$name" "$kind"
              ;;
          esac
        done
        shopt -u dotglob
        echo "=== END $(date -Iseconds) ==="
      } > "$integrity_log"
      cat "$integrity_log"

      if [ "$skip_source_integrity" = 0 ] && grep -q FAIL "$integrity_log"; then
        log "Source integrity failures detected. Refusing to continue. See $integrity_log"
        exit 2
      fi

      # -----------------------------------------------------------------
      # Step 2: extract sources into staging (new)
      # -----------------------------------------------------------------
      log "Step 2/5: extract sources"
      shopt -s nullglob dotglob
      for f in "$backup_dir"/*; do
        [ -e "$f" ] || continue
        name="$(basename "$f")"
        match_excluded "$name" && { log "skip (excluded): $name"; continue; }
        kind="$(classify "$f")"
        bname="$(stage_basename "$f")"
        case "$kind" in
          tar|zip|7z)
            target="$stage_dir/$bname"
            log "extract $kind: $name -> $target"
            if [ "$dry_run" = 0 ]; then
              mkdir -p "$target"
              if ! extract_archive "$f" "$target"; then
                log "extract FAILED for $name; copying raw instead"
                cp -a "$f" "$stage_dir/"
              fi
            fi
            if [ "$keep_archives" = 1 ]; then
              log "keep-archive: copy $name -> $stage_dir/$name"
              run cp -a "$f" "$stage_dir/"
            fi
            ;;
          dir)
            target="$stage_dir/$bname"
            log "copy dir: $name -> $target"
            run cp -a "$f" "$target"
            ;;
          *)
            # regular file
            target="$stage_dir/$bname"
            log "copy file: $name -> $target"
            run cp -a "$f" "$target"
            ;;
        esac
      done
      shopt -u dotglob

      file_count="$(find "$stage_dir" -type f 2>/dev/null | wc -l)"
      stage_bytes="$(du -sb "$stage_dir" 2>/dev/null | awk '{print $1}')"
      log "Stage (new) ready: $file_count files, $stage_bytes bytes"

      # -----------------------------------------------------------------
      # Step 3: merge with previous archive (append-only)
      # -----------------------------------------------------------------
      log "Step 3/5: merge with previous archive"
      if [ -f "$output" ] && lrzip -t "$output" 2>>"$pack_log"; then
        prev_extract="$stage_dir/.prev"
        mkdir -p "$prev_extract"
        log "Extracting previous archive for merge"
        if lrzip -d -o - "$output" 2>>"$pack_log" | tar -xf - -C "$prev_extract"; then
          # Build merged: new wins, prev fills gaps (append-only).
          merged="$stage_dir/.merged"
          mkdir -p "$merged"
          shopt -s dotglob nullglob
          for item in "$stage_dir"/*; do
            name="$(basename "$item")"
            [ "$name" = ".prev" ] && continue
            [ "$name" = ".merged" ] && continue
            cp -a "$item" "$merged/"
          done
          shopt -u dotglob nullglob
          # Append-only fill: copy prev contents into merged, skipping anything
          # already provided by new sources. rsync --ignore-existing applies the
          # skip per-file (rather than per-directory), so prev-only files inside
          # directories that exist in both prev and new are preserved.
          rsync -a --ignore-existing "$prev_extract"/ "$merged"/
          rm -rf "$prev_extract"
          # Replace stage_dir contents with merged (skip .merged itself).
          find "$stage_dir" -mindepth 1 -maxdepth 1 ! -name '.merged' -exec rm -rf {} +
          shopt -s dotglob nullglob
          mv "$stage_dir/.merged"/* "$stage_dir/"
          shopt -u dotglob nullglob
          rmdir "$stage_dir/.merged"
          log "Merged with previous archive (append-only)"
        else
          log "Failed to extract previous archive; pack will be a full rebuild"
          rm -rf "$prev_extract"
        fi
      else
        log "No previous archive (or it failed integrity test); pack is a full rebuild"
      fi

      file_count="$(find "$stage_dir" -type f 2>/dev/null | wc -l)"
      stage_bytes="$(du -sb "$stage_dir" 2>/dev/null | awk '{print $1}')"
      log "Stage (final) ready: $file_count files, $stage_bytes bytes"

      # -----------------------------------------------------------------
      # Step 4: pack with lrzip
      # -----------------------------------------------------------------
      log "Step 4/5: pack with lrzip"
      {
        echo "=== PACK $(date -Iseconds) ==="
        echo "Stage files: $file_count"
        echo "Stage size: $stage_bytes bytes"
        echo "Output: $output"
        echo "lrzip args: -m $maxram -w $window -L $level -p $threads"
      } >> "$pack_log"

      if [ "$dry_run" = 0 ]; then
        tmp_output="$output.tmp.$$"
        if ! ( cd "$stage_dir" && tar -cf - . ) \
             | lrzip -m "$maxram" -w "$window" -L "$level" -p "$threads" -o "$tmp_output" 2>>"$pack_log"; then
          echo "lrzip failed; see $pack_log" >&2
          rm -f "$tmp_output"
          exit 3
        fi

        if ! lrzip -t "$tmp_output" 2>>"$pack_log"; then
          echo "lrzip -t failed on $tmp_output; see $pack_log" >&2
          rm -f "$tmp_output"
          exit 4
        fi

        # Snapshot rotation: if retain > 0, move current archive to datestamped
        # before replacing. PID appended to avoid collisions when two packs
        # complete in the same wall-clock second.
        if [ -f "$output" ] && [ "$retain" -gt 0 ]; then
          stamp="$(date +%Y%m%d-%H%M%S)-$$"
          snapshot="$backup_dir/archive-$stamp.lrz"
          log "Snapshotting previous archive to $snapshot"
          mv "$output" "$snapshot"
        fi

        mv "$tmp_output" "$output"

        # Apply retention by count
        if [ "$retain" -gt 0 ]; then
          snap_list="$stage_dir/.snapshots.list"
          : > "$snap_list"
          for s in "$backup_dir"/archive-*.lrz; do
            [ -e "$s" ] && printf '%s\n' "$s" >> "$snap_list"
          done
          sort -o "$snap_list" "$snap_list"
          count=$(wc -l < "$snap_list")
          extra=$(( count - retain ))
          if [ "$extra" -gt 0 ]; then
            log "Retention: dropping $extra oldest snapshot(s)"
            while IFS= read -r old && [ "$extra" -gt 0 ]; do
              log "  removing $old"
              run rm -f "$old"
              extra=$(( extra - 1 ))
            done < "$snap_list"
          fi
          rm -f "$snap_list"
        fi
        # Apply retention by age
        if [ "$retain_days" -gt 0 ]; then
          for old in "$backup_dir"/archive-*.lrz; do
            [ -e "$old" ] || continue
            age_days=$(( ( $(date +%s) - $(stat -c%Y "$old") ) / 86400 ))
            if [ "$age_days" -gt "$retain_days" ]; then
              log "  removing (older than $retain_days days): $old"
              run rm -f "$old"
            fi
          done
        fi
      fi

      # -----------------------------------------------------------------
      # Step 5: write summary
      # -----------------------------------------------------------------
      log "Step 5/5: write summary"
      output_size="$(stat -c%s "$output" 2>/dev/null || echo 0)"
      {
        echo "Backup: $backup_dir"
        echo "Generated: $(date -Iseconds)"
        echo "Files staged: $file_count"
        echo "Stage bytes: $stage_bytes"
        echo "Output size: $output_size bytes"
        echo "Compression ratio: $(awk -v a="$stage_bytes" -v b="$output_size" 'BEGIN{ if(a>0){printf "%.2f%%", (1-b/a)*100}else{print "n/a"} }')"
        echo "lrzip flags: -m $maxram -w $window -L $level -p $threads"
        echo "Excludes: ''${exclude_patterns[*]:-<none>}"
        echo "Keep archives: $keep_archives"
      } > "$summary"
      cat "$summary"

      if [ "$dry_run" = 0 ]; then
        if ! lrzip -d -o - "$output" 2>/dev/null | tar -tf - > "$manifest_dir/file-list.txt"; then
          echo "tar -tf failed on decompressed stream" >&2
          exit 4
        fi
        entries="$(wc -l < "$manifest_dir/file-list.txt")"
        log "Archive list: $entries entries"
      fi

      if [ "$clean_temp" = 1 ] && [ "$dry_run" = 0 ]; then
        log "Cleaning temporary stage/logs"
        run rm -rf "$stage_dir" "$logs_dir"
      fi

      if [ "$clean_source" = 1 ] && [ "$dry_run" = 0 ]; then
        if lrzip -t "$output" >/dev/null 2>&1; then
          log "Cleaning sources (excluding preserved)"
          shopt -s nullglob dotglob
          for f in "$backup_dir"/*; do
            [ -e "$f" ] || continue
            name="$(basename "$f")"
            match_excluded "$name" && continue
            log "  rm $name"
            run rm -rf "$f"
          done
          shopt -u dotglob
        else
          echo "Refusing to clean source: lrzip -t failed on $output" >&2
          exit 5
        fi
      fi

      log "Done"
    '';
  };

  testScript = pkgs.writeShellApplication {
    name = "archive-pack-test";
    runtimeInputs = backupRuntime ++ [ packScript ];
    text = ''
      set -euo pipefail

      work="$(mktemp -d -t archive-pack-test.XXXXXX)"
      trap 'rm -rf "$work"' EXIT

      backup_dir="$work/backup"
      mkdir -p "$backup_dir"

      log() { printf '[test %s] %s\n' "$(date +%H:%M:%S)" "$*"; }

      log "Building synthetic backup at $backup_dir"

      # Type variety: tar.gz, zip, plain file, directory
      mkdir -p "$work/src_a" "$work/src_b"
      printf 'hello a\n' > "$work/src_a/file1.txt"
      printf 'shared content\n' > "$work/src_a/shared.txt"
      printf 'unique b\n' > "$work/src_b/file2.txt"
      printf 'shared content\n' > "$work/src_b/shared.txt"

      ( cd "$work/src_a" && tar -czf "$backup_dir/sample-a.tgz" . )
      ( cd "$work/src_b" && tar -czf "$backup_dir/sample-b.tgz" . )
      rm -f "$backup_dir/sample-c.zip"
      ( cd "$work/src_a" && zip -qr "$backup_dir/sample-c.zip" . )
      printf 'plain note\n' > "$backup_dir/notes.txt"
      mkdir -p "$backup_dir/docs"
      printf 'readme\n' > "$backup_dir/docs/readme.txt"

      log "First archive-pack (no prev archive)"
      ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
        --threads 2 --maxram 40 --window 5 --level 1

      test -f "$backup_dir/archive.lrz" || { echo "FAIL: missing archive.lrz" >&2; exit 1; }

      log "Validate first archive with lrzip -t"
      lrzip -t "$backup_dir/archive.lrz"

      log "Validate first archive contents"
      lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - > "$work/list1.txt"
      cat "$work/list1.txt"

      grep -q "file1.txt" "$work/list1.txt" || { echo "FAIL: file1.txt missing" >&2; exit 1; }
      grep -q "file2.txt" "$work/list1.txt" || { echo "FAIL: file2.txt missing" >&2; exit 1; }
      grep -q "notes.txt" "$work/list1.txt" || { echo "FAIL: notes.txt missing" >&2; exit 1; }
      grep -q "docs/readme.txt" "$work/list1.txt" || { echo "FAIL: docs/readme.txt missing" >&2; exit 1; }

      log "Simulate append-only: remove source, re-pack, old file should remain"
      rm -f "$backup_dir/sample-a.tgz"
      ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
        --threads 2 --maxram 40 --window 5 --level 1

      lrzip -t "$backup_dir/archive.lrz"
      lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - > "$work/list2.txt"

      grep -q "file1.txt" "$work/list2.txt" || { echo "FAIL: file1.txt missing after re-pack (append-only broken)" >&2; exit 1; }
      grep -q "file2.txt" "$work/list2.txt" || { echo "FAIL: file2.txt missing after re-pack" >&2; exit 1; }

      log "Test --exclude"
      rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
        "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
      ( cd "$work/src_a" && tar -czf "$backup_dir/sample-a.tgz" . )
      ( cd "$work/src_b" && tar -czf "$backup_dir/sample-b.tgz" . )
      ( cd "$work/src_a" && zip -qr "$backup_dir/sample-c.zip" . )
      printf 'plain note\n' > "$backup_dir/notes.txt"
      mkdir -p "$backup_dir/docs"
      printf 'readme\n' > "$backup_dir/docs/readme.txt"

      ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
        --threads 2 --maxram 40 --window 5 --level 1 \
        --exclude 'sample-b.tgz'

      lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - > "$work/list3.txt"
      # Tight assertion: forbid the literal archive file and its extracted prefix.
      # Two greps so each pattern has its own anchoring (single regex with
      # alternation gets ambiguous when one alternative needs prefix-match and the
      # other needs exact-match).
      if grep -qE "^\\./sample-b\\.tgz$" "$work/list3.txt" \
         || grep -qE "^\\./sample-b-extract/" "$work/list3.txt"; then
        echo "FAIL: sample-b should have been excluded" >&2
        exit 1
      fi

      log "Test append-only preserves files inside directories that exist in both prev and new"
      rm -rf "$backup_dir/stage" "$backup_dir/logs" "$backup_dir/manifest" \
        "$backup_dir/archive.lrz" "$backup_dir"/archive-*.lrz
      mkdir -p "$backup_dir/docs"
      printf 'readme\n' > "$backup_dir/docs/readme.txt"
      printf 'old file\n' > "$backup_dir/docs/keep-me.txt"

      log "First pack: seeds archive with docs/keep-me.txt and docs/readme.txt"
      ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
        --threads 2 --maxram 40 --window 5 --level 1

      log "Simulate user removing keep-me.txt from source and adding newer.txt"
      rm "$backup_dir/docs/keep-me.txt"
      printf 'newer file\n' > "$backup_dir/docs/newer.txt"

      log "Re-pack: keep-me.txt must survive in archive even though removed from source"
      log "(this guards the append-only invariant for files inside shared dirs)"
      ARCHIVE_PACK_DIR="$backup_dir" archive-pack \
        --threads 2 --maxram 40 --window 5 --level 1

      lrzip -d -o - "$backup_dir/archive.lrz" | tar -tf - > "$work/list4.txt"

      grep -q "^\\./docs/keep-me\\.txt$" "$work/list4.txt" \
        || { echo "FAIL: docs/keep-me.txt missing after re-pack (append-only regression)" >&2; exit 1; }
      grep -q "^\\./docs/newer\\.txt$" "$work/list4.txt" \
        || { echo "FAIL: docs/newer.txt missing after re-pack" >&2; exit 1; }
      grep -q "^\\./docs/readme\\.txt$" "$work/list4.txt" \
        || { echo "FAIL: docs/readme.txt missing after re-pack" >&2; exit 1; }

      log "PASS"
    '';
  };
in
{
  inherit packScript testScript;
  backupRuntime = backupRuntime;
}

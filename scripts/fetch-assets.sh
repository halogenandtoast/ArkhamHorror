#!/usr/bin/env bash
#
# Downloads image assets from the public CloudFront CDN.
# No AWS credentials required — uses curl over public HTTPS.
#
# Usage:
#   ./scripts/fetch-assets.sh cards   # English card images only (~755 MB)
#   ./scripts/fetch-assets.sh en      # All English images (cards + UI assets)
#   ./scripts/fetch-assets.sh fr      # French card translations
#   ./scripts/fetch-assets.sh all     # Everything (~2.9 GB)
#
# Environment variables:
#   FETCH_PARALLEL  — number of parallel downloads (default: 10)
#   FETCH_RETRIES   — retry attempts per file (default: 3)
#   FETCH_VERIFY    — re-download corrupt/empty files (default: 1, set 0 to skip)
#
set -euo pipefail

CDN_BASE="https://assets.arkhamhorror.app"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PUBLIC_DIR="$ROOT_DIR/frontend/public"
MANIFEST="$ROOT_DIR/frontend/image-manifest.json"
PARALLEL="${FETCH_PARALLEL:-10}"
RETRIES="${FETCH_RETRIES:-3}"
VERIFY="${FETCH_VERIFY:-1}"

# Minimum valid file size in bytes (anything smaller is likely corrupt)
MIN_FILE_SIZE=100

# Temp file to collect failures across parallel jobs
FAIL_LOG=$(mktemp)
trap 'rm -f "$FAIL_LOG"' EXIT

# ── Helpers ──────────────────────────────────────────────────────────────

die()  { echo "Error: $*" >&2; exit 1; }

usage() {
  cat <<EOF
Usage: $(basename "$0") <target>

Targets:
  cards   English card images only (~755 MB)
  en      All English images (cards + boxes, portraits, etc.)
  fr      French translated card images
  es      Spanish translated card images
  ita     Italian translated card images
  ko      Korean translated card images
  zh      Chinese translated card images
  all     Everything (~2.9 GB)

Options (via env):
  FETCH_PARALLEL=N   parallel downloads (default: 10)
  FETCH_RETRIES=N    retry attempts per file (default: 3)
  FETCH_VERIFY=0     skip corrupt file check (default: enabled)
EOF
  exit 1
}

# Read file list from manifest for given directory keys.
# Arguments: one or more manifest keys (e.g. "img/arkham/cards")
files_for_keys() {
  node -e "
    const m = require('$MANIFEST');
    const keys = process.argv.slice(1);
    for (const k of keys) {
      if (m[k]) m[k].forEach(f => console.log(f));
    }
  " "$@"
}

# Check if a file looks valid (not empty, not too small, not an HTML error page)
is_valid_file() {
  local file="$1"
  [ -f "$file" ] || return 1

  local size
  size=$(wc -c < "$file" | tr -d ' ')

  # Too small — likely empty or truncated
  [ "$size" -ge "$MIN_FILE_SIZE" ] || return 1

  # Check if it's actually an HTML error page (CDN 403/404 can return HTML)
  if head -c 20 "$file" | grep -qi '<!doctype\|<html'; then
    return 1
  fi

  return 0
}

# Download a single file with retries. Removes corrupt output on failure.
fetch_one() {
  local rel_path="$1"
  local dest="$PUBLIC_DIR/$rel_path"
  local url="$CDN_BASE/$rel_path"
  local attempt=0
  local tmp_dest="${dest}.tmp"

  mkdir -p "$(dirname "$dest")"

  while [ "$attempt" -lt "$RETRIES" ]; do
    attempt=$((attempt + 1))

    if curl -sf --retry 2 --connect-timeout 10 --max-time 60 -o "$tmp_dest" "$url"; then
      if is_valid_file "$tmp_dest"; then
        mv "$tmp_dest" "$dest"
        return 0
      fi
      # Downloaded but invalid — retry
      rm -f "$tmp_dest"
    else
      rm -f "$tmp_dest"
    fi

    if [ "$attempt" -lt "$RETRIES" ]; then
      sleep $((attempt * 2))
    fi
  done

  # All retries exhausted
  rm -f "$tmp_dest" "$dest"
  echo "$rel_path" >> "$FAIL_LOG"
  echo "  FAILED after $RETRIES attempts: $rel_path" >&2
  return 1
}

download_files() {
  local total=0 skipped=0 downloaded=0 redownloaded=0
  local file_list
  file_list=$(mktemp)

  # Read stdin into temp file and count
  cat > "$file_list"
  total=$(wc -l < "$file_list" | tr -d ' ')

  if [ "$total" -eq 0 ]; then
    echo "No files to download."
    rm "$file_list"
    return
  fi

  echo "Downloading $total files (parallel: $PARALLEL, retries: $RETRIES)..."

  # Reset fail log
  : > "$FAIL_LOG"

  local count=0
  local pids=()

  while IFS= read -r rel_path; do
    local dest="$PUBLIC_DIR/$rel_path"

    # Check existing files
    if [ -f "$dest" ]; then
      # Verify existing file integrity if enabled
      if [ "$VERIFY" = "1" ] && ! is_valid_file "$dest"; then
        echo "  Re-downloading corrupt file: $rel_path"
        rm -f "$dest"
        redownloaded=$((redownloaded + 1))
      else
        skipped=$((skipped + 1))
        count=$((count + 1))
        continue
      fi
    fi

    # Launch download in background
    fetch_one "$rel_path" &
    pids+=($!)

    # Throttle parallelism
    if [ ${#pids[@]} -ge "$PARALLEL" ]; then
      wait "${pids[0]}" 2>/dev/null || true
      pids=("${pids[@]:1}")
    fi

    count=$((count + 1))
    if [ $((count % 500)) -eq 0 ]; then
      echo "  Progress: $count / $total (skipped $skipped existing)"
    fi
  done < "$file_list"

  # Wait for remaining downloads
  for pid in "${pids[@]}"; do
    wait "$pid" 2>/dev/null || true
  done

  rm "$file_list"

  local fail_count=0
  if [ -s "$FAIL_LOG" ]; then
    fail_count=$(wc -l < "$FAIL_LOG" | tr -d ' ')
  fi

  downloaded=$((total - skipped - fail_count))
  echo ""
  echo "Done: $downloaded downloaded, $skipped already existed."
  if [ "$redownloaded" -gt 0 ]; then
    echo "  $redownloaded corrupt files were re-downloaded."
  fi
  if [ "$fail_count" -gt 0 ]; then
    echo "  $fail_count files failed — run again to retry."
  fi
}

# ── Main ─────────────────────────────────────────────────────────────────

[ $# -ge 1 ] || usage

[ -f "$MANIFEST" ] || die "Manifest not found: $MANIFEST
Run 'make generate-manifest' first (or 'git checkout frontend/image-manifest.json')."

TARGET="$1"

# Define directory groups
EN_EXTRA_KEYS=(
  "img/arkham/boxes"
  "img/arkham/portraits"
  "img/arkham/tarot"
  "img/arkham/encounter-sets"
  "img/arkham/mini-cards"
  "img/arkham/sets"
  "img/arkham/customizations"
  "img/arkham/seals"
  "img/arkham/playing-cards"
)

case "$TARGET" in
  cards)
    echo "=== Fetching English card images ==="
    files_for_keys "img/arkham/cards" | download_files
    ;;
  en)
    echo "=== Fetching all English images ==="
    files_for_keys "img/arkham/cards" "${EN_EXTRA_KEYS[@]}" | download_files
    ;;
  fr|es|ita|ko|zh)
    echo "=== Fetching $TARGET translated images ==="
    files_for_keys "img/arkham/$TARGET" | download_files
    ;;
  all)
    echo "=== Fetching all English images ==="
    files_for_keys "img/arkham/cards" "${EN_EXTRA_KEYS[@]}" | download_files
    for lang in es fr ita ko zh; do
      echo ""
      echo "=== Fetching $lang translated images ==="
      files_for_keys "img/arkham/$lang" | download_files
    done
    ;;
  *)
    usage
    ;;
esac

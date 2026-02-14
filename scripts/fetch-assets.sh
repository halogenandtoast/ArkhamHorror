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
#
set -euo pipefail

CDN_BASE="https://assets.arkhamhorror.app"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PUBLIC_DIR="$ROOT_DIR/frontend/public"
MANIFEST="$ROOT_DIR/frontend/image-manifest.json"
PARALLEL="${FETCH_PARALLEL:-10}"

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

download_files() {
  local total=0 skipped=0 downloaded=0 failed=0
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

  echo "Downloading $total files (parallel: $PARALLEL)..."

  local count=0
  local pids=()

  while IFS= read -r rel_path; do
    local dest="$PUBLIC_DIR/$rel_path"

    # Skip if already exists
    if [ -f "$dest" ]; then
      skipped=$((skipped + 1))
      count=$((count + 1))
      continue
    fi

    mkdir -p "$(dirname "$dest")"
    local url="$CDN_BASE/$rel_path"

    # Launch download in background
    (
      if curl -sf --retry 2 -o "$dest" "$url"; then
        : # success
      else
        echo "  FAILED: $rel_path" >&2
      fi
    ) &
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
  downloaded=$((total - skipped))
  echo "Done: $downloaded downloaded, $skipped already existed."
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

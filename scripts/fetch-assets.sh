#!/usr/bin/env bash
#
# Downloads image assets using S3 for listing and CloudFront for delivery.
# Listing via S3 (no credentials required — public bucket).
# Download via CloudFront for faster edge delivery.
#
# Differential sync: skips files that already exist with the correct size.
# New/changed files are downloaded to a .tmp file, size-verified, then
# atomically renamed into place — interrupted downloads are always caught.
#
# Usage:
#   ./scripts/fetch-assets.sh cards      # English card images only (~755 MB)
#   ./scripts/fetch-assets.sh en         # All English/static images (~1.3 GB)
#   ./scripts/fetch-assets.sh fr         # French translated card images only
#   ./scripts/fetch-assets.sh en+fr      # English/static + French translations
#   ./scripts/fetch-assets.sh all        # Everything (~2.9 GB)
#
# Environment variables:
#   FETCH_S3_BUCKET   S3 bucket name for listing (default: arkham-horror-assets)
#   FETCH_CDN_BASE    CloudFront base URL (default: https://assets.arkhamhorror.app)
#   FETCH_PARALLEL    Parallel download count (default: 8)
#
set -euo pipefail

S3_BUCKET="${FETCH_S3_BUCKET:-arkham-horror-assets}"
CDN_BASE="${FETCH_CDN_BASE:-https://assets.arkhamhorror.app}"
PARALLEL="${FETCH_PARALLEL:-8}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PUBLIC_DIR="$ROOT_DIR/frontend/public"

# ── Helpers ───────────────────────────────────────────────────────────────────

die() { echo "Error: $*" >&2; exit 1; }

usage() {
  cat <<EOF
Usage: $(basename "$0") <target>

Targets:
  cards       English card images only (~755 MB)
  en          All English/static images (~1.3 GB)
  en+fr       English/static + French translations
  en+es       English/static + Spanish translations
  en+ita      English/static + Italian translations
  en+ko       English/static + Korean translations
  en+zh       English/static + Chinese translations
  fr          French translated card images only
  es          Spanish translated card images only
  ita         Italian translated card images only
  ko          Korean translated card images only
  zh          Chinese translated card images only
  all         Everything (~2.9 GB)

Options (via env):
  FETCH_S3_BUCKET=...         S3 bucket name for listing (default: arkham-horror-assets)
  FETCH_CDN_BASE=https://...  CloudFront base URL (default: https://assets.arkhamhorror.app)
  FETCH_PARALLEL=N            Parallel downloads (default: 8)
EOF
  exit 1
}

# Cross-platform file size (Linux: stat -c%s, macOS: stat -f%z)
_file_size() { stat -c%s "$1" 2>/dev/null || stat -f%z "$1"; }
export -f _file_size

# Download a single file from CloudFront, called in parallel via xargs.
# Args: <expected_size> <key>
_fetch_one() {
  local expected_size="$1"
  local key="$2"
  local dest="$PUBLIC_DIR/$key"
  local tmp="$dest.tmp"

  # Skip if the file already exists with the correct size
  if [ -f "$dest" ] && [ "$(_file_size "$dest")" = "$expected_size" ]; then
    return 0
  fi

  mkdir -p "$(dirname "$dest")"

  if ! curl -fsSL "$CDN_BASE/$key" -o "$tmp"; then
    rm -f "$tmp"
    echo "FAIL (download error): $key" >&2
    return 1
  fi

  local actual_size
  actual_size=$(_file_size "$tmp")

  if [ "$actual_size" != "$expected_size" ]; then
    rm -f "$tmp"
    echo "FAIL (size mismatch): $key (expected $expected_size, got $actual_size)" >&2
    return 1
  fi

  mv "$tmp" "$dest"
}
export -f _fetch_one
export PUBLIC_DIR CDN_BASE

# List all objects under an S3 prefix.
# AWS CLI v2 auto-paginates list-objects-v2, so this returns all objects
# even when there are more than 1000.
# Output: SIZE<TAB>KEY per line.
_list_objects() {
  aws s3api list-objects-v2 \
    --bucket "$S3_BUCKET" \
    --prefix "$1" \
    --no-sign-request \
    --output text \
    --query 'Contents[].[Size,Key]' \
    | grep -v '^None$' || true
}

# Sync all files under a prefix. Optional grep arguments are passed directly
# to filter the listing (e.g. -vE 'img/arkham/(es|fr)/' to exclude languages).
_sync_prefix() {
  local prefix="$1"; shift
  local filter_args=("$@")

  echo "Listing ${prefix}..."
  local listing
  listing=$(_list_objects "$prefix")

  if [ ${#filter_args[@]} -gt 0 ]; then
    listing=$(echo "$listing" | grep "${filter_args[@]}" || true)
  fi

  local total
  total=$(echo "$listing" | grep -c . || true)
  echo "Syncing $total files (parallel: $PARALLEL)..."

  local errors=0
  # Each listing line is "SIZE<TAB>KEY". xargs splits on whitespace,
  # so -n 2 passes SIZE and KEY as $1 and $2 to _fetch_one.
  echo "$listing" | xargs -P "$PARALLEL" -n 2 bash -c '_fetch_one "$@"' _ \
    || errors=$?

  if [ "$errors" -ne 0 ]; then
    echo "WARNING: some files failed to download — check stderr above" >&2
    return 1
  fi

  echo "Done."
}

# ── Main ──────────────────────────────────────────────────────────────────────

command -v aws  >/dev/null 2>&1 || die "aws CLI not found. Install from https://aws.amazon.com/cli/ or use 'make fetch-images-docker'."
command -v curl >/dev/null 2>&1 || die "curl not found."

[ $# -ge 1 ] || usage

ALL_LANGS=(es fr ita ko zh)

# Build a regex that excludes all language dirs except the one given.
# e.g. _other_langs_pattern fr -> "img/arkham/(es|ita|ko|zh)/"
_other_langs_pattern() {
  local keep="$1"
  local others=()
  for lang in "${ALL_LANGS[@]}"; do
    [ "$lang" != "$keep" ] && others+=("$lang")
  done
  local joined
  printf -v joined '%s|' "${others[@]}"
  echo "img/arkham/(${joined%|})/"
}

case "$1" in
  cards)
    echo "=== Fetching English card images ==="
    _sync_prefix "img/arkham/cards/"
    ;;
  en)
    echo "=== Fetching all English/static images ==="
    _sync_prefix "img/" -vE "img/arkham/($(IFS='|'; echo "${ALL_LANGS[*]}"))/"
    ;;
  en+fr|en+es|en+ita|en+ko|en+zh)
    lang="${1#en+}"
    echo "=== Fetching English/static + $lang translations ==="
    _sync_prefix "img/" -vE "$(_other_langs_pattern "$lang")"
    ;;
  fr|es|ita|ko|zh)
    echo "=== Fetching $1 translated images only ==="
    _sync_prefix "img/arkham/$1/"
    ;;
  all)
    echo "=== Fetching all images ==="
    _sync_prefix "img/"
    ;;
  *)
    usage
    ;;
esac

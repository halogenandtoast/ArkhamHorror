#!/usr/bin/env bash
#
# Downloads image assets from the public S3 bucket.
# No AWS credentials required — uses public bucket listing.
#
# Usage:
#   ./scripts/fetch-assets.sh cards   # English card images only
#   ./scripts/fetch-assets.sh en      # All English/static images (no translations)
#   ./scripts/fetch-assets.sh fr      # French card translations
#   ./scripts/fetch-assets.sh all     # Everything
#
# Environment variables:
#   FETCH_S3_BUCKET  — override the S3 bucket (default: s3://arkham-horror-assets)
#
set -euo pipefail

S3_BUCKET="${FETCH_S3_BUCKET:-s3://arkham-horror-assets}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PUBLIC_DIR="$ROOT_DIR/frontend/public"

BASE_ARGS=(--no-sign-request --exclude "*.DS_Store")

# Language-specific subdirectories to exclude from English/static syncs
LANG_EXCLUDES=(
  --exclude "arkham/es/*"
  --exclude "arkham/fr/*"
  --exclude "arkham/ita/*"
  --exclude "arkham/ko/*"
  --exclude "arkham/zh/*"
)

# ── Helpers ──────────────────────────────────────────────────────────────

die() { echo "Error: $*" >&2; exit 1; }

usage() {
  cat <<EOF
Usage: $(basename "$0") <target>

Targets:
  cards   English card images only
  en      All English/static images (cards, portraits, tokens, icons, etc.)
  fr      French translated card images
  es      Spanish translated card images
  ita     Italian translated card images
  ko      Korean translated card images
  zh      Chinese translated card images
  all     Everything

Options (via env):
  FETCH_S3_BUCKET=s3://...   S3 bucket URL (default: s3://arkham-horror-assets)
EOF
  exit 1
}

s3_sync() {
  local s3_path="$1"; shift
  aws s3 sync "$S3_BUCKET/$s3_path" "$PUBLIC_DIR/$s3_path" "${BASE_ARGS[@]}" "$@"
}

# ── Main ─────────────────────────────────────────────────────────────────

command -v aws >/dev/null 2>&1 \
  || die "aws CLI not found. Install it from https://aws.amazon.com/cli/ or use 'make fetch-images-docker'."

[ $# -ge 1 ] || usage

case "$1" in
  cards)
    echo "=== Fetching English card images ==="
    s3_sync "img/arkham/cards/"
    ;;
  en)
    echo "=== Fetching all English/static images ==="
    s3_sync "img/" "${LANG_EXCLUDES[@]}"
    ;;
  fr|es|ita|ko|zh)
    echo "=== Fetching $1 translated images ==="
    s3_sync "img/arkham/$1/"
    ;;
  all)
    echo "=== Fetching all images ==="
    s3_sync "img/"
    ;;
  *)
    usage
    ;;
esac

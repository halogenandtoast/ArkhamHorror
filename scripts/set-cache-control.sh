#!/usr/bin/env bash
#
# Backfills Cache-Control on images already in the public bucket.
#
# The objects were uploaded without one, so browsers fall back to heuristic
# freshness and CloudFront reports `x-cache: Miss` more than it should. New
# uploads get the header from `make sync-images`; this is for everything
# already up there.
#
# The trap this script exists to avoid: `--metadata-directive REPLACE` replaces
# ALL system metadata, so an object copied without an explicit --content-type
# comes back as binary/octet-stream and the browser refuses to render it. That
# is why the work is split per extension with the type named every time, and
# why --canary exists.
#
# Usage:
#   ./scripts/set-cache-control.sh --canary     # one object, then verify by hand
#   ./scripts/set-cache-control.sh --dry-run    # print what would run
#   ./scripts/set-cache-control.sh              # the real thing
#   PREFIX=img/arkham/cards ./scripts/set-cache-control.sh
#
set -euo pipefail

BUCKET="${BUCKET:-arkham-horror-assets}"
PREFIX="${PREFIX:-img}"
CACHE_CONTROL="${CACHE_CONTROL:-public, max-age=604800}"
CANARY_KEY="${CANARY_KEY:-img/arkham/cards/01000.avif}"

MODE=run
for arg in "$@"; do
  case "$arg" in
    --canary) MODE=canary ;;
    --dry-run) MODE=dry ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

command -v aws >/dev/null || { echo "aws cli is required" >&2; exit 1; }

# Extension -> the Content-Type that must be restored with it.
TYPES=(
  "avif:image/avif"
  "png:image/png"
  "jpg:image/jpeg"
  "jpeg:image/jpeg"
  "webp:image/webp"
  "svg:image/svg+xml"
  "gif:image/gif"
)

echo "bucket        : s3://$BUCKET/$PREFIX"
echo "cache-control : $CACHE_CONTROL"
echo

if [ "$MODE" = "canary" ]; then
  echo "canary on $CANARY_KEY"
  before=$(aws s3api head-object --bucket "$BUCKET" --key "$CANARY_KEY" \
    --query '[ContentType,CacheControl]' --output text)
  echo "  before: $before"
  aws s3api copy-object --bucket "$BUCKET" --key "$CANARY_KEY" \
    --copy-source "$BUCKET/$CANARY_KEY" \
    --metadata-directive REPLACE \
    --content-type image/avif \
    --cache-control "$CACHE_CONTROL" >/dev/null
  after=$(aws s3api head-object --bucket "$BUCKET" --key "$CANARY_KEY" \
    --query '[ContentType,CacheControl]' --output text)
  echo "  after : $after"
  echo
  echo "Now confirm the CDN still serves it, on a cache-busted URL so the answer"
  echo "comes from the origin rather than an edge:"
  echo "  curl -sI 'https://assets.arkhamhorror.app/$CANARY_KEY?x=\$RANDOM' | grep -i 'HTTP/\\|content-type\\|cache-control'"
  exit 0
fi

# Each pass re-lists the whole prefix, and one pass over 38,000 card faces runs
# for half an hour, so the work is split by sub-prefix and run several at a
# time. The sub-prefixes are disjoint, so the passes cannot collide.
PREFIXES=(
  "$PREFIX/arkham/cards"
  "$PREFIX/arkham/es"
  "$PREFIX/arkham/fr"
  "$PREFIX/arkham/ita"
  "$PREFIX/arkham/ko"
  "$PREFIX/arkham/rus"
  "$PREFIX/arkham/zh"
  "$PREFIX/arkham/homebrew"
  "$PREFIX/ah3e"
)
# Anything under img/ that the list above does not name -- portraits, sets,
# boxes, encounter-sets, chaos tokens, loose files. Named explicitly rather
# than swept, so a new directory is a visible omission instead of a silent one.
REST_EXCLUDES=()
for p in "${PREFIXES[@]}"; do REST_EXCLUDES+=(--exclude "${p#"$PREFIX/"}/*"); done

JOBS="${JOBS:-5}"

run_pass() {
  local prefix="$1" ext="$2" type="$3"
  shift 3
  aws s3 cp "s3://$BUCKET/$prefix/" "s3://$BUCKET/$prefix/" \
    --recursive --exclude "*" --include "*.$ext" "$@" \
    --metadata-directive REPLACE \
    --content-type "$type" \
    --cache-control "$CACHE_CONTROL" \
    --only-show-errors
}

running=0
for entry in "${TYPES[@]}"; do
  ext="${entry%%:*}"
  type="${entry#*:}"
  for prefix in "${PREFIXES[@]}" "$PREFIX"; do
    extra=()
    label="$prefix"
    if [ "$prefix" = "$PREFIX" ]; then
      extra=("${REST_EXCLUDES[@]}")
      label="$PREFIX (everything else)"
    fi
    if [ "$MODE" = "dry" ]; then
      echo "run_pass $prefix $ext $type ${extra[*]}"
      continue
    fi
    echo "== .$ext  $label"
    run_pass "$prefix" "$ext" "$type" "${extra[@]}" &
    running=$((running + 1))
    if [ "$running" -ge "$JOBS" ]; then wait -n 2>/dev/null || wait; running=$((running - 1)); fi
  done
done
[ "$MODE" = "dry" ] || wait

if [ "$MODE" != "dry" ]; then
  echo
  echo "Done. The objects were rewritten, so edges hold the old headers until"
  echo "they expire or are invalidated:"
  echo "  aws cloudfront create-invalidation --distribution-id EMYS27JG801W1 --paths '/img/*'"
fi

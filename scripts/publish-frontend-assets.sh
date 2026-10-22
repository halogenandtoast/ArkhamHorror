#!/usr/bin/env bash
#
# Uploads the frontend's content-hashed bundles to the CDN bucket.
#
# Why this exists: every pod serves /assets/ out of its own image, and bundle
# names differ between builds, so during a rolling update the load balancer's
# backend pool holds two sets of names at once -- and a tab left open across a
# deploy goes on importing the chunks of the build it was loaded with. The CDN
# bucket is the one place that can hold the union of every build that has
# shipped, and prod.nginxconf's @cdn_assets falls back to it for any /assets/
# name a pod does not have.
#
# The sync therefore NEVER deletes. Old bundle names going stale is exactly what
# this is here to survive; pruning them would reintroduce the bug on a slower
# clock. If the prefix ever needs trimming, do it by age, by hand, and well
# behind the oldest session anyone might still have open.
#
# Usage:
#   scripts/publish-frontend-assets.sh            # build from the working tree
#   scripts/publish-frontend-assets.sh <git-ref>  # build from committed contents
#
# Environment:
#   ASSET_BUCKET            bucket to upload to (default: arkham-horror-assets)
#   V2_BUILDER              buildx builder to use (default: arkham-multiarch)
#   V2_CACHE_DIR            local buildx cache to read (optional, but a miss
#                           means rebuilding the frontend from scratch)
#   BUILD_ARGS              extra --build-arg flags, matching the deploy's
#   EXPORT_PLATFORM         platform to export (default: linux/amd64)
#   ARKHAM_SKIP_ASSET_SYNC  set to 1 to skip entirely
#
set -euo pipefail

if [ "${ARKHAM_SKIP_ASSET_SYNC:-0}" = "1" ]; then
  echo ">> ARKHAM_SKIP_ASSET_SYNC=1 — skipping the CDN asset upload"
  echo "   (pods will 404 on bundles from a build they were not made from)"
  exit 0
fi

BUCKET="${ASSET_BUCKET:-arkham-horror-assets}"
BUILDER="${V2_BUILDER:-arkham-multiarch}"
# The dist tree is the same whichever architecture built it, so one is enough --
# and `--output type=local` with a multi-platform build would write per-platform
# subdirectories instead of the tree this script expects.
PLATFORM="${EXPORT_PLATFORM:-linux/amd64}"
REF="${1:-}"

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

command -v aws >/dev/null || {
  echo "aws CLI not found — install it, or set ARKHAM_SKIP_ASSET_SYNC=1" >&2
  exit 1
}

DEST="$(mktemp -d)"
trap 'rm -rf "$DEST"' EXIT

CACHE_FROM=()
if [ -n "${V2_CACHE_DIR:-}" ] && [ -d "${V2_CACHE_DIR}" ]; then
  CACHE_FROM=(--cache-from "type=local,src=${V2_CACHE_DIR}")
fi
# No --cache-to: the deploy's build owns that cache directory, and this run has
# nothing to add to it.

# shellcheck disable=SC2086  # BUILD_ARGS is a flag list and must word-split
if [ -n "$REF" ]; then
  echo ">> exporting frontend bundles from committed ref $REF"
  git archive --format=tar "$REF" | docker buildx build --builder "$BUILDER" \
    --platform "$PLATFORM" "${CACHE_FROM[@]}" ${BUILD_ARGS:-} \
    --target frontend-dist --output "type=local,dest=$DEST" \
    --file Dockerfile -
else
  echo ">> exporting frontend bundles from the working tree"
  docker buildx build --builder "$BUILDER" \
    --platform "$PLATFORM" "${CACHE_FROM[@]}" ${BUILD_ARGS:-} \
    --target frontend-dist --output "type=local,dest=$DEST" .
fi

IMMUTABLE="public, max-age=31536000, immutable"

upload() {
  local src="$1" label="$2"

  if [ ! -d "$src" ]; then
    echo "!! $label: no assets at $src — skipping" >&2
    return 0
  fi

  # --size-only is what makes this cheap AND correct. A fresh build's mtimes are
  # always newer than the bucket's, so the default size+mtime comparison would
  # re-upload every object on every deploy; and a content-hashed name that is
  # already up there is by definition the same bytes, so size alone is a safe
  # test.
  #
  # Content types are stated rather than guessed: the aws CLI's guesses come from
  # the local Python's mimetypes table, and a module script served as
  # application/octet-stream is refused by the browser outright.
  #
  # The .br/.gz siblings scripts/precompress.cjs writes are for nginx's
  # gzip_static; CloudFront negotiates compression itself, so uploading them
  # would just be dead weight in the bucket.
  echo ">> $label -> s3://$BUCKET/assets/"
  aws s3 sync "$src" "s3://$BUCKET/assets/" --size-only --acl public-read \
    --cache-control "$IMMUTABLE" --content-type "text/javascript" \
    --exclude "*" --include "*.js"
  aws s3 sync "$src" "s3://$BUCKET/assets/" --size-only --acl public-read \
    --cache-control "$IMMUTABLE" --content-type "text/css" \
    --exclude "*" --include "*.css"
  aws s3 sync "$src" "s3://$BUCKET/assets/" --size-only --acl public-read \
    --cache-control "$IMMUTABLE" \
    --exclude "*.js" --exclude "*.css" --exclude "*.br" --exclude "*.gz" \
    --exclude "*.DS_Store"
}

# Both frontends share the prefix. Their bundle names can only collide if the
# base name AND the hash match, and the hash is taken over the contents -- so a
# collision would be two identical files.
upload "$DEST/frontend/assets" "frontend"
upload "$DEST/frontend-3ed/assets" "frontend-3ed"

echo ">> done"

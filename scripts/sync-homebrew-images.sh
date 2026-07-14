#!/usr/bin/env bash
#
# Syncs homebrew campaign images (frontend/homebrew/<campaign>/img/) to S3 at
# their CDN paths (img/arkham/homebrew/<campaign>/). Campaign directory names
# are kebab-case, matching the campaign id (:circus-ex-mortis ->
# circus-ex-mortis) and the CDN path.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
HOMEBREW_DIR="$ROOT_DIR/frontend/homebrew"

[ -d "$HOMEBREW_DIR" ] || exit 0

for dir in "$HOMEBREW_DIR"/*/img; do
  [ -d "$dir" ] || continue
  campaign="$(basename "$(dirname "$dir")")"
  echo "Syncing homebrew images: $campaign -> img/arkham/homebrew/$campaign/"
  aws s3 sync "$dir" "s3://arkham-horror-assets/img/arkham/homebrew/$campaign" \
    --acl public-read --exclude ".DS_Store"
done

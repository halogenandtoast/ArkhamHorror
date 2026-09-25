#!/usr/bin/env bash
#
# Syncs local images to S3 and regenerates the image manifest.
# Run this after adding or updating image assets.
#
# Usage: ./scripts/sync-and-manifest.sh
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== Step 1/2: Syncing images to S3 ==="
cd "$ROOT_DIR/frontend/public"
aws s3 sync . s3://arkham-horror-assets --acl public-read --exclude ".DS_Store" --exclude "img/custom/*"
cd "$ROOT_DIR"
./scripts/sync-homebrew-images.sh
# Third edition art lives in its own frontend, under the same img/ prefix
if [ -d "$ROOT_DIR/frontend-3ed/public/img/ah3e" ]; then
  aws s3 sync "$ROOT_DIR/frontend-3ed/public/img/ah3e" s3://arkham-horror-assets/img/ah3e --acl public-read --exclude ".DS_Store"
fi
echo ""

echo "=== Step 2/2: Regenerating image manifest ==="
node "$ROOT_DIR/scripts/generate-manifest.cjs"
echo ""

echo "Done! Remember to commit the updated frontend/image-manifest.json and frontend-3ed/image-manifest.json"

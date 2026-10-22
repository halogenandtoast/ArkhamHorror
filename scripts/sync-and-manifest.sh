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
# Delegate to the Makefile so the bucket split (watermarked renditions to the
# public bucket, clean masters to the private originals bucket) has one
# definition rather than two that drift.
cd "$ROOT_DIR"
make sync-images
echo ""

echo "=== Step 2/2: Regenerating image manifest ==="
node "$ROOT_DIR/scripts/generate-manifest.cjs"
echo ""

echo "Done! Remember to commit the updated frontend/image-manifest.json and frontend-3ed/image-manifest.json"

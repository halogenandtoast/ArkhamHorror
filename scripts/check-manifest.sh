#!/usr/bin/env bash
#
# Pre-commit hook: checks if image-manifest.json is out of date.
#
# If image files under frontend/public/img/arkham/ have been added, removed,
# or renamed, but frontend/image-manifest.json hasn't been updated, this hook
# warns and blocks the commit.
#
# Install: make install-hooks
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# When running as a git hook, $0 is .git/hooks/pre-commit
# When running from scripts/, adjust accordingly
if [[ "$SCRIPT_DIR" == *".git/hooks"* ]]; then
  ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
else
  ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
fi

MANIFEST="$ROOT_DIR/frontend/image-manifest.json"
GENERATE_SCRIPT="$ROOT_DIR/scripts/generate-manifest.cjs"

# Only check if there are staged changes touching image directories
IMAGE_DIRS=(
  "frontend/public/img/arkham/cards"
  "frontend/public/img/arkham/boxes"
  "frontend/public/img/arkham/portraits"
  "frontend/public/img/arkham/tarot"
  "frontend/public/img/arkham/encounter-sets"
  "frontend/public/img/arkham/mini-cards"
  "frontend/public/img/arkham/sets"
  "frontend/public/img/arkham/customizations"
  "frontend/public/img/arkham/seals"
  "frontend/public/img/arkham/playing-cards"
  "frontend/public/img/arkham/es"
  "frontend/public/img/arkham/fr"
  "frontend/public/img/arkham/ita"
  "frontend/public/img/arkham/ko"
  "frontend/public/img/arkham/zh"
)

# Check if any staged files are under the image asset directories
has_image_changes=false
for dir in "${IMAGE_DIRS[@]}"; do
  if git diff --cached --name-only -- "$dir" 2>/dev/null | grep -q .; then
    has_image_changes=true
    break
  fi
done

# If no image files are staged, nothing to check
if [ "$has_image_changes" = false ]; then
  exit 0
fi

# Image files are staged — check if manifest is also staged
if ! git diff --cached --name-only -- "frontend/image-manifest.json" | grep -q .; then
  echo ""
  echo "⚠️  IMAGE MANIFEST OUT OF DATE"
  echo ""
  echo "You have staged changes to image files but frontend/image-manifest.json"
  echo "has not been updated."
  echo ""
  echo "Run the following to fix:"
  echo ""
  echo "  make sync-and-manifest"
  echo "  git add frontend/image-manifest.json"
  echo ""
  echo "Or to regenerate the manifest only (if images are already on S3):"
  echo ""
  echo "  make generate-manifest"
  echo "  git add frontend/image-manifest.json"
  echo ""
  echo "To skip this check: git commit --no-verify"
  echo ""
  exit 1
fi

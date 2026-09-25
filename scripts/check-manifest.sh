#!/usr/bin/env bash
#
# Pre-commit hook: checks if image-manifest.json is out of date.
#
# If image files under frontend/public/img/ (or frontend-3ed/public/img/) have
# been added, removed, or renamed, but that app's image-manifest.json hasn't been
# updated, this hook warns and blocks the commit.
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

# Each frontend has its own image directory and manifest: frontend/ and the
# third edition's frontend-3ed/.
stale=()
for app in frontend frontend-3ed; do
  # Any staged file under the image directory (catches all subdirectories,
  # including newly added ones). img/custom is dev-server-written custom card
  # art: local-only, never synced, and never in the manifest.
  if git diff --cached --name-only -- "$app/public/img/" ":(exclude)$app/public/img/custom/" 2>/dev/null | grep -q . \
    && ! git diff --cached --name-only -- "$app/image-manifest.json" | grep -q .; then
    stale+=("$app/image-manifest.json")
  fi
done

if [ "${#stale[@]}" -gt 0 ]; then
  echo ""
  echo "⚠️  IMAGE MANIFEST OUT OF DATE"
  echo ""
  echo "You have staged changes to image files but ${stale[*]}"
  echo "has not been updated."
  echo ""
  echo "Run the following to fix:"
  echo ""
  echo "  make sync-and-manifest"
  echo "  git add ${stale[*]}"
  echo ""
  echo "Or to regenerate the manifests only (if images are already on S3):"
  echo ""
  echo "  make generate-manifest"
  echo "  git add ${stale[*]}"
  echo ""
  echo "To skip this check: git commit --no-verify"
  echo ""
  exit 1
fi

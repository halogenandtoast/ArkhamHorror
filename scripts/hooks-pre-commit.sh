#!/usr/bin/env bash
#
# Installed as .git/hooks/pre-commit by `make install-hooks`. Runs each check in
# turn and fails on the first one that objects, so adding a check means adding a
# line here rather than replacing whichever hook was installed last.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [[ "$SCRIPT_DIR" == *".git/hooks"* ]]; then
  ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
else
  ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
fi

"$ROOT_DIR/scripts/check-manifest.sh"
"$ROOT_DIR/scripts/check-setup-sql.sh"

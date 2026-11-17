#!/usr/bin/env bash
#
# Pre-commit check: setup.sql is generated from migrations/, so a commit that
# changes the migration plan without regenerating it puts the two out of step.
#
# Deliberately cheap: it compares what is staged rather than replaying the
# migrations, because the real check needs a PostgreSQL server and has no place
# in a pre-commit hook. `make setup-sql-check` does the thorough version.
#
# Install: make install-hooks
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [[ "$SCRIPT_DIR" == *".git/hooks"* ]]; then
  ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
else
  ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
fi
cd "$ROOT_DIR"

# Only the plan and the deploy scripts feed setup.sql. A revert or verify script
# changes nothing about the schema a fresh install gets.
if git diff --cached --name-only -- migrations/sqitch.plan migrations/deploy/ 2>/dev/null | grep -q . \
  && ! git diff --cached --name-only -- setup.sql | grep -q .; then
  cat >&2 <<'MSG'

⚠️  setup.sql IS OUT OF DATE

You have staged migration changes but setup.sql has not been regenerated.
setup.sql is the schema every fresh install starts from -- the offline package,
a new Docker install, a new checkout -- and it is generated from migrations/.

Leaving it behind is what broke registration for offline users in November:
the shipped schema was 26 migrations old, so `users` had no
phase_transition_notifications column and every sign-up failed.

Run the following to fix:

  make setup-sql
  git add setup.sql

To skip this check: git commit --no-verify

MSG
  exit 1
fi

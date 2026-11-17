#!/usr/bin/env bash
#
# Regenerate setup.sql from migrations/.
#
# setup.sql used to be a pg_dump someone took by hand, which meant it drifted
# the moment a migration landed without a fresh dump. By August it was 26
# migrations behind, so every fresh install -- offline package included -- came
# up with a `users` table that had no `phase_transition_notifications` and died
# on the first registration.
#
# This replays migrations/deploy/*.sql in sqitch.plan order into a throwaway
# database, dumps the schema, and stamps the whole plan into
# arkham_schema_migrations so a database built from it knows it is current.
# The migration plan is the source of truth; setup.sql is derived.
#
# Usage:
#   scripts/gen-setup-sql.sh            # rewrite setup.sql
#   scripts/gen-setup-sql.sh --check    # exit 1 if setup.sql is out of date
#   scripts/gen-setup-sql.sh --docker   # force the docker path
#
# Needs PostgreSQL 14 client tools and a server, or docker.
#
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PLAN="$ROOT/migrations/sqitch.plan"
DEPLOY="$ROOT/migrations/deploy"
TARGET="$ROOT/setup.sql"
PG_MAJOR=14
IMAGE="postgres:${PG_MAJOR}-alpine"

MODE=write
FORCE_DOCKER=0
for arg in "$@"; do
  case "$arg" in
    --check)  MODE=check ;;
    --docker) FORCE_DOCKER=1 ;;
    -h|--help) sed -n '2,22p' "$0"; exit 0 ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

[ -f "$PLAN" ] || { echo "no plan at $PLAN" >&2; exit 1; }

migrations=$(grep -vE '^[[:space:]]*(%|#|$)' "$PLAN" | awk '{print $1}')
[ -n "$migrations" ] || { echo "empty plan" >&2; exit 1; }

for n in $(echo "$migrations"); do
  [ -f "$DEPLOY/$n.sql" ] || { echo "plan names '$n' but $DEPLOY/$n.sql is missing" >&2; exit 1; }
done

TMPDIR_RUN="$(mktemp -d)"
RAW="$TMPDIR_RUN/raw.sql"
OUT="$TMPDIR_RUN/setup.sql"

# ── how we get a database ───────────────────────────────────────────────────
# Local tools when they are the right major version, docker otherwise. Keep the
# server version fixed: pg_dump emits slightly different DDL across majors, and
# --check has to mean the same thing on every machine.
USE_DOCKER=1
if [ "$FORCE_DOCKER" = "0" ] \
  && command -v psql >/dev/null 2>&1 \
  && command -v pg_dump >/dev/null 2>&1 \
  && command -v createdb >/dev/null 2>&1 \
  && [ "$(psql --version | awk '{print $3}' | cut -d. -f1)" = "$PG_MAJOR" ] \
  && pg_isready -q 2>/dev/null; then
  USE_DOCKER=0
fi

if [ "$USE_DOCKER" = "1" ]; then
  command -v docker >/dev/null 2>&1 || {
    echo "Need PostgreSQL $PG_MAJOR client tools with a running server, or docker." >&2
    exit 1
  }
fi

CONTAINER=""
SCRATCH_DB=""
cleanup() {
  [ -n "$CONTAINER" ] && docker rm -f "$CONTAINER" >/dev/null 2>&1 || true
  [ -n "$SCRATCH_DB" ] && dropdb --if-exists "$SCRATCH_DB" >/dev/null 2>&1 || true
  rm -rf "$TMPDIR_RUN"
}
trap cleanup EXIT

if [ "$USE_DOCKER" = "0" ]; then
  SCRATCH_DB="arkham_setup_sql_$$"
  echo "==> replaying $(echo "$migrations" | wc -l | tr -d ' ') migrations into $SCRATCH_DB (local postgres)"
  dropdb --if-exists "$SCRATCH_DB" >/dev/null 2>&1 || true
  createdb "$SCRATCH_DB"
  while IFS= read -r n; do
    [ -n "$n" ] || continue
    psql -v ON_ERROR_STOP=1 -q -d "$SCRATCH_DB" -f "$DEPLOY/$n.sql" >/dev/null
  done <<< "$migrations"
  pg_dump --schema-only --no-owner --no-privileges -d "$SCRATCH_DB" > "$RAW"
else
  CONTAINER="arkham-setup-sql-$$"
  echo "==> replaying $(echo "$migrations" | wc -l | tr -d ' ') migrations in $IMAGE"
  docker run -d --name "$CONTAINER" \
    -e POSTGRES_PASSWORD=scratch -e POSTGRES_DB=scratch \
    -v "$ROOT/migrations:/migrations:ro" \
    "$IMAGE" >/dev/null
  # Probe the database, not the server: postgres:*-alpine starts a temporary
  # server while it runs its init scripts, so pg_isready answers yes well before
  # POSTGRES_DB has been created.
  ready=0
  for _ in $(seq 1 90); do
    if docker exec "$CONTAINER" psql -qtA -U postgres -d scratch -c 'SELECT 1' >/dev/null 2>&1; then
      ready=1; break
    fi
    sleep 1
  done
  [ "$ready" = "1" ] || {
    echo "postgres container never became ready" >&2
    docker logs --tail 20 "$CONTAINER" >&2 2>/dev/null || true
    exit 1
  }
  while IFS= read -r n; do
    [ -n "$n" ] || continue
    docker exec "$CONTAINER" psql -v ON_ERROR_STOP=1 -q -U postgres -d scratch \
      -f "/migrations/deploy/$n.sql" >/dev/null
  done <<< "$migrations"
  docker exec "$CONTAINER" pg_dump --schema-only --no-owner --no-privileges \
    -U postgres -d scratch > "$RAW"
fi

# ── assemble ────────────────────────────────────────────────────────────────
# Drop pg_dump's two version comments: they name the machine that ran this and
# would make --check disagree between a local run and CI for no real reason.
{
  cat <<'HEADER'
--
-- Arkham Horror LCG schema.
--
-- GENERATED FILE -- do not edit by hand.
-- Produced by scripts/gen-setup-sql.sh, which replays migrations/deploy/*.sql
-- in sqitch.plan order into a throwaway database and dumps the result. To
-- change the schema, add a migration and run `make setup-sql`.
--
HEADER
  # Normalise away everything that varies by who ran the dump, so --check
  # means the same thing on a developer's machine and in CI:
  #
  #   * the two version comments name the machine's pg_dump;
  #   * \restrict / \unrestrict carry a fresh random token on every run and are
  #     not understood by psql before 14.19 -- the offline package bundles
  #     14.15, so leaving them in would stop it importing this file at all;
  #   * runs of blank lines differ between pg_dump versions.
  grep -v -e '^-- Dumped from database version' \
          -e '^-- Dumped by pg_dump version' \
          -e '^-- PostgreSQL database dump$' \
          -e '^-- PostgreSQL database dump complete$' \
          -e '^\\restrict ' \
          -e '^\\unrestrict ' "$RAW" \
    | cat -s
  "$ROOT/scripts/gen-schema-baseline.sh"
} > "$OUT"

if [ "$MODE" = check ]; then
  if diff -q "$TARGET" "$OUT" >/dev/null 2>&1; then
    echo "setup.sql is up to date."
    exit 0
  fi
  echo ""
  echo "setup.sql is out of date with migrations/."
  echo ""
  diff -u "$TARGET" "$OUT" | head -60 || true
  echo ""
  echo "Run: make setup-sql"
  exit 1
fi

mv "$OUT" "$TARGET"
echo "==> wrote $TARGET ($(grep -c 'CREATE TABLE' "$TARGET") tables, $(echo "$migrations" | wc -l | tr -d ' ') migrations stamped)"

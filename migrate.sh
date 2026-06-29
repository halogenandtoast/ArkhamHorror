#!/bin/sh
#
# One-shot migration runner for the docker-compose `migrate` service.
# Applies pending SQL migrations, then exits 0 so `web` (which waits for this
# service to complete) can start. Safe to run on every `docker compose up` —
# already-applied migrations are skipped via the arkham_schema_migrations table.
#
# Runs in postgres:*-alpine (has psql, pg_isready, busybox awk/grep).
#
set -eu

PGHOST="${PGHOST:-db}"
PGUSER="${PGUSER:-arkham_pg_user}"
PGDATABASE="${PGDATABASE:-arkham-horror-backend}"
export PGHOST PGUSER PGDATABASE

# Last migration the shipped setup.sql contains. A DB with no tracking table is
# assumed initialised from setup.sql, so migrations after this are applied.
# Keep in sync with BASELINE_THROUGH in upgrade.sh.
BASELINE_THROUGH="add_step_constraint"
PLAN="/migrations/sqitch.plan"
DEPLOY="/migrations/deploy"

if [ -z "${PGPASSWORD:-}" ]; then
  if [ -n "${POSTGRES_PASSWORD:-}" ]; then
    PGPASSWORD="$POSTGRES_PASSWORD"
  elif [ -f /run/secrets/postgres_password ]; then
    PGPASSWORD="$(tr -d '\n\r' < /run/secrets/postgres_password)"
  fi
fi
export PGPASSWORD

echo "==> migrate: waiting for database at $PGHOST..."
i=0
until pg_isready -q; do
  i=$((i + 1)); [ "$i" -ge 60 ] && { echo "migrate: database never became ready" >&2; exit 1; }
  sleep 1
done

q() { psql -v ON_ERROR_STOP=1 -qtA "$@"; }

[ -f "$PLAN" ] || { echo "migrate: no plan at $PLAN" >&2; exit 1; }
names="$(grep -vE '^[[:space:]]*(%|#|$)' "$PLAN" | awk '{print $1}')"
[ -n "$names" ] || { echo "migrate: empty plan, nothing to do"; exit 0; }

existed="$(q -c "SELECT to_regclass('public.arkham_schema_migrations') IS NOT NULL")"
q -c "CREATE TABLE IF NOT EXISTS arkham_schema_migrations (
  name text PRIMARY KEY, applied_at timestamptz NOT NULL DEFAULT now());" >/dev/null

if [ "$existed" != "t" ] && [ "${ARKHAM_FORCE_MIGRATIONS:-0}" != "1" ]; then
  echo "==> migrate: no tracking table — seeding baseline through '$BASELINE_THROUGH'."
  vals=""
  for n in $names; do
    vals="${vals:+$vals,}('$n')"
    [ "$n" = "$BASELINE_THROUGH" ] && break
  done
  q -c "INSERT INTO arkham_schema_migrations(name) VALUES $vals ON CONFLICT DO NOTHING;" >/dev/null
fi

applied="$(q -c "SELECT string_agg(name, ' ') FROM arkham_schema_migrations")"
count=0
for n in $names; do
  case " $applied " in *" $n "*) continue ;; esac
  echo "==> migrate: applying $n"
  psql -v ON_ERROR_STOP=1 -q -f "$DEPLOY/$n.sql"
  q -c "INSERT INTO arkham_schema_migrations(name) VALUES ('$n') ON CONFLICT DO NOTHING;" >/dev/null
  count=$((count + 1))
done

if [ "$count" -eq 0 ]; then echo "==> migrate: schema already up to date."
else echo "==> migrate: applied $count migration(s)."; fi

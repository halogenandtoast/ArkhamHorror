#!/usr/bin/env bash
# db-unstick.sh -- find Postgres backends that are stuck idle-in-transaction
# while blocking other queries, and (optionally) terminate them.
#
# The pattern this catches:
#   - An app worker took a row lock (typically `lockGame` -> FOR UPDATE on
#     arkham_games), then dropped into long-running CPU work (e.g. runMessages
#     spinning on a poison game state). PG sees it as
#     state='idle in transaction', wait_event='ClientRead'.
#   - Other workers trying to load the same row queue behind it on a tuple
#     lock for minutes. Real users see hung pages.
#
# Reporting is the default. Pass --kill to actually terminate the holders.
# Each terminate rolls back the open transaction (no committed work lost --
# if it had committed, it wouldn't be `idle in transaction`).
#
# Usage:
#   scripts/db-unstick.sh                    # report only (default)
#   scripts/db-unstick.sh --kill             # report + terminate
#   scripts/db-unstick.sh --threshold 120    # only flag stuck >120s (default 60)
#   scripts/db-unstick.sh --min-blocked 0    # include holders that block nobody
#
# DB URL resolution (first match wins):
#   1. $DATABASE_URL
#   2. database_url in terraform/terraform.tfvars

set -euo pipefail

THRESHOLD=60
MIN_BLOCKED=1
KILL=0

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
  exit 0
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --kill)        KILL=1; shift ;;
    --threshold)   THRESHOLD="$2"; shift 2 ;;
    --min-blocked) MIN_BLOCKED="$2"; shift 2 ;;
    -h|--help)     usage ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

# Resolve connection string.
if [[ -z "${DATABASE_URL:-}" ]]; then
  here="$(cd "$(dirname "$0")/.." && pwd)"
  tfvars="$here/terraform/terraform.tfvars"
  if [[ -f "$tfvars" ]]; then
    DATABASE_URL=$(awk -F\" '/^database_url[[:space:]]*=/ {print $2; exit}' "$tfvars")
  fi
fi
if [[ -z "${DATABASE_URL:-}" ]]; then
  echo "ERROR: set DATABASE_URL or populate terraform/terraform.tfvars" >&2
  exit 1
fi
export DATABASE_URL

PSQL="psql $DATABASE_URL -X -P pager=off"

# Pull stuck candidates as TSV. One row per stuck holder; blocked_count is
# the number of other backends currently waiting on a lock this one holds.
read -r -d '' SQL <<SQL || true
SELECT
  blocking.pid,
  EXTRACT(EPOCH FROM (now() - blocking.xact_start))::int AS xact_age_s,
  COALESCE(host(blocking.client_addr), '-')             AS client,
  COALESCE(blocking.application_name, '-')              AS app,
  blocking.wait_event,
  count(DISTINCT blocked.pid)                           AS blocked_count,
  COALESCE(
    (SELECT (regexp_match(blocking.query, '[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}'))[1]),
    '-')                                                 AS suspected_uuid,
  regexp_replace(LEFT(blocking.query, 200), E'[\\n\\r]+', ' ', 'g') AS query
FROM pg_stat_activity blocking
LEFT JOIN pg_stat_activity blocked
  ON blocking.pid = ANY(pg_blocking_pids(blocked.pid))
WHERE blocking.pid <> pg_backend_pid()
  AND blocking.state = 'idle in transaction'
  AND now() - blocking.xact_start > make_interval(secs => ${THRESHOLD})
GROUP BY blocking.pid, blocking.xact_start, blocking.client_addr,
         blocking.application_name, blocking.wait_event, blocking.query
HAVING count(DISTINCT blocked.pid) >= ${MIN_BLOCKED}
ORDER BY blocking.xact_start;
SQL

tsv=$($PSQL -At -F $'\t' -c "$SQL")

if [[ -z "$tsv" ]]; then
  echo "No stuck backends matching: idle-in-transaction > ${THRESHOLD}s, blocking >= ${MIN_BLOCKED}."
  exit 0
fi

# Pretty-print findings.
printf '\n%s\n' "Stuck backends (threshold=${THRESHOLD}s, min_blocked=${MIN_BLOCKED}):"
printf -- '----------------------------------------------------------------------\n'
pids=()
while IFS=$'\t' read -r pid age client app wait blocked_count uuid query; do
  pids+=("$pid")
  printf '  pid:           %s\n' "$pid"
  printf '  xact age:      %ss\n' "$age"
  printf '  client:        %s  (app=%s)\n' "$client" "$app"
  printf '  wait_event:    %s\n' "$wait"
  printf '  blocking:      %s other queries\n' "$blocked_count"
  printf '  suspected uuid: %s\n' "$uuid"
  printf '  query:         %s\n' "$query"
  printf -- '----------------------------------------------------------------------\n'
done <<< "$tsv"

count=${#pids[@]}
echo "Found $count stuck backend(s)."

if [[ $KILL -eq 0 ]]; then
  echo
  echo "Re-run with --kill to terminate them."
  exit 0
fi

echo
echo "Terminating: ${pids[*]}"
# Build "pg_terminate_backend(pid)" calls; one row per pid in the report.
sql_terms=$(printf 'pg_terminate_backend(%s),' "${pids[@]}")
sql_terms=${sql_terms%,}
$PSQL -c "SELECT unnest(ARRAY[${pids[*]// /,}]) AS pid, unnest(ARRAY[$sql_terms]) AS terminated;"

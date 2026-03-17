#!/bin/sh
set -e

if [ -n "${PGPASSWORD}" ]; then
  POSTGRES_PASSWORD="${PGPASSWORD}"
elif [ -f /run/secrets/postgres_password ]; then
  POSTGRES_PASSWORD=$(tr -d '\n\r' < /run/secrets/postgres_password)
else
  echo "Postgres password secret not found!" >&2
  exit 1
fi

export PGUSER="${PGUSER:-arkham_pg_user}"
export PGHOST="${PGHOST:-db}"
export PGPORT="${PGPORT:-5432}"
export PGDATABASE="${PGDATABASE:-arkham-horror-backend}"
export PGPASSWORD="${POSTGRES_PASSWORD}"

if [ -z "${DATABASE_URL}" ]; then
  export DATABASE_URL="postgres://${PGUSER}:${PGPASSWORD}@${PGHOST}:${PGPORT}/${PGDATABASE}"
fi

unset POSTGRES_PASSWORD

exec "$@"

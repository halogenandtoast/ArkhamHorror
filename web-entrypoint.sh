#!/bin/sh
set -e

# Only construct DATABASE_URL from Docker secrets if not already provided
if [ -z "${DATABASE_URL}" ]; then
  if [ -n "${PGPASSWORD}" ]; then
    POSTGRES_PASSWORD="${PGPASSWORD}"
  elif [ -f /run/secrets/postgres_password ]; then
    POSTGRES_PASSWORD=$(tr -d '\n\r' < /run/secrets/postgres_password)
  else
    echo "Postgres password secret not found!" >&2
    exit 1
  fi

  POSTGRES_USER="${PGUSER:-arkham_pg_user}"
  POSTGRES_HOST="${PGHOST:-db}"
  POSTGRES_PORT="${PGPORT:-5432}"
  POSTGRES_DB="${PGDATABASE:-arkham-horror-backend}"

  export DATABASE_URL="postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@${POSTGRES_HOST}:${POSTGRES_PORT}/${POSTGRES_DB}"
  unset POSTGRES_PASSWORD
fi

exec "$@"

#!/bin/sh
set -e

# Read Postgres password from Docker secret
if [ -f /run/secrets/postgres_password ]; then
  export POSTGRES_PASSWORD=$(cat /run/secrets/postgres_password)
else
  echo "Postgres password secret not found!" >&2
  exit 1
fi

# Set fixed values for user, host, db
export POSTGRES_USER=arkham_pg_user
export POSTGRES_HOST=db
export POSTGRES_PORT=5432
export POSTGRES_DB=arkham-horror-backend

# Construct DATABASE_URL
export DATABASE_URL="postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@${POSTGRES_HOST}:${POSTGRES_PORT}/${POSTGRES_DB}"

# Optionally print for debug (remove in production)
echo "DATABASE_URL set for web container."

# Execute the main container command
exec "$@"

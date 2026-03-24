#!/bin/sh
set -e

# Auto-detect ASSET_HOST if not explicitly set.
# If the mounted image directory has content, serve images locally.
# Otherwise fall back to the CloudFront CDN.
# Override by setting ASSET_HOST explicitly in docker-compose.yml:
#   ASSET_HOST=                                  # force local
#   ASSET_HOST=https://assets.arkhamhorror.app   # force CDN
if [ -z "${ASSET_HOST+x}" ]; then
  img_dir="/opt/arkham/src/frontend/dist/img"
  if [ -n "$(ls -A "$img_dir" 2>/dev/null)" ]; then
    export ASSET_HOST=""
  else
    export ASSET_HOST="https://assets.arkhamhorror.app"
  fi
fi

# If the app already got a full DATABASE_URL, leave it alone.
if [ -z "${DATABASE_URL}" ]; then
  # Prefer env-injected password first (good for Kamal), then Docker secret file.
  if [ -n "${PGPASSWORD}" ]; then
    DB_PASSWORD="${PGPASSWORD}"
  elif [ -n "${POSTGRES_PASSWORD}" ]; then
    DB_PASSWORD="${POSTGRES_PASSWORD}"
  elif [ -f /run/secrets/postgres_password ]; then
    DB_PASSWORD=$(tr -d '\n\r' < /run/secrets/postgres_password)
  else
    echo "No database password found. Set DATABASE_URL, PGPASSWORD, POSTGRES_PASSWORD, or mount /run/secrets/postgres_password." >&2
    exit 1
  fi

  export PGUSER="${PGUSER:-arkham_pg_user}"
  export PGHOST="${PGHOST:-db}"
  export PGPORT="${PGPORT:-5432}"
  export PGDATABASE="${PGDATABASE:-arkham-horror-backend}"
  export PGPASSWORD="${DB_PASSWORD}"

  export DATABASE_URL="postgres://${PGUSER}:${PGPASSWORD}@${PGHOST}:${PGPORT}/${PGDATABASE}"

  unset DB_PASSWORD
fi

exec "$@"

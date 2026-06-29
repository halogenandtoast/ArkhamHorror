#!/usr/bin/env bash
#
# Arkham Horror LCG — in-place upgrader (companion to install.sh)
#
# Usage:
#   # Docker self-host (the install.sh layout) — from anywhere:
#   curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/upgrade.sh | bash
#   # ...or from inside the install dir / a git checkout:
#   ./upgrade.sh
#
# What this does (auto-detected per install type):
#   * git checkout   — git pull --ff-only, then migrate + (optional) image sync
#   * docker install — refresh control files, docker compose pull, migrate,
#                      recreate web, re-sync local images if present
#
# Migrations: there is no engine-level applied-set (runtime automigration is
# off and the sqitch registry has diverged), so this script keeps its own
# `arkham_schema_migrations` table. On the FIRST run against an existing DB it
# seeds that table with the current plan as "already applied" — a Docker DB is
# initialised from setup.sql, which is current as of its install, so replaying
# the (non-idempotent) history would only error. Set ARKHAM_FORCE_MIGRATIONS=1
# to apply every plan entry regardless (use only on a known-empty/old DB).
#
set -euo pipefail

REPO_RAW="https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main"
INSTALL_DIR="${ARKHAM_INSTALL_DIR:-arkham-horror}"
PG_USER="arkham_pg_user"
PG_DB="arkham-horror-backend"
# Last migration the shipped setup.sql contains. A DB with no tracking table is
# assumed initialised from setup.sql, so migrations after this are applied.
BASELINE_THROUGH="add_step_constraint"

info() { printf '\033[0;32m==> %s\033[0m\n' "$*"; }
warn() { printf '\033[0;33mWARN: %s\033[0m\n' "$*" >&2; }
die()  { printf '\033[0;31mERROR: %s\033[0m\n' "$*" >&2; exit 1; }

# ── Detect install type ─────────────────────────────────────────────────────

MODE=""
if git rev-parse --show-toplevel >/dev/null 2>&1; then
  cd "$(git rev-parse --show-toplevel)"
  MODE="git"
elif [ -f docker-compose.yml ]; then
  MODE="docker"
elif [ -f "$INSTALL_DIR/docker-compose.yml" ]; then
  cd "$INSTALL_DIR"
  MODE="docker"
else
  die "Nothing to upgrade here. Run from a git checkout or the install dir (or set ARKHAM_INSTALL_DIR)."
fi
info "Detected install type: $MODE  (in $(pwd))"

# ── Migration runner ────────────────────────────────────────────────────────
#
# Two independent choices, decoupled so a git checkout running on Docker
# doesn't need DATABASE_URL:
#   1. Where the plan/deploy SQL comes from  — disk (git) or curl (docker).
#   2. How we reach the database             — DATABASE_URL psql, else the
#                                               compose `db` service via exec.
#
# db_psql <args...>    : run psql, args passed through
# db_query <sql>       : run sql, return single trimmed value
# db_apply_file <path> : run a .sql file with ON_ERROR_STOP

# (1) Migration file source.
if [ "$MODE" = "docker" ]; then
  PLAN_FILE="$(mktemp)"
  curl -fsSL "$REPO_RAW/migrations/sqitch.plan" -o "$PLAN_FILE"
  # In docker mode the deploy scripts aren't on disk; fetch each on demand.
  get_deploy() { local name="$1" out; out="$(mktemp)"; curl -fsSL "$REPO_RAW/migrations/deploy/$name.sql" -o "$out" && echo "$out"; }
else
  PLAN_FILE="migrations/sqitch.plan"
  get_deploy() { echo "migrations/deploy/$1.sql"; }
fi

# (2) DB transport. Prefer an explicit DATABASE_URL; otherwise, if a compose
# `db` service is defined, run psql inside it — no DATABASE_URL finagling.
compose_has_db() {
  command -v docker >/dev/null 2>&1 \
    && docker compose config --services 2>/dev/null | grep -qx db
}
if [ -n "${DATABASE_URL:-}" ] && command -v psql >/dev/null 2>&1; then
  info "Migrations via DATABASE_URL."
  db_psql()       { psql "$DATABASE_URL" "$@"; }
  db_apply_file() { psql "$DATABASE_URL" -v ON_ERROR_STOP=1 -f "$1"; }
elif compose_has_db; then
  info "Migrations via docker compose 'db' service."
  docker compose up -d db >/dev/null
  for _ in $(seq 1 30); do
    docker compose exec -T db pg_isready -U "$PG_USER" -d "$PG_DB" >/dev/null 2>&1 && break
    sleep 1
  done
  docker compose exec -T db pg_isready -U "$PG_USER" -d "$PG_DB" >/dev/null 2>&1 \
    || die "Database did not become ready."
  db_psql()       { docker compose exec -T db psql -U "$PG_USER" -d "$PG_DB" "$@"; }
  db_apply_file() { docker compose exec -T db psql -U "$PG_USER" -d "$PG_DB" -v ON_ERROR_STOP=1 < "$1"; }
else
  warn "No DATABASE_URL and no docker compose 'db' service — skipping migrations."
  SKIP_MIGRATIONS=1
  db_psql()       { return 0; }
  db_apply_file() { return 0; }
fi
db_query() { db_psql -tA -c "$1" | tr -d '[:space:]'; }

run_migrations() {
  [ "${SKIP_MIGRATIONS:-0}" = "1" ] && return 0

  # Plan order, ignoring pragmas/blank lines; change name is the first token.
  local plan_names
  plan_names="$(grep -vE '^\s*(%|#|$)' "$PLAN_FILE" | awk '{print $1}')"
  [ -n "$plan_names" ] || { warn "Empty migration plan; nothing to do."; return 0; }

  local existed
  existed="$(db_query "SELECT to_regclass('public.arkham_schema_migrations') IS NOT NULL")"
  db_psql -q -c "CREATE TABLE IF NOT EXISTS arkham_schema_migrations (
    name text PRIMARY KEY, applied_at timestamptz NOT NULL DEFAULT now());" >/dev/null

  if [ "$existed" != "t" ] && [ "${ARKHAM_FORCE_MIGRATIONS:-0}" != "1" ]; then
    # No tracking table → a legacy DB initialised from setup.sql, which contains
    # the schema through $BASELINE_THROUGH but not arkham_epic. Seed everything
    # up to and including that as already-applied; the apply loop below then runs
    # arkham_epic and anything newer. Keep $BASELINE_THROUGH in step with the
    # last migration setup.sql actually contains.
    # ponytail: override with ARKHAM_FORCE_MIGRATIONS=1 to apply the whole plan
    # (only safe on a genuinely empty DB — historical migrations aren't idempotent).
    info "No migration table — seeding baseline through '$BASELINE_THROUGH', will apply newer migrations."
    local n
    while IFS= read -r n; do
      [ -n "$n" ] || continue
      db_psql -q -c "INSERT INTO arkham_schema_migrations(name) VALUES ('$n') ON CONFLICT DO NOTHING;" >/dev/null
      [ "$n" = "$BASELINE_THROUGH" ] && break
    done <<< "$plan_names"
  fi

  local applied count=0 name file
  applied="$(db_query "SELECT string_agg(name, ' ') FROM arkham_schema_migrations" || true)"
  while IFS= read -r name; do
    [ -n "$name" ] || continue
    case " $applied " in *" $name "*) continue ;; esac
    info "Applying migration: $name"
    file="$(get_deploy "$name")" || die "Could not fetch migration $name."
    db_apply_file "$file" || die "Migration $name failed — aborting (no record written)."
    db_psql -q -c "INSERT INTO arkham_schema_migrations(name) VALUES ('$name') ON CONFLICT DO NOTHING;" >/dev/null
    count=$((count + 1))
  done <<< "$plan_names"

  if [ "$count" -eq 0 ]; then info "Database schema already up to date."
  else info "Applied $count migration(s)."; fi
}

# ── Image re-sync (only if local images are already present) ────────────────

resync_images() {
  local img_dir="frontend/public/img"
  [ -n "$(ls -A "$img_dir" 2>/dev/null)" ] || { info "No local images — serving from CDN, nothing to sync."; return 0; }

  # Pick a fetch target matching what's already on disk, override with env.
  local target="${ARKHAM_IMAGE_TARGET:-}"
  if [ -z "$target" ]; then
    local langs=() l
    for l in es fr ita ko zh; do [ -d "$img_dir/arkham/$l" ] && langs+=("$l"); done
    case "${#langs[@]}" in
      0) target="en" ;;
      1) target="en+${langs[0]}" ;;
      *) target="all" ;;   # multiple translations on disk → only "all" covers them
    esac
  fi

  info "Re-syncing local images (target: $target)..."
  if [ "$MODE" = "docker" ]; then
    docker compose --profile fetch-images run --rm fetch-images "$target"
  elif [ -x scripts/fetch-assets.sh ] && command -v aws >/dev/null 2>&1; then
    ./scripts/fetch-assets.sh "$target"
  else
    warn "Local images present but no aws CLI — run: make fetch-images-docker (or scripts/fetch-assets.sh $target)"
    return 0
  fi
}

# ── Drive the upgrade ───────────────────────────────────────────────────────

if [ "$MODE" = "git" ]; then
  info "Pulling latest source (git pull --ff-only)..."
  git pull --ff-only || die "git pull failed (uncommitted changes or diverged branch). Resolve, then re-run."
  run_migrations
  resync_images
  echo ""
  info "Source updated. Rebuild to pick up changes:"
  echo "  backend:  cd backend && make api.watch        # (you build; engine errors report back)"
  echo "  frontend: cd frontend && npm install && npm run build"
  echo "  or, if you run via Docker: docker compose build && docker compose up -d"
else
  command -v docker >/dev/null 2>&1 || die "Docker is not installed."
  docker info >/dev/null 2>&1      || die "Docker daemon is not running."

  info "Refreshing control files from GitHub..."
  [ -f docker-compose.yml ] && cp docker-compose.yml docker-compose.yml.bak
  curl -fsSL "$REPO_RAW/docker-compose.yml" -o docker-compose.yml
  curl -fsSL "$REPO_RAW/setup.sql"          -o setup.sql
  mkdir -p scripts
  curl -fsSL "$REPO_RAW/scripts/fetch-assets.sh" -o scripts/fetch-assets.sh
  chmod +x scripts/fetch-assets.sh
  if [ -f docker-compose.yml.bak ] && ! diff -q docker-compose.yml docker-compose.yml.bak >/dev/null 2>&1; then
    warn "docker-compose.yml changed — your previous version is saved as docker-compose.yml.bak (re-apply any local env edits)."
  fi

  info "Pulling latest images..."
  docker compose pull

  run_migrations

  info "Recreating containers..."
  docker compose up -d

  resync_images
  [ -n "$(ls -A frontend/public/img 2>/dev/null)" ] && docker compose restart web

  echo ""
  info "Upgrade complete — http://localhost:3000"
fi

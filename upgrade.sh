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
#   * git checkout   — git pull --ff-only, migrate, (optional) image sync
#   * docker install — refresh control files, docker compose pull, migrate,
#                      recreate web, re-sync local images if present
#
# Migrations are applied by the compose `migrate` one-shot service (see
# migrate.sh / docker-compose.yml), so `docker compose up` self-migrates too;
# this script just invokes it so the upgrade is deterministic.
#
set -euo pipefail

REPO_RAW="https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main"
INSTALL_DIR="${ARKHAM_INSTALL_DIR:-arkham-horror}"

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

# ── Migrations: delegate to the compose one-shot `migrate` service ──────────

apply_migrations() {
  if command -v docker >/dev/null 2>&1 && docker compose config --services 2>/dev/null | grep -qx migrate; then
    info "Applying migrations (compose migrate service)..."
    docker compose run --rm migrate
  else
    warn "No docker compose 'migrate' service here — apply migrations manually (sqitch/psql)."
  fi
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
  fi
}

# ── Drive the upgrade ───────────────────────────────────────────────────────

if [ "$MODE" = "git" ]; then
  info "Pulling latest source (git pull --ff-only)..."
  git pull --ff-only || die "git pull failed (uncommitted changes or diverged branch). Resolve, then re-run."
  apply_migrations
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
  curl -fsSL "$REPO_RAW/migrate.sh"         -o migrate.sh
  mkdir -p scripts migrations/deploy
  curl -fsSL "$REPO_RAW/scripts/fetch-assets.sh" -o scripts/fetch-assets.sh
  chmod +x scripts/fetch-assets.sh

  info "Refreshing migrations..."
  curl -fsSL "$REPO_RAW/migrations/sqitch.plan" -o migrations/sqitch.plan
  grep -vE '^[[:space:]]*(%|#|$)' migrations/sqitch.plan | awk '{print $1}' | while read -r m; do
    curl -fsSL "$REPO_RAW/migrations/deploy/$m.sql" -o "migrations/deploy/$m.sql"
  done

  if [ -f docker-compose.yml.bak ] && ! diff -q docker-compose.yml docker-compose.yml.bak >/dev/null 2>&1; then
    warn "docker-compose.yml changed — your previous version is saved as docker-compose.yml.bak (re-apply any local env edits)."
  fi

  info "Pulling latest images..."
  docker compose pull

  apply_migrations

  info "Recreating containers..."
  docker compose up -d

  # img is a live volume mount, so re-synced files are served without a restart.
  resync_images

  echo ""
  info "Upgrade complete — http://localhost:3000"
fi

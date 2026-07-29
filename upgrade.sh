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
# This script keeps itself up to date: a docker install re-fetches upgrade.sh
# and re-execs if it changed, and a git checkout re-execs if the pull rewrote
# it. Set ARKHAM_SKIP_SELF_UPDATE=1 to disable.
#
set -euo pipefail

REPO_RAW="https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main"
INSTALL_DIR="${ARKHAM_INSTALL_DIR:-arkham-horror}"

info() { printf '\033[0;32m==> %s\033[0m\n' "$*"; }
warn() { printf '\033[0;33mWARN: %s\033[0m\n' "$*" >&2; }
die()  { printf '\033[0;31mERROR: %s\033[0m\n' "$*" >&2; exit 1; }

# ── Locate ourselves (before any cd, so a relative $0 still resolves) ────────
#
# Empty when piped from curl (`curl … | bash`), which is also the case where we
# already are the newest version and self-update is a no-op.
SELF="${BASH_SOURCE[0]:-}"
if [ -n "$SELF" ] && [ -f "$SELF" ]; then
  SELF="$(cd "$(dirname "$SELF")" && pwd)/$(basename "$SELF")"
else
  SELF=""
fi

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

# ── Self-update ─────────────────────────────────────────────────────────────
#
# install.sh drops a copy of this script into the install dir, so a docker
# install can be running an arbitrarily old upgrade.sh. Re-fetch it, and if it
# differs, swap it in and re-exec so the rest of the upgrade runs on new logic.
#
# Skipped when piped from curl (already newest) and in git checkouts (the file
# is git-managed there — `git pull` owns it, see the post-pull re-exec below).
# Set ARKHAM_SKIP_SELF_UPDATE=1 to opt out; ARKHAM_UPGRADE_REEXEC guards against
# an update loop if the fetched copy never compares equal.

reexec_self() {
  export ARKHAM_UPGRADE_REEXEC=1
  exec bash "$SELF" "$@"
}

self_update() {
  [ -n "$SELF" ]                  || return 0
  command -v curl >/dev/null 2>&1 || return 0
  if [ "${ARKHAM_SKIP_SELF_UPDATE:-0}" = "1" ] || [ "${ARKHAM_UPGRADE_REEXEC:-0}" = "1" ]; then
    return 0
  fi

  info "Checking for a newer upgrade.sh..."

  # Staged alongside $SELF so the swap is an atomic same-filesystem rename: the
  # running bash keeps reading the old inode, which is what makes this safe to
  # do to a script that is currently executing.
  local staged
  staged="$(mktemp "$SELF.XXXXXX" 2>/dev/null)" || return 0

  if ! curl -fsSL "$REPO_RAW/upgrade.sh" -o "$staged" || [ ! -s "$staged" ]; then
    rm -f "$staged"
    warn "Couldn't fetch the latest upgrade.sh — continuing with this copy."
    return 0
  fi

  # Guard against a truncated/HTML response replacing a working script.
  if ! head -n1 "$staged" | grep -q '^#!.*sh'; then
    rm -f "$staged"
    warn "Fetched upgrade.sh doesn't look like a shell script — continuing with this copy."
    return 0
  fi

  if cmp -s "$staged" "$SELF"; then
    rm -f "$staged"
    return 0
  fi

  chmod +x "$staged"
  mv -f "$staged" "$SELF" || { rm -f "$staged"; warn "Couldn't replace $SELF — continuing with this copy."; return 0; }
  info "Updated upgrade.sh — restarting with the new version..."
  reexec_self "$@"
}

if [ "$MODE" = "docker" ]; then
  self_update "$@"
fi

# ── Postgres password secret ────────────────────────────────────────────────
#
# docker-compose.yml mounts config/postgres_password.txt as a secret, which in
# non-swarm compose is a plain bind mount. When the source path is missing,
# dockerd materialises it — as an empty DIRECTORY — and Postgres then starts
# with no usable password. install.sh generates the file, but a git checkout
# never has it (gitignored) and neither does a hand-made install dir, so
# upgrade.sh has to guarantee it before touching compose.

ensure_password_file() {
  local f=config/postgres_password.txt
  mkdir -p config
  if [ -d "$f" ]; then
    rmdir "$f" 2>/dev/null || die "$f is a non-empty directory (Docker created it) — remove it and re-run."
    warn "$f existed as a directory (Docker created it for the missing secret) — removed."
  fi
  [ -s "$f" ] && return 0
  info "Generating Postgres password ($f)..."
  # hex, not base64: +/= break DATABASE_URL
  if command -v openssl >/dev/null 2>&1; then
    openssl rand -hex 32 > "$f"
  else
    head -c 32 /dev/urandom | od -An -tx1 | tr -d ' \n' > "$f"
    echo "" >> "$f"
  fi
  warn "If the db volume was already initialised with a different password, reset it: docker compose down -v"
}

if command -v docker >/dev/null 2>&1; then ensure_password_file; fi

# ── Migrations: delegate to the compose one-shot `migrate` service ──────────

apply_migrations() {
  if command -v docker >/dev/null 2>&1 && docker compose config --services 2>/dev/null | grep -qx migrate; then
    info "Applying migrations (compose migrate service)..."
    # -T + </dev/null: this script is usually run via `curl … | bash`, where stdin
    # is the pipe — `compose run` would otherwise fail with "the input device is
    # not a TTY" (and could swallow the rest of the piped script).
    docker compose run --rm -T migrate </dev/null
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
    docker compose --profile fetch-images run --rm -T fetch-images "$target" </dev/null
  elif [ -x scripts/fetch-assets.sh ] && command -v aws >/dev/null 2>&1; then
    ./scripts/fetch-assets.sh "$target"
  else
    warn "Local images present but no aws CLI — run: make fetch-images-docker (or scripts/fetch-assets.sh $target)"
  fi
}

# ── Drive the upgrade ───────────────────────────────────────────────────────

if [ "$MODE" = "git" ]; then
  info "Pulling latest source (git pull --ff-only)..."
  self_before=""
  [ -n "$SELF" ] && self_before="$(cksum < "$SELF")"
  git pull --ff-only || die "git pull failed (uncommitted changes or diverged branch). Resolve, then re-run."

  # The pull may have rewritten this very script. bash reads a script from disk
  # incrementally, so continuing here can execute garbage — re-exec instead.
  if [ -n "$SELF" ] && [ -f "$SELF" ] && [ "${ARKHAM_UPGRADE_REEXEC:-0}" != "1" ] \
     && [ "$(cksum < "$SELF")" != "$self_before" ]; then
    info "upgrade.sh changed in that pull — restarting with the new version..."
    reexec_self "$@"
  fi

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

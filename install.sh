#!/usr/bin/env bash
#
# Arkham Horror LCG — one-shot installer
#
# Usage (no git required):
#   curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/install.sh | bash
#
# What this does:
#   1. Creates an arkham-horror/ directory
#   2. Downloads docker-compose.yml and setup.sql from GitHub
#   3. Generates a random Postgres password
#   4. Optionally fetches all game images (~2.9 GB) via Docker
#   5. Starts the app with docker compose up -d
#
set -euo pipefail

REPO_RAW="https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main"
INSTALL_DIR="${ARKHAM_INSTALL_DIR:-arkham-horror}"

# ── Helpers ──────────────────────────────────────────────────────────────────

info()    { printf '\033[0;32m==> %s\033[0m\n' "$*"; }
warn()    { printf '\033[0;33mWARN: %s\033[0m\n' "$*" >&2; }
die()     { printf '\033[0;31mERROR: %s\033[0m\n' "$*" >&2; exit 1; }

command -v docker >/dev/null 2>&1 || die "Docker is not installed. Get it from https://www.docker.com/"
docker info >/dev/null 2>&1      || die "Docker daemon is not running. Please start Docker and try again."

# Detect whether we are running interactively (e.g. piped from curl)
is_interactive() {
  [ -t 0 ] && [ -t 1 ]
}

# ── Setup directory ───────────────────────────────────────────────────────────

info "Creating install directory: $INSTALL_DIR"
mkdir -p "$INSTALL_DIR/config" "$INSTALL_DIR/frontend/public/img"
cd "$INSTALL_DIR"

# ── Download required files ───────────────────────────────────────────────────

info "Downloading docker-compose.yml"
curl -fsSL "$REPO_RAW/docker-compose.yml" -o docker-compose.yml

info "Downloading setup.sql"
curl -fsSL "$REPO_RAW/setup.sql" -o setup.sql

# ── Generate Postgres password ────────────────────────────────────────────────

if [ ! -f config/postgres_password.txt ]; then
  info "Generating Postgres password"
  # Use openssl if available, otherwise fall back to /dev/urandom
  if command -v openssl >/dev/null 2>&1; then
    openssl rand -base64 32 | tr -d '\n' > config/postgres_password.txt
  else
    head -c 32 /dev/urandom | base64 | tr -d '\n/+=' | head -c 32 > config/postgres_password.txt
  fi
  echo "" >> config/postgres_password.txt
  info "Password saved to config/postgres_password.txt"
else
  info "config/postgres_password.txt already exists, skipping"
fi

# ── Optionally fetch images ───────────────────────────────────────────────────

fetch_images=false

if is_interactive; then
  echo ""
  echo "Game images are ~2.9 GB and fetched from S3 via Docker."
  printf "Download images now? (y/N) "
  read -r answer </dev/tty
  case "$answer" in
    [Yy]*) fetch_images=true ;;
  esac
else
  warn "Non-interactive mode detected (curl | bash). Skipping image download."
  warn "Run the following after installation to fetch images:"
  warn "  cd $INSTALL_DIR && docker compose --profile fetch-images run --rm fetch-images"
fi

if [ "$fetch_images" = true ]; then
  info "Fetching images (this may take a while)..."
  docker compose --profile fetch-images run --rm fetch-images
fi

# ── Start the app ─────────────────────────────────────────────────────────────

info "Starting Arkham Horror LCG..."
docker compose up -d

echo ""
echo "Installation complete!"
echo ""
echo "  App:  http://localhost:3000"
echo ""
echo "To stop:  cd $INSTALL_DIR && docker compose down"
echo "To update: cd $INSTALL_DIR && docker compose pull && docker compose up -d"
echo ""
if [ "$fetch_images" = false ]; then
  echo "To fetch images later:"
  echo "  cd $INSTALL_DIR && docker compose --profile fetch-images run --rm fetch-images"
  echo ""
fi

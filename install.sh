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

info "Downloading fetch-assets.sh"
mkdir -p scripts
curl -fsSL "$REPO_RAW/scripts/fetch-assets.sh" -o scripts/fetch-assets.sh
chmod +x scripts/fetch-assets.sh

# ── Generate Postgres password ────────────────────────────────────────────────

if [ ! -f config/postgres_password.txt ]; then
  info "Generating Postgres password"
  # Use openssl if available, otherwise fall back to /dev/urandom
  # Use hex to guarantee URL-safe characters (base64 produces +/= which break DATABASE_URL)
  if command -v openssl >/dev/null 2>&1; then
    openssl rand -hex 32 > config/postgres_password.txt
  else
    head -c 32 /dev/urandom | od -An -tx1 | tr -d ' \n' > config/postgres_password.txt
    echo "" >> config/postgres_password.txt
  fi
  info "Password saved to config/postgres_password.txt"
else
  info "config/postgres_password.txt already exists, skipping"
fi

# ── Optionally fetch images ───────────────────────────────────────────────────

fetch_target=""

if is_interactive; then
  echo ""
  echo "Images are served from the CDN by default — local download is optional."
  echo ""
  echo "  en       English only (~1.3 GB)  ← recommended"
  echo "  en+fr    English + French"
  echo "  en+es    English + Spanish"
  echo "  en+ita   English + Italian"
  echo "  en+ko    English + Korean"
  echo "  en+zh    English + Chinese"
  echo "  all      Everything (~2.9 GB)"
  echo ""
  printf "Download images now? Enter a target above, or press Enter to skip: "
  read -r fetch_target </dev/tty
else
  warn "Non-interactive mode (curl | bash): skipping image download."
  warn "Images will load from the CDN automatically until you fetch them locally."
  warn "To fetch English images and switch to local serving:"
  warn "  cd $INSTALL_DIR && docker compose --profile fetch-images run --rm fetch-images && docker compose restart web"
  warn "To fetch English + a language (e.g. French):"
  warn "  cd $INSTALL_DIR && docker compose --profile fetch-images run --rm fetch-images en+fr && docker compose restart web"
fi

if [ -n "$fetch_target" ]; then
  info "Fetching images: $fetch_target (this may take a while)..."
  docker compose --profile fetch-images run --rm fetch-images "$fetch_target"
fi

# ── Start the app ─────────────────────────────────────────────────────────────

info "Starting Arkham Horror LCG..."
docker compose up -d

echo ""
echo "Installation complete!"
echo ""
echo "  App:     http://localhost:3000"
echo ""
echo "  Stop:    cd $INSTALL_DIR && docker compose down"
echo "  Restart: cd $INSTALL_DIR && docker compose restart web"
echo "  Update:  cd $INSTALL_DIR && docker compose pull && docker compose up -d"
echo ""
if [ -z "$fetch_target" ]; then
  echo "Images are currently loading from the CDN."
  echo ""
  echo "To fetch images locally (English only, ~1.3 GB) and switch to local serving:"
  echo "  cd $INSTALL_DIR && docker compose --profile fetch-images run --rm fetch-images && docker compose restart web"
  echo ""
  echo "To include a language (e.g. French):"
  echo "  cd $INSTALL_DIR && docker compose --profile fetch-images run --rm fetch-images en+fr && docker compose restart web"
  echo ""
else
  echo "Run 'docker compose restart web' any time to pick up newly fetched images."
  echo ""
fi

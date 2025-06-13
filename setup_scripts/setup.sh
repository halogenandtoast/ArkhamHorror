#!/usr/bin/env bash

# Setup script for Arkham Horror backend using Stack + Docker
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# 1. System deps
echo "➤ Ensuring system packages are in place..."
sudo apt-get update
sudo apt-get install -y libgmp-dev apt-transport-https ca-certificates curl gnupg lsb-release

# 2. Docker install (if needed)
if ! command -v docker >/dev/null 2>&1; then
  echo "➤ Installing Docker..."
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
  echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] \
    https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
  sudo apt-get update
  sudo apt-get install -y docker-ce docker-ce-cli containerd.io
fi

# 3. Optional: add current user to docker group (no sudo needed later)
if ! groups "$USER" | grep -q docker; then
  echo "➤ Adding user '$USER' to docker group..."
  sudo usermod -aG docker "$USER"
fi

# 4. Install Stack if missing
if ! command -v stack >/dev/null 2>&1; then
  echo "➤ Installing Haskell Stack..."
  curl -sSL https://get.haskellstack.org/ | sh
fi

# 5. Build backend
cd "$REPO_ROOT/backend"
echo "➤ Running stack setup & build..."
stack setup
stack build --fast

echo "🎉 All set! Run your services with Docker:"
echo "    cd $REPO_ROOT && docker compose up"

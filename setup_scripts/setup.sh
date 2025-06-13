#!/usr/bin/env bash

# Setup script for the Arkham Horror backend using Stack.
# This follows the instructions in the project README.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Install Stack if it's not already available
if ! command -v stack >/dev/null 2>&1; then
  echo "Stack not found, installing..."
  curl -sSL https://get.haskellstack.org/ | sh
fi

cd "$REPO_ROOT/backend"

# Download the correct GHC toolchain and build dependencies
stack setup
stack build --fast

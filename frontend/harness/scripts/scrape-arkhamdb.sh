#!/usr/bin/env bash
# Scrape the top-liked ArkhamDB decks and cache them in harness/fixtures/decks/.
# Usage: ./harness/scripts/scrape-arkhamdb.sh [page-url]
set -euo pipefail

URL="${1:-https://arkhamdb.com/decklists?sort=likes}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DECKS="$HERE/../fixtures/decks"
mkdir -p "$DECKS"

ids=$(curl -fsS "$URL" --max-time 15 | grep -oE 'href="/decklist/view/[0-9]+/' | grep -oE '[0-9]+' | sort -un)
n=0
fail=0
for id in $ids; do
  if curl -fsS "https://arkhamdb.com/api/public/decklist/${id}" --max-time 10 -o "$DECKS/${id}.json"; then
    n=$((n+1))
  else
    echo "WARN: failed to fetch $id" >&2
    fail=$((fail+1))
  fi
done
echo "fetched $n decks into $DECKS ($fail failures)"

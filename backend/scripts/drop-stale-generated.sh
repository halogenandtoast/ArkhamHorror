#!/bin/sh
#
# Drop arkham-api build artifacts for the modules `cards-discover` generates,
# whenever the generator itself has changed.
#
# Why this exists:
#
# A module with
#
#   {-# OPTIONS_GHC -F -pgmF cards-discover ... #-}
#
# is a one-line pragma on disk; its real contents come out of cards-discover at
# compile time. GHC's recompilation check hashes the *un-preprocessed* source
# plus the import list it parses back out of the preprocessor's output -- the
# generator is not an input it knows about. So when cards-discover starts
# emitting something new (a new export, a new declaration, a different shape),
# every one of those modules looks unchanged and keeps the interface it was last
# compiled with. The first importer that mentions the new name then fails:
#
#   Module 'Arkham.Homebrew.UltimatumEntries' does not export 'DiscoveredModules'
#
# ...on a cache GHC is certain is up to date. Only dropping the stale artifacts
# fixes it; touching the sources does not, since Stack and GHC compare content
# hashes rather than mtimes.
#
# We therefore keep a stamp of the generator's own sources beside the cache. When
# it moves, the generated modules' artifacts go. Their dependents are left to
# GHC: the regenerated interfaces either hash the same (no cascade) or differ
# (normal cascade), which is exactly what the missing dependency edge would have
# given us.
#
# Set DRY_RUN=1 to print what would be removed.
#
# Usage: drop-stale-generated.sh <arkham-api-dir> <cards-discover-dir>
set -eu

API=${1:?arkham-api package directory}
DISCOVER=${2:?cards-discover package directory}

WORK="$API/.stack-work"
STAMP="$WORK/.cards-discover-hash"

# Ubuntu has sha256sum, macOS only shasum.
if command -v sha256sum >/dev/null 2>&1; then
  checksum="sha256sum"
else
  checksum="shasum -a 256"
fi

# Only the generator's hand-written sources: .stack-work below it holds build
# products whose contents are not stable between builds.
hash=$(
  find "$DISCOVER/app" "$DISCOVER/library" -type f -name '*.hs' \
    | LC_ALL=C sort \
    | xargs cat \
    | $checksum \
    | cut -d' ' -f1
)

previous=$(cat "$STAMP" 2>/dev/null || true)

if [ "$hash" = "$previous" ]; then
  exit 0
fi

# An empty stamp also drops: either the cache predates this script, or it was
# written by a generator we cannot identify. Both are the stale case.
modules=$(
  find "$API/library" -type f -name '*.hs' -exec grep -lF 'pgmF cards-discover' {} + \
    | sed -e "s|^$API/library/||" -e 's|\.hs$||'
)

echo ">> cards-discover changed -- dropping stale interfaces for $(printf '%s\n' "$modules" | grep -c .) generated module(s)"

for build in "$WORK"/dist/*/ghc-*/build; do
  [ -d "$build" ] || continue
  for mod in $modules; do
    for ext in o hi dyn_o dyn_hi p_o p_hi; do
      artifact="$build/$mod.$ext"
      [ -f "$artifact" ] || continue
      if [ "${DRY_RUN:-}" = 1 ]; then
        echo "   would remove $artifact"
      else
        rm -f "$artifact"
      fi
    done
  done
done

if [ "${DRY_RUN:-}" != 1 ]; then
  mkdir -p "$WORK"
  printf '%s\n' "$hash" > "$STAMP"
fi

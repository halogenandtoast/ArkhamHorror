#!/bin/sh
#
# Build arkham-api inside the Docker `api` stage, healing a poisoned build cache
# first.
#
# Why this exists:
#
# .stack-work is a BuildKit `--mount=type=cache`, so it is mutable state that
# outlives the build that wrote it -- including builds that never finished. A
# multi-platform build cancels every other platform the moment one of them
# fails, which kills GHC mid-write and leaves valid-looking .hi files next to
# truncated or zero-byte .o files.
#
# GHC's recompilation check only stats those files, so it never notices, never
# rebuilds them, and every later build dies at link time with
#
#   undefined reference to `ZCMain_main_closure'
#
# Once that happens the cache is wedged permanently: the failure cancels the
# other platform, which poisons its cache too, and the deploy can never
# succeed again on its own.
#
# The fix is a stamp file. We drop it only after a build finishes. If it is
# missing when we start, the previous build died part-way and we repair the
# cache before building. On the happy path this costs one stat and one touch,
# so the cache keeps doing its job.
set -eu

WORK=/opt/arkham/src/backend/arkham-api/.stack-work
STAMP="$WORK/.build-complete"

if [ -d "$WORK" ] && [ ! -f "$STAMP" ]; then
  echo ">> stack cache was left dirty by an unfinished build -- repairing"

  # Truncated artifacts a killed compiler leaves behind. Removing them only
  # costs a recompile of the modules they belong to.
  find "$WORK" -type f \
    \( -name '*.o' -o -name '*.dyn_o' -o -name '*.hi' -o -name '*.dyn_hi' \
       -o -name '*.a' -o -name '*.so' \) \
    -size 0 -delete 2>/dev/null || true

  # The executables are the usual casualty: they link last, so a cancel lands
  # on them, and they are cheap to rebuild. Library objects live beside these
  # directories (build/Arkham/..., build/libHSarkham-api-*) and are left alone.
  rm -rf "$WORK"/dist/*/ghc-*/build/arkham-* 2>/dev/null || true
fi

# Mark the cache dirty for the duration of the build.
rm -f "$STAMP"

stack build --no-terminal --system-ghc \
  --ghc-options '-rtsopts -with-rtsopts=-V0 -j4 +RTS -V0 -A128m -n2m -RTS'
stack --no-terminal --local-bin-path /opt/arkham/bin install

mkdir -p "$WORK"
touch "$STAMP"

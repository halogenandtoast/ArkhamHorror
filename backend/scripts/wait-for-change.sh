#!/usr/bin/env bash
#
# Block until a .hs file under the given trees is modified or added, then exit 0.
# Ctrl-C exits non-zero so the caller's loop can stop.
#
# This replaces `entr -d` in the watch loops. entr opens a descriptor per watched
# file plus one per containing directory, and macOS caps a process at OPEN_MAX
# (10240) however high the rlimit says; this tree reached 8858 modules over 1545
# directories and entr now refuses to start:
#
#   entr: Too many files listed; the hard limit for your login class is 10240.
#
# Feeding entr directories instead (-d accepts them explicitly) does not work:
# a kqueue watch on a directory fires on added/removed entries, not on an edit
# to a file already in it, so in-place saves were missed.
#
# ponytail: polls instead of watching. One find over the trees is ~30ms, so a
# 1s tick costs a few percent of a core and scales with the file count rather
# than hitting a wall. Deletions are not detected (nothing ends up newer than
# the stamp); the next edit picks them up.
set -uo pipefail

[ $# -gt 0 ] || { echo "usage: wait-for-change.sh DIR [DIR...]" >&2; exit 1; }

stamp=$(mktemp)
trap 'rm -f "$stamp"' EXIT

while :; do
  sleep 1
  if [ -n "$(find "$@" -name '*.hs' -newer "$stamp" -print -quit 2>/dev/null)" ]; then
    exit 0
  fi
done

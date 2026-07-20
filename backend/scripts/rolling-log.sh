#!/usr/bin/env bash
# Tee stdin to stdout and to a size-capped rolling log file.
# Usage: some-command 2>&1 | rolling-log.sh [logfile] [max-bytes]
# When the log exceeds max-bytes, it is trimmed to the newest half, so the
# file always holds the most recent output (build errors stay near the end).

LOG="${1:-../.claude/build.log}"
MAX="${2:-2000000}"

mkdir -p "$(dirname "$LOG")"
: > "$LOG"

n=0
while IFS= read -r line; do
  printf '%s\n' "$line"
  printf '%s\n' "$line" >> "$LOG"
  n=$((n + 1))
  if [ $((n % 200)) -eq 0 ] && [ "$(wc -c < "$LOG")" -gt "$MAX" ]; then
    tail -c $((MAX / 2)) "$LOG" > "$LOG.tmp" && mv "$LOG.tmp" "$LOG"
  fi
done

#!/usr/bin/env bash
#
# Downloads image assets using S3 for listing and CloudFront for delivery.
# Listing via S3 (no credentials required — public bucket).
# Download via CloudFront for faster edge delivery.
#
# Differential sync: skips files that already exist with the correct size.
# New/changed files are downloaded to a .tmp file, size-verified, then
# atomically renamed into place — interrupted downloads are always caught.
# Failed downloads are retried with exponential backoff.
#
# Usage:
#   ./scripts/fetch-assets.sh en        # All English/static images (~1.3 GB)
#   ./scripts/fetch-assets.sh en+fr     # English/static + French translations
#   ./scripts/fetch-assets.sh cards     # English card images only (~755 MB)
#   ./scripts/fetch-assets.sh all       # Everything (~2.9 GB)
#
# Environment variables:
#   FETCH_S3_BUCKET   S3 bucket name for listing (default: arkham-horror-assets)
#   FETCH_CDN_BASE    CloudFront base URL (default: https://assets.arkhamhorror.app)
#   FETCH_PARALLEL    Concurrent downloads (default: 8)
#   FETCH_RETRIES     Retries per file on failure (default: 3)
#
set -euo pipefail

S3_BUCKET="${FETCH_S3_BUCKET:-arkham-horror-assets}"
CDN_BASE="${FETCH_CDN_BASE:-https://assets.arkhamhorror.app}"
PARALLEL="${FETCH_PARALLEL:-8}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PUBLIC_DIR="$ROOT_DIR/frontend/public"

# ── Terminal ──────────────────────────────────────────────────────────────────

_tty=false; [ -t 1 ] && _tty=true

# Hardcoded ANSI codes — no tput/TERM dependency, works in Docker and CI alike
if $_tty; then
  _RED=$'\033[31m'; _GREEN=$'\033[32m'; _CYAN=$'\033[36m'
  _BOLD=$'\033[1m'; _DIM=$'\033[2m';   _RESET=$'\033[0m'
  _HIDE=$'\033[?25l'; _SHOW=$'\033[?25h'
else
  _RED='' _GREEN='' _CYAN='' _BOLD='' _DIM='' _RESET='' _HIDE='' _SHOW=''
fi

# Restore cursor on exit/interrupt
trap 'printf "%s" "${_SHOW}" 2>/dev/null; exit' INT TERM

_fmt_time() {
  local s=$1
  if   [ "$s" -ge 3600 ]; then printf '%dh %dm'  $((s/3600)) $(( (s%3600)/60 ))
  elif [ "$s" -ge    60 ]; then printf '%dm %ds'  $((s/60))   $((s%60))
  else                          printf '%ds'       "$s"
  fi
}

_bar() {
  local done=$1 total=$2 width=$3
  local filled=$(( total > 0 ? done * width / total : 0 ))
  local empty=$(( width - filled ))
  local i f='' e=''
  for ((i=0; i<filled; i++)); do f="${f}█"; done
  for ((i=0; i<empty;  i++)); do e="${e}░"; done
  printf '%s%s%s%s%s' "$_CYAN" "$f" "$_DIM" "$e" "$_RESET"
}

_SPIN=('⠋' '⠙' '⠹' '⠸' '⠼' '⠴' '⠦' '⠧' '⠇' '⠏')

# ── Helpers ───────────────────────────────────────────────────────────────────

die() { printf '\n%sError: %s%s\n' "$_RED" "$*" "$_RESET" >&2; exit 1; }

usage() {
  cat <<EOF
Usage: $(basename "$0") <target>

Targets:
  cards       English card images only (~755 MB)
  en          All English/static images (~1.3 GB)
  en+fr       English/static + French translations
  en+es       English/static + Spanish translations
  en+ita      English/static + Italian translations
  en+ko       English/static + Korean translations
  en+zh       English/static + Chinese translations
  fr          French translated card images only
  es          Spanish translated card images only
  ita         Italian translated card images only
  ko          Korean translated card images only
  zh          Chinese translated card images only
  all         Everything (~2.9 GB)

Env vars:
  FETCH_S3_BUCKET=...         S3 bucket for listing (default: arkham-horror-assets)
  FETCH_CDN_BASE=https://...  CloudFront base URL (default: https://assets.arkhamhorror.app)
  FETCH_PARALLEL=N            Concurrent downloads (default: 8)
  FETCH_RETRIES=N             Retries per file on failure (default: 3)
EOF
  exit 1
}

_file_size() { stat -c%s "$1" 2>/dev/null || stat -f%z "$1"; }
export -f _file_size

# Called in parallel by xargs. Args: <expected_size> <key>
_fetch_one() {
  local expected_size="$1" key="$2"
  local dest="$PUBLIC_DIR/$key" tmp="$PUBLIC_DIR/$key.tmp"
  local retries="${FETCH_RETRIES:-3}"

  # Skip if the file already exists with the correct size
  if [ -f "$dest" ] && [ "$(_file_size "$dest")" = "$expected_size" ]; then
    printf 'k' >> "$FETCH_PROGRESS_DIR/skip"
    return 0
  fi

  mkdir -p "$(dirname "$dest")"

  local attempt=1
  while [ "$attempt" -le "$retries" ]; do
    if curl -fsSL --retry 0 "$CDN_BASE/$key" -o "$tmp" 2>/dev/null; then
      local actual; actual=$(_file_size "$tmp")
      if [ "$actual" = "$expected_size" ]; then
        mv "$tmp" "$dest"
        printf 'o' >> "$FETCH_PROGRESS_DIR/ok"
        return 0
      fi
      printf 'size mismatch (attempt %d/%d): %s — expected %s got %s\n' \
        "$attempt" "$retries" "$key" "$expected_size" "$actual" \
        >> "$FETCH_PROGRESS_DIR/errors"
    else
      printf 'download failed (attempt %d/%d): %s\n' \
        "$attempt" "$retries" "$key" \
        >> "$FETCH_PROGRESS_DIR/errors"
    fi
    rm -f "$tmp"
    attempt=$((attempt + 1))
    [ "$attempt" -le "$retries" ] && sleep $((attempt * 2))
  done

  printf 'x' >> "$FETCH_PROGRESS_DIR/fail"
  return 1
}
export -f _fetch_one
export PUBLIC_DIR CDN_BASE

_list_objects() {
  aws s3api list-objects-v2 \
    --bucket "$S3_BUCKET" \
    --prefix "$1" \
    --no-sign-request \
    --output text \
    --query 'Contents[].[Size,Key]' \
    | grep -v '^None$' || true
}

_sync_prefix() {
  local prefix="$1"; shift
  local filter_args=("$@")

  # ── Listing phase ─────────────────────────────────────────────────────────

  local listing_tmp; listing_tmp=$(mktemp)

  if $_tty; then
    printf '%s' "$_HIDE"
    _list_objects "$prefix" > "$listing_tmp" &
    local list_pid=$! i=0
    while kill -0 "$list_pid" 2>/dev/null; do
      printf '\r  %s  Listing %s...' "${_SPIN[$((i % 10))]}" "$prefix"
      i=$((i+1)); sleep 0.1
    done
    wait "$list_pid"
  else
    printf 'Listing %s...' "$prefix"
    _list_objects "$prefix" > "$listing_tmp"
  fi

  local listing; listing=$(cat "$listing_tmp"); rm -f "$listing_tmp"

  if [ ${#filter_args[@]} -gt 0 ]; then
    listing=$(printf '%s\n' "$listing" | grep "${filter_args[@]}" || true)
  fi

  local total; total=$(printf '%s\n' "$listing" | grep -c . || true)

  if $_tty; then
    printf '\r\033[K  %s✓%s  Found %s%d%s files\n' \
      "$_GREEN" "$_RESET" "$_BOLD" "$total" "$_RESET"
  else
    printf ' %d files\n' "$total"
  fi

  [ "$total" -eq 0 ] && return 0

  # ── Download phase ────────────────────────────────────────────────────────

  local progress_dir; progress_dir=$(mktemp -d)
  touch "$progress_dir/ok" "$progress_dir/skip" "$progress_dir/fail" "$progress_dir/errors"
  export FETCH_PROGRESS_DIR="$progress_dir"

  local start; start=$(date +%s)
  local monitor_pid=''

  if $_tty; then
    # Background monitor: redraws progress line every 100ms
    (
      trap - INT TERM  # don't inherit parent's signal trap
      local i=0
      while true; do
        local ok skip fail completed now elapsed pct rate eta rem
        ok=$(wc -c   < "$progress_dir/ok"   2>/dev/null | tr -d ' \t'); ok=${ok:-0}
        skip=$(wc -c < "$progress_dir/skip" 2>/dev/null | tr -d ' \t'); skip=${skip:-0}
        fail=$(wc -c < "$progress_dir/fail" 2>/dev/null | tr -d ' \t'); fail=${fail:-0}
        completed=$(( ok + skip + fail ))
        now=$(date +%s); elapsed=$(( now - start ))
        pct=$(( total > 0 ? completed * 100 / total : 0 ))

        if [ "$elapsed" -gt 0 ] && [ "$completed" -gt 0 ]; then
          rate=$(( completed / elapsed ))
          rem=$(( total - completed ))
          eta=$(( rate > 0 ? rem / rate : 0 ))
        else
          rate=0; eta=0
        fi

        printf '\r  %s  [' "${_SPIN[$((i % 10))]}"
        _bar "$completed" "$total" 28
        printf ']  %s%3d%%%s  %d/%d' "$_BOLD" "$pct" "$_RESET" "$completed" "$total"
        [ "$ok"   -gt 0 ] && printf '  %s↓ %d%s'  "$_GREEN" "$ok"   "$_RESET"
        [ "$skip" -gt 0 ] && printf '  %s↷ %d%s'  "$_DIM"   "$skip" "$_RESET"
        [ "$fail" -gt 0 ] && printf '  %s✗ %d%s'  "$_RED"   "$fail" "$_RESET"
        printf '  %d/s  ETA %s  ' "$rate" "$(_fmt_time "$eta")"

        i=$((i+1)); sleep 0.1
      done
    ) &
    monitor_pid=$!
  fi

  local xargs_rc=0
  printf '%s\n' "$listing" \
    | xargs -P "$PARALLEL" -n 2 bash -c '_fetch_one "$@"' _ \
    || xargs_rc=$?

  # ── Summary ───────────────────────────────────────────────────────────────

  if [ -n "$monitor_pid" ]; then
    kill "$monitor_pid" 2>/dev/null
    wait "$monitor_pid" 2>/dev/null || true
  fi

  local ok skip fail end_t elapsed
  ok=$(wc -c   < "$progress_dir/ok"   | tr -d ' \t'); ok=${ok:-0}
  skip=$(wc -c < "$progress_dir/skip" | tr -d ' \t'); skip=${skip:-0}
  fail=$(wc -c < "$progress_dir/fail" | tr -d ' \t'); fail=${fail:-0}
  end_t=$(date +%s); elapsed=$(( end_t - start ))

  if $_tty; then
    # Draw one final bar at the true completed count before clearing
    local completed=$(( ok + skip + fail ))
    printf '\r  ✓  ['
    _bar "$completed" "$total" 28
    printf ']  %s100%%%s  %d/%d' "$_BOLD" "$_RESET" "$completed" "$total"
    [ "$ok"   -gt 0 ] && printf '  %s↓ %d%s' "$_GREEN" "$ok"   "$_RESET"
    [ "$skip" -gt 0 ] && printf '  %s↷ %d%s' "$_DIM"   "$skip" "$_RESET"
    [ "$fail" -gt 0 ] && printf '  %s✗ %d%s' "$_RED"   "$fail" "$_RESET"
    printf '\n%s' "$_SHOW"
  fi

  if [ -s "$progress_dir/errors" ]; then
    printf '\n%sErrors:%s\n' "$_RED" "$_RESET" >&2
    sort -u "$progress_dir/errors" | while IFS= read -r line; do
      printf '  %s\n' "$line" >&2
    done
  fi

  printf '\n'
  [ "$ok"   -gt 0 ] && printf '  %s↓%s  %d downloaded\n'        "$_GREEN" "$_RESET" "$ok"
  [ "$skip" -gt 0 ] && printf '  %s↷%s  %d already up to date\n' "$_DIM"   "$_RESET" "$skip"
  [ "$fail" -gt 0 ] && printf '  %s✗%s  %d failed\n'             "$_RED"   "$_RESET" "$fail"
  printf '  %s⏱%s  %s\n\n' "$_DIM" "$_RESET" "$(_fmt_time "$elapsed")"

  rm -rf "$progress_dir"

  [ "$fail" -eq 0 ] && [ "$xargs_rc" -eq 0 ]
}

# ── Main ──────────────────────────────────────────────────────────────────────

command -v aws  >/dev/null 2>&1 || die "aws CLI not found. Install from https://aws.amazon.com/cli/ or use 'make fetch-images-docker'."
command -v curl >/dev/null 2>&1 || die "curl not found."

[ $# -ge 1 ] || usage

ALL_LANGS=(es fr ita ko zh)

_other_langs_pattern() {
  local keep="$1" lang others=()
  for lang in "${ALL_LANGS[@]}"; do
    [ "$lang" != "$keep" ] && others+=("$lang")
  done
  local joined; printf -v joined '%s|' "${others[@]}"
  printf 'img/arkham/(%s)/' "${joined%|}"
}

ALL_LANG_PATTERN="img/arkham/($(IFS='|'; printf '%s' "${ALL_LANGS[*]}")/)"

printf '\n%s=== %s ===%s\n\n' "$_BOLD" \
  "$(case "$1" in
    cards)             echo 'Fetching English card images' ;;
    en)                echo 'Fetching all English/static images' ;;
    en+*)              echo "Fetching English/static + ${1#en+} translations" ;;
    fr|es|ita|ko|zh)   echo "Fetching $1 translated images only" ;;
    all)               echo 'Fetching all images' ;;
  esac)" "$_RESET"

case "$1" in
  cards)
    _sync_prefix "img/arkham/cards/"
    ;;
  en)
    _sync_prefix "img/" -vE "$ALL_LANG_PATTERN"
    ;;
  en+fr|en+es|en+ita|en+ko|en+zh)
    _sync_prefix "img/" -vE "$(_other_langs_pattern "${1#en+}")"
    ;;
  fr|es|ita|ko|zh)
    _sync_prefix "img/arkham/$1/"
    ;;
  all)
    _sync_prefix "img/"
    ;;
  *)
    usage
    ;;
esac

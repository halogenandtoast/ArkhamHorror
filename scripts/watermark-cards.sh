#!/usr/bin/env bash
#
# Builds the public card renditions -- resolution-capped and watermarked -- from
# the clean masters under frontend/public/img/arkham.
#
# The masters are never modified. This writes a derived tree that `make
# sync-images` uploads to the public bucket, while the masters go to the private
# originals bucket. Only `cards/` and `<lang>/cards/` are processed: homebrew art
# (img/arkham/homebrew/) and custom card art (img/custom/) are never touched.
#
# Usage:
#   ./scripts/watermark-cards.sh              # all languages, incremental
#   ./scripts/watermark-cards.sh en           # English only
#   ./scripts/watermark-cards.sh --force      # rebuild even when up to date
#   WM_OPACITY=0.24 ./scripts/watermark-cards.sh en --force   # try a stronger mark
#
# Environment variables:
#   WM_FILE     Watermark art, transparent PNG or SVG (default: frontend/watermark/watermark.png)
#   WM_MODE     center | tile  (default: center)
#   WM_SCALE    Mark width as a fraction of the card's short edge (default: 0.62)
#   WM_OPACITY  0..1, multiplied into the art's own alpha (default: 0.16)
#   CAP         Max short edge in px; larger cards are downscaled (default: 940)
#   QUALITY     AVIF quality (default: 50 -- size parity with the masters)
#   JOBS        Parallel magick processes (default: 6)
#   OUT_DIR     Derived tree (default: frontend/.watermarked/img/arkham)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SRC_DIR="${SRC_DIR:-$ROOT_DIR/frontend/public/img/arkham}"

WM_FILE="${WM_FILE:-$ROOT_DIR/frontend/watermark/watermark.png}"
WM_MODE="${WM_MODE:-center}"
WM_SCALE="${WM_SCALE:-0.62}"
WM_OPACITY="${WM_OPACITY:-0.28}"
CAP="${CAP:-940}"
QUALITY="${QUALITY:-50}"
JOBS="${JOBS:-6}"
OUT_DIR="${OUT_DIR:-$ROOT_DIR/frontend/.watermarked/img/arkham}"

# ko and rus are listed so they are picked up if they are ever populated.
LANGS=(es fr ita ko rus zh)

FORCE=0
WHICH="all"
for arg in "$@"; do
  case "$arg" in
    --force) FORCE=1 ;;
    en) WHICH="en" ;;
    all) WHICH="all" ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

command -v magick >/dev/null || { echo "ImageMagick (magick) is required" >&2; exit 1; }
[ -f "$WM_FILE" ] || { echo "watermark art not found: $WM_FILE" >&2; exit 1; }
[ -d "$SRC_DIR/cards" ] || { echo "no card masters at $SRC_DIR/cards" >&2; exit 1; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# ── The per-card filter ───────────────────────────────────────────────────────
# One magick process per card: resize (only when over the cap), build the
# watermark layer sized to that card, composite, encode.
cat > "$WORK/filter.sh" <<'FILTER'
#!/usr/bin/env bash
set -euo pipefail

src="$1"
rel="${src#"$SRC_DIR"/}"
out="$OUT_DIR/$rel"

# Incremental: skip when the rendition is newer than both its source and the
# watermark art, so changing the watermark invalidates every card.
if [ "$FORCE" -eq 0 ] && [ -f "$out" ] && [ "$out" -nt "$src" ] && [ "$out" -nt "$WM_FILE" ]; then
  exit 0
fi

mkdir -p "$(dirname "$out")"

# `identify` emits no trailing newline, which would make `read` return non-zero
# and trip `set -e`; the here-string supplies one. head -1 guards multi-frame files.
dims=$(magick identify -format "%w %h\n" "$src" 2>/dev/null | head -1)
read -r w h <<< "$dims"
[ -n "${w:-}" ] && [ -n "${h:-}" ] || { echo "unreadable: $src" >&2; exit 1; }

# The short edge is the card's 2.5in dimension in either orientation.
if [ "$w" -lt "$h" ]; then short=$w; else short=$h; fi

resize_args=()
short_after=$short
if [ "$short" -gt "$CAP" ]; then
  # Scale so the SHORT edge lands on CAP, whatever the orientation.
  if [ "$w" -lt "$h" ]; then resize_args=(-resize "${CAP}x"); else resize_args=(-resize "x${CAP}"); fi
  short_after=$CAP
fi

# Size the mark off the card's short edge so every card carries the same visual
# weight regardless of resolution or orientation.
mark_w=$(awk -v s="$short_after" -v f="$WM_SCALE" 'BEGIN{ printf "%d", (s*f < 16 ? 16 : s*f) }')

# Cache the scaled mark per width. There are only a handful of distinct sizes
# across the whole set, so this renders a few times rather than 37,000.
tile="$WORK/mark-${mark_w}-${WM_MODE}.png"
if [ ! -f "$tile" ]; then
  tmp="$tile.$$"
  if [ "$WM_MODE" = "tile" ]; then
    # Pad the mark into a square cell and rotate it so tiling lays it out
    # diagonally. Rotation is done on PNG, never on AVIF output, where
    # ImageMagick silently drops it.
    cell=$(( mark_w * 3 / 2 ))
    magick -background none "$WM_FILE" -resize "${mark_w}x" \
      -background none -gravity center -extent "${cell}x${cell}" \
      -background none -rotate -30 \
      -channel A -evaluate multiply "$WM_OPACITY" +channel "$tmp"
  else
    magick -background none "$WM_FILE" -resize "${mark_w}x" \
      -channel A -evaluate multiply "$WM_OPACITY" +channel "$tmp"
  fi
  mv -n "$tmp" "$tile" 2>/dev/null || rm -f "$tmp"
fi

if [ "$WM_MODE" = "tile" ]; then
  # +clone takes the card's exact post-resize geometry, so one recipe covers
  # every dimension and both orientations.
  magick "$src" "${resize_args[@]}" \
    \( +clone -alpha transparent -background none -tile "$tile" -draw "color 0,0 reset" \) \
    -compose over -composite -quality "$QUALITY" "$out"
else
  magick "$src" "${resize_args[@]}" \
    "$tile" -gravity center -compose over -composite \
    -quality "$QUALITY" "$out"
fi
FILTER
chmod +x "$WORK/filter.sh"

export SRC_DIR OUT_DIR WM_FILE WM_MODE WM_SCALE WM_OPACITY CAP QUALITY WORK FORCE

# ── Gather the work ───────────────────────────────────────────────────────────
dirs=("$SRC_DIR/cards")
if [ "$WHICH" = "all" ]; then
  for l in "${LANGS[@]}"; do
    [ -d "$SRC_DIR/$l/cards" ] && dirs+=("$SRC_DIR/$l/cards")
  done
fi

# The per-card skip test only looks at mtimes, so a changed setting would
# otherwise leave every existing rendition in place and silently do nothing.
# Stamp the settings and force a full rebuild whenever they differ.
stamp_file="$OUT_DIR/.build-settings"
stamp="mode=$WM_MODE scale=$WM_SCALE opacity=$WM_OPACITY cap=$CAP quality=$QUALITY"
if [ "$FORCE" -eq 0 ] && [ -f "$stamp_file" ] && [ "$(cat "$stamp_file")" != "$stamp" ]; then
  echo "settings changed since the last build -- rebuilding every card"
  echo "  was: $(cat "$stamp_file")"
  echo "  now: $stamp"
  echo
  FORCE=1
fi

list="$WORK/list.txt"
: > "$list"
for d in "${dirs[@]}"; do
  # Only .avif is served; the stray .jpg/.png under some card dirs are stale
  # artifacts. `back_*` is the face-down card back and must never carry a mark.
  find "$d" -type f -iname '*.avif' ! -iname 'back_*' -print >> "$list"
done
total=$(wc -l < "$list" | tr -d ' ')

echo "watermark : $WM_FILE"
echo "            mode=$WM_MODE scale=$WM_SCALE opacity=$WM_OPACITY"
echo "cap       : ${CAP}px short edge     quality: $QUALITY"
echo "output    : $OUT_DIR"
echo "cards     : $total across ${#dirs[@]} director$([ ${#dirs[@]} -eq 1 ] && echo y || echo ies)"
echo

start=$(date +%s)
xargs -P "$JOBS" -n 1 "$WORK/filter.sh" < "$list"
end=$(date +%s)

mkdir -p "$OUT_DIR"
printf '%s' "$stamp" > "$stamp_file"

built=$(find "$OUT_DIR" -type f ! -name '.*' | wc -l | tr -d ' ')
echo
echo "done in $((end - start))s -- $built renditions in $OUT_DIR"

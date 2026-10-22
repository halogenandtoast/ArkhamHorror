#!/usr/bin/env bash
#
# Reverses the watermark composite: given a public rendition, reconstructs the
# image that was underneath the mark.
#
# The mark is alpha-composited, so it inverts algebraically. For mark art W with
# per-pixel alpha a:
#
#   forward:  C' = C(1-a) + W*a
#   inverse:  C  = (C' - W*a) / (1-a)
#
# Peak alpha is 0.28, so 1-a never falls below 0.72 and the forward map never
# clips -- which makes the inverse exact up to the AVIF quantization. Measured
# against the masters it lands on the re-encode noise floor; the numbers are in
# docs/card-watermarking.md.
#
# This is committed on purpose. The watermark is a provenance signal and a
# deterrent against scraping the bucket, not a protection -- anyone can derive
# this in an afternoon, and acting otherwise only means the next person here
# re-derives it. What it genuinely cannot undo is the 940px cap: resolution the
# rendition never carried is gone.
#
# The settings MUST match the ones the rendition was built with, which is why the
# defaults are the same as scripts/watermark-cards.sh. A mismatch does not fail;
# it leaves a ghost of the mark behind.
#
# Usage:
#   ./scripts/unwatermark-cards.sh in.avif out.avif
#   ./scripts/unwatermark-cards.sh -r IN_DIR OUT_DIR
#   EXT=png ./scripts/unwatermark-cards.sh -r IN_DIR OUT_DIR   # no second lossy pass
#
# Environment variables:
#   WM_FILE     Watermark art used to build the renditions (default: frontend/watermark/watermark.png)
#   WM_MODE     center | tile  (default: center)
#   WM_SCALE    Mark width as a fraction of the short edge (default: 0.62)
#   WM_OPACITY  0..1, as multiplied into the art's alpha (default: 0.28)
#   QUALITY     Re-encode quality for lossy output formats (default: 50)
#   JOBS        Parallel magick processes (default: 6)
#   EXT         Override the output extension in -r mode (default: keep the input's)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

WM_FILE="${WM_FILE:-$ROOT_DIR/frontend/watermark/watermark.png}"
WM_MODE="${WM_MODE:-center}"
WM_SCALE="${WM_SCALE:-0.62}"
WM_OPACITY="${WM_OPACITY:-0.28}"
QUALITY="${QUALITY:-50}"
JOBS="${JOBS:-6}"
EXT="${EXT:-}"

RECURSIVE=0
if [ "${1:-}" = "-r" ] || [ "${1:-}" = "--recursive" ]; then
  RECURSIVE=1
  shift
fi

usage() {
  # Line-range-free so the header can be edited without silently breaking this.
  sed -n '/^# Usage:/,/^#$/p' "$0" | sed 's/^# \{0,1\}//'
  exit 2
}

[ $# -eq 2 ] || usage

IN="$1"
OUT="$2"

command -v magick >/dev/null || { echo "ImageMagick (magick) is required" >&2; exit 1; }
[ -f "$WM_FILE" ] || { echo "watermark art not found: $WM_FILE" >&2; exit 1; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# ── The per-file inverse ──────────────────────────────────────────────────────
cat > "$WORK/filter.sh" <<'FILTER'
#!/usr/bin/env bash
set -euo pipefail

src="$1"
out="$2"

mkdir -p "$(dirname "$out")"

# `identify` emits no trailing newline, which would make `read` return non-zero
# and trip `set -e`; the here-string supplies one. head -1 guards multi-frame files.
dims=$(magick identify -format "%w %h\n" "$src" 2>/dev/null | head -1)
read -r w h <<< "$dims"
[ -n "${w:-}" ] && [ -n "${h:-}" ] || { echo "unreadable: $src" >&2; exit 1; }

# The forward pass sized the mark off the card's short edge AFTER the resolution
# cap, and that is exactly the rendition's own short edge -- so reading it here
# reproduces the same mark width without knowing what CAP was.
if [ "$w" -lt "$h" ]; then short=$w; else short=$h; fi
mark_w=$(awk -v s="$short" -v f="$WM_SCALE" 'BEGIN{ printf "%d", (s*f < 16 ? 16 : s*f) }')

# Two card-sized maps are all the inverse needs: the mark premultiplied by its
# own alpha (W*a, over black) and one-minus-alpha. Both depend only on the
# geometry, so they are cached -- there are a handful of distinct card sizes.
key="${mark_w}-${w}x${h}-${WM_MODE}"
premul="$WORK/premul-$key.png"
den="$WORK/den-$key.png"
if [ ! -f "$premul" ] || [ ! -f "$den" ]; then
  layer="$WORK/layer-$key.$$.png"
  if [ "$WM_MODE" = "tile" ]; then
    cell=$(( mark_w * 3 / 2 ))
    # Rotation on PNG, never on AVIF output, where ImageMagick drops it silently.
    magick -background none "$WM_FILE" -resize "${mark_w}x" \
      -background none -gravity center -extent "${cell}x${cell}" \
      -background none -rotate -30 \
      -channel A -evaluate multiply "$WM_OPACITY" +channel "$layer.cell.png"
    magick -size "${w}x${h}" xc:none -tile "$layer.cell.png" \
      -draw "color 0,0 reset" "$layer"
    rm -f "$layer.cell.png"
  else
    magick -background none "$WM_FILE" -resize "${mark_w}x" \
      -channel A -evaluate multiply "$WM_OPACITY" +channel \
      -background none -gravity center -extent "${w}x${h}" "$layer"
  fi
  magick "$layer" \
    \( +clone -alpha extract -negate -write "$den.$$.png" +delete \) \
    -background black -alpha remove -alpha off "$premul.$$.png"
  mv -n "$den.$$.png" "$den" 2>/dev/null || rm -f "$den.$$.png"
  mv -n "$premul.$$.png" "$premul" 2>/dev/null || rm -f "$premul.$$.png"
  rm -f "$layer"
fi

# ImageMagick names these operators after the operand being consumed, not the
# one on the left: MinusSrc is "Src subtracted FROM Dst" (what we want) while
# MinusDst is the reverse and yields a black image with no error at all. Same
# trap on DivideSrc / DivideDst.
q=()
case "${out##*.}" in
  avif|jpg|jpeg|webp|heic) q=(-quality "$QUALITY") ;;
esac
magick "$src" "$premul" -compose MinusSrc -composite \
                 "$den" -compose DivideSrc -composite \
  -clamp -alpha off "${q[@]}" "$out"
FILTER
chmod +x "$WORK/filter.sh"

export WM_FILE WM_MODE WM_SCALE WM_OPACITY QUALITY WORK

echo "watermark : $WM_FILE"
echo "            mode=$WM_MODE scale=$WM_SCALE opacity=$WM_OPACITY"

if [ "$RECURSIVE" -eq 0 ]; then
  [ -f "$IN" ] || { echo "not a file: $IN  (use -r for a directory)" >&2; exit 1; }
  echo
  "$WORK/filter.sh" "$IN" "$OUT"
  echo "recovered $OUT"
  exit 0
fi

[ -d "$IN" ] || { echo "not a directory: $IN" >&2; exit 1; }

# `back_*` is the face-down card back, which the forward pass never marked, so
# inverting it would damage a clean image.
list="$WORK/list"
: > "$list"
while IFS= read -r -d '' src; do
  rel="${src#"$IN"/}"
  out="$OUT/$rel"
  [ -n "$EXT" ] && out="${out%.*}.$EXT"
  printf '%s\0%s\0' "$src" "$out" >> "$list"
done < <(find "$IN" -type f \( -iname '*.avif' -o -iname '*.png' -o -iname '*.jpg' \) \
           ! -iname 'back_*' -print0)

total=$(( $(tr -dc '\0' < "$list" | wc -c | tr -d ' ') / 2 ))
echo "input     : $IN"
echo "output    : $OUT${EXT:+   (re-encoded as .$EXT)}"
echo "files     : $total"
echo

start=$(date +%s)
xargs -0 -P "$JOBS" -n 2 "$WORK/filter.sh" < "$list"
end=$(date +%s)

echo "done in $((end - start))s -- $total recovered in $OUT"

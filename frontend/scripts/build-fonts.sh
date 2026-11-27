#!/usr/bin/env bash
#
# Regenerates src/assets/fonts/*.woff2 from the TTF/OTF originals in
# fonts-src/.
#
# Why the sources are not what ships: they are TTF and OTF, which is to say
# uncompressed. The set was 5.45 MB on the wire, and `public/fonts/` is copied
# to the dist root where prod.nginxconf's `location /` gives it
# `max-age=0, must-revalidate` -- so every cold load paid for all of it again.
# As woff2 under src/assets/ the same set is 1.9 MB, vite content-hashes the
# names, and the `/assets/` rules give them a year and `immutable`. That also
# puts them on the CDN for free, because publish-frontend-assets.sh uploads
# dist/assets wholesale.
#
# NotoSans is the one that mattered and the one treated specially. It is the UI
# body font and it is a *variable* font whose `gvar` table -- the per-axis
# outline deltas -- is 1.78 MB of its 2.49 MB. Nothing was using that: the old
# @font-face declared no `font-weight` range, so the browser clamped to the
# default instance and synthesised every bold on the page. So it is instanced
# to the two weights the stylesheets actually resolve to (400 and 700; CSS
# weight matching sends 500 to 400, and 600/800/900 to 700) and subset to the
# scripts the UI locales need. 2.49 MB -> 113 KB on the critical path, with
# real bold instead of a smeared one.
#
# The subset deliberately stops at Latin + Latin Extended + Vietnamese. The
# font has no CJK to begin with (3,094 cmap entries), so ko/zh were already
# falling back to system faces, and a Cyrillic username falls back per-glyph
# the same way -- which is what browsers do and what the sans-serif in every
# stack is for.
#
# The display and icon faces are converted whole. They are small, and which
# glyphs a campaign's flavor text reaches for is not something this script can
# know.
#
# Requires fonttools with brotli:  pip install 'fonttools[woff]'
#
set -euo pipefail

cd "$(dirname "$0")/.."

SRC="fonts-src"
OUT="src/assets/fonts"

command -v pyftsubset >/dev/null || {
  echo "pyftsubset not found — pip install 'fonttools[woff]'" >&2
  exit 1
}
# Use the interpreter pyftsubset itself runs on, so fontTools and brotli are
# guaranteed importable; a bare `python3` is often a different install.
PY="$(head -1 "$(command -v pyftsubset)" | sed 's|^#!||')"

[ -d "$SRC" ] || { echo "no $SRC — nothing to build from" >&2; exit 1; }
mkdir -p "$OUT"

# Latin, Latin Extended, Vietnamese, plus the punctuation, currency and
# arrow ranges the UI draws. Mirrors Google Fonts' latin + latin-ext + vietnamese
# subsets so the coverage is a known quantity rather than a guess.
UNICODES="U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,U+02DC"
UNICODES="$UNICODES,U+0304,U+0308,U+0329,U+2000-206F,U+2074,U+20AC,U+2122"
UNICODES="$UNICODES,U+2191,U+2193,U+2212,U+2215,U+FEFF,U+FFFD"
UNICODES="$UNICODES,U+0100-02AF,U+0259,U+1E00-1EFF,U+2020,U+20A0-20CF,U+2113"
UNICODES="$UNICODES,U+2C60-2C7F,U+A720-A7FF"
UNICODES="$UNICODES,U+0102-0103,U+0110-0111,U+0128-0129,U+0168-0169"
UNICODES="$UNICODES,U+01A0-01A1,U+01AF-01B0,U+1EA0-1EF9,U+20AB"

total_src=0
total_out=0

report() {
  local label="$1" src_bytes="$2" out="$3"
  local out_bytes
  out_bytes=$(stat -f%z "$out" 2>/dev/null || stat -c%s "$out")
  total_src=$((total_src + src_bytes))
  total_out=$((total_out + out_bytes))
  printf '  %-28s %9d -> %8d  (%d%% smaller)\n' \
    "$label" "$src_bytes" "$out_bytes" "$(((src_bytes - out_bytes) * 100 / src_bytes))"
}

echo ">> NotoSans: instance + subset"
noto="$SRC/NotoSans.ttf"
noto_bytes=$(stat -f%z "$noto" 2>/dev/null || stat -c%s "$noto")
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

for weight in 400 700; do
  "$PY" -m fontTools.varLib.instancer "$noto" "wght=$weight" \
    -o "$tmp/noto-$weight.ttf" >/dev/null
  pyftsubset "$tmp/noto-$weight.ttf" \
    --output-file="$OUT/NotoSans-$weight.woff2" \
    --flavor=woff2 --unicodes="$UNICODES" --layout-features='*' \
    --desubroutinize
done
# Both weights come out of the one source file; charge it to the pair rather
# than counting 2.49 MB twice. Only the 400 is on the critical path -- the 700
# is fetched when the first bold glyph is laid out.
report "NotoSans-400 + -700.woff2" "$noto_bytes" "$OUT/NotoSans-400.woff2"
printf '  %-28s %9s -> %8d\n' "(of which bold, lazy)" "" \
  "$(stat -f%z "$OUT/NotoSans-700.woff2")"
total_out=$((total_out + $(stat -f%z "$OUT/NotoSans-700.woff2")))

echo ">> display + icon faces: whole-font woff2"
for f in "$SRC"/*.ttf "$SRC"/*.otf; do
  [ -e "$f" ] || continue
  base="$(basename "$f")"
  [ "$base" = "NotoSans.ttf" ] && continue
  stem="${base%.*}"
  src_bytes=$(stat -f%z "$f" 2>/dev/null || stat -c%s "$f")
  "$PY" - "$f" "$OUT/$stem.woff2" <<'PYEOF'
import sys
from fontTools.ttLib import TTFont
font = TTFont(sys.argv[1])
font.flavor = "woff2"
font.save(sys.argv[2])
PYEOF
  report "$stem.woff2" "$src_bytes" "$OUT/$stem.woff2"
done

# Already-woff2 sources are copied through so fonts.css has one place to point.
for f in "$SRC"/*.woff2; do
  [ -e "$f" ] || continue
  base="$(basename "$f")"
  cp "$f" "$OUT/$base"
  src_bytes=$(stat -f%z "$f" 2>/dev/null || stat -c%s "$f")
  report "$base (copied)" "$src_bytes" "$OUT/$base"
done

echo
printf '>> %d bytes of source -> %d bytes shipped (%d%% smaller)\n' \
  "$total_src" "$total_out" "$(((total_src - total_out) * 100 / total_src))"

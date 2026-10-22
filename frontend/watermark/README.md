# Card watermark

`watermark.png` is the mark `scripts/watermark-cards.sh` composites onto every
card rendition served from the public bucket. It is the FFG diamond reduced to
a white outline on transparency, so it reads as an overlay rather than a solid
block and stays legible over both light and dark art.

It was derived from a 512×512 FFG logo by taking the edge of its alpha channel:

```bash
magick ffg-logo.png -alpha extract -morphology EdgeOut Octagon:4 mask.png
magick -size 512x512 xc:white mask.png -alpha off -compose copy_opacity -composite watermark.png
```

Replace this file to change the mark; the build is keyed on its mtime, so the
next `scripts/watermark-cards.sh` run rebuilds every card. Any transparent PNG
or SVG works — it is scaled to `WM_SCALE` of each card's short edge and tinted
down to `WM_OPACITY`, so author it at full opacity.

## Why the masters are not marked

`frontend/public/img/arkham/cards/` holds the clean masters and is the source of
truth. They are uploaded to the private originals bucket; only the derived tree
under `frontend/.watermarked/` reaches the public bucket. Nothing in local dev
sees a watermark unless you point `VITE_ASSET_HOST` at production.

Homebrew art (`img/arkham/homebrew/`) and custom card art (`img/custom/`) are
never processed.

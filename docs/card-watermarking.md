# Card watermarking and the originals bucket

Card faces served from `assets.arkhamhorror.app` carry an outlined FFG diamond
and are capped at 940px on the short edge. The clean masters never leave the
private originals bucket.

This exists so the app cannot be a convenient source of print-ready card art.
The watermark is the measure that does the work: a mark across the face is
something no genuine printed card has, so a marked image is self-evidently not a
real card. The resolution cap is a secondary trim -- see "What this does not
achieve" below.

It is a deterrent and a provenance signal, not a protection. The composite can
be reversed, and the tool that reverses it is in this repo -- see "Reversing the
mark".

## How the pieces fit

| | |
| --- | --- |
| masters (clean) | `frontend/public/img/arkham/{cards,<lang>/cards}/` -- gitignored, source of truth |
| renditions (public) | `frontend/.watermarked/img/arkham/` -- gitignored build artifact |
| watermark art | `frontend/watermark/watermark.png` (see that directory's README) |
| filter | `scripts/watermark-cards.sh` |
| reversal | `scripts/unwatermark-cards.sh` (CLI), `frontend/src/arkham/unwatermark.ts` (in app, production builds only) |
| public bucket | `s3://arkham-horror-assets` -> CloudFront `EMYS27JG801W1` (assets.arkhamhorror.app) |
| originals bucket | `s3://arkham-horror-originals` -- private, no CloudFront |

Homebrew art (`img/arkham/homebrew/`) and custom card art (`img/custom/`) are
never processed and never watermarked.

Local development is unaffected: the masters stay clean, and `vite dev` serves
them directly. You only see a watermark locally if the asset host is the CDN --
which is the usual case, since no local checkout holds 50,000 card faces, so
`/site-settings` hands back production's host.

The in-app reversal does not run there. It is gated on `import.meta.env.PROD`
(the `ENABLED` constant at the top of `unwatermark.ts`, which `isMarked` reads,
and `isMarked` is what every entry point asks first), so outside a production
build you see the marked renditions exactly as the CDN holds them -- which is
what you want when checking the filter's own output, and saves a second CORS
download plus a canvas pass per card the rest of the time.

## Settings

Defaults live at the top of `scripts/watermark-cards.sh`:

| variable | default | notes |
| --- | --- | --- |
| `WM_OPACITY` | `0.28` | visible at modal size on light and dark art, unobtrusive at play size |
| `WM_SCALE` | `0.62` | mark width as a fraction of the card's short edge |
| `WM_MODE` | `center` | `tile` repeats it diagonally instead |
| `CAP` | `940` | max short edge; clears the 470px details modal at 2x |
| `QUALITY` | `50` | size parity with the masters |

The build is incremental on mtime **and** on these settings: changing any of
them rewrites `.build-settings` and forces a full rebuild, so a changed knob
can never silently skip.

## Going live

### 1. Create the private originals bucket (one time)

```bash
aws s3api create-bucket --bucket arkham-horror-originals --region us-east-1

aws s3api put-public-access-block --bucket arkham-horror-originals \
  --public-access-block-configuration \
  BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true
```

### 2. Build the renditions

```bash
./scripts/watermark-cards.sh          # all languages, ~30 min cold
```

Spot-check a few before uploading anything:

```bash
open frontend/.watermarked/img/arkham/cards/02020.avif
```

### 3. Upload

`make sync-images` sends the masters to the originals bucket and the renditions
to the public bucket in one pass. Originals go first in the recipe, so the
private copy exists before the public prefix is overwritten.

```bash
make sync-images
```

### 4. Close the direct-S3 read path (one time)

Objects were previously uploaded `--acl public-read`, which let anyone fetch
them from `arkham-horror-assets.s3.amazonaws.com`, bypassing CloudFront. The
sync no longer sets that ACL, so anything it re-uploads is already private;
objects it did not touch keep theirs until stripped. Keys can contain spaces and
quotes, so the list is NUL-delimited and `aws` is invoked directly -- no
`sh -c`, which would re-parse them:

```bash
aws s3api list-objects-v2 --bucket arkham-horror-assets --query 'Contents[].Key' --output text \
  | tr '\t' '\n' | tr '\n' '\0' \
  | xargs -0 -P 16 -I{} aws s3api put-object-acl --bucket arkham-horror-assets --key {} --acl private
```

Strip one key first and confirm the CDN still serves it. **A query string does
not bust the edge** -- this distribution does not include one in its cache key,
so `?x=1` returns `x-cache: Hit` and tells you nothing about the origin.
Invalidate the single path and then check:

```bash
aws cloudfront create-invalidation --distribution-id EMYS27JG801W1 \
  --paths "/img/arkham/cards/01000.avif"
curl -sI https://assets.arkhamhorror.app/img/arkham/cards/01000.avif
```

That canary costs nothing, and a mistake here blanks every image on the site.

CloudFront is unaffected -- it reads through origin access identity
`E1HKSII8X83RZP`, which the bucket policy grants `s3:GetObject` directly.

**Leave the bucket's public `s3:ListBucket` in place.** `scripts/fetch-assets.sh`
lists anonymously via S3 and downloads via CloudFront; removing it breaks
`make fetch-images` for anyone without credentials.

`scripts/publish-frontend-assets.sh` still sets `--acl public-read` on the JS/CSS
bundle. That is the deploy path with the mid-deploy `/assets/` 404 fallback and
was deliberately left alone.

### 5. Invalidate the CDN

```bash
aws cloudfront create-invalidation --distribution-id EMYS27JG801W1 \
  --paths "/img/arkham/cards/*" "/img/arkham/es/cards/*" "/img/arkham/fr/cards/*" \
          "/img/arkham/ita/cards/*" "/img/arkham/zh/cards/*"
```

Drop the localized paths if you only rebuilt English, and spell each language
out rather than using `/img/arkham/*/cards/*` -- an invalidation path takes a
trailing wildcard, not one in the middle. Invalidation takes a few minutes;
until it completes, edge caches keep serving the old clean images.

### 6. Verify

```bash
# the CDN serves the watermarked rendition (smaller than the 387343-byte master)
curl -sI https://assets.arkhamhorror.app/img/arkham/cards/01000.avif | grep -i content-length

# direct S3 is now closed
curl -so /dev/null -w "%{http_code}\n" \
  https://arkham-horror-assets.s3.amazonaws.com/img/arkham/cards/01000.avif   # expect 403

# contributors can still fetch
./scripts/fetch-assets.sh cards
```

## Fetching: who gets the masters

`scripts/fetch-assets.sh` is differential on size, which makes the watermarking
dangerous to it by default: every master differs in size from its rendition, so
a plain fetch would download the renditions over the masters and quietly destroy
the local source of truth. Worse, the next `make sync-images` would then upload
those into the originals bucket and watermark them a second time on the way out.

So the fetch routes per key. On startup it tries to list the originals bucket:

- **Credentials that can read it** (the owner): card faces are fetched from
  `s3://arkham-horror-originals` with signed requests, and the size compared
  against is the master's. Masters already on disk are skipped, not replaced.
- **No credentials** (contributors): card faces come from the CDN as watermarked
  renditions, exactly as before. This is the intended contributor experience.

Either way the run prints which mode it is in, so a surprise is visible:

```
  ↹  card faces: clean masters from s3://arkham-horror-originals
  ↹  card faces: watermarked renditions from the CDN
```

Keys with no master -- homebrew card art, box art, portraits, 3ed -- always come
from the CDN, in both modes.

The consequence worth remembering: **if you lose access to the originals bucket,
a fetch will overwrite your masters with watermarked copies.** The notice line
is the warning; the originals bucket is the recovery.

## Rolling back

The masters are intact locally and in the originals bucket, so a rollback is a
re-upload:

```bash
cd frontend/public && aws s3 sync . s3://arkham-horror-assets \
  --exclude "*" --include "img/arkham/cards/*.avif"
aws cloudfront create-invalidation --distribution-id EMYS27JG801W1 --paths "/img/arkham/cards/*"
```

## Reversing the mark

`scripts/unwatermark-cards.sh` undoes the composite, using the same settings
the forward pass used:

```bash
./scripts/unwatermark-cards.sh in.avif out.png            # one file
EXT=png ./scripts/unwatermark-cards.sh -r IN_DIR OUT_DIR  # a tree
```

It ships deliberately. Alpha compositing is algebraically invertible, and these
settings leave an unusually clean inverse: peak alpha is about 0.29 (the 0.28
setting plus a little resample overshoot), so the forward map is
`C' = 0.71C + 73.95`, which lands exactly on 255 at C=255. Nothing clips, so
there is nothing destroyed to reconstruct -- only about half a bit of
quantization, amplified by 1/(1-a) = 1.4 on the way back.

Measured against the masters, with AVIF re-encode noise as the floor no
inversion can beat:

| card | floor | marked | recovered | marked under the mark | recovered under the mark |
| --- | --- | --- | --- | --- | --- |
| 02020 (750x1050) | 33.93 dB | 29.91 | **33.68** | 17.14 | **29.42** |
| 01039 (423x600) | 39.05 dB | 31.79 | **38.10** | 18.32 | **30.22** |
| 03001 (1050x750) | 32.39 dB | 28.58 | **32.31** | 15.94 | **30.36** |

Whole-image recovery sits on the floor; the mark is visually gone. Both
`center` and `tile` modes round-trip to the same figure.

Two things it cannot do. It cannot undo the 940px cap -- resolution the
rendition never carried is gone. And writing the result back to AVIF spends a
second lossy generation, which is what `EXT=png` avoids.

Hiding this would not have raised the bar; the mark is identical on all ~34,700
renditions, always centered, always 0.62 of the short edge, always one opacity,
which is exactly the setup that Dekel et al., *On the Effectiveness of Visible
Watermarks* (CVPR 2017) recovers from the images alone without being told the
mark at all. The countermeasure that paper recommends -- and the only one that
would change this materially -- is randomizing position, scale, rotation and
opacity per image, so a multi-image attack has no single target. That is not
implemented.

## Caching

Card images shipped without a `Cache-Control` header, so browsers fell back to
heuristic freshness and CloudFront missed more than it should.
`make sync-images` now sets `public, max-age=604800` on everything it uploads,
and `scripts/set-cache-control.sh` backfills objects already in the bucket.

A week, not a year, and not `immutable`: a card keeps its URL when its art is
replaced, so a long TTL would mean stale art for that long after any re-upload.

The backfill rewrites each object with `--metadata-directive REPLACE`, which
replaces **all** system metadata -- an object copied without an explicit
`--content-type` comes back as `binary/octet-stream` and stops rendering. That
is why the script works one extension at a time with the type named each time,
and why it has a `--canary` mode. Use it.

## What this does not achieve

Worth being clear about, so nobody later assumes more protection than exists.

- **The cap does not get art below print resolution.** The details modal renders
  at 470 CSS px, which needs 940px at 2x -- about 376 dpi, above the 300 dpi a
  printer needs. The cap only trims the ~200 cards that were at 450-674 dpi.
  Nothing short of degrading the modal would get under print, so the watermark,
  not the cap, is the real measure.
- **Scraping cannot be prevented.** Anything a browser renders, a scraper can
  take. Closing direct S3 reads is a speed bump: card codes are public from
  ArkhamDB, so the CDN can still be enumerated.
- **The mark is removable.** See "Reversing the mark" -- the inverse is exact
  up to the re-encode noise. It makes a scraped image self-evidently not a
  genuine card, and makes a straight scrape of the bucket not worth much; it
  does not make the clean art unobtainable.
- **Do not add client-side un-watermarking.** A CLI in this repo is a documented
  property of the scheme. A canvas shader in the app would be the scheme
  undoing itself automatically, for every visitor, on every card -- strictly
  worse than having no watermark.

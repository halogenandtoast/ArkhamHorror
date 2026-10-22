# Card watermarking and the originals bucket

Card faces served from `assets.arkhamhorror.app` carry an outlined FFG diamond
and are capped at 940px on the short edge. The clean masters never leave the
private originals bucket.

This exists so the app cannot be a convenient source of print-ready card art.
The watermark is the measure that does the work: a mark across the face is
something no genuine printed card has, so a marked image is self-evidently not a
real card. The resolution cap is a secondary trim -- see "What this does not
achieve" below.

## How the pieces fit

| | |
| --- | --- |
| masters (clean) | `frontend/public/img/arkham/{cards,<lang>/cards}/` -- gitignored, source of truth |
| renditions (public) | `frontend/.watermarked/img/arkham/` -- gitignored build artifact |
| watermark art | `frontend/watermark/watermark.png` (see that directory's README) |
| filter | `scripts/watermark-cards.sh` |
| public bucket | `s3://arkham-horror-assets` -> CloudFront `EMYS27JG801W1` (assets.arkhamhorror.app) |
| originals bucket | `s3://arkham-horror-originals` -- private, no CloudFront |

Homebrew art (`img/arkham/homebrew/`) and custom card art (`img/custom/`) are
never processed and never watermarked.

Local development is unaffected: the masters stay clean, and `vite dev` serves
them directly. You only see a watermark locally if `VITE_ASSET_HOST` points at
production.

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

Strip one key first and confirm the CDN still serves it on a cache-busted URL
(`?x=1`, so the answer comes from the origin rather than an edge cache). That
canary costs nothing, and a mistake here blanks every image on the site.

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
- **Do not add client-side un-watermarking.** The composite is invertible
  (`C = (C' - aW)/(1-a)`), so a canvas shader could strip the mark. Shipping that
  would hand every visitor an automated un-watermarker -- strictly worse than
  having no watermark.

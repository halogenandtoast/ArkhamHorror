/* Undoes the watermark the CDN renditions carry, for display only.
 *
 * The mark is alpha-composited, so it inverts exactly:
 *
 *   forward:  C' = C(1-a) + 255a      (the mark is white)
 *   inverse:  C  = (C' - 255a) / (1-a)
 *
 * The constants below MUST match scripts/watermark-cards.sh, which builds the
 * renditions. A mismatch does not fail; it leaves a ghost of the mark behind.
 * docs/card-watermarking.md has the measurements.
 *
 * Only the pixels on screen are clean. The marked file is still what the CDN
 * holds and still what "save image as" writes, which is the arrangement we
 * want: clean while you play, marked once it leaves the page.
 */
import { ref, watch, type Ref } from 'vue'
import markUrl from '../../watermark/watermark.png'

const WM_SCALE = 0.62
const WM_OPACITY = 0.28

/* One card is on screen at a time; the cache is for going back to one just
 * looked at. Each entry holds a decoded frame, so it is kept short. */
const CACHE_LIMIT = 8

/* What the filter marks: img/arkham/cards/NAME.avif and the localized
 * img/arkham/<lang>/cards/NAME.avif. back_* is the face-down card back, which
 * the filter skips; homebrew art, custom art and the customization sheets live
 * under other prefixes and are never marked. */
const MARKED_PATH = /\/img\/arkham\/(?:(?:es|fr|ita|ko|rus|zh)\/)?cards\/(?!back_)[^/]+\.avif$/

/* Only renditions are marked, and only the asset host serves them. A relative
 * URL is vite serving the clean masters in development. */
export function isMarked(url: string | null | undefined): boolean {
  if (!url || !/^https?:\/\//i.test(url)) return false
  try {
    return MARKED_PATH.test(new URL(url).pathname)
  } catch {
    return false
  }
}

let markArt: Promise<HTMLImageElement> | null = null

function loadImage(url: string, cors: boolean): Promise<HTMLImageElement> {
  return new Promise((resolve, reject) => {
    const image = new Image()
    /* The canvas gets read back, so the pixels have to be origin-clean.
     * assets.arkhamhorror.app answers with access-control-allow-origin: *. */
    if (cors) image.crossOrigin = 'anonymous'
    image.decoding = 'async'
    image.onload = () => resolve(image)
    image.onerror = () => reject(new Error(`could not load ${url}`))
    image.src = url
  })
}

function loadMark(): Promise<HTMLImageElement> {
  if (!markArt) {
    markArt = loadImage(markUrl, false).catch((error) => {
      markArt = null
      throw error
    })
  }
  return markArt
}

/* The mark's alpha at every pixel of a card this size. It depends only on the
 * geometry, and there are a handful of card sizes, so it is built once each. */
const alphaMaps = new Map<string, Float32Array>()

async function alphaMapFor(w: number, h: number): Promise<Float32Array> {
  const key = `${w}x${h}`
  const known = alphaMaps.get(key)
  if (known) return known

  const mark = await loadMark()
  /* The filter sizes the mark off the short edge AFTER its resolution cap,
   * which is exactly the rendition's own short edge. `awk printf "%d"`
   * truncates, so this does too. */
  const markW = Math.max(16, Math.trunc(Math.min(w, h) * WM_SCALE))
  const markH = Math.round((mark.naturalHeight * markW) / mark.naturalWidth)

  const canvas = document.createElement('canvas')
  canvas.width = w
  canvas.height = h
  const ctx = canvas.getContext('2d', { willReadFrequently: true })
  if (!ctx) throw new Error('no 2d context')
  ctx.imageSmoothingQuality = 'high'
  /* ImageMagick's `-gravity center -extent` floors the offset. One pixel out
   * and the mark is subtracted from the wrong place, leaving an outline. */
  ctx.drawImage(mark, Math.floor((w - markW) / 2), Math.floor((h - markH) / 2), markW, markH)

  const { data } = ctx.getImageData(0, 0, w, h)
  const alpha = new Float32Array(w * h)
  for (let i = 0, p = 3; i < alpha.length; i += 1, p += 4) {
    alpha[i] = (data[p] / 255) * WM_OPACITY
  }
  alphaMaps.set(key, alpha)
  return alpha
}

const recovered = new Map<string, string>()
const inFlight = new Map<string, Promise<string>>()

function remember(url: string, objectUrl: string) {
  recovered.set(url, objectUrl)
  while (recovered.size > CACHE_LIMIT) {
    const oldest = recovered.keys().next().value as string
    const stale = recovered.get(oldest)
    recovered.delete(oldest)
    if (stale) URL.revokeObjectURL(stale)
  }
}

/* Measured on a 750x1050 card: JPEG at 0.9 costs 0.33dB against the lossless
 * canvas and encodes in 5ms, where WebP 0.95 costs 0.49dB and takes 90ms, and
 * PNG is lossless but 1.6MB a card. Card faces are opaque, so losing the alpha
 * channel costs nothing. */
function encode(canvas: HTMLCanvasElement): Promise<string> {
  return new Promise((resolve, reject) => {
    canvas.toBlob(
      (blob) => (blob ? resolve(URL.createObjectURL(blob)) : reject(new Error('canvas encode failed'))),
      'image/jpeg',
      0.9,
    )
  })
}

async function build(url: string): Promise<string> {
  const image = await loadImage(url, true)
  const w = image.naturalWidth
  const h = image.naturalHeight
  const alpha = await alphaMapFor(w, h)

  const canvas = document.createElement('canvas')
  canvas.width = w
  canvas.height = h
  const ctx = canvas.getContext('2d', { willReadFrequently: true })
  if (!ctx) throw new Error('no 2d context')
  ctx.drawImage(image, 0, 0)

  const frame = ctx.getImageData(0, 0, w, h)
  const px = frame.data
  for (let i = 0, p = 0; i < alpha.length; i += 1, p += 4) {
    const a = alpha[i]
    if (a === 0) continue
    const gain = 1 / (1 - a)
    const bias = 255 * a
    /* Uint8ClampedArray rounds and clamps on assignment, so the arithmetic
     * can stay in floats and land back in range by itself. */
    px[p] = (px[p] - bias) * gain
    px[p + 1] = (px[p + 1] - bias) * gain
    px[p + 2] = (px[p + 2] - bias) * gain
  }
  ctx.putImageData(frame, 0, 0)
  return encode(canvas)
}

export function unwatermark(url: string): Promise<string> {
  const done = recovered.get(url)
  if (done) return Promise.resolve(done)

  let pending = inFlight.get(url)
  if (!pending) {
    pending = build(url)
      .then((objectUrl) => {
        remember(url, objectUrl)
        return objectUrl
      })
      .finally(() => inFlight.delete(url))
    inFlight.set(url, pending)
  }
  return pending
}

/* Follows a card URL and gives back one to render.
 *
 * Nothing to reverse (local art, homebrew, the card back) passes straight
 * through, and so does a failure -- showing a marked card beats showing none.
 * While a reversal is running the value is null, so a caller that gates on it
 * shows nothing rather than flashing the mark it is about to remove. */
export function useUnwatermarked(source: Ref<string | null | undefined>): Ref<string | null> {
  const display = ref<string | null>(null)
  let latest = 0

  watch(
    source,
    (url) => {
      const mine = (latest += 1)
      if (!url) {
        display.value = null
        return
      }
      if (!isMarked(url)) {
        display.value = url
        return
      }
      /* Synchronous on a cache hit, so returning to a card does not blink. */
      const done = recovered.get(url)
      if (done) {
        display.value = done
        return
      }
      display.value = null
      unwatermark(url)
        .then((clean) => {
          if (mine === latest) display.value = clean
        })
        .catch(() => {
          if (mine === latest) display.value = url
        })
    },
    { immediate: true },
  )

  return display
}

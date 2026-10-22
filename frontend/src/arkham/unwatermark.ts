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
import { computed, ref, watch, type Ref } from 'vue'
import markUrl from '../../watermark/watermark.png'

/* Only a production build reverses anything.
 *
 * Nothing watermarks locally -- the masters stay clean -- but a dev whose asset
 * host is the CDN (which is the default, since no local checkout holds 50,000
 * card faces) is served the renditions, and reversing them there buys nothing:
 * it spends a second CORS download and a canvas pass per card to hide the very
 * thing a dev wants to be able to see, and it is the one place the marked art
 * should be visible for checking the filter's own output.
 *
 * isMarked is the single gate: every entry point -- the composable, the grid's
 * warm, the crossorigin attribute, the sweep -- asks it first, so one constant
 * turns the whole thing off. */
const ENABLED = import.meta.env.PROD

const WM_SCALE = 0.62
const WM_OPACITY = 0.28

/* A table holds about fifty distinct cards, and warming reverses all of them,
 * so the cache has to outlast a sweep or the work is thrown away as fast as it
 * is done. Entries are JPEG blobs of about 150KB. */
const CACHE_LIMIT = 64

/* Per-pixel the inverse needs 1/(1-a) and 255a, and alpha only ever takes 256
 * values, so both are tabulated once instead of divided out a million times a
 * card. */
const GAIN = new Float32Array(256)
const BIAS = new Float32Array(256)
for (let v = 0; v < 256; v += 1) {
  const a = (v / 255) * WM_OPACITY
  GAIN[v] = 1 / (1 - a)
  BIAS[v] = 255 * a
}

/* What the filter marks: img/arkham/cards/NAME.avif and the localized
 * img/arkham/<lang>/cards/NAME.avif. back_* is the face-down card back, which
 * the filter skips; homebrew art, custom art and the customization sheets live
 * under other prefixes and are never marked. */
const MARKED_PATH = /\/img\/arkham\/(?:(?:es|fr|ita|ko|rus|zh)\/)?cards\/(?!back_)[^/]+\.avif$/

/* Only renditions are marked, and only the asset host serves them. A relative
 * URL is vite serving the clean masters in development. */
export function isMarked(url: string | null | undefined): boolean {
  if (!ENABLED) return false
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

/* The mark's own alpha, over the box it occupies rather than the whole card.
 * The mark covers about a quarter of a card, so holding the box instead of the
 * frame is the difference between 216KB and 3MB a geometry -- and the pixel
 * pass then walks only the rows that can change. Opacity is applied through
 * GAIN/BIAS at use, so this stays the art's own alpha. */
interface AlphaBox {
  x: number
  y: number
  w: number
  h: number
  alpha: Uint8Array
}

const alphaBoxes = new Map<string, AlphaBox>()

async function alphaBoxFor(w: number, h: number): Promise<AlphaBox> {
  const key = `${w}x${h}`
  const known = alphaBoxes.get(key)
  if (known) return known

  const mark = await loadMark()
  /* The filter sizes the mark off the short edge AFTER its resolution cap,
   * which is exactly the rendition's own short edge. `awk printf "%d"`
   * truncates, so this does too. */
  const markW = Math.max(16, Math.trunc(Math.min(w, h) * WM_SCALE))
  const markH = Math.round((mark.naturalHeight * markW) / mark.naturalWidth)
  /* ImageMagick's `-gravity center -extent` floors the offset. One pixel out
   * and the mark is subtracted from the wrong place, leaving an outline. */
  const x = Math.floor((w - markW) / 2)
  const y = Math.floor((h - markH) / 2)

  const canvas = document.createElement('canvas')
  canvas.width = markW
  canvas.height = markH
  const ctx = canvas.getContext('2d', { willReadFrequently: true })
  if (!ctx) throw new Error('no 2d context')
  ctx.imageSmoothingQuality = 'high'
  ctx.drawImage(mark, 0, 0, markW, markH)

  const { data } = ctx.getImageData(0, 0, markW, markH)
  const alpha = new Uint8Array(markW * markH)
  for (let i = 0, p = 3; i < alpha.length; i += 1, p += 4) alpha[i] = data[p]

  const box: AlphaBox = { x, y, w: markW, h: markH, alpha }
  alphaBoxes.set(key, box)
  return box
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

/* Measured against the masters: JPEG 0.94 lands 33.81dB on a 750x1050 card and
 * 36.45 on a 423x600 one, in 2-5ms. WebP 0.92 is no better (33.42 / 36.41) and
 * takes 28-84ms, which matters when a sweep encodes fifty cards; PNG is
 * lossless but 1.6MB a card. Below 0.94 the small cards lose almost a dB to
 * chroma subsampling. Card faces are opaque, so dropping alpha costs nothing. */
function encode(canvas: HTMLCanvasElement): Promise<string> {
  return new Promise((resolve, reject) => {
    canvas.toBlob(
      (blob) => (blob ? resolve(URL.createObjectURL(blob)) : reject(new Error('canvas encode failed'))),
      'image/jpeg',
      0.94,
    )
  })
}

async function build(url: string): Promise<string> {
  const image = await loadImage(url, true)
  const w = image.naturalWidth
  const h = image.naturalHeight
  const box = await alphaBoxFor(w, h)

  const canvas = document.createElement('canvas')
  canvas.width = w
  canvas.height = h
  const ctx = canvas.getContext('2d', { willReadFrequently: true })
  if (!ctx) throw new Error('no 2d context')
  ctx.drawImage(image, 0, 0)

  /* Only the box the mark covers is read back and written again; the rest of
   * the card is already what it should be. */
  const frame = ctx.getImageData(box.x, box.y, box.w, box.h)
  const px = frame.data
  const { alpha } = box
  for (let i = 0, p = 0; i < alpha.length; i += 1, p += 4) {
    const a = alpha[i]
    if (a === 0) continue
    const gain = GAIN[a]
    const bias = BIAS[a]
    /* Uint8ClampedArray rounds and clamps on assignment, so the arithmetic
     * can stay in floats and land back in range by itself. */
    px[p] = (px[p] - bias) * gain
    px[p + 1] = (px[p + 1] - bias) * gain
    px[p + 2] = (px[p + 2] - bias) * gain
  }
  ctx.putImageData(frame, box.x, box.y)
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

/* Fetch the mark art before anything needs it, so the first card of a session
 * does not wait on a second round trip. */
export function primeWatermark(): void {
  if (!ENABLED) return
  void loadMark().catch(() => {})
}

/* Reverse cards before anyone asks for one.
 *
 * A reversal needs its own CORS fetch of the card, which the browser will not
 * serve from the entry the page's own <img> filled, so hovering an unwarmed
 * card pays a download it looks like it should not. Warming moves that cost
 * off the hover and into idle time, one card at a time so a sweep of a whole
 * table never competes with the game for a frame. */
const warmQueued = new Set<string>()
const warmQueue: string[] = []
const warmListeners = new Map<string, Set<(objectUrl: string) => void>>()
let warmRunning = false

const whenIdle: (fn: () => void) => void =
  typeof requestIdleCallback === 'function'
    ? (fn) => { requestIdleCallback(() => fn(), { timeout: 2000 }) }
    : (fn) => { setTimeout(fn, 16) }

function drainWarmQueue() {
  const next = warmQueue.shift()
  if (next === undefined) {
    warmRunning = false
    return
  }
  warmRunning = true
  warmQueued.delete(next)
  void unwatermark(next)
    .then((objectUrl) => {
      const listeners = warmListeners.get(next)
      warmListeners.delete(next)
      if (listeners) for (const listener of listeners) listener(objectUrl)
    })
    .catch(() => { warmListeners.delete(next) })
    .then(() => whenIdle(drainWarmQueue))
}

export function warmCards(urls: Iterable<string>): void {
  for (const url of urls) {
    if (!isMarked(url)) continue
    if (recovered.has(url) || inFlight.has(url) || warmQueued.has(url)) continue
    warmQueued.add(url)
    warmQueue.push(url)
  }
  if (!warmRunning && warmQueue.length > 0) whenIdle(drainWarmQueue)
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

/* A card in a grid reverses once the browser has actually loaded it. That
 * piggybacks on loading="lazy", which already declines to fetch a card nobody
 * has scrolled to -- and unlike watching for the element to near the viewport,
 * it cannot be fooled by the grid's own layout, where a card with no image yet
 * has no size and every card in the set sits piled at the top of the page.
 *
 * It goes through the same idle queue as the table sweep, so fifty cards
 * appearing at once cannot stall a hover: an on-demand unwatermark() starts
 * immediately, while this waits its turn. */
export function warmCard(url: string, onReady: (objectUrl: string) => void): () => void {
  if (!ENABLED) return () => {}
  const done = recovered.get(url)
  if (done) {
    onReady(done)
    return () => {}
  }
  let live = true
  const listeners = warmListeners.get(url) ?? new Set()
  const listener = (objectUrl: string) => { if (live) onReady(objectUrl) }
  listeners.add(listener)
  warmListeners.set(url, listeners)
  warmCards([url])
  return () => {
    live = false
    listeners.delete(listener)
    if (listeners.size === 0) warmListeners.delete(url)
  }
}

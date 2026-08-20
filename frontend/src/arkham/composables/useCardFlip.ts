import { onBeforeUnmount, ref, watch, type Ref } from 'vue'

// width > height, per image url. Shared across every flipping card: the same few
// faces turn over repeatedly, and a face measured once never needs measuring again.
const sidewaysCache = new Map<string, boolean>()

// Long enough for a decode off the browser cache, short enough that a slow image
// flips on the plain axis instead of stalling the animation.
const MEASURE_TIMEOUT_MS = 60

function measure(url: string, done: (sideways: boolean | null) => void) {
  const cached = sidewaysCache.get(url)
  if (cached !== undefined) return done(cached)

  let settled = false
  const settle = (value: boolean | null) => {
    if (settled) return
    settled = true
    done(value)
  }
  const probe = new Image()
  probe.decoding = 'async'
  probe.onload = () => {
    const sideways = probe.naturalWidth > probe.naturalHeight
    sidewaysCache.set(url, sideways)
    settle(sideways)
  }
  probe.onerror = () => settle(null)
  probe.src = url
  window.setTimeout(() => settle(null), MEASURE_TIMEOUT_MS)
}

export function useCardFlip<T>(
  image: Readonly<Ref<T>>,
  shouldFlip: (nextImage: T, previousImage: T) => boolean = () => true,
) {
  const displayedImage = ref<T>(image.value)
  const flipping = ref(false)
  // An act or agenda that flips to an enemy or location lands in the other
  // orientation. Turning about the 45-degree diagonal maps a w-by-h card onto
  // h-by-w, so it flips into the shape it is becoming instead of popping into it.
  const flippingDiagonally = ref(false)
  let imageSwapTimer: number | undefined
  let animationTimer: number | undefined
  let flipToken = 0

  watch(image, (nextImage, previousImage) => {
    window.clearTimeout(imageSwapTimer)
    window.clearTimeout(animationTimer)
    const token = ++flipToken

    if (
      !shouldFlip(nextImage, previousImage) ||
      window.matchMedia('(prefers-reduced-motion: reduce)').matches
    ) {
      displayedImage.value = nextImage
      flipping.value = false
      flippingDiagonally.value = false
      return
    }

    const start = (diagonal: boolean) => {
      if (token !== flipToken) return // a later flip already took over
      flipping.value = true
      flippingDiagonally.value = diagonal
      imageSwapTimer = window.setTimeout(() => {
        displayedImage.value = nextImage
      }, 225)
      animationTimer = window.setTimeout(() => {
        flipping.value = false
        flippingDiagonally.value = false
      }, 450)
    }

    // Both faces have to be measured before the animation can pick an axis; the
    // wait is one decode off cache, or nothing at all once they are cached.
    if (typeof nextImage !== 'string' || typeof previousImage !== 'string') return start(false)
    measure(previousImage, (was) => {
      if (token !== flipToken) return
      measure(nextImage, (becomes) => {
        start(was !== null && becomes !== null && was !== becomes)
      })
    })
  })

  onBeforeUnmount(() => {
    window.clearTimeout(imageSwapTimer)
    window.clearTimeout(animationTimer)
  })

  return { displayedImage, flipping, flippingDiagonally }
}

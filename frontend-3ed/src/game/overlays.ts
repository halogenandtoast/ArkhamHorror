import { reactive, ref, shallowRef } from 'vue'
import { PHASE_BANNERS, sleep } from '@/game/util'

// the running view transition; banners and flying cards wait for it, since
// everything tweening in one is drawn above the whole page
export const viewTransition = shallowRef<ViewTransition | null>(null)
export const transitionDone = () => viewTransition.value?.finished.catch(() => {}) ?? Promise.resolve()

// ---- phase banners ------------------------------------------------------
export const banner = ref<{ name: string; color: string; key: number } | null>(null)
const phaseQueue: string[] = []
let phasePlaying = false
let bannerKey = 0
export function announcePhase(phase: string) {
  if (!PHASE_BANNERS[phase]) return
  phaseQueue.push(phase)
  if (!phasePlaying) void playPhaseBanners()
}
async function playPhaseBanners() {
  phasePlaying = true
  while (phaseQueue.length) {
    const [name, color] = PHASE_BANNERS[phaseQueue.shift()!]
    await transitionDone()
    banner.value = { name, color, key: ++bannerKey }
    await sleep(1300)
    banner.value = null
  }
  phasePlaying = false
}

// ---- flash ----------------------------------------------------------------
export const flashState = reactive({ text: '', key: 0 })
export function flash(text: string) {
  flashState.text = text
  flashState.key++
}

// ---- zoom -----------------------------------------------------------------
export type Zoom =
  | { kind: 'images'; srcs: string[] }
  // a card whose back falls back to its owner's card back
  | { kind: 'card'; src: string; owner: string }
  // an enlarged two-sided sheet that turns over on click; it opens showing `shown`
  | { kind: 'flip'; shown: string; other: string; ratio: number }
  // the enlarged card keeps the arrow pointing at the section to read
  | { kind: 'marked'; src: string; top: number }
  // debug: look through a deck and take any card out of it
  | { kind: 'browse'; key: string }

export const zoomState = shallowRef<Zoom | null>(null)
export const zoom = (...srcs: string[]) => {
  zoomState.value = { kind: 'images', srcs }
}
export const zoomCard = (src: string, owner: string) => {
  zoomState.value = { kind: 'card', src, owner }
}
export const zoomFlip = (shown: string, other: string, ratio = 1503 / 1200) => {
  zoomState.value = { kind: 'flip', shown, other, ratio }
}
export const zoomMarked = (src: string, top: number) => {
  zoomState.value = { kind: 'marked', src, top }
}
export const closeZoom = () => {
  zoomState.value = null
}

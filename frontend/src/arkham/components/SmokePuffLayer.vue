<script lang="ts" setup>
/* The board's smoke puffs: an investigator taken out of the game implodes into
 * a cloud, and condenses back out of one. See smokePuff.ts for the effect.
 *
 * __Told, not inferred.__ The first version watched for an investigator's
 * placement becoming Unplaced and back. That cannot work: measured on a solo
 * game, Ritual Circle's haunted ability, the rest of the round, the mythos
 * phase and the return all arrived in a *single* update, so the client went
 * from at-the-location to at-the-location with nothing observable in between.
 * The engine announces each half over the UI channel instead, and carries the
 * colour with it so the card doing the vanishing is the one that picks it.
 *
 * __Why a layer__ rather than something inside the portrait: in the other case
 * -- an update per phase -- the portrait is gone. Location.vue unmounts it in
 * the same update that would tell us to animate it, so nothing living with the
 * portrait can outlive it. One canvas over the viewport, one WebGL context,
 * and puffs drawn into it at screen coordinates.
 */
import { inject, onBeforeUnmount, onMounted, ref } from 'vue'
import type { Ref } from 'vue'
import useEmitter from '@/composable/useEmitter'
import { createSmokePuffLayer, parseRgb, type SmokePuffLayer } from '@/arkham/smokePuff'

const emitter = useEmitter()

/* Game.vue's update lock. While it is held an incoming GameUpdate is queued
 * and replayed on release, which is what keeps the next state from landing
 * while a puff is in flight -- the engine does not pause for us, so the update
 * that follows a vanishing is already on its way.
 *
 * Taken on the first puff and released by the last, and only if it was free: a
 * revelation or a draw spotlight may hold it, and releasing someone else's
 * lock would restore a question they meant to keep hidden. */
const uiLock = inject<Ref<boolean>>('uiLock', ref(false))

const canvas = ref<HTMLCanvasElement | null>(null)
let layer: SmokePuffLayer | null = null
let inFlight = 0
let heldLock = false

const FALLBACK: [number, number, number] = [0.34, 0.88, 0.48]
/* How long to wait for the portrait to turn up. A return is announced before
 * the placement that puts the investigator back, and the update carrying it is
 * flushed to the board even while the lock is held -- so the element arrives
 * within a frame or two. Past this the board evidently has not got them, and a
 * puff over empty space is worse than no puff. */
const WAIT_FOR_PORTRAIT_MS = 600

function acquireLock() {
  inFlight += 1
  if (inFlight === 1 && !uiLock.value) {
    uiLock.value = true
    heldLock = true
  }
}

function releaseLock() {
  inFlight = Math.max(inFlight - 1, 0)
  if (inFlight === 0 && heldLock) {
    heldLock = false
    uiLock.value = false
  }
}

function moverFor(id: string): HTMLElement | null {
  return document.querySelector<HTMLElement>(`[data-investigator-mini="${CSS.escape(id)}"]`)
}

/** The portrait inside a mini-mover, whichever of its three shapes it took. */
function portraitFor(id: string): HTMLElement | null {
  return moverFor(id)?.querySelector<HTMLElement>('.portrait') ?? null
}

/* `<img class="portrait">` for most investigators, a div with a
 * background-image for one using its card art, and neither for the replacement
 * marker -- which has no art to dissolve and gets a silhouette. */
function artOf(element: HTMLElement): string | null {
  if (element instanceof HTMLImageElement) return element.currentSrc || element.src || null
  const background = getComputedStyle(element).backgroundImage
  return background.match(/url\(["']?(.*?)["']?\)/)?.[1] ?? null
}

function waitForPortrait(id: string): Promise<HTMLElement | null> {
  const found = portraitFor(id)
  if (found) return Promise.resolve(found)
  return new Promise((resolve) => {
    const deadline = performance.now() + WAIT_FOR_PORTRAIT_MS
    const poll = () => {
      const element = portraitFor(id)
      if (element) return resolve(element)
      if (performance.now() >= deadline) return resolve(null)
      requestAnimationFrame(poll)
    }
    requestAnimationFrame(poll)
  })
}

/* `smokePuff:<direction>:<investigator>:<colour>`, with the prefix already
 * stripped by Game.vue. */
async function onPuff(payload: string) {
  if (!layer) return
  const [direction, id, color] = payload.split(':')
  if (!id || (direction !== 'out' && direction !== 'in')) return

  // Synchronously, before any await: the other half of the vanishing is
  // already on the socket, and Game.vue decides whether to queue it by looking
  // at the lock.
  acquireLock()
  let mover: HTMLElement | null = null
  try {
    const element = await waitForPortrait(id)
    if (!element || !layer) return
    const box = element.getBoundingClientRect()
    if (box.width <= 0 || box.height <= 0) return

    /* Hide the real portrait for the length of the puff. Both directions need
     * it: the shader draws its own copy, and where the whole vanishing arrives
     * in one update the board never stops showing the investigator at all --
     * without this you would watch a portrait dissolve next to itself. */
    mover = moverFor(id)
    if (mover) mover.style.opacity = '0'

    await layer.play({
      rect: { left: box.left, top: box.top, width: box.width, height: box.height },
      direction,
      image: artOf(element),
      imageElement: element instanceof HTMLImageElement ? element : null,
      color: parseRgb(color ?? '', FALLBACK),
      radius: parseFloat(getComputedStyle(element).borderTopLeftRadius) || 0,
    })
  } finally {
    if (mover) mover.style.opacity = ''
    releaseLock()
  }
}

onMounted(() => {
  if (canvas.value) layer = createSmokePuffLayer(canvas.value)
  emitter.on('arkham:smoke-puff', onPuff)
})

onBeforeUnmount(() => {
  emitter.off('arkham:smoke-puff', onPuff)
  layer?.destroy()
  layer = null
  // destroy() resolves whatever is in flight, but not if the layer was never
  // created. Leaving the lock held would freeze the board for good.
  inFlight = 0
  if (heldLock) {
    heldLock = false
    uiLock.value = false
  }
})
</script>

<template>
  <canvas ref="canvas" class="smoke-puff-layer" aria-hidden="true" />
</template>

<style scoped>
.smoke-puff-layer {
  position: fixed;
  inset: 0;
  width: 100%;
  height: 100%;
  max-width: none;
  pointer-events: none;
  z-index: var(--z-board-smoke-puff);
}
</style>

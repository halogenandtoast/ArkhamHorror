<script lang="ts" setup>
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import {
  createDroplets,
  supportsHtmlInCanvas,
  type DropletsInstance,
  type DropletsOptions,
} from '@/arkham/droplets'
import { RAIN_FAR, RAIN_NEAR } from '@/arkham/rainTexture'

export interface Props {
  /** Whether the rain should run at all (scenario, setup state, settings). */
  enabled?: boolean
  options?: DropletsOptions
}

const props = defineProps<Props>()

// Probed once at setup. The capability cannot change within a session, and
// checking here keeps the whole wrapping structure out of the DOM for everyone
// who does not have html-in-canvas — which today is everyone, since Chrome
// ships it behind a flag. Without it the drops would have nothing to refract,
// so the effect is simply not rendered rather than rendered badly.
const native = supportsHtmlInCanvas()

// The wrapped content always renders; only the canvases come and go. Putting
// the gate here rather than on a v-if at the call site means the board is never
// unmounted just because the rain turned off.
const active = computed(() => native && props.enabled !== false)

const sourceRef = ref<HTMLCanvasElement | null>(null)
const contentRef = ref<HTMLElement | null>(null)
const outputRef = ref<HTMLCanvasElement | null>(null)
let instance: DropletsInstance | null = null

function teardown() {
  instance?.destroy()
  instance = null
}

function setup() {
  if (!active.value || instance) return
  const source = sourceRef.value
  const content = contentRef.value
  const output = outputRef.value
  if (!source || !content || !output) return
  instance = createDroplets({ source, content, output }, props.options ?? {})
}

onMounted(setup)
watch(active, (on) => (on ? setup() : teardown()), { flush: 'post' })
watch(() => props.options, (options) => instance?.setOptions(options ?? {}), { deep: true })
onBeforeUnmount(teardown)
</script>

<template>
  <!-- No html-in-canvas: pass the content straight through, adding nothing at
       all to the tree. -->
  <slot v-if="!active" />
  <div v-else class="rain-host">
    <!-- `layoutsubtree` is what makes the browser lay this canvas's children
         out as real, interactive DOM while also letting drawElementImage()
         rasterise them for the shader to refract. -->
    <canvas ref="sourceRef" class="rain-source" layoutsubtree="true">
      <div ref="contentRef" class="rain-content">
        <!-- Falling rain, inside the captured subtree on purpose: the drops
             refract whatever is behind them, and refraction of a flat colour is
             a no-op, so this is what gives them something to bend over the
             empty parts of the board. -->
        <div
          class="rain-backdrop"
          aria-hidden="true"
          :style="{ '--rain-near': RAIN_NEAR, '--rain-far': RAIN_FAR }"
        ></div>
        <slot />
      </div>
    </canvas>
    <canvas ref="outputRef" class="rain-output" aria-hidden="true" />
  </div>
</template>

<style scoped>
/* Mirrors the flex behaviour of the element this wraps, so slotting it in does
   not disturb the surrounding layout. */
.rain-host {
  position: relative;
  /* Own stacking context, so the output canvas's z-index stays internal and can
     never paint over floating UI that lives outside what we wrap. */
  isolation: isolate;
  display: flex;
  flex: 1;
  min-width: 0;
  min-height: 0;
}

.rain-source {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  max-width: none;
}

.rain-content {
  position: relative;
  display: flex;
  width: 100%;
  height: 100%;
  min-width: 0;
  min-height: 0;
  /* The refracted image IS the captured subtree — the shader takes its alpha
     straight from the content texture, so wherever the capture is transparent
     nothing is drawn at all. What we wrap normally inherits its background from
     an ancestor further up, which is outside the capture, leaving most of the
     area empty and the rain visible only over the cards themselves. Painting
     the background inside the wrapper makes the whole region opaque, and so
     refractable, without changing how it looks. */
  /* Base colour plus a flat dark wash, so the empty board reads as overcast.
     Sits under the rain layer and behind the cards, which keep their own
     colour. */
  background-color: var(--background);
  background-image: linear-gradient(rgba(6, 10, 16, 0.26), rgba(6, 10, 16, 0.26));
}

.rain-backdrop {
  position: absolute;
  inset: 0;
  overflow: hidden;
  pointer-events: none;
  z-index: 0;
}

/* Two layers at different sizes and speeds so the rain has depth.
   The tiles hold vertical streaks; the slant is a constant rotate(). The fall
   is animated on background-position, NOT on a transform: translating the box
   downwards drags its own top edge into view part-way through each cycle, which
   shows up as empty space popping in at the top corner the rotation swings
   lowest. With the box static that cannot happen, and shifting the background
   by exactly one tile height still loops without a seam.
   The inset only has to cover what the rotation swings in. */
.rain-backdrop::before,
.rain-backdrop::after {
  content: '';
  position: absolute;
  inset: -45% -35%;
  background-repeat: repeat;
  will-change: background-position;
}

.rain-backdrop::before {
  background-image: var(--rain-near);
  background-size: 360px 360px;
  transform: rotate(9deg);
  animation: rain-near 0.75s linear infinite;
}

.rain-backdrop::after {
  background-image: var(--rain-far);
  background-size: 300px 300px;
  transform: rotate(6deg);
  animation: rain-far 1.15s linear infinite;
}

@keyframes rain-near {
  from { background-position: 0 0; }
  to { background-position: 0 360px; }
}

@keyframes rain-far {
  from { background-position: 0 0; }
  to { background-position: 0 300px; }
}

.rain-output {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  max-width: none;
  pointer-events: none;
  /* Only needs to beat .rain-source, its earlier sibling. */
  z-index: 1;
}
</style>

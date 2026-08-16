<script lang="ts" setup>
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { createFlameWrap, type FlameWrapInstance, type FlameWrapOptions } from '@/arkham/flameWrap'

export interface Props {
  /**
   * The element to set on fire. Only its bounding box is read — the flames are
   * drawn on an overlay canvas above it, nothing about the element changes.
   * Defaults to the overlay's parent.
   */
  target?: HTMLElement | null
  options?: FlameWrapOptions
}

const props = defineProps<Props>()

const output = ref<HTMLCanvasElement | null>(null)
// A canvas whose context could not be created renders as Chrome's "sad canvas"
// placeholder, which would sit on the board looking like a broken card. Hide it
// instead and let the location render normally, just without fire.
const failed = ref(false)
let instance: FlameWrapInstance | null = null
let attachedTo: HTMLElement | null = null

// The flames reach well above the element and glow a little on the other three
// sides, so the canvas has to be bigger than what it is burning. Same sizing as
// upstream's React wrapper.
const reach = computed(() => Math.round(Math.max(props.options?.height ?? 170, 24) * 1.5) + 40)
const glow = computed(() => Math.round(Math.max(props.options?.spread ?? 8, 8) * 3) + 16)

function teardown() {
  instance?.destroy()
  instance = null
  attachedTo = null
}

function setup() {
  const canvas = output.value
  const content = props.target ?? canvas?.parentElement ?? null
  if (!canvas || !content) return
  // Idempotent: mount and the target watcher both call this, and a WebGL
  // context is far too expensive to throw away and rebuild for nothing.
  if (instance && attachedTo === content) return
  teardown()
  attachedTo = content
  instance = createFlameWrap({ content, output: canvas }, props.options ?? {})
  failed.value = !instance
}

onMounted(setup)
// The target is usually a template ref in the parent, which may not have
// resolved by the time we mount.
watch(() => props.target, setup, { flush: 'post' })
watch(() => props.options, (options) => instance?.setOptions(options ?? {}), { deep: true })

onBeforeUnmount(teardown)
</script>

<template>
  <canvas
    ref="output"
    class="flame-wrap"
    aria-hidden="true"
    :style="{
      '--flame-reach': `${reach}px`,
      '--flame-glow': `${glow}px`,
      display: failed ? 'none' : undefined,
    }"
  />
</template>

<style scoped>
.flame-wrap {
  position: absolute;
  top: calc(-1 * var(--flame-reach));
  right: calc(-1 * var(--flame-glow));
  bottom: calc(-1 * var(--flame-glow));
  left: calc(-1 * var(--flame-glow));
  width: calc(100% + var(--flame-glow) * 2);
  height: calc(100% + var(--flame-reach) + var(--flame-glow));
  max-width: none;
  pointer-events: none;
}
</style>

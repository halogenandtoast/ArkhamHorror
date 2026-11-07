import { computed, ref } from 'vue'

/* True while the FX layer is playing something. Shared on purpose: the layer
 * lives inside the scenario, the autopilot in the game view, and the autopilot
 * must not answer the next prompt over an animation the player is still reading. */
const running = ref(0)

export const fxBusy = computed(() => running.value > 0)

export function holdFxBusy(ms: number) {
  running.value += 1
  setTimeout(() => {
    running.value = Math.max(0, running.value - 1)
  }, ms)
}

<script lang="ts" setup>
import { ref, computed, onMounted, onBeforeUnmount } from 'vue'
import { TokenFace, chaosTokenImage } from '@/arkham/types/ChaosToken'

const props = defineProps<{ from: TokenFace, to: TokenFace }>()

const face = ref<TokenFace>(props.from)
const flipping = ref(false)
const image = computed(() => chaosTokenImage(face.value))

const timers: number[] = []
const after = (ms: number, fn: () => void) => { timers.push(window.setTimeout(fn, ms)) }

onMounted(() => {
  // Hold on the original face for a beat, then flip. The face is swapped at the
  // midpoint of the flip (while the token is edge-on) so it reads as a single
  // token turning over to reveal its lower value.
  after(900, () => {
    flipping.value = true
    after(225, () => { face.value = props.to })
    after(450, () => { flipping.value = false })
  })
})

onBeforeUnmount(() => timers.forEach(window.clearTimeout))
</script>

<template>
  <div class="chaos-token-morph">
    <img class="token" :class="{ flipping }" :src="image" />
  </div>
</template>

<style scoped>
.chaos-token-morph {
  display: inline-block;
  perspective: 600px;
  margin: 0 0.25em;
}

.token {
  width: 140px;
  height: 140px;
  object-fit: contain;
  transform-style: preserve-3d;
}

@keyframes token-coin-flip {
  0% { transform: rotateY(0deg); }
  50% { transform: rotateY(90deg); }
  100% { transform: rotateY(0deg); }
}

.token.flipping {
  animation: token-coin-flip 0.45s ease-in-out;
}
</style>

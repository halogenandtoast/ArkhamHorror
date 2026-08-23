<script lang="ts" setup>
import { computed, ref } from 'vue'
import Token from '@/arkham/components/Token.vue'
import { ChaosToken } from '@/arkham/types/ChaosToken'
import { Game } from '@/arkham/types/Game'

const props = defineProps<{
  tokens: ChaosToken[]
  game: Game
  playerId: string
}>()

const emit = defineEmits<{ choose: [value: number] }>()

type SealedChaosTokenLayout = {
  positions: Array<{ '--sealed-x': string; '--sealed-y': string }>
  width: number
  height: number
  shapePath: string
}

function tokenShapePath(points: Array<[number, number]>, closed: boolean) {
  if (points.length === 0) return ''
  if (points.length === 1) {
    const [[x, y]] = points
    return `M ${x - 1} ${y} a 1 1 0 1 0 2 0 a 1 1 0 1 0 -2 0`
  }

  return `M ${points.map(([x, y]) => `${x} ${y}`).join(' L ')}${closed ? ' Z' : ''}`
}

const layout = computed<SealedChaosTokenLayout>(() => {
  const n = props.tokens.length
  const tokenSize = 20
  const padding = 18
  const margin = padding / 2
  const step = 26
  if (n <= 0) return { positions: [], width: tokenSize, height: tokenSize, shapePath: '' }

  let points: Array<[number, number]>
  let outline: Array<[number, number]>
  let closed = true

  if (n === 1) {
    points = [[0, 0]]
    outline = points
    closed = false
  } else if (n === 2) {
    points = [[0, 0], [step, 0]]
    outline = points
    closed = false
  } else if (n === 3) {
    points = [[step / 2, 0], [0, step], [step, step]]
    outline = points
  } else if (n === 4) {
    points = [[0, 0], [step, 0], [0, step], [step, step]]
    outline = [[0, 0], [step, 0], [step, step], [0, step]]
  } else {
    const outerCount = n >= 7 ? n - 1 : n
    const radius = step
    const center = radius
    const outer = Array.from({ length: outerCount }, (_, index): [number, number] => {
      const angle = -Math.PI / 2 + (2 * Math.PI * index) / outerCount
      return [center + radius * Math.cos(angle), center + radius * Math.sin(angle)]
    })

    points = n >= 7 ? [[center, center], ...outer] : outer
    outline = outer
  }

  const minX = Math.min(...points.map(([x]) => x))
  const minY = Math.min(...points.map(([, y]) => y))
  const maxX = Math.max(...points.map(([x]) => x))
  const maxY = Math.max(...points.map(([, y]) => y))
  const positions = points.map(([x, y]) => ({
    '--sealed-x': `${x - minX + margin}px`,
    '--sealed-y': `${y - minY + margin}px`,
  }))

  const width = maxX - minX + tokenSize + padding
  const height = maxY - minY + tokenSize + padding
  const shapePoints = outline.map(([x, y]) => [x - minX + margin + tokenSize / 2, y - minY + margin + tokenSize / 2] as [number, number])

  return {
    positions,
    width,
    height,
    shapePath: tokenShapePath(shapePoints, closed),
  }
})

const positions = computed(() => layout.value.positions)

const spreadStyle = computed(() => ({
  '--sealed-count': props.tokens.length,
  '--sealed-bg-width': `${layout.value.width}px`,
  '--sealed-bg-height': `${layout.value.height}px`,
  '--sealed-bg-collapsed-scale': `${Math.min(1, 20 / Math.max(layout.value.width, layout.value.height))}`,
}))
const shapePath = computed(() => layout.value.shapePath)
const expanded = ref(false)
</script>

<template>
  <div
    v-if="tokens.length > 0"
    class="sealed-chaos-tokens no-card-overlay"
    :class="{ 'sealed-chaos-tokens--expanded': expanded }"
    :style="spreadStyle"
    @mouseleave="expanded = false"
  >
    <svg
      class="sealed-chaos-token-bg"
      :viewBox="`0 0 ${layout.width} ${layout.height}`"
      aria-hidden="true"
    >
      <path class="sealed-chaos-token-bg-border" :d="shapePath" />
      <path class="sealed-chaos-token-bg-fill" :d="shapePath" />
    </svg>
    <Token
      v-for="(sealedToken, index) in tokens"
      :key="index"
      :token="sealedToken"
      :playerId="playerId"
      :game="game"
      @choose="emit('choose', $event)"
      class="sealed sealed-token"
      :style="{ '--sealed-index': index, ...positions[index] }"
      @mouseenter="expanded = true"
    />
  </div>
</template>

<style scoped>
.sealed-chaos-tokens {
  --sealed-token-width: 20px;
  --sealed-token-peek: 4px;
  position: relative;
  width: var(--sealed-token-width);
  height: 30px;
  pointer-events: auto;
  overflow: visible;
  isolation: isolate;
  z-index: var(--z-index-4);
}

.sealed-chaos-token-bg {
  position: absolute;
  top: 0;
  left: 0;
  width: var(--sealed-bg-width);
  height: var(--sealed-bg-height);
  max-width: none;
  opacity: 0;
  transform: scale(var(--sealed-bg-collapsed-scale));
  transform-origin: top left;
  transition: opacity 0.08s ease, transform 0.16s ease;
  pointer-events: none;
  z-index: 0;
  overflow: visible;
}

.sealed-chaos-token-bg path {
  fill: rgba(0, 0, 0, 0.68);
  stroke-linecap: round;
  stroke-linejoin: round;
  filter: drop-shadow(0 4px 12px rgba(0, 0, 0, 0.3));
}

.sealed-chaos-token-bg-border {
  stroke: rgba(255, 255, 255, 0.32);
  stroke-width: 39;
}

.sealed-chaos-token-bg-fill {
  stroke: rgba(0, 0, 0, 0.68);
  stroke-width: 36;
}

.sealed-chaos-tokens--expanded {
  z-index: var(--z-index-30000);
}

.sealed-chaos-tokens--expanded .sealed-chaos-token-bg {
  opacity: 1;
  pointer-events: auto;
  transform: scale(1);
}

.sealed-chaos-tokens .sealed-token {
  position: absolute;
  left: 0;
  top: 0;
  width: var(--sealed-token-width);
  z-index: calc(1 + var(--sealed-index));
  transform: translateX(calc(var(--sealed-index) * var(--sealed-token-peek)));
  transition: transform 0.16s ease;
}

/* Size the token art here rather than relying on the host card's pool rules,
   so the group looks the same wherever it is mounted. */
.sealed-chaos-tokens .sealed-token :deep(img.token) {
  width: var(--sealed-token-image-width, var(--card-token-width, 25px));
  height: auto;
}

.sealed-chaos-tokens.sealed-chaos-tokens--expanded .sealed-token {
  transform: translate(var(--sealed-x), var(--sealed-y));
}
</style>

<script setup lang="ts">
import { computed } from 'vue'

type Point = { x: number; y: number }

const props = withDefaults(defineProps<{
  data: number[]
  width?: number
  height?: number
  stroke?: string
  strokeWidth?: number
  fill?: string | null
  smooth?: boolean
  showDots?: boolean
  showDelta?: boolean
  formatDelta?: (n: number) => string
  ariaLabel?: string
}>(), {
  width: 280,
  height: 64,
  stroke: '#7dd3fc',
  strokeWidth: 2,
  fill: 'rgba(125, 211, 252, 0.15)',
  smooth: true,
  showDots: false,
  showDelta: true,
  ariaLabel: 'sparkline'
})

// unique gradient id
const uid = `grad-${Math.random().toString(36).slice(2, 8)}`
const viewBox = computed(() => `0 0 ${props.width} ${props.height}`)

const minMax = computed(() => {
  if (!props.data.length) return { min: 0, max: 0 }
  let min = Math.min(...props.data)
  let max = Math.max(...props.data)
  if (min === max) { min -= 1; max += 1 }
  return { min, max }
})

const points = computed<Point[]>(() => {
  const { width, height, data } = props
  const { min, max } = minMax.value
  if (!data.length) return []
  const xStep = data.length <= 1 ? width : width / (data.length - 1)
  const scaleY = (v: number) => height - ((v - min) / (max - min)) * height
  return data.map((v, i) => ({ x: i * xStep, y: scaleY(v) }))
})

function pathLinear(pts: Point[]): string {
  if (!pts.length) return ''
  return 'M ' + pts.map(p => `${p.x} ${p.y}`).join(' L ')
}

function pathSmooth(pts: Point[]): string {
  if (pts.length < 2) return pathLinear(pts)
  const d: string[] = []
  d.push(`M ${pts[0].x} ${pts[0].y}`)
  for (let i = 0; i < pts.length - 1; i++) {
    const p0 = i > 0 ? pts[i - 1] : pts[i]
    const p1 = pts[i]
    const p2 = pts[i + 1]
    const p3 = i !== pts.length - 2 ? pts[i + 2] : p2
    const t = 0.5
    const c1x = p1.x + (p2.x - p0.x) * t / 6
    const c1y = p1.y + (p2.y - p0.y) * t / 6
    const c2x = p2.x - (p3.x - p1.x) * t / 6
    const c2y = p2.y - (p3.y - p1.y) * t / 6
    d.push(`C ${c1x} ${c1y}, ${c2x} ${c2y}, ${p2.x} ${p2.y}`)
  }
  return d.join(' ')
}

const linePath = computed(() =>
  props.smooth ? pathSmooth(points.value) : pathLinear(points.value)
)

const areaPath = computed(() => {
  if (!props.fill || !points.value.length) return null
  const first = `M ${points.value[0].x} ${points.value[0].y}`
  const line = props.smooth
    ? pathSmooth(points.value).replace(/^M[^C]+/, first)
    : pathLinear(points.value)
  const last = points.value.at(-1)!
  return `${line} L ${last.x} ${props.height} L ${points.value[0].x} ${props.height} Z`
})

const delta = computed(() => {
  if (props.data.length < 2) return 0
  return props.data.at(-1)! - props.data[0]
})

const deltaText = computed(() => {
  const d = delta.value
  const fmt = props.formatDelta ?? ((n: number) => (n > 0 ? '+' : '') + n.toFixed(1))
  return fmt(d)
})

const isUp = computed(() => delta.value >= 0)
</script>

<template>
  <div class="sparkline" :aria-label="ariaLabel">
    <svg :viewBox="viewBox" :width="width" :height="height" role="img">
      <defs>
        <!-- linearGradient is declared here -->
        <linearGradient :id="uid" x1="0" y1="0" x2="0" y2="1">
          <stop offset="0%" :stop-color="stroke" stop-opacity="0.35" />
          <stop offset="100%" :stop-color="stroke" stop-opacity="0.02" />
        </linearGradient>
      </defs>

      <path v-if="areaPath" :d="areaPath" :fill="`url(#${uid})`" />

      <path :d="linePath" :stroke="stroke" :stroke-width="strokeWidth"
            fill="none" stroke-linecap="round" />

      <g v-if="showDots">
        <circle v-for="(p,i) in points" :key="i" :cx="p.x" :cy="p.y"
                r="2.5" :fill="stroke" />
      </g>
    </svg>

    <div v-if="showDelta" class="delta" :class="{ up: isUp, down: !isUp }">
      <span>{{ deltaText }}</span>
      <svg viewBox="0 0 24 24" width="14" height="14">
        <path v-if="isUp" d="M12 5l7 7h-4v7H9v-7H5z" fill="currentColor"/>
        <path v-else d="M12 19l-7-7h4V5h6v7h4z" fill="currentColor"/>
      </svg>
    </div>
  </div>
</template>

<style scoped>
.sparkline { position: relative; width: 100%; }
svg { display: block; width: 100%; height: auto; }
.delta {
  position: absolute; right: 6px; top: 6px;
  display: flex; align-items: center; gap: 4px;
  padding: 3px 7px; border-radius: 999px;
  font-size: 12px; font-weight: 600;
  background: rgba(0,0,0,.4); color: #fca5a5;
}
.delta.up { color: #86efac; }
</style>

<script lang="ts" setup>
import { computed, ref, nextTick, onUnmounted } from 'vue'
import PoolItem from '@/arkham/components/PoolItem.vue'
import { type Token, type Tokens } from '@/arkham/types/Token'

// Unique per TokenPool instance, so view-transition-names never collide between
// different cards' pools.
let tokenPoolUidCounter = 0
const uid = tokenPoolUidCounter++
const vtName = (key: string | number) =>
  `tp-${uid}-${String(key).replace(/[^a-zA-Z0-9_-]/g, '-')}`

export type TokenPoolItem = {
  key: string
  type: string
  amount?: number
  tooltip?: string
  class?: unknown
  force?: boolean
}

export type TokenPoolOverride = {
  type?: string
  tooltip?: string
  class?: unknown
  force?: boolean
}

const TOKEN_CONFIG: Partial<Record<Token, { type: string; tooltip?: string }>> = {
  AlarmLevel: { type: 'doom', tooltip: 'Alarm Level' },
  Clue: { type: 'clue' },
  Damage: { type: 'health' },
  Doom: { type: 'doom' },
  Horror: { type: 'horror' },
  Resource: { type: 'resource' },
  Pillar: { type: 'resource' },
  Kindling: { type: 'resource' },
  Leyline: { type: 'resource', tooltip: 'Leyline' },
  Shard: { type: 'resource', tooltip: 'Shard' },
  ScoutingReport: { type: 'resource', tooltip: 'Scouting Report' },
  Scrap: { type: 'resource', tooltip: 'Scrap' },
  Depletion: { type: 'resource', tooltip: 'Scouting Report' },
  Antiquity: { type: 'resource', tooltip: 'Antiquity' },
  Civilian: { type: 'resource', tooltip: 'Civilian' },
  Study: { type: 'resource', tooltip: 'Civilian' },
  Target: { type: 'resource', tooltip: 'Target' },
  Time: { type: 'resource', tooltip: 'Time' },
  Newspaper: { type: 'resource', tooltip: 'Newspaper' },
  Shipment: { type: 'resource', tooltip: 'Shipment' },
  Seed: { type: 'resource', tooltip: 'Seed' },
  TimeCapsule: { type: 'resource', tooltip: 'Time Capsule' },
  Seal: { type: 'resource', tooltip: 'Seal' },
  Depth: { type: 'resource' },
  LostSoul: { type: 'resource' },
  Overgrowth: { type: 'resource' },
  Bounty: { type: 'resource' },
  Evidence: { type: 'resource', tooltip: 'Evidence' },
  Warning: { type: 'resource', tooltip: 'Warning' },
  Memory: { type: 'resource', tooltip: 'Memory' },
  Brilliance: { type: 'resource', tooltip: 'Brilliance' },
  Charge: { type: 'resource' },
}

const props = withDefaults(defineProps<{
  tokens?: Tokens
  order?: readonly Token[]
  overrides?: Partial<Record<Token, TokenPoolOverride>>
  extraItems?: readonly TokenPoolItem[]
}>(), {
  tokens: () => ({}),
  overrides: () => ({}),
  extraItems: () => [],
})

const emit = defineEmits<{ choose: [key: string] }>()

const tokenKeys = computed<Token[]>(() => {
  if (props.order) return [...props.order]

  const keys = Object.keys(props.tokens) as Token[]
  for (const key of Object.keys(props.overrides) as Token[]) {
    if (!keys.includes(key)) keys.push(key)
  }
  return keys
})

const tokenItems = computed<TokenPoolItem[]>(() => tokenKeys.value.flatMap((token) => {
  const amount = props.tokens[token] ?? 0
  const config = TOKEN_CONFIG[token] ?? { type: 'resource', tooltip: token }
  const override = props.overrides[token] ?? {}
  const force = override.force ?? false
  if (!force && amount <= 0) return []

  return [{
    key: token,
    type: override.type ?? config.type,
    tooltip: override.tooltip ?? config.tooltip,
    class: override.class,
    amount,
    force,
  }]
}))

const items = computed(() => [
  ...props.extraItems.filter((item) => item.force || (item.amount ?? 0) > 0),
  ...tokenItems.value,
])

// When there are more tokens than fit comfortably, clump them into an
// overlapping stack and fan them out into an auto-orienting shape on hover —
// mirroring the sealed-chaos-token popover.
const CLUMP_THRESHOLD = 2
const clumped = computed(() => items.value.length > CLUMP_THRESHOLD)
// Keep exactly-two-token pools side by side (the parent .pool wraps, which can
// stack them vertically on narrow asset cards).
const pairRow = computed(() => !clumped.value && items.value.length === 2)
const expanded = ref(false)

type ClumpLayout = {
  positions: Array<{ '--token-x': string; '--token-y': string }>
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

const clumpLayout = computed<ClumpLayout>(() => {
  const n = items.value.length
  const tokenSize = 24
  const padding = 18
  const margin = padding / 2
  const step = 31
  if (n <= 0) return { positions: [], width: tokenSize, height: tokenSize, shapePath: '' }

  let points: Array<[number, number]>
  let outline: Array<[number, number]>
  let closed = true

  if (n === 1) {
    points = [[0, 0]]; outline = points; closed = false
  } else if (n === 2) {
    points = [[0, 0], [step, 0]]; outline = points; closed = false
  } else if (n === 3) {
    points = [[step / 2, 0], [0, step], [step, step]]; outline = points
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
    '--token-x': `${x - minX + margin}px`,
    '--token-y': `${y - minY + margin}px`,
  }))
  const width = maxX - minX + tokenSize + padding
  const height = maxY - minY + tokenSize + padding
  const shapePoints = outline.map(([x, y]) => [x - minX + margin + tokenSize / 2, y - minY + margin + tokenSize / 2] as [number, number])

  return { positions, width, height, shapePath: tokenShapePath(shapePoints, closed) }
})

// The expanded fan is teleported to <body> and positioned centered over the
// collapsed stack, so it is never clipped by the scrollable player area.
const anchorEl = ref<HTMLElement | null>(null)
const overlayEl = ref<HTMLElement | null>(null)
const overlayStyle = ref<Record<string, string>>({})
let collapseTimer: ReturnType<typeof setTimeout> | null = null
let pointerMoveHandler: ((e: PointerEvent) => void) | null = null

// Toggle `expanded` inside a view transition so each token (which shares a
// view-transition-name between its collapsed in-flow copy and its fanned overlay
// copy) appears to glide into position. The in-flow tokens are hidden while
// expanded so the names aren't duplicated.
function setExpanded(open: boolean) {
  const startViewTransition = (document as Document & {
    startViewTransition?: (cb: () => Promise<void> | void) => void
  }).startViewTransition
  if (typeof startViewTransition === 'function') {
    startViewTransition.call(document, async () => { expanded.value = open; await nextTick() })
  } else {
    expanded.value = open
  }
}

function rectContains(r: DOMRect | undefined, x: number, y: number, pad = 6) {
  return !!r && x >= r.left - pad && x <= r.right + pad && y >= r.top - pad && y <= r.bottom + pad
}

// Collapse is driven by the actual pointer position against the anchor/overlay
// rects rather than mouseenter/leave. Those events are unreliable here: the
// teleported overlay pops up under a stationary cursor and the view transition
// briefly muddles hit-testing, which caused expand/collapse to flicker.
function pointerWithin(x: number, y: number) {
  return (
    rectContains(anchorEl.value?.getBoundingClientRect(), x, y) ||
    rectContains(overlayEl.value?.getBoundingClientRect(), x, y)
  )
}

function attachPointerTracking() {
  if (pointerMoveHandler) return
  pointerMoveHandler = (e: PointerEvent) => {
    if (pointerWithin(e.clientX, e.clientY)) {
      if (collapseTimer) { clearTimeout(collapseTimer); collapseTimer = null }
    } else if (!collapseTimer) {
      collapseTimer = setTimeout(() => {
        collapseTimer = null
        setExpanded(false)
        detachPointerTracking()
      }, 120)
    }
  }
  window.addEventListener('pointermove', pointerMoveHandler, { passive: true })
}

function detachPointerTracking() {
  if (pointerMoveHandler) {
    window.removeEventListener('pointermove', pointerMoveHandler)
    pointerMoveHandler = null
  }
}

function onEnter() {
  if (!clumped.value || expanded.value) return
  const el = anchorEl.value
  if (el) {
    const r = el.getBoundingClientRect()
    const cx = r.left + r.width / 2
    const cy = r.top + r.height / 2
    const { width, height } = clumpLayout.value
    overlayStyle.value = {
      left: `${cx - width / 2}px`,
      top: `${cy - height / 2}px`,
      width: `${width}px`,
      height: `${height}px`,
    }
  }
  setExpanded(true)
  attachPointerTracking()
}

onUnmounted(() => {
  if (collapseTimer) clearTimeout(collapseTimer)
  detachPointerTracking()
})
</script>

<template>
  <!-- In-flow: normal layout when few tokens, or a compact peeking stack when
       clumped. This is the hover anchor. -->
  <div
    class="token-pool"
    :class="{ 'token-pool--clumped': clumped, 'token-pool--row': pairRow }"
    :style="clumped ? { '--token-count': items.length } : undefined"
    ref="anchorEl"
    @mouseenter="onEnter"
  >
    <PoolItem
      v-for="(item, i) in items"
      :key="item.key"
      :type="item.type"
      :amount="item.amount"
      :tooltip="item.tooltip"
      :class="item.class"
      :style="clumped ? { '--token-index': i, viewTransitionName: expanded ? undefined : vtName(item.key), display: expanded ? 'none' : undefined } : undefined"
      @choose="emit('choose', item.key)"
    />
  </div>

  <!-- Expanded fan, teleported to the top level so the scrollable player area
       cannot clip it. Each token shares its in-flow copy's view-transition-name
       so it appears to glide from the stack into the fan. -->
  <Teleport to="body">
    <div
      v-if="clumped && expanded"
      ref="overlayEl"
      class="token-pool-overlay"
      :style="overlayStyle"
    >
      <svg
        class="token-pool-bg"
        :viewBox="`0 0 ${clumpLayout.width} ${clumpLayout.height}`"
        aria-hidden="true"
      >
        <path class="token-pool-bg-border" :d="clumpLayout.shapePath" />
        <path class="token-pool-bg-fill" :d="clumpLayout.shapePath" />
      </svg>
      <PoolItem
        v-for="(item, i) in items"
        :key="item.key"
        :type="item.type"
        :amount="item.amount"
        :tooltip="item.tooltip"
        :class="item.class"
        :style="{ ...clumpLayout.positions[i], viewTransitionName: vtName(item.key) }"
        @choose="emit('choose', item.key)"
      />
    </div>
  </Teleport>
</template>

<style scoped>
/* Not clumped: pass tokens straight into the parent .pool flex layout. */
.token-pool {
  display: contents;
}

/* Exactly two tokens: keep them on one horizontal line. */
.token-pool--row {
  display: inline-flex;
  flex-wrap: nowrap;
  align-items: center;
}

/* Clumped (collapsed): a compact peeking stack that acts as the hover anchor.
   The box spans the whole stack (incl. the peek) and keeps its size even when
   the tokens are hidden on expand, so the cursor never falls off the anchor. */
.token-pool--clumped {
  display: block;
  position: relative;
  width: calc(var(--card-token-width) + (var(--token-count, 1) - 1) * 5px);
  height: var(--card-token-width);
  pointer-events: auto;
  overflow: visible;
}

.token-pool--clumped :deep(.poolItem) {
  position: absolute;
  top: 0;
  left: 0;
  z-index: calc(1 + var(--token-index));
  transform: translateX(calc(var(--token-index) * 5px));
}
</style>

<style>
/* Teleported to <body>, so these are intentionally global (not scoped). */
.token-pool-overlay {
  position: fixed;
  z-index: var(--z-index-30000, 30000);
  pointer-events: auto;
  isolation: isolate;
}

.token-pool-overlay .token-pool-bg {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  max-width: none;
  pointer-events: none;
  overflow: visible;
  z-index: 0;
}

.token-pool-overlay .token-pool-bg path {
  fill: rgba(0, 0, 0, 0.72);
  stroke-linecap: round;
  stroke-linejoin: round;
  filter: drop-shadow(0 4px 12px rgba(0, 0, 0, 0.35));
}

.token-pool-overlay .token-pool-bg-border {
  stroke: rgba(255, 255, 255, 0.34);
  stroke-width: 46;
}

.token-pool-overlay .token-pool-bg-fill {
  stroke: rgba(0, 0, 0, 0.72);
  stroke-width: 42;
}

.token-pool-overlay .poolItem {
  position: absolute;
  top: 0;
  left: 0;
  transform: translate(var(--token-x), var(--token-y));
}

/* The health (damage) token art sits a touch right; nudge it left to balance. */
.token-pool-overlay .poolItem-health {
  left: -3px;
}

/* The doom token is dark and blends into the blob; trace it with a faint white
   outline so it stays visible. */
.token-pool-overlay .poolItem-doom img {
  filter:
    drop-shadow(1px 0 0 rgba(255, 255, 255, 0.25))
    drop-shadow(-1px 0 0 rgba(255, 255, 255, 0.25))
    drop-shadow(0 1px 0 rgba(255, 255, 255, 0.25))
    drop-shadow(0 -1px 0 rgba(255, 255, 255, 0.25));
}

.token-pool-overlay .poolItem img {
  width: var(--card-token-width, 25px) !important;
  height: auto !important;
}

.token-pool-overlay .poolItem span {
  font-size: calc(var(--card-token-width, 25px) * 0.62) !important;
}
</style>

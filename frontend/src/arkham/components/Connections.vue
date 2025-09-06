<script lang="ts" setup>
import { onMounted, onBeforeUnmount, computed, ref, nextTick, watch } from 'vue'
import type {Game} from '@/arkham/types/Game'

export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()
const locations = computed(() =>
  Object.values(props.game.locations).filter(a => a.inFrontOf === null && a.label !== 'cosmos')
)
const sortByDataId = (a: HTMLElement, b: HTMLElement) => {
  const aId = a.dataset.id, bId = b.dataset.id
  if (!aId || !bId) return 0
  return aId < bId ? -1 : aId > bId ? 1 : 0
}
const toConnection = (div1: HTMLElement, div2: HTMLElement): string | undefined => {
  const [leftDiv, rightDiv] = [div1, div2].sort(sortByDataId)
  const { id: leftDivId } = leftDiv.dataset
  const { id: rightDivId } = rightDiv.dataset
  return leftDivId && rightDivId ? `${leftDivId}:${rightDivId}` : undefined
}

const svgRef = ref<SVGSVGElement | null>(null)
const protoRef = ref<SVGLineElement | null>(null)
let svgEl: SVGSVGElement | null = null
let lineProto: SVGLineElement | null = null

const EPS = 0.5
const close = (a: number, b: number) => Math.abs(a - b) < EPS
const linesByConn = new Map<string, SVGLineElement>()

function makeOrUpdateLine(div1: HTMLElement, div2: HTMLElement) {
  const [leftDiv, rightDiv] = [div1, div2].sort(sortByDataId)
  const leftDivId = leftDiv.dataset.id
  const rightDivId = rightDiv.dataset.id
  if (!leftDivId || !rightDivId || !svgEl || !lineProto) return

  const connection = `${leftDivId}:${rightDivId}`
  const svgRect = svgEl.getBoundingClientRect()
  const lRect = leftDiv.getBoundingClientRect()
  const rRect = rightDiv.getBoundingClientRect()

  const x1 = (lRect.left - svgRect.left) + (lRect.width / 2)
  const y1 = (lRect.top - svgRect.top) + (lRect.height / 2)
  const x2 = (rRect.left - svgRect.left) + (rRect.width / 2)
  const y2 = (rRect.top - svgRect.top) + (rRect.height / 2)

  const investigator = Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
  const activeLine =
    !!investigator &&
    (
      (leftDivId === investigator.location && investigator.connectedLocations.includes(rightDivId)) ||
      (rightDivId === investigator.location && investigator.connectedLocations.includes(leftDivId))
    )

  let line = linesByConn.get(connection)
  if (!line) {
    line = lineProto.cloneNode(true) as SVGLineElement
    line.classList.remove('original')
    line.classList.add('connection')
    // ensure no duplicate ids leak to the DOM
    line.removeAttribute('id')
    line.dataset.connection = connection
    svgEl.appendChild(line)
    linesByConn.set(connection, line)
  }

  const ex1 = Number(line.getAttribute('x1') ?? NaN)
  const ey1 = Number(line.getAttribute('y1') ?? NaN)
  const ex2 = Number(line.getAttribute('x2') ?? NaN)
  const ey2 = Number(line.getAttribute('y2') ?? NaN)
  if (!close(ex1, x1)) line.setAttribute('x1', String(x1))
  if (!close(ey1, y1)) line.setAttribute('y1', String(y1))
  if (!close(ex2, x2)) line.setAttribute('x2', String(x2))
  if (!close(ey2, y2)) line.setAttribute('y2', String(y2))

  if (activeLine) line.classList.add('active')
  else line.classList.remove('active')
}

function handleConnections() {
  if(!svgEl) return
  const live = new Set<string>()

  for (const location of locations.value) {
    const { id, connectedLocations } = location
    const connections = Array.isArray(connectedLocations)
      ? connectedLocations
      : Object.values(connectedLocations)

    const start = document.querySelector<HTMLElement>(`[data-id="${id}"]`)
    if (!start) continue

    for (const dst of connections) {
      const end = document.querySelector<HTMLElement>(`[data-id="${dst}"]`)
      if (!end) continue

      const conn = toConnection(start, end)
      if (!conn) continue

      if (location.modifiers?.some(m =>
        m.type?.tag === 'DoNotDrawConnection' &&
        conn === `${m.type.contents?.[0]}:${m.type.contents?.[1]}`
      )) continue

      live.add(conn)
      makeOrUpdateLine(start, end)
    }
  }

  for (const [conn, el] of linesByConn) {
    if (!live.has(conn)) {
      el.remove()
      linesByConn.delete(conn)
    }
  }
}

const requestId = ref<number | null>(null)
let lastTime = 0
const FRAME_MS = 1000 / 30 // 30fps

function tick(ts: number) {
  requestId.value = null
  if (ts - lastTime >= FRAME_MS) {
    lastTime = ts
    handleConnections()
  }
  requestId.value = window.requestAnimationFrame(tick)
}

onMounted(async () => {
  await nextTick() // ensure template is in DOM
  svgEl = svgRef.value
  lineProto = protoRef.value
  // first draw immediately so a cold refresh shows lines at once
  handleConnections()
  requestId.value = window.requestAnimationFrame(tick)
})

// keep lines fresh if the set of locations changes
watch(locations, ()=> { handleConnections() }, { flush: 'post' })

onBeforeUnmount(()=> {
  if(requestId.value !== null) cancelAnimationFrame(requestId.value)
  requestId.value = null
  for (const [,el] of linesByConn) el.remove()
  linesByConn.clear()
  svgEl = null
  lineProto = null
})
</script>

<template>
  <svg ref="svgRef" class="connections-svg">
    <line ref="protoRef" class="line original" stroke-dasharray="5, 5"/>
  </svg>
</template>

<style lang="scss" scoped>
.connections-svg{
  pointer-events: none;
  position: absolute;
  isolation: isolate;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index:-1;
  overflow: visible !important;
}

.line{
  stroke-width: 6px;
  stroke: rgba(255, 255, 255, 0.2);
}
.active{
  stroke: rgba(255, 255, 255, 0.7) !important;
}
</style>

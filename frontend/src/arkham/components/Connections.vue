<script lang="ts" setup>
import { onMounted, onBeforeUnmount, computed, ref, nextTick, watch } from 'vue'
import type {Game} from '@/arkham/types/Game'

export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()
const locations = computed(() =>
  Object.values(props.game.locations).filter(a => a.placement === null && a.label !== 'cosmos')
)


const enemies = computed(() =>
  Object.values(props.game.enemies).filter(a => a.asSelfLocation && a.placement.tag === "AtLocation")
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
const chevronProtoRef = ref<SVGPathElement | null>(null)
let svgEl: SVGSVGElement | null = null
let lineProto: SVGLineElement | null = null
let chevronProto: SVGPathElement | null = null

const EPS = 0.5
const close = (a: number, b: number) => Math.abs(a - b) < EPS
const linesByConn = new Map<string, SVGLineElement>()
const chevronsByConn = new Map<string, SVGPathElement>()

function makeOrUpdateLine(div1: HTMLElement, div2: HTMLElement, className?: string) {
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
    if (className) line.classList.add(className)
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

// Renders a one-way connection as a stream of filled chevron polygons
// (silhouette with V-notch back, like a chevron-right glyph) pointing from
// source to destination. Chevrons are placed in the visible band between the
// two cards (clipped to each card's actual bounding rect), with a fixed
// spacing so they never get stretched to land on the card edges.
const CHEVRON_SPACING = 10   // px between chevron centers along the line
const CHEVRON_LEN = 8        // along-axis depth (back of polygon to outer tip)
const CHEVRON_HEIGHT = 10    // total perpendicular height (wing tip to wing tip)
const CHEVRON_EDGE_PAD = 8   // extra px past each card edge before drawing
function makeOrUpdateChevrons(srcDiv: HTMLElement, dstDiv: HTMLElement, connection: string) {
  if (!svgEl || !chevronProto) return
  const svgRect = svgEl.getBoundingClientRect()
  const sRect = srcDiv.getBoundingClientRect()
  const dRect = dstDiv.getBoundingClientRect()

  const x1 = (sRect.left - svgRect.left) + (sRect.width / 2)
  const y1 = (sRect.top - svgRect.top) + (sRect.height / 2)
  const x2 = (dRect.left - svgRect.left) + (dRect.width / 2)
  const y2 = (dRect.top - svgRect.top) + (dRect.height / 2)

  const dx = x2 - x1
  const dy = y2 - y1
  const dist = Math.hypot(dx, dy)
  if (dist < 1) return
  const ux = dx / dist
  const uy = dy / dist
  const px = -uy
  const py = ux

  // Distance from each card center to where the center-line exits that card's
  // bounding rect, so chevrons start past the card art rather than under it.
  const exitDist = (halfW: number, halfH: number) => {
    const tx = Math.abs(ux) > 1e-6 ? halfW / Math.abs(ux) : Infinity
    const ty = Math.abs(uy) > 1e-6 ? halfH / Math.abs(uy) : Infinity
    return Math.min(tx, ty)
  }
  const startD = exitDist(sRect.width / 2, sRect.height / 2) + CHEVRON_EDGE_PAD
  const endD = dist - exitDist(dRect.width / 2, dRect.height / 2) - CHEVRON_EDGE_PAD
  const span = endD - startD
  if (span < 0) return // cards overlap or are flush

  // Fixed spacing, centered in the visible band — never stretches chevrons
  // to the boundary.
  const count = Math.max(1, Math.round(span / CHEVRON_SPACING) + 1)
  const usedSpan = (count - 1) * CHEVRON_SPACING
  const offset = (span - usedSpan) / 2
  const segments: string[] = []
  for (let i = 0; i < count; i++) {
    const d = startD + offset + i * CHEVRON_SPACING
    const cx = x1 + ux * d
    const cy = y1 + uy * d
    segments.push(chevronPath(cx, cy, ux, uy, px, py))
  }
  const pathD = segments.join(' ')

  const investigator = Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
  const activeLine =
    !!investigator &&
    srcDiv.dataset.id === investigator.location &&
    !!dstDiv.dataset.id &&
    investigator.connectedLocations.includes(dstDiv.dataset.id)

  let path = chevronsByConn.get(connection)
  if (!path) {
    path = chevronProto.cloneNode(true) as SVGPathElement
    path.classList.remove('original')
    path.classList.add('chevrons')
    path.removeAttribute('id')
    path.dataset.connection = connection
    svgEl.appendChild(path)
    chevronsByConn.set(connection, path)
  }

  if (path.getAttribute('d') !== pathD) path.setAttribute('d', pathD)
  if (activeLine) path.classList.add('active')
  else path.classList.remove('active')
}

function chevronPath(cx: number, cy: number, ux: number, uy: number, px: number, py: number): string {
  // Filled chevron polygon, 6 vertices, ratios pulled from the chevron-right
  // reference SVG. Going clockwise from the tip:
  //   F (tip) -> A (top wing) -> B (top outer back) -> C (notch tip)
  //   -> D (bottom outer back) -> E (bottom wing) -> close.
  const L = CHEVRON_LEN
  const H = CHEVRON_HEIGHT / 2
  // Local-to-world projection: lx along (ux,uy), ly perpendicular along (px,py).
  const toWorld = (lx: number, ly: number) =>
    `${(cx + lx * ux + ly * px).toFixed(1)},${(cy + lx * uy + ly * py).toFixed(1)}`
  const f = toWorld(L / 2, 0)
  const a = toWorld(-0.227 * L, -H)
  const b = toWorld(-L / 2, -0.625 * H)
  const c = toWorld(-0.045 * L, 0)
  const d = toWorld(-L / 2, 0.625 * H)
  const e = toWorld(-0.227 * L, H)
  return `M${f} L${a} L${b} L${c} L${d} L${e} Z`
}

function handleConnections() {
  if(!svgEl) return
  const live = new Set<string>()

  // Build directed edge set so we can detect one-way connections by absence of
  // the reverse edge. connectedLocations is symmetric for normal connections
  // but asymmetric when a location's connectedMatchers don't match back.
  const directed = new Set<string>()
  for (const loc of locations.value) {
    const cs = Array.isArray(loc.connectedLocations)
      ? loc.connectedLocations
      : Object.values(loc.connectedLocations)
    for (const dst of cs) directed.add(`${loc.id}->${dst}`)
  }

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

      const reverseExists = directed.has(`${dst}->${id}`)

      if (reverseExists) {
        const conn = toConnection(start, end)
        if (!conn) continue
        if (location.modifiers?.some(m =>
          m.type?.tag === 'DoNotDrawConnection' &&
          conn === `${m.type.contents?.[0]}:${m.type.contents?.[1]}`
        )) continue
        live.add(conn)
        makeOrUpdateLine(start, end)
      } else {
        const conn = `${id}->${dst}`
        if (location.modifiers?.some(m =>
          m.type?.tag === 'DoNotDrawConnection' &&
          (
            (m.type.contents?.[0] === id && m.type.contents?.[1] === dst) ||
            (m.type.contents?.[0] === dst && m.type.contents?.[1] === id)
          )
        )) continue
        live.add(conn)
        makeOrUpdateChevrons(start, end, conn)
      }
    }
  }

  for (const enemy of enemies.value) {
    const { id, placement } = enemy
    if (placement.tag !== "AtLocation") continue

    const start = document.querySelector<HTMLElement>(`[data-id="${id}"]`)
    if (!start) continue

    const end = document.querySelector<HTMLElement>(`[data-id="${placement.contents}"]`)
    if (!end) continue

    const conn = toConnection(start, end)
    if (!conn) continue

    live.add(conn)
    makeOrUpdateLine(start, end, "enemy-line")
  }

  for (const [conn, el] of linesByConn) {
    if (!live.has(conn)) {
      el.remove()
      linesByConn.delete(conn)
    }
  }
  for (const [conn, el] of chevronsByConn) {
    if (!live.has(conn)) {
      el.remove()
      chevronsByConn.delete(conn)
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
  chevronProto = chevronProtoRef.value
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
  for (const [,el] of chevronsByConn) el.remove()
  chevronsByConn.clear()
  svgEl = null
  lineProto = null
  chevronProto = null
})
</script>

<template>
  <svg ref="svgRef" class="connections-svg">
    <line ref="protoRef" class="line original" stroke-dasharray="5, 5"/>
    <path ref="chevronProtoRef" class="chevrons original"/>
  </svg>
</template>

<style scoped>
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
.line.active{
  stroke: rgba(255, 255, 255, 0.7) !important;
}

.chevrons{
  fill: rgba(255, 255, 255, 0.2);
  stroke: none;
}
.chevrons.active{
  fill: rgba(255, 255, 255, 0.7);
}

.enemy-line{
  stroke: rgba(255 0 0 / 0.4);
  stroke-dasharray: unset;
}
</style>

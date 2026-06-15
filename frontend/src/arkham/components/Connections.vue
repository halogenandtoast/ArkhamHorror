<script lang="ts" setup>
import { onMounted, onBeforeUnmount, computed, ref, nextTick, watch } from 'vue'
import type {Game} from '@/arkham/types/Game'

export interface Props {
  game: Game
  playerId: string
  enableCosmicEmissaryAnimation?: boolean
}

const props = defineProps<Props>()
const allLocations = computed(() => Object.values(props.game.locations))

const locations = computed(() =>
  allLocations.value.filter(a => a.placement === null && a.label !== 'cosmos')
)


const enemies = computed(() =>
  Object.values(props.game.enemies).filter(a => a.asSelfLocation && a.placement.tag === "AtLocation")
)

const fateOfTheValeEnemyLocations: Record<string, string> = {
  cosmicEmissaryPhantasm: 'mirrorNestLeft',
  cosmicEmissaryAbyss: 'mirrorNestTop',
  cosmicEmissaryBrilliance: 'mirrorNestBottom',
  cosmicEmissaryMiasma: 'mirrorNestRight',
}

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
let defsEl: SVGDefsElement | null = null
let lineProto: SVGLineElement | null = null
let chevronProto: SVGPathElement | null = null

const EPS = 0.5
const close = (a: number, b: number) => Math.abs(a - b) < EPS
const linesByConn = new Map<string, SVGLineElement>()
const chevronsByConn = new Map<string, SVGPathElement>()

type GridDirection = 'North' | 'East' | 'South' | 'West'

const mineCart = computed(() =>
  Object.values(props.game.assets).find((asset) => asset.cardCode === 'c10507' && asset.placement.tag === 'AtLocation')
)

const isWrittenInRockAct2 = computed(() =>
  (props.game.scenario?.id === 'c10501' || props.game.scenario?.id === 'c10502') &&
  Object.values(props.game.acts).some((act) => act.sequence.number === 2)
)

function mineCartDirection(): GridDirection {
  let degrees = 0
  const modifiers = mineCart.value?.modifiers ?? []
  for (let i = modifiers.length - 1; i >= 0; i--) {
    const t: any = modifiers[i]?.type
    if (t?.tag === 'UIModifier' && t?.contents?.tag === 'Rotated') {
      degrees = t.contents.contents
      break
    }
  }
  switch ((degrees + 360) % 360) {
    case 90: return 'South'
    case 180: return 'West'
    case 270: return 'North'
    default: return 'East'
  }
}

function locationInDirection(locationId: string, direction: GridDirection): string | null {
  const location = props.game.locations[locationId]
  const match = location?.label.match(/^pos(\d{2})(\d{2})$/)
  if (!match) return null

  let x = Number(match[1])
  let y = Number(match[2])
  switch (direction) {
    case 'North': y += 1; break
    case 'East': x += 1; break
    case 'South': y -= 1; break
    case 'West': x -= 1; break
  }

  const label = `pos${String(x).padStart(2, '0')}${String(y).padStart(2, '0')}`
  return Object.values(props.game.locations).find((loc) => loc.label === label)?.id ?? null
}

function connectionKey(id1: string, id2: string): string {
  const [left, right] = [id1, id2].sort()
  return `${left}:${right}`
}

function mineCartNextConnection(): string | null {
  const cart = mineCart.value
  if ((props.game.scenario?.id !== 'c10501' && props.game.scenario?.id !== 'c10502') || cart?.placement.tag !== 'AtLocation') {
    return null
  }

  const src = props.game.locations[cart.placement.contents]
  const dst = locationInDirection(cart.placement.contents, mineCartDirection())
  return dst && src?.connectedLocations.includes(dst) ? connectionKey(cart.placement.contents, dst) : null
}

function mineCartInvalidDirection(): { locationId: string; direction: GridDirection } | null {
  const cart = mineCart.value
  if (!isWrittenInRockAct2.value || cart?.placement.tag !== 'AtLocation') {
    return null
  }

  const direction = mineCartDirection()
  const src = props.game.locations[cart.placement.contents]
  const dst = locationInDirection(cart.placement.contents, direction)
  if (dst && src?.connectedLocations.includes(dst)) return null
  return { locationId: cart.placement.contents, direction }
}

function directionVector(direction: GridDirection): { x: number; y: number } {
  switch (direction) {
    case 'North': return { x: 0, y: -1 }
    case 'East': return { x: 1, y: 0 }
    case 'South': return { x: 0, y: 1 }
    case 'West': return { x: -1, y: 0 }
  }
}

function makeOrUpdateLine(div1: HTMLElement, div2: HTMLElement, className?: string, preserveDirection = false) {
  const [leftDiv, rightDiv] = preserveDirection ? [div1, div2] : [div1, div2].sort(sortByDataId)
  const leftDivId = leftDiv.dataset.id
  const rightDivId = rightDiv.dataset.id
  if (!leftDivId || !rightDivId || !svgEl || !lineProto) return

  const connection = `${leftDivId}:${rightDivId}`
  const svgRect = svgEl.getBoundingClientRect()
  const lRect = leftDiv.getBoundingClientRect()
  const rRect = rightDiv.getBoundingClientRect()

  const lCenterX = (lRect.left - svgRect.left) + (lRect.width / 2)
  const lCenterY = (lRect.top - svgRect.top) + (lRect.height / 2)
  const rCenterX = (rRect.left - svgRect.left) + (rRect.width / 2)
  const rCenterY = (rRect.top - svgRect.top) + (rRect.height / 2)
  const offsetTrackLine = !className && isWrittenInRockAct2.value
  const vertical = Math.abs(rCenterY - lCenterY) > Math.abs(rCenterX - lCenterX)
  const x1 = offsetTrackLine && vertical ? (lRect.left - svgRect.left) + (lRect.width * 0.78) : lCenterX
  const y1 = offsetTrackLine && !vertical ? (lRect.top - svgRect.top) + (lRect.height * 0.8) : lCenterY
  const x2 = offsetTrackLine && vertical ? (rRect.left - svgRect.left) + (rRect.width * 0.78) : rCenterX
  const y2 = offsetTrackLine && !vertical ? (rRect.top - svgRect.top) + (rRect.height * 0.8) : rCenterY

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

  if (!className && connection === mineCartNextConnection()) line.classList.add('mine-cart-next-line')
  else line.classList.remove('mine-cart-next-line')

  if (className === 'fate-of-the-vale-enemy-line') {
    if (props.enableCosmicEmissaryAnimation === false) {
      line.removeAttribute('style')
    } else {
      updateFateOfTheValeEnemyLineGradient(line, connection, x1, y1, x2, y2)
    }
  }

  if (activeLine) line.classList.add('active')
  else line.classList.remove('active')
}

function makeOrUpdateMineCartInvalidLine(locationDiv: HTMLElement, direction: GridDirection): string[] {
  if (!svgEl || !lineProto || !chevronProto) return []
  const locationId = locationDiv.dataset.id
  if (!locationId) return []

  const svgRect = svgEl.getBoundingClientRect()
  const rect = locationDiv.getBoundingClientRect()
  const { x: dx, y: dy } = directionVector(direction)
  const vertical = direction === 'North' || direction === 'South'
  const x1 = (rect.left - svgRect.left) + (vertical ? rect.width * 0.78 : rect.width / 2)
  const y1 = (rect.top - svgRect.top) + (vertical ? rect.height / 2 : rect.height * 0.8)
  const lineDistance = 65
  const xDistance = 76
  const x2 = x1 + dx * lineDistance
  const y2 = y1 + dy * lineDistance
  const xMarkCenter = x1 + dx * xDistance
  const yMarkCenter = y1 + dy * xDistance
  const lineConnection = `mine-cart-invalid-line:${locationId}:${direction}`
  const xConnection = `mine-cart-invalid-x:${locationId}:${direction}`

  let line = linesByConn.get(lineConnection)
  if (!line) {
    line = lineProto.cloneNode(true) as SVGLineElement
    line.classList.remove('original')
    line.classList.add('connection', 'mine-cart-invalid-line')
    line.removeAttribute('id')
    line.dataset.connection = lineConnection
    svgEl.appendChild(line)
    linesByConn.set(lineConnection, line)
  }

  line.setAttribute('x1', String(x1))
  line.setAttribute('y1', String(y1))
  line.setAttribute('x2', String(x2))
  line.setAttribute('y2', String(y2))

  let xMark = chevronsByConn.get(xConnection)
  if (!xMark) {
    xMark = chevronProto.cloneNode(true) as SVGPathElement
    xMark.classList.remove('original')
    xMark.classList.add('mine-cart-invalid-x')
    xMark.removeAttribute('id')
    xMark.dataset.connection = xConnection
    svgEl.appendChild(xMark)
    chevronsByConn.set(xConnection, xMark)
  }

  const size = 7
  const thickness = 3
  xMark.setAttribute('d', [
    `M${xMarkCenter - size},${yMarkCenter - size + thickness}`,
    `L${xMarkCenter - size + thickness},${yMarkCenter - size}`,
    `L${xMarkCenter + size},${yMarkCenter + size - thickness}`,
    `L${xMarkCenter + size - thickness},${yMarkCenter + size}`,
    'Z',
    `M${xMarkCenter + size - thickness},${yMarkCenter - size}`,
    `L${xMarkCenter + size},${yMarkCenter - size + thickness}`,
    `L${xMarkCenter - size + thickness},${yMarkCenter + size}`,
    `L${xMarkCenter - size},${yMarkCenter + size - thickness}`,
    'Z',
  ].join(' '))
  return [lineConnection, xConnection]
}

function updateFateOfTheValeEnemyLineGradient(line: SVGLineElement, connection: string, x1: number, y1: number, x2: number, y2: number) {
  if (!defsEl) return

  const gradientId = `fate-of-the-vale-enemy-line-gradient-${connection.replace(/[^a-zA-Z0-9_-]/g, '-')}`
  let gradient = defsEl.querySelector<SVGLinearGradientElement>(`#${gradientId}`)
  if (!gradient) {
    gradient = document.createElementNS('http://www.w3.org/2000/svg', 'linearGradient')
    gradient.id = gradientId
    gradient.setAttribute('gradientUnits', 'userSpaceOnUse')
    gradient.setAttribute('spreadMethod', 'repeat')
    gradient.innerHTML = `
      <stop offset="0%" stop-color="#88ADA4" stop-opacity="1" />
      <stop offset="25%" stop-color="#366672" stop-opacity="1" />
      <stop offset="50%" stop-color="#DDF2EB" stop-opacity="1" />
      <stop offset="75%" stop-color="#84CAC7" stop-opacity="1" />
      <stop offset="100%" stop-color="#88ADA4" stop-opacity="1" />
      <animateTransform attributeName="gradientTransform" type="translate" dur="7s" repeatCount="indefinite" />
    `
    defsEl.appendChild(gradient)
  }

  const dx = x2 - x1
  const dy = y2 - y1
  const dist = Math.hypot(dx, dy)
  if (dist < 1) return
  const ux = dx / dist
  const uy = dy / dist
  const patternLength = 96

  gradient.setAttribute('x1', String(x1))
  gradient.setAttribute('y1', String(y1))
  gradient.setAttribute('x2', String(x1 + ux * patternLength))
  gradient.setAttribute('y2', String(y1 + uy * patternLength))
  gradient.querySelector('animateTransform')?.setAttribute('from', '0 0')
  gradient.querySelector('animateTransform')?.setAttribute('to', `${ux * patternLength} ${uy * patternLength}`)
  line.setAttribute('stroke', `url(#${gradientId})`)
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

function handleConnections(includeFateOfTheVale = true) {
  if(!svgEl) return
  const live = new Set<string>()

  // Build directed edge set so we can detect one-way connections by absence of
  // the reverse edge. connectedLocations is symmetric for normal connections
  // but asymmetric when a location's connectedMatchers don't match back.
  const directed = new Set<string>()
  for (const loc of allLocations.value) {
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

  const invalidMineCart = mineCartInvalidDirection()
  if (invalidMineCart) {
    const start = document.querySelector<HTMLElement>(`[data-id="${invalidMineCart.locationId}"]`)
    if (start) {
      for (const conn of makeOrUpdateMineCartInvalidLine(start, invalidMineCart.direction)) live.add(conn)
    }
  }

  const isFateOfTheVale = props.game.scenario?.id === 'c10651'
  if (includeFateOfTheVale && isFateOfTheVale) {
    for (const [enemyLabel, locationLabel] of Object.entries(fateOfTheValeEnemyLocations)) {
      const start = document.querySelector<HTMLElement>(`[data-label="${enemyLabel}"] [data-id]`)
      const end = document.querySelector<HTMLElement>(`.location-cell[data-label="${locationLabel}"] [data-id]`)
      if (!start || !end) continue

      const conn = `${start.dataset.id}:${end.dataset.id}`
      live.add(conn)
      makeOrUpdateLine(start, end, "fate-of-the-vale-enemy-line", true)
    }
  }

  for (const enemy of enemies.value) {
    const { id, placement, asSelfLocation } = enemy
    if (isFateOfTheVale && asSelfLocation && asSelfLocation in fateOfTheValeEnemyLocations) continue
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
      if (!includeFateOfTheVale && el.classList.contains('fate-of-the-vale-enemy-line')) continue
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
const connectionUpdateRequestId = ref<number | null>(null)
let connectionObserver: MutationObserver | null = null
let resizeObserver: ResizeObserver | null = null
let connectionUpdateTimeouts: number[] = []
let lastTime = 0
const FRAME_MS = 1000 / 30 // 30fps for normal/enemy connection following only

function requestConnectionUpdate() {
  if (connectionUpdateRequestId.value !== null) return
  connectionUpdateRequestId.value = window.requestAnimationFrame(() => {
    connectionUpdateRequestId.value = null
    handleConnections(true)
  })
}

function tick(ts: number) {
  requestId.value = null
  if (ts - lastTime >= FRAME_MS) {
    lastTime = ts
    handleConnections(false)
  }
  requestId.value = window.requestAnimationFrame(tick)
}

onMounted(async () => {
  await nextTick() // ensure template is in DOM
  svgEl = svgRef.value
  defsEl = svgEl?.querySelector('defs') ?? null
  lineProto = protoRef.value
  chevronProto = chevronProtoRef.value
  // First draw immediately so a cold refresh shows lines at once, then redraw
  // after layout/images/cached Cosmic Emissary transforms settle. The normal
  // animation tick intentionally skips Fate of the Vale enemy lines, so without
  // these delayed full updates they can remain at the initial pre-layout
  // positions after leaving and re-entering a game.
  handleConnections(true)
  connectionUpdateTimeouts = [50, 150, 500, 1500].map((delay) => window.setTimeout(requestConnectionUpdate, delay))
  requestId.value = window.requestAnimationFrame(tick)

  window.addEventListener('resize', requestConnectionUpdate)
  window.addEventListener('scroll', requestConnectionUpdate, true)
  window.addEventListener('arkham-location-layout-change', requestConnectionUpdate)

  const locationCards = document.querySelector('.location-cards') as HTMLElement | null
  if (locationCards) {
    connectionObserver = new MutationObserver(requestConnectionUpdate)
    connectionObserver.observe(locationCards, {
      childList: true,
      subtree: true,
      attributes: true,
      attributeFilter: ['style', 'class', 'data-id', 'data-label'],
    })

    resizeObserver = new ResizeObserver(requestConnectionUpdate)
    resizeObserver.observe(locationCards)
    if (svgEl?.parentElement) resizeObserver.observe(svgEl.parentElement)
  }
})

// keep lines fresh if the set of locations changes
watch(locations, ()=> { requestConnectionUpdate() }, { flush: 'post' })
watch(mineCart, ()=> { requestConnectionUpdate() }, { flush: 'post' })
watch(isWrittenInRockAct2, ()=> { requestConnectionUpdate() }, { flush: 'post' })
watch(enemies, ()=> { requestConnectionUpdate() }, { flush: 'post' })
watch(() => props.enableCosmicEmissaryAnimation, () => { requestConnectionUpdate() }, { flush: 'post' })

onBeforeUnmount(()=> {
  window.removeEventListener('resize', requestConnectionUpdate)
  window.removeEventListener('scroll', requestConnectionUpdate, true)
  window.removeEventListener('arkham-location-layout-change', requestConnectionUpdate)
  connectionObserver?.disconnect()
  connectionObserver = null
  resizeObserver?.disconnect()
  resizeObserver = null
  connectionUpdateTimeouts.forEach((timeoutId) => clearTimeout(timeoutId))
  connectionUpdateTimeouts = []
  if(requestId.value !== null) cancelAnimationFrame(requestId.value)
  requestId.value = null
  if(connectionUpdateRequestId.value !== null) cancelAnimationFrame(connectionUpdateRequestId.value)
  connectionUpdateRequestId.value = null
  for (const [,el] of linesByConn) el.remove()
  linesByConn.clear()
  for (const [,el] of chevronsByConn) el.remove()
  chevronsByConn.clear()
  svgEl = null
  defsEl = null
  lineProto = null
  chevronProto = null
})
</script>

<template>
  <svg ref="svgRef" class="connections-svg" :class="{ 'cosmic-emissary-animation-disabled': props.enableCosmicEmissaryAnimation === false }">
    <defs>
      <filter id="fate-of-the-vale-smoke-filter" x="-1000" y="-1000" width="4000" height="4000" filterUnits="userSpaceOnUse" color-interpolation-filters="sRGB">
        <feTurbulence type="fractalNoise" baseFrequency="0.03" numOctaves="2" seed="17" result="smokeNoise">
          <animate v-if="props.enableCosmicEmissaryAnimation !== false" attributeName="baseFrequency" values="0.024;0.036;0.024" dur="9s" repeatCount="indefinite" />
        </feTurbulence>
        <feDisplacementMap in="SourceGraphic" in2="smokeNoise" scale="5" xChannelSelector="R" yChannelSelector="G" result="distorted" />
        <feGaussianBlur in="distorted" stdDeviation="1.9" />
      </filter>
    </defs>
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
.line.active:not(.mine-cart-next-line){
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

.mine-cart-next-line{
  stroke: rgba(74 190 111 / 0.85);
  filter: drop-shadow(0 0 2px rgba(74 190 111 / 0.35));
}

.mine-cart-invalid-line{
  stroke: rgba(220 48 48 / 0.85);
  filter: drop-shadow(0 0 2px rgba(220 48 48 / 0.45));
}

.mine-cart-invalid-x{
  fill: rgba(220 48 48 / 0.95);
  stroke: none;
  filter: drop-shadow(0 0 2px rgba(220 48 48 / 0.45));
}

.fate-of-the-vale-enemy-line{
  stroke-width: 10px;
  stroke-dasharray: 86 10;
  stroke-linecap: round;
  stroke-opacity: 1;
  vector-effect: non-scaling-stroke;
  animation: fate-of-the-vale-smoke-flow 7.5s linear infinite;
  filter:
    url(#fate-of-the-vale-smoke-filter)
    drop-shadow(0 0 4px rgba(221 242 235 / 0.9))
    drop-shadow(0 0 10px rgba(132 202 199 / 0.95))
    drop-shadow(0 0 18px rgba(54 102 114 / 0.85));
}

.cosmic-emissary-animation-disabled .fate-of-the-vale-enemy-line {
  animation: none;
  filter: none;
  stroke: rgba(132 202 199 / 0.7);
  stroke-dasharray: unset;
}

@keyframes fate-of-the-vale-smoke-flow {
  from { stroke-dashoffset: 0; }
  to { stroke-dashoffset: -96; }
}
</style>

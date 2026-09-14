<script lang="ts" setup>
import { onMounted, onBeforeUnmount, computed, ref, nextTick, watch } from 'vue'
import type {Game} from '@/arkham/types/Game'
import { createLaserBeam, COSMIC_EMISSARY_STOPS, type LaserBeamInstance } from '@/arkham/laserBeam'
import { useSettings } from '@/stores/settings'

export interface Props {
  game: Game
  playerId: string
  allowCurvedPaths?: boolean
  enableCosmicEmissaryAnimation?: boolean
}

// The laser layer is a sibling of the SVG rather than part of it, so this
// component now has two roots and must not try to inherit attributes.
defineOptions({ inheritAttrs: false })

const props = defineProps<Props>()
const settings = useSettings()
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
const connectionProtoRef = ref<SVGPathElement | null>(null)
const chevronProtoRef = ref<SVGPathElement | null>(null)
let svgEl: SVGSVGElement | null = null
let defsEl: SVGDefsElement | null = null
let lineProto: SVGLineElement | null = null
let connectionProto: SVGPathElement | null = null
let chevronProto: SVGPathElement | null = null

const EPS = 0.5
const close = (a: number, b: number) => Math.abs(a - b) < EPS
const linesByConn = new Map<string, SVGLineElement>()
const connectionPathsByConn = new Map<string, SVGPathElement>()
const fateGlowLinesByConn = new Map<string, SVGLineElement>()
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

type ConnectionCandidate = {
  connection: string
  start: HTMLElement
  end: HTMLElement
  x1: number
  y1: number
  x2: number
  y2: number
}

function connectionPoints(div1: HTMLElement, div2: HTMLElement) {
  if (!svgEl) return null
  const svgRect = svgEl.getBoundingClientRect()
  const lRect = div1.getBoundingClientRect()
  const rRect = div2.getBoundingClientRect()
  const lCenterX = (lRect.left - svgRect.left) + (lRect.width / 2)
  const lCenterY = (lRect.top - svgRect.top) + (lRect.height / 2)
  const rCenterX = (rRect.left - svgRect.left) + (rRect.width / 2)
  const rCenterY = (rRect.top - svgRect.top) + (rRect.height / 2)
  const offsetTrackLine = isWrittenInRockAct2.value
  const vertical = Math.abs(rCenterY - lCenterY) > Math.abs(rCenterX - lCenterX)

  return {
    x1: offsetTrackLine && vertical ? (lRect.left - svgRect.left) + (lRect.width * 0.78) : lCenterX,
    y1: offsetTrackLine && !vertical ? (lRect.top - svgRect.top) + (lRect.height * 0.8) : lCenterY,
    x2: offsetTrackLine && vertical ? (rRect.left - svgRect.left) + (rRect.width * 0.78) : rCenterX,
    y2: offsetTrackLine && !vertical ? (rRect.top - svgRect.top) + (rRect.height * 0.8) : rCenterY,
  }
}

function segmentsConflict(a: ConnectionCandidate, b: ConnectionCandidate): boolean {
  // Lines meeting at the same location are expected to share an endpoint.
  if (a.start.dataset.id === b.start.dataset.id || a.start.dataset.id === b.end.dataset.id ||
      a.end.dataset.id === b.start.dataset.id || a.end.dataset.id === b.end.dataset.id) return false

  const cross = (ax: number, ay: number, bx: number, by: number, cx: number, cy: number) =>
    (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  const c1 = cross(a.x1, a.y1, a.x2, a.y2, b.x1, b.y1)
  const c2 = cross(a.x1, a.y1, a.x2, a.y2, b.x2, b.y2)
  const c3 = cross(b.x1, b.y1, b.x2, b.y2, a.x1, a.y1)
  const c4 = cross(b.x1, b.y1, b.x2, b.y2, a.x2, a.y2)
  const tolerance = 1

  if (((c1 > tolerance && c2 < -tolerance) || (c1 < -tolerance && c2 > tolerance)) &&
      ((c3 > tolerance && c4 < -tolerance) || (c3 < -tolerance && c4 > tolerance))) return true

  // Collinear segments need a visible shared run, not merely a touching point.
  if ([c1, c2, c3, c4].every(value => Math.abs(value) <= tolerance)) {
    const useX = Math.abs(a.x2 - a.x1) >= Math.abs(a.y2 - a.y1)
    const aMin = Math.min(useX ? a.x1 : a.y1, useX ? a.x2 : a.y2)
    const aMax = Math.max(useX ? a.x1 : a.y1, useX ? a.x2 : a.y2)
    const bMin = Math.min(useX ? b.x1 : b.y1, useX ? b.x2 : b.y2)
    const bMax = Math.max(useX ? b.x1 : b.y1, useX ? b.x2 : b.y2)
    return Math.min(aMax, bMax) - Math.max(aMin, bMin) > 8
  }

  return false
}

function curveOffsets(candidates: ConnectionCandidate[]): Map<string, number> {
  const conflictCounts = new Map<string, number>()
  const lengthSquared = (candidate: ConnectionCandidate) =>
    (candidate.x2 - candidate.x1) ** 2 + (candidate.y2 - candidate.y1) ** 2

  for (let i = 0; i < candidates.length; i++) {
    for (let j = i + 1; j < candidates.length; j++) {
      const a = candidates[i]
      const b = candidates[j]
      if (!segmentsConflict(a, b)) continue
      // Keep the shorter/local connection straight and bend the connection
      // spanning more of the board. Ties are resolved by the stable id.
      const curved = lengthSquared(a) === lengthSquared(b)
        ? (a.connection < b.connection ? b : a)
        : (lengthSquared(a) > lengthSquared(b) ? a : b)
      conflictCounts.set(curved.connection, (conflictCounts.get(curved.connection) ?? 0) + 1)
    }
  }

  const result = new Map<string, number>()
  for (const [connection, count] of conflictCounts) {
    const sign = Array.from(connection).reduce((sum, char) => sum + char.charCodeAt(0), 0) % 2 === 0 ? 1 : -1
    result.set(connection, sign * Math.min(34 + (count - 1) * 10, 64))
  }

  if (!svgEl) return result
  const svgRect = svgEl.getBoundingClientRect()
  const locationRects = locations.value.flatMap(location => {
    const element = document.querySelector<HTMLElement>(`[data-id="${location.id}"]`)
    if (!element) return []
    const rect = element.getBoundingClientRect()
    return [{
      id: location.id,
      x: rect.left - svgRect.left - 6,
      y: rect.top - svgRect.top - 6,
      width: rect.width + 12,
      height: rect.height + 12,
    }]
  })
  const boardCenter = locationRects.reduce(
    (sum, rect) => ({ x: sum.x + rect.x + rect.width / 2, y: sum.y + rect.y + rect.height / 2 }),
    { x: 0, y: 0 },
  )
  if (locationRects.length > 0) {
    boardCenter.x /= locationRects.length
    boardCenter.y /= locationRects.length
  }

  const intersectionsForOffset = (candidate: ConnectionCandidate, curveOffset: number) => {
    const endpointIds = new Set([candidate.start.dataset.id, candidate.end.dataset.id])
    const dx = candidate.x2 - candidate.x1
    const dy = candidate.y2 - candidate.y1
    const distance = Math.hypot(dx, dy) || 1
    const controlX = (candidate.x1 + candidate.x2) / 2 - (dy / distance) * curveOffset
    const controlY = (candidate.y1 + candidate.y2) / 2 + (dx / distance) * curveOffset
    const hitIds = new Set<string>()
    for (let step = 1; step < 50; step++) {
      const t = step / 50
      const oneMinusT = 1 - t
      const x = oneMinusT ** 2 * candidate.x1 + 2 * oneMinusT * t * controlX + t ** 2 * candidate.x2
      const y = oneMinusT ** 2 * candidate.y1 + 2 * oneMinusT * t * controlY + t ** 2 * candidate.y2
      for (const rect of locationRects) {
        if (endpointIds.has(rect.id)) continue
        if (x >= rect.x && x <= rect.x + rect.width && y >= rect.y && y <= rect.y + rect.height) hitIds.add(rect.id)
      }
    }
    return hitIds.size
  }

  for (const candidate of candidates) {
    const straightIntersections = intersectionsForOffset(candidate, 0)
    if (straightIntersections === 0) continue
    // Give longer routes crossing several cards a distinctly wider lane so
    // nested connections do not continue to sit on top of one another.
    const magnitude = Math.min(90 + (straightIntersections - 1) * 75, 165)
    const positiveIntersections = intersectionsForOffset(candidate, magnitude)
    const negativeIntersections = intersectionsForOffset(candidate, -magnitude)
    if (positiveIntersections !== negativeIntersections) {
      result.set(candidate.connection, positiveIntersections < negativeIntersections ? magnitude : -magnitude)
      continue
    }

    // If both sides are equally clear, bend toward the outside of the board.
    const normalX = -(candidate.y2 - candidate.y1) / Math.sqrt(lengthSquared(candidate))
    const normalY = (candidate.x2 - candidate.x1) / Math.sqrt(lengthSquared(candidate))
    const midpointX = (candidate.x1 + candidate.x2) / 2
    const midpointY = (candidate.y1 + candidate.y2) / 2
    const outwardDot = (midpointX - boardCenter.x) * normalX + (midpointY - boardCenter.y) * normalY
    result.set(candidate.connection, (outwardDot >= 0 ? 1 : -1) * magnitude)
  }
  return result
}

function obstructedChevronCurve(candidate: ConnectionCandidate): number | null {
  // Shifting an endpoint can add or remove an obstruction, so base this on the
  // current layout rather than particular cards or original grid positions.
  if (!svgEl) return null
  const svgRect = svgEl.getBoundingClientRect()
  const endpointIds = new Set([candidate.start.dataset.id, candidate.end.dataset.id])
  const obstructed = locations.value.some(location => {
    if (endpointIds.has(location.id)) return false
    const element = document.querySelector<HTMLElement>(`[data-id="${location.id}"]`)
    if (!element) return false
    const rect = element.getBoundingClientRect()
    const left = rect.left - svgRect.left - 6
    const right = rect.right - svgRect.left + 6
    const top = rect.top - svgRect.top - 6
    const bottom = rect.bottom - svgRect.top + 6
    return Array.from({ length: 49 }, (_, index) => (index + 1) / 50).some(t => {
      const x = candidate.x1 + (candidate.x2 - candidate.x1) * t
      const y = candidate.y1 + (candidate.y2 - candidate.y1) * t
      return x >= left && x <= right && y >= top && y <= bottom
    })
  })
  if (!obstructed) return null

  const centers = locations.value.flatMap(location => {
    const element = document.querySelector<HTMLElement>(`[data-id="${location.id}"]`)
    const points = element && connectionPoints(element, element)
    return points ? [{ x: points.x1, y: points.y1 }] : []
  })
  if (centers.length === 0) return null

  const boardCenter = centers.reduce((sum, point) => ({ x: sum.x + point.x, y: sum.y + point.y }), { x: 0, y: 0 })
  boardCenter.x /= centers.length
  boardCenter.y /= centers.length

  const dx = candidate.x2 - candidate.x1
  const dy = candidate.y2 - candidate.y1
  const distance = Math.hypot(dx, dy) || 1
  const normalX = -dy / distance
  const normalY = dx / distance
  const midpointX = (candidate.x1 + candidate.x2) / 2
  const midpointY = (candidate.y1 + candidate.y2) / 2
  const outwardDot = (midpointX - boardCenter.x) * normalX + (midpointY - boardCenter.y) * normalY
  // A shallow lane clears a card without swinging into the next row.
  const magnitude = Math.min(Math.max(distance * 0.28, 100), 140)
  return (outwardDot >= 0 ? 1 : -1) * magnitude
}

function makeOrUpdateConnectionPath(candidate: ConnectionCandidate, curveOffset = 0) {
  if (!svgEl || !connectionProto) return
  const { connection, start, end, x1, y1, x2, y2 } = candidate
  const leftDivId = start.dataset.id
  const rightDivId = end.dataset.id
  if (!leftDivId || !rightDivId) return

  let path = connectionPathsByConn.get(connection)
  if (!path) {
    // Clone a template node so Vue's scoped-style attribute is retained.
    path = connectionProto.cloneNode(true) as SVGPathElement
    path.classList.remove('original')
    path.classList.add('connection')
    path.dataset.connection = connection
    svgEl.appendChild(path)
    connectionPathsByConn.set(connection, path)
  }

  if (curveOffset === 0) {
    path.setAttribute('d', `M ${x1} ${y1} L ${x2} ${y2}`)
    path.classList.remove('curved')
  } else {
    const dx = x2 - x1
    const dy = y2 - y1
    const distance = Math.hypot(dx, dy) || 1
    const controlX = (x1 + x2) / 2 - (dy / distance) * curveOffset
    const controlY = (y1 + y2) / 2 + (dx / distance) * curveOffset
    path.setAttribute('d', `M ${x1} ${y1} Q ${controlX} ${controlY} ${x2} ${y2}`)
    path.classList.add('curved')
  }

  if (connection === mineCartNextConnection()) path.classList.add('mine-cart-next-line')
  else path.classList.remove('mine-cart-next-line')

  const investigator = Object.values(props.game.investigators).find(i => i.playerId === props.playerId)
  const activeLine = !!investigator && (
    (leftDivId === investigator.location && investigator.connectedLocations.includes(rightDivId)) ||
    (rightDivId === investigator.location && investigator.connectedLocations.includes(leftDivId))
  )
  path.classList.toggle('active', activeLine)
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
    // The laser replaces both SVG lines outright, so it is tried first and the
    // SVG treatment only rebuilt if it declines.
    if (useLaserBeams.value && updateLaserBeam(connection, x1, y1, x2, y2)) {
      removeFateSvgDecoration(connection)
      line.style.display = 'none'
    } else {
      removeLaserBeam(connection)
      line.style.display = ''
      updateFateGlowLine(connection, line, x1, y1, x2, y2)
      line.style.filter = ''
      if (props.enableCosmicEmissaryAnimation === false) {
        line.removeAttribute('style')
      } else {
        updateFateOfTheValeEnemyLineGradient(line, connection, x1, y1, x2, y2)
      }
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

function setSvgAttr(el: SVGElement, name: string, value: string) {
  if (el.getAttribute(name) !== value) el.setAttribute(name, value)
}

// --- Cosmic Emissary laser beams -------------------------------------------
//
// The four emissary connections are drawn as WebGL laser beams instead of the
// SVG glow + dashed gradient pair. Each beam gets a canvas sized to the
// connection's length and rotated into place, because the shader always draws
// along the canvas's horizontal centre line.
//
// The SVG treatment is NOT deleted — it stays as the fallback for browsers
// without WebGL2, for prefers-reduced-motion, and for the extra-animations
// setting. laserBeamsSupported flips to false the first time a context fails to
// come up, and every connection falls back together rather than one by one.

// Tall enough that the glow has decayed to nothing well before the canvas
// border; the shader's edge fade cleans up whatever is left.
const LASER_BEAM_HEIGHT = 72
const laserLayerRef = ref<HTMLElement | null>(null)
const laserBeamsSupported = ref(true)
const lasersByConn = new Map<string, { canvas: HTMLCanvasElement; instance: LaserBeamInstance }>()

const useLaserBeams = computed(
  () =>
    laserBeamsSupported.value &&
    settings.extraAnimations &&
    props.enableCosmicEmissaryAnimation !== false,
)

function updateLaserBeam(connection: string, x1: number, y1: number, x2: number, y2: number): boolean {
  const layer = laserLayerRef.value
  if (!layer) return false

  const dx = x2 - x1
  const dy = y2 - y1
  const length = Math.hypot(dx, dy)
  if (length < 1) return false

  let entry = lasersByConn.get(connection)
  if (!entry) {
    const canvas = document.createElement('canvas')
    canvas.className = 'laser-beam'
    canvas.dataset.connection = connection
    layer.appendChild(canvas)
    const instance = createLaserBeam(canvas, { stops: COSMIC_EMISSARY_STOPS })
    if (!instance) {
      canvas.remove()
      laserBeamsSupported.value = false
      return false
    }
    entry = { canvas, instance }
    lasersByConn.set(connection, entry)
  }

  const { canvas, instance } = entry
  canvas.style.width = `${length}px`
  canvas.style.height = `${LASER_BEAM_HEIGHT}px`
  canvas.style.left = `${(x1 + x2) / 2 - length / 2}px`
  canvas.style.top = `${(y1 + y2) / 2 - LASER_BEAM_HEIGHT / 2}px`
  canvas.style.transform = `rotate(${Math.atan2(dy, dx)}rad)`
  instance.resize()
  return true
}

function removeLaserBeam(connection: string) {
  const entry = lasersByConn.get(connection)
  if (!entry) return
  entry.instance.destroy()
  entry.canvas.remove()
  lasersByConn.delete(connection)
}

function removeAllLaserBeams() {
  for (const connection of [...lasersByConn.keys()]) removeLaserBeam(connection)
}

// Drops the SVG glow line and its smoke filter for a connection the laser has
// taken over.
function removeFateSvgDecoration(connection: string) {
  fateGlowLinesByConn.get(connection)?.remove()
  fateGlowLinesByConn.delete(connection)
  defsEl?.querySelector(`#fate-of-the-vale-smoke-filter-${connection.replace(/[^a-zA-Z0-9_-]/g, '-')}`)?.remove()
}

function updateFateSmokeFilter(connection: string, x1: number, y1: number, x2: number, y2: number): string | null {
  if (!defsEl) return null
  const filterId = `fate-of-the-vale-smoke-filter-${connection.replace(/[^a-zA-Z0-9_-]/g, '-')}`
  let filter = defsEl.querySelector<SVGFilterElement>(`#${filterId}`)
  if (!filter) {
    filter = document.createElementNS('http://www.w3.org/2000/svg', 'filter')
    filter.id = filterId
    filter.setAttribute('filterUnits', 'userSpaceOnUse')
    filter.setAttribute('color-interpolation-filters', 'sRGB')
    filter.innerHTML = `
      <feTurbulence type="fractalNoise" baseFrequency="0.03" numOctaves="2" seed="17" result="smokeNoise" />
      <feDisplacementMap in="SourceGraphic" in2="smokeNoise" scale="5" xChannelSelector="R" yChannelSelector="G" result="distorted" />
      <feGaussianBlur in="distorted" stdDeviation="1.9" />
    `
    defsEl.appendChild(filter)
  }

  const pad = 96
  setSvgAttr(filter, 'x', String(Math.min(x1, x2) - pad))
  setSvgAttr(filter, 'y', String(Math.min(y1, y2) - pad))
  setSvgAttr(filter, 'width', String(Math.abs(x2 - x1) + pad * 2))
  setSvgAttr(filter, 'height', String(Math.abs(y2 - y1) + pad * 2))
  return filterId
}

function updateFateGlowLine(connection: string, line: SVGLineElement, x1: number, y1: number, x2: number, y2: number) {
  if (!svgEl || !lineProto) return
  let glowLine = fateGlowLinesByConn.get(connection)
  if (!glowLine) {
    glowLine = lineProto.cloneNode(true) as SVGLineElement
    glowLine.classList.remove('original')
    glowLine.classList.add('connection', 'fate-of-the-vale-enemy-line-glow')
    glowLine.removeAttribute('id')
    glowLine.dataset.connection = `${connection}:glow`
    svgEl.insertBefore(glowLine, line)
    fateGlowLinesByConn.set(connection, glowLine)
  }
  setSvgAttr(glowLine, 'x1', String(x1))
  setSvgAttr(glowLine, 'y1', String(y1))
  setSvgAttr(glowLine, 'x2', String(x2))
  setSvgAttr(glowLine, 'y2', String(y2))
  if (props.enableCosmicEmissaryAnimation === false) {
    glowLine.removeAttribute('filter')
  } else {
    const filterId = updateFateSmokeFilter(connection, x1, y1, x2, y2)
    if (filterId) setSvgAttr(glowLine, 'filter', `url(#${filterId})`)
  }
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

  setSvgAttr(gradient, 'x1', String(x1))
  setSvgAttr(gradient, 'y1', String(y1))
  setSvgAttr(gradient, 'x2', String(x1 + ux * patternLength))
  setSvgAttr(gradient, 'y2', String(y1 + uy * patternLength))
  gradient.querySelector('animateTransform')?.setAttribute('from', '0 0')
  gradient.querySelector('animateTransform')?.setAttribute('to', `${ux * patternLength} ${uy * patternLength}`)
  setSvgAttr(line, 'stroke', `url(#${gradientId})`)

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
function makeOrUpdateChevrons(srcDiv: HTMLElement, dstDiv: HTMLElement, connection: string, curveOffset = 0) {
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
  // Straight chevrons stop outside the card edges. Curved routes continue to
  // each card's center and are naturally hidden underneath the location cards.
  const startD = curveOffset === 0 ? exitDist(sRect.width / 2, sRect.height / 2) + CHEVRON_EDGE_PAD : 0
  const endD = curveOffset === 0 ? dist - exitDist(dRect.width / 2, dRect.height / 2) - CHEVRON_EDGE_PAD : dist
  const span = endD - startD
  if (span < 0) return // cards overlap or are flush

  // Fixed spacing, centered in the visible band — never stretches chevrons
  // to the boundary.
  const count = Math.max(1, Math.round(span / CHEVRON_SPACING) + 1)
  const usedSpan = (count - 1) * CHEVRON_SPACING
  const offset = (span - usedSpan) / 2
  const segments: string[] = []
  const controlX = (x1 + x2) / 2 - uy * curveOffset
  const controlY = (y1 + y2) / 2 + ux * curveOffset
  for (let i = 0; i < count; i++) {
    const d = startD + offset + i * CHEVRON_SPACING
    if (curveOffset === 0) {
      const cx = x1 + ux * d
      const cy = y1 + uy * d
      segments.push(chevronPath(cx, cy, ux, uy, px, py))
    } else {
      const t = d / dist
      const oneMinusT = 1 - t
      const cx = oneMinusT ** 2 * x1 + 2 * oneMinusT * t * controlX + t ** 2 * x2
      const cy = oneMinusT ** 2 * y1 + 2 * oneMinusT * t * controlY + t ** 2 * y2
      const tangentX = 2 * oneMinusT * (controlX - x1) + 2 * t * (x2 - controlX)
      const tangentY = 2 * oneMinusT * (controlY - y1) + 2 * t * (y2 - controlY)
      const tangentLength = Math.hypot(tangentX, tangentY) || 1
      const curveUx = tangentX / tangentLength
      const curveUy = tangentY / tangentLength
      segments.push(chevronPath(cx, cy, curveUx, curveUy, -curveUy, curveUx))
    }
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

  const normalConnections = new Map<string, ConnectionCandidate>()
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
        if (!normalConnections.has(conn)) {
          const [left, right] = [start, end].sort(sortByDataId)
          const points = connectionPoints(left, right)
          if (points) normalConnections.set(conn, { connection: conn, start: left, end: right, ...points })
        }
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
        const points = connectionPoints(start, end)
        const candidate = points
          ? { connection: conn, start, end, ...points }
          : null
        const curveOffset = props.allowCurvedPaths && candidate
          ? (obstructedChevronCurve(candidate) ?? 0)
          : 0
        makeOrUpdateChevrons(start, end, conn, curveOffset)
      }
    }
  }

  const candidates = Array.from(normalConnections.values())
  const offsets = props.allowCurvedPaths ? curveOffsets(candidates) : new Map<string, number>()
  for (const candidate of candidates) {
    makeOrUpdateConnectionPath(candidate, offsets.get(candidate.connection) ?? 0)
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

  for (const [conn, el] of connectionPathsByConn) {
    if (!live.has(conn)) {
      el.remove()
      connectionPathsByConn.delete(conn)
    }
  }
  for (const [conn, el] of linesByConn) {
    if (!live.has(conn)) {
      if (!includeFateOfTheVale && el.classList.contains('fate-of-the-vale-enemy-line')) continue
      removeFateSvgDecoration(conn)
      removeLaserBeam(conn)
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
let transientTrackingUntil = 0

function stopTransientTracking() {
  transientTrackingUntil = 0
  if (requestId.value !== null) cancelAnimationFrame(requestId.value)
  requestId.value = null
}

function requestTransientConnectionTracking(durationMs = 260) {
  transientTrackingUntil = Math.max(transientTrackingUntil, performance.now() + durationMs)
  if (requestId.value !== null) return
  const tick = (ts: number) => {
    requestId.value = null
    if (ts >= transientTrackingUntil) return
    handleConnections(false)
    requestId.value = window.requestAnimationFrame(tick)
  }
  requestId.value = window.requestAnimationFrame(tick)
}

function requestConnectionUpdate() {
  if (connectionUpdateRequestId.value !== null) return
  connectionUpdateRequestId.value = window.requestAnimationFrame(() => {
    connectionUpdateRequestId.value = null
    handleConnections(true)
    requestTransientConnectionTracking()
  })
}

onMounted(async () => {
  await nextTick() // ensure template is in DOM
  svgEl = svgRef.value
  defsEl = svgEl?.querySelector('defs') ?? null
  lineProto = protoRef.value
  connectionProto = connectionProtoRef.value
  chevronProto = chevronProtoRef.value
  // First draw immediately so a cold refresh shows lines at once, then redraw
  // after layout/images/cached Cosmic Emissary transforms settle. The normal
  // animation tick intentionally skips Fate of the Vale enemy lines, so without
  // these delayed full updates they can remain at the initial pre-layout
  // positions after leaving and re-entering a game.
  handleConnections(true)
  requestTransientConnectionTracking()
  connectionUpdateTimeouts = [50, 150, 500, 1500].map((delay) => window.setTimeout(requestConnectionUpdate, delay))

  window.addEventListener('resize', requestConnectionUpdate)
  window.addEventListener('scroll', requestConnectionUpdate, { capture: true, passive: true })
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
// Turning the beams off has to tear the canvases down, not just stop drawing
// them; the redraw then rebuilds the SVG lines in their place.
watch(useLaserBeams, (enabled) => {
  if (!enabled) removeAllLaserBeams()
  requestConnectionUpdate()
}, { flush: 'post' })

onBeforeUnmount(()=> {
  window.removeEventListener('resize', requestConnectionUpdate)
  window.removeEventListener('scroll', requestConnectionUpdate, { capture: true })
  window.removeEventListener('arkham-location-layout-change', requestConnectionUpdate)
  connectionObserver?.disconnect()
  connectionObserver = null
  resizeObserver?.disconnect()
  resizeObserver = null
  connectionUpdateTimeouts.forEach((timeoutId) => clearTimeout(timeoutId))
  connectionUpdateTimeouts = []
  stopTransientTracking()
  if(connectionUpdateRequestId.value !== null) cancelAnimationFrame(connectionUpdateRequestId.value)
  connectionUpdateRequestId.value = null
  for (const [conn, el] of linesByConn) {
    defsEl?.querySelector(`#fate-of-the-vale-smoke-filter-${conn.replace(/[^a-zA-Z0-9_-]/g, '-')}`)?.remove()
    el.remove()
  }
  for (const [, el] of connectionPathsByConn) el.remove()
  for (const [,el] of fateGlowLinesByConn) el.remove()
  removeAllLaserBeams()
  linesByConn.clear()
  connectionPathsByConn.clear()
  fateGlowLinesByConn.clear()
  for (const [,el] of chevronsByConn) el.remove()
  chevronsByConn.clear()
  svgEl = null
  defsEl = null
  lineProto = null
  connectionProto = null
  chevronProto = null
})
</script>

<template>
  <svg ref="svgRef" class="connections-svg" :class="{ 'cosmic-emissary-animation-disabled': props.enableCosmicEmissaryAnimation === false }">
    <defs>
    </defs>
    <line ref="protoRef" class="line original" stroke-dasharray="5, 5"/>
    <path ref="connectionProtoRef" class="line original" stroke-dasharray="5, 5"/>
    <path ref="chevronProtoRef" class="chevrons original"/>
  </svg>
  <div ref="laserLayerRef" class="connections-lasers" aria-hidden="true"></div>
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
  z-index: 0;
  overflow: hidden;
}

/* Shares the SVG's coordinate space and stacking level, so the beams sit under
   the location cards exactly like the lines they replace. */
.connections-lasers{
  pointer-events: none;
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: 0;
  overflow: hidden;
}

.connections-lasers :deep(.laser-beam){
  position: absolute;
  max-width: none;
  transform-origin: 50% 50%;
  pointer-events: none;
}

.line{
  fill: none;
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

.fate-of-the-vale-enemy-line-glow {
  stroke: rgba(132 202 199 / 0.52);
  stroke-width: 22px;
  stroke-linecap: round;
  stroke-opacity: 0.9;
  vector-effect: non-scaling-stroke;
}

.fate-of-the-vale-enemy-line{
  stroke-width: 10px;
  stroke-dasharray: 86 10;
  stroke-linecap: round;
  stroke-opacity: 1;
  vector-effect: non-scaling-stroke;
  animation: fate-of-the-vale-smoke-flow 7.5s linear infinite;
}

.cosmic-emissary-animation-disabled .fate-of-the-vale-enemy-line,
.cosmic-emissary-animation-disabled .fate-of-the-vale-enemy-line-glow {
  animation: none;
  filter: none;
  stroke: rgba(132 202 199 / 0.7);
  stroke-dasharray: unset;
}

@media (prefers-reduced-motion: reduce) {
  .fate-of-the-vale-enemy-line {
    animation: none;
  }

  .fate-of-the-vale-enemy-line-glow {
    filter: drop-shadow(0 0 8px rgba(132 202 199 / 0.55));
  }
}

@keyframes fate-of-the-vale-smoke-flow {
  from { stroke-dashoffset: 0; }
  to { stroke-dashoffset: -96; }
}
</style>

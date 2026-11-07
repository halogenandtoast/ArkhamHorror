/**
 * Pure graph helpers for the location map view.
 * Deliberately import-free (types are structural) so node tests can load it directly.
 */
export type GraphLocation = {
  id: string
  connectedLocations: string[]
  investigators: string[]
  enemies: string[]
  /** Scenario grid label, when the scenario lays its locations out on a grid. */
  label?: string
}

export type LocationGraph = Record<string, GraphLocation>

/** Neighbours of a location, treating connections as undirected and ignoring locations not in play. */
function neighbours(graph: LocationGraph): Map<string, Set<string>> {
  const result = new Map<string, Set<string>>(Object.keys(graph).map((id) => [id, new Set<string>()]))
  for (const [id, location] of Object.entries(graph)) {
    for (const other of location.connectedLocations) {
      if (other === id || !(other in graph)) continue
      result.get(id)!.add(other)
      result.get(other)!.add(id)
    }
  }
  return result
}

/** Each connection once, as [from, to]. */
export function locationLinks(graph: LocationGraph): [string, string][] {
  const links: [string, string][] = []
  for (const [id, next] of neighbours(graph)) {
    for (const other of next) if (id < other) links.push([id, other])
  }
  return links
}

/** Direction of a displayed connection, independent of the order of its endpoints. */
export function locationLinkDirection(
  a: Pick<GraphLocation, 'id' | 'connectedLocations'>,
  b: Pick<GraphLocation, 'id' | 'connectedLocations'>,
): 'both' | 'forward' | 'backward' | null {
  const forward = a.connectedLocations.includes(b.id)
  const backward = b.connectedLocations.includes(a.id)
  if (forward && backward) return 'both'
  if (forward) return 'forward'
  return backward ? 'backward' : null
}

/**
 * Fewest-hops route from `from` to `to`, both ends included; empty when unreachable.
 * `isBlocked` rules out intermediate and final stops (e.g. a barred location); the start is never blocked.
 */
export function shortestPath(
  graph: LocationGraph,
  from: string,
  to: string,
  isBlocked: (id: string) => boolean = () => false,
): string[] {
  if (!(from in graph) || !(to in graph)) return []
  if (from === to) return [from]
  const next = neighbours(graph)
  const previous = new Map<string, string>()
  const seen = new Set([from])
  const queue = [from]
  for (let head = 0; head < queue.length; head++) {
    const current = queue[head]
    for (const other of next.get(current) ?? []) {
      if (seen.has(other) || isBlocked(other)) continue
      seen.add(other)
      previous.set(other, current)
      if (other === to) {
        const route = [to]
        while (route[0] !== from) route.unshift(previous.get(route[0])!)
        return route
      }
      queue.push(other)
    }
  }
  return []
}

export function locationRoster(graph: LocationGraph, id: string): { investigators: string[]; enemies: string[] } {
  const location = graph[id]
  return { investigators: [...(location?.investigators ?? [])], enemies: [...(location?.enemies ?? [])] }
}

export type Point = [number, number]

/** Centre of every label in a scenario grid, as percentages of the map. Labels may span several cells. */
export function gridPositions(rows: string[]): Record<string, Point> {
  const cells = rows.map((row) => row.trim().split(/\s+/))
  const width = Math.max(1, ...cells.map((r) => r.length))
  const height = Math.max(1, cells.length)
  const sums = new Map<string, { x: number; y: number; n: number }>()
  cells.forEach((row, y) => row.forEach((label, x) => {
    if (label === '.' || label === '') return
    const entry = sums.get(label) ?? { x: 0, y: 0, n: 0 }
    sums.set(label, { x: entry.x + x + 0.5, y: entry.y + y + 0.5, n: entry.n + 1 })
  }))
  return Object.fromEntries(
    [...sums].map(([label, s]) => [label, [(s.x / s.n / width) * 100, (s.y / s.n / height) * 100] as Point]),
  )
}

/** Layered layout for locations without a grid: columns by distance from the first location of each cluster. */
export function autoPositions(graph: LocationGraph): Record<string, Point> {
  const next = neighbours(graph)
  const cell = new Map<string, [number, number]>()
  let rowOffset = 0
  let columns = 1
  for (const root of Object.keys(graph)) {
    if (cell.has(root)) continue
    const layers: string[][] = []
    const depth = new Map([[root, 0]])
    const queue = [root]
    for (let head = 0; head < queue.length; head++) {
      const id = queue[head]
      const d = depth.get(id)!
      ;(layers[d] ??= []).push(id)
      for (const other of next.get(id) ?? []) {
        if (cell.has(other) || depth.has(other)) continue
        depth.set(other, d + 1)
        queue.push(other)
      }
    }
    layers.forEach((ids, column) => ids.forEach((id, row) => cell.set(id, [column, rowOffset + row])))
    columns = Math.max(columns, layers.length)
    rowOffset += Math.max(...layers.map((l) => l.length))
  }
  const rows = Math.max(1, rowOffset)
  return Object.fromEntries(
    [...cell].map(([id, [c, r]]) => [id, [((c + 0.5) / columns) * 100, ((r + 0.5) / rows) * 100] as Point]),
  )
}

/**
 * Where each location sits on the map: by the scenario grid when its label is on it, otherwise in a
 * strip along the bottom laid out by connections. The grid is squeezed up only when that strip is needed.
 */
export function mapPositions(graph: LocationGraph, layout: string[] | null): Record<string, Point> {
  const grid = layout ? gridPositions(layout) : {}
  const placed: Record<string, Point> = {}
  const loose: LocationGraph = {}
  for (const [id, location] of Object.entries(graph)) {
    const point = location.label !== undefined ? grid[location.label] : undefined
    if (point) placed[id] = point
    else loose[id] = location
  }
  if (Object.keys(loose).length === 0) return placed
  if (Object.keys(placed).length === 0) return autoPositions(loose)
  const squeezed = Object.fromEntries(Object.entries(placed).map(([id, [x, y]]) => [id, [x, y * 0.8] as Point]))
  const strip = Object.fromEntries(
    Object.entries(autoPositions(loose)).map(([id, [x, y]]) => [id, [x, 84 + y * 0.14] as Point]),
  )
  return { ...squeezed, ...strip }
}

// The painted chambers zigzag left to right. Anchors sit above the open floor
// so figures have their own space below each plate.
const MINE_DESCENT_ANCHORS: Record<string, Point> = {
  pos0101: [14, 66],
  pos0201: [27, 20],
  pos0301: [48, 66],
  pos0401: [70, 20],
  pos0501: [85, 66],
}

// The walkway centers sit off the direct line between some room plates.
const MINE_DESCENT_PASSAGES: Record<string, Point> = {
  'pos0101:pos0201': [17, 47],
  'pos0201:pos0301': [38, 47],
  'pos0301:pos0401': [58, 47],
  'pos0401:pos0501': [79, 47],
}

export function mineDescentPassage(a: string, b: string): Point | undefined {
  return MINE_DESCENT_PASSAGES[a < b ? `${a}:${b}` : `${b}:${a}`]
}

export function mineDescentPositions(
  graph: LocationGraph,
  fallback: Record<string, Point>,
): Record<string, Point> {
  return Object.fromEntries(Object.entries(graph).map(([id, location]) => [
    id,
    MINE_DESCENT_ANCHORS[location.label ?? ''] ?? fallback[id],
  ]))
}

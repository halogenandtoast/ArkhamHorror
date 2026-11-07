import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import ts from 'typescript'

// locationGraph.ts is import-free on purpose so it can be loaded here without the Vue toolchain.
const source = readFileSync(new URL('../src/arkham/locationGraph.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { locationLinks, locationLinkDirection, shortestPath, locationRoster } = await import(
  'data:text/javascript;base64,' + Buffer.from(js).toString('base64')
)

const loc = (id, connectedLocations = [], extra = {}) =>
  ({ id, connectedLocations, investigators: [], enemies: [], ...extra })
const world = (...ls) => Object.fromEntries(ls.map(l => [l.id, l]))

test('connection arrows show the actual direction regardless of endpoint order', () => {
  const a = loc('a', ['b'])
  const b = loc('b')
  assert.equal(locationLinkDirection(a, b), 'forward')
  assert.equal(locationLinkDirection(b, a), 'backward')
  assert.equal(locationLinkDirection(a, loc('b', ['a'])), 'both')
  assert.equal(locationLinkDirection(b, loc('c')), null)
})

test('links are undirected and deduplicated', () => {
  const g = world(loc('a', ['b']), loc('b', ['a', 'c']), loc('c'))
  const links = locationLinks(g).map(l => [...l].sort().join('-')).sort()
  assert.deepEqual(links, ['a-b', 'b-c'])
})

test('one-sided connection data still yields a link', () => {
  const g = world(loc('a', ['b']), loc('b'))
  assert.equal(locationLinks(g).length, 1)
})

test('links to locations not in play are ignored', () => {
  const g = world(loc('a', ['ghost']))
  assert.deepEqual(locationLinks(g), [])
})

test('shortest path picks the fewest hops', () => {
  const g = world(loc('a', ['b', 'd']), loc('b', ['a', 'c']), loc('c', ['b', 'd']), loc('d', ['a', 'c']))
  assert.equal(shortestPath(g, 'a', 'c').length, 3)
})

test('path from a location to itself is that location', () => {
  assert.deepEqual(shortestPath(world(loc('a')), 'a', 'a'), ['a'])
})

test('disconnected target gives an empty path', () => {
  assert.deepEqual(shortestPath(world(loc('a'), loc('b')), 'a', 'b'), [])
})

test('unknown endpoints give an empty path', () => {
  assert.deepEqual(shortestPath(world(loc('a')), 'a', 'zzz'), [])
  assert.deepEqual(shortestPath(world(loc('a')), 'zzz', 'a'), [])
})

test('blocked locations are routed around or make target unreachable', () => {
  const g = world(loc('a', ['b', 'c']), loc('b', ['a', 'd']), loc('c', ['a', 'd']), loc('d', ['b', 'c']))
  assert.deepEqual(shortestPath(g, 'a', 'd', id => id === 'b'), ['a', 'c', 'd'])
  assert.deepEqual(shortestPath(g, 'a', 'd', id => id === 'b' || id === 'c'), [])
})

test('roster lists investigators and enemies at a location', () => {
  const g = world(loc('a', [], { investigators: ['i1', 'i2'], enemies: ['e1'] }), loc('b'))
  assert.deepEqual(locationRoster(g, 'a'), { investigators: ['i1', 'i2'], enemies: ['e1'] })
  assert.deepEqual(locationRoster(g, 'b'), { investigators: [], enemies: [] })
  assert.deepEqual(locationRoster(g, 'nope'), { investigators: [], enemies: [] })
})

const { gridPositions, autoPositions, mapPositions, mineDescentPositions, mineDescentPassage } = await import(
  'data:text/javascript;base64,' + Buffer.from(js).toString('base64')
)

test('the mine entrance uses painted chamber positions even when the grid has extra columns', () => {
  const g = world(
    loc('station', [], { label: 'pos0501' }),
    loc('random-cave', [], { label: 'pos0101' }),
    loc('off-grid'),
  )
  const fallback = mapPositions(g, ['posn0201 posn0101 pos0001 pos0101 pos0201 pos0301 pos0401 pos0501'])
  assert.deepEqual(mineDescentPositions(g, fallback), {
    station: [85, 66],
    'random-cave': [14, 66],
    'off-grid': fallback['off-grid'],
  })
})

test('the mine passage anchor is the same for either direction', () => {
  assert.deepEqual(mineDescentPassage('pos0201', 'pos0101'), [17, 47])
  assert.equal(mineDescentPassage('pos0101', 'pos0301'), undefined)
})

test('grid layout centres each label in its cell, as percentages', () => {
  const p = gridPositions(['a b', 'c .'])
  assert.deepEqual(p.a, [25, 25])
  assert.deepEqual(p.b, [75, 25])
  assert.deepEqual(p.c, [25, 75])
  assert.equal('.' in p, false)
})

test('grid layout uses the middle of a label spanning several cells', () => {
  const p = gridPositions(['a a', 'b c'])
  assert.deepEqual(p.a, [50, 25])
})

test('grid layout tolerates ragged rows and extra spaces', () => {
  const p = gridPositions(['  a   b ', 'c'])
  assert.equal(p.a[1], 25)
  assert.equal(p.c[1], 75)
})

test('auto layout places every location inside the map, neighbours in later columns', () => {
  const g = world(loc('a', ['b']), loc('b', ['a', 'c']), loc('c', ['b']), loc('lonely'))
  const p = autoPositions(g)
  assert.equal(Object.keys(p).length, 4)
  assert.ok(p.a[0] < p.b[0] && p.b[0] < p.c[0])
  for (const [x, y] of Object.values(p)) assert.ok(x > 0 && x < 100 && y > 0 && y < 100)
})

test('auto layout never stacks two locations on one point', () => {
  const g = world(loc('a', ['b', 'c', 'd']), loc('b', ['a']), loc('c', ['a']), loc('d', ['a']), loc('e'))
  const keys = Object.values(autoPositions(g)).map(p => p.join(','))
  assert.equal(new Set(keys).size, keys.length)
})

test('auto layout of an empty or single graph is safe', () => {
  assert.deepEqual(autoPositions({}), {})
  assert.deepEqual(autoPositions(world(loc('a'))), { a: [50, 50] })
})

test('map positions follow the scenario grid by label and fall back to auto layout for the rest', () => {
  const g = world(loc('a', ['b'], { label: 'x' }), loc('b', ['a'], { label: 'y' }), loc('c', [], { label: 'zzz' }))
  const p = mapPositions(g, ['x y'])
  assert.equal(p.a[0], 25)
  assert.equal(p.b[0], 75)
  assert.equal(p.a[1], p.b[1])
  assert.ok(p.c)
  assert.notDeepEqual(p.c, p.a)
  assert.notDeepEqual(p.c, p.b)
})

test('map positions without a grid use auto layout for all', () => {
  const g = world(loc('a', ['b']), loc('b', ['a']))
  assert.equal(Object.keys(mapPositions(g, null)).length, 2)
})

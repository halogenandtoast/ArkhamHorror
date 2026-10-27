import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import vm from 'node:vm'
import ts from 'typescript'

const source = readFileSync(new URL('../src/arkham/components/Connections.vue', import.meta.url), 'utf8')
const routing = source.slice(source.indexOf('const previousCurveOffsets'), source.indexOf('function obstructedChevronCurve'))
function router() {
  const zoom = { value: 1 }
  const obstacle = { left: 90, top: -10.01, width: 20, height: 20 }
  const context = vm.createContext({
    mapZoom: zoom, scaled: px => px * zoom.value, segmentsConflict: () => false,
    svgEl: { getBoundingClientRect: () => ({ left: 0, top: 0 }) },
    locations: { value: [{ id: 'obstacle' }] },
    document: { querySelector: () => ({ getBoundingClientRect: () => obstacle }) },
  })
  vm.runInContext(ts.transpile(routing, { target: ts.ScriptTarget.ES2022 }), context)
  const candidate = {
    connection: 'a:b', start: { dataset: { id: 'a' } }, end: { dataset: { id: 'b' } },
    x1: 0, y1: 0, x2: 200, y2: 0,
  }
  return { offsets: () => context.curveOffsets([candidate]).get('a:b') ?? 0, context, obstacle, zoom }
}
test('retains a clear curve despite changes in outward preference', () => {
  const r = router()
  const initial = r.offsets()
  assert.notEqual(initial, 0)
  for (let i = 0; i < 20; i++) {
    r.obstacle.top += i % 2 === 0 ? 0.02 : -0.02
    assert.equal(r.offsets(), initial)
  }
})
test('switches when the current curve becomes obstructed', () => {
  const r = router()
  const initial = r.offsets()
  r.obstacle.top = initial > 0 ? -5 : -55
  r.obstacle.height = 60
  assert.equal(Math.sign(r.offsets()), -Math.sign(initial))
})
test('scales remembered curves with zoom and forgets removed connections', () => {
  const r = router()
  const initial = r.offsets()
  r.zoom.value = 2
  assert.equal(r.offsets(), initial * 2)
  r.context.curveOffsets([])
  r.zoom.value = 1
  r.obstacle.top = -9.99
  assert.equal(Math.sign(r.offsets()), -Math.sign(initial))
})

test('one-way arrow keeps its lane until another lane avoids more cards', () => {
  const zoom = { value: 1 }
  const rect = { left: 90, right: 110, top: -10, bottom: 10 }
  const context = vm.createContext({
    mapZoom: zoom, scaled: px => px * zoom.value,
    svgEl: { getBoundingClientRect: () => ({ left: 0, top: 0 }) },
    locations: { value: [{ id: 'obstacle' }] },
    elementFor: () => ({ getBoundingClientRect: () => rect }),
  })
  const arrows = source.slice(source.indexOf('const previousChevronOffsets'), source.indexOf('function obstructedChevronCurve'))
  vm.runInContext(ts.transpile(arrows, { target: ts.ScriptTarget.ES2022 }), context)
  const candidate = {
    connection: 'a->b', start: { dataset: { id: 'a' } }, end: { dataset: { id: 'b' } },
    x1: 0, y1: 0, x2: 200, y2: 0,
  }
  assert.equal(context.stableChevronCurve(candidate, 100), 100)
  for (let i = 0; i < 20; i++) {
    rect.top += i % 2 === 0 ? 0.02 : -0.02
    assert.equal(context.stableChevronCurve(candidate, i % 2 ? 100 : -100), 100)
  }
  zoom.value = 2
  assert.equal(context.stableChevronCurve(candidate, -200), 200)
  zoom.value = 1
  rect.top = -5
  rect.bottom = 60
  assert.equal(context.stableChevronCurve(candidate, -100), -100)
})


test('returns a previously curved connection to straight after an obstruction moves', () => {
  const r = router()
  assert.notEqual(r.offsets(), 0)
  r.obstacle.top = 200
  assert.equal(r.offsets(), 0)
})

test('returns a one-way arrow to straight when routing no longer needs a bend', () => {
  const context = vm.createContext({
    mapZoom: { value: 1 }, scaled: px => px,
    svgEl: { getBoundingClientRect: () => ({ left: 0, top: 0 }) },
    locations: { value: [] }, elementFor: () => null,
  })
  const arrows = source.slice(source.indexOf('const previousChevronOffsets'), source.indexOf('function obstructedChevronCurve'))
  vm.runInContext(ts.transpile(arrows, { target: ts.ScriptTarget.ES2022 }), context)
  const candidate = {
    connection: 'a->b', start: { dataset: { id: 'a' } }, end: { dataset: { id: 'b' } },
    x1: 0, y1: 0, x2: 200, y2: 0,
  }
  assert.equal(context.stableChevronCurve(candidate, 100), 100)
  assert.equal(context.stableChevronCurve(candidate, 0), 0)
})

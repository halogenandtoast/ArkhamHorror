import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import ts from 'typescript'

const source = readFileSync(new URL('../src/arkham/gameDeltaFx.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { diffSnapshots, snapshotOf } = await import('data:text/javascript;base64,' + Buffer.from(js).toString('base64'))

const inv = (damage = 0, horror = 0) => ({ damage, horror, clues: 0, resources: 0 })
const snap = (o = {}) => ({ investigators: {}, enemies: {}, agendas: {}, ...o })

test('reports damage and horror taken, with the hit staggered after the beat', () => {
  const events = diffSnapshots(snap({ investigators: { a: inv() } }), snap({ investigators: { a: inv(2, 1) } }))
  assert.deepEqual(events.map((e) => [e.kind, e.amount]), [['damage', 2], ['horror', 1]])
  assert.ok(events[1].delayMs > events[0].delayMs)
})

test('an exhausting engaged enemy that hurt its investigator lunges first', () => {
  const prev = snap({ investigators: { a: inv() }, enemies: { e: { damage: 0, exhausted: false, engagedWith: 'a' } } })
  const next = snap({ investigators: { a: inv(1) }, enemies: { e: { damage: 0, exhausted: true, engagedWith: 'a' } } })
  const events = diffSnapshots(prev, next)
  assert.equal(events[0].kind, 'attack')
  assert.equal(events[0].toward, 'investigator-a')
  assert.ok(events.find((e) => e.kind === 'damage').delayMs >= 400)
})

test('doom on an agenda and a defeated enemy are reported; unchanged boards are quiet', () => {
  const prev = snap({ agendas: { g: 1 }, enemies: { e: { damage: 0, exhausted: false, engagedWith: null } } })
  const next = snap({ agendas: { g: 2 } })
  assert.deepEqual(diffSnapshots(prev, next).map((e) => [e.kind, e.amount]), [['enemyDefeated', 0], ['doom', 1]])
  assert.deepEqual(diffSnapshots(next, next), [])
})

test('snapshotOf reads tokens and pending assignments', () => {
  const s = snapshotOf({
    investigators: { a: { tokens: { Damage: 1, Clue: 2 }, assignedHealthDamage: 1 } },
    enemies: { e: { tokens: { Damage: 3 }, assignedDamage: 1, exhausted: true, placement: { tag: 'InThreatArea', contents: 'a' } } },
    agendas: { g: { doom: 4 } },
  })
  assert.equal(s.investigators.a.damage, 2)
  assert.equal(s.investigators.a.clues, 2)
  assert.deepEqual(s.enemies.e, { damage: 4, exhausted: true, engagedWith: 'a' })
  assert.equal(s.agendas.g, 4)
})

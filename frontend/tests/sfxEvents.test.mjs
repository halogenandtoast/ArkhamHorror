import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import ts from 'typescript'

// sfxEvents.ts is import-free on purpose, like locationGraph.ts.
const source = readFileSync(new URL('../src/arkham/sfxEvents.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { sfxForUpdate } = await import('data:text/javascript;base64,' + Buffer.from(js).toString('base64'))

const inv = (o = {}) => ({ location: 'a', hand: [], tokens: {}, ...o })
const game = (o = {}) => ({ phase: 'InvestigationPhase', scenario: { id: 's' }, investigators: { i1: inv() }, enemies: {}, ...o })

test('first state and identical states are silent', () => {
  assert.deepEqual(sfxForUpdate(null, game()), [])
  assert.deepEqual(sfxForUpdate(game(), game()), [])
})

test('movement, damage, horror, clue and draw are detected', () => {
  const next = game({ investigators: { i1: inv({ location: 'b', hand: [1], tokens: { Damage: 1, Horror: 1, Clue: 1 } }) } })
  const got = sfxForUpdate(game(), next)
  assert.ok(got.includes('hurt') && got.includes('horror') && got.includes('move') && got.length <= 4)
})

test('enemy damage is a hit, a vanished enemy is a defeat', () => {
  const prev = game({ enemies: { e1: { tokens: {} }, e2: { tokens: {} } } })
  const next = game({ enemies: { e1: { tokens: { Damage: 2 } } } })
  const got = sfxForUpdate(prev, next)
  assert.ok(got.includes('hit') && got.includes('enemyDefeat'))
})

test('phase or scenario change is a transition', () => {
  assert.deepEqual(sfxForUpdate(game(), game({ phase: 'EnemyPhase' })), ['transition'])
  assert.deepEqual(sfxForUpdate(game(), game({ scenario: { id: 'other' } })), ['transition'])
})

test('skill test: token reveal, then success or failure', () => {
  const testing = (n) => game({ skillTest: { id: 't', revealedChaosTokens: new Array(n).fill(0) } })
  assert.deepEqual(sfxForUpdate(testing(0), testing(1)), ['token'])
  const pass = game({ skillTestResults: { skillTestResultsSuccess: true } })
  const fail = game({ skillTestResults: { skillTestResultsSuccess: false } })
  assert.deepEqual(sfxForUpdate(game(), pass), ['success'])
  assert.deepEqual(sfxForUpdate(game(), fail), ['fail'])
  assert.deepEqual(sfxForUpdate(pass, game({ skillTestResults: { skillTestResultsSuccess: true } })), [])
})

import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import ts from 'typescript'

// locationInfoModel.ts is import-free on purpose so it can be loaded here without the Vue toolchain.
const source = readFileSync(new URL('../src/arkham/locationInfoModel.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { buildLocationInfo, currentShroud, hasEnterEffect, faceUp, choiceAction, abilityActions } = await import(
  'data:text/javascript;base64,' + Buffer.from(js).toString('base64')
)

const attic = {
  code: '01113', name: 'Attic', text: '<b>Forced</b> - After you enter the Attic: Take 1 horror.',
  flavor: 'The bloody carcass...', back_flavor: 'The smell of rotten meat...', shroud: 1, clues: 2, victory: 1, traits: '',
}
const loc = (extra = {}) => ({
  id: 'l1', label: 'attic', cardCode: '01113', revealed: false, exhausted: false, shroud: 1, tokens: {},
  investigators: [], enemies: [], connectedLocations: ['l0'], keys: [], seals: [], floodLevel: null, brazier: null,
  breaches: null, modifiers: [], ...extra,
})
const ctx = (extra = {}) => ({
  front: attic, ownBack: null, fallbackName: '01113', playerCount: 2, myLocationId: 'l0', enemyCount: 0,
  choices: [], selectIndex: -1, ...extra,
})
const ability = (...actions) => ({
  ability: { basic: true, type: { tag: 'ActionAbility', actions: { tag: 'OrActions', contents: actions.map((a) => ({ tag: 'SingleAction', contents: a })) } } },
})

test('an unrevealed location shows only its unrevealed side', () => {
  const info = buildLocationInfo(loc(), ctx())
  assert.equal(info.face.name, 'Attic')
  assert.equal(info.face.text, null)
  assert.equal(info.face.flavor, 'The smell of rotten meat...')
  assert.equal(info.shroud, null)
  assert.equal(info.clues.printed, null)
  assert.equal(info.victory, null)
  assert.equal(info.onEnter, false)
})

test('a revealed location shows its revealed side, shroud, clues per player and victory', () => {
  const info = buildLocationInfo(loc({ revealed: true, tokens: { Clue: 3 } }), ctx())
  assert.equal(info.face.text, attic.text)
  assert.deepEqual(info.shroud, { printed: 1, current: 1, x: false })
  assert.deepEqual(info.clues, { left: 3, printed: 4 })
  assert.equal(info.victory, 1)
  assert.equal(info.onEnter, true)
})

test('fixed clue values are not multiplied by the player count', () => {
  const info = buildLocationInfo(loc({ revealed: true }), ctx({ front: { ...attic, clues_fixed: true } }))
  assert.equal(info.clues.printed, 2)
})

test('a different unrevealed name is used instead of the revealed one', () => {
  const front = { ...attic, name: 'Secret Room', back_name: 'Strange Door', back_text: 'Locked.' }
  assert.deepEqual(faceUp(false, front, null, 'x'), { name: 'Strange Door', known: true, traits: null, text: 'Locked.', flavor: 'The smell of rotten meat...' })
  const own = { code: '01113b', name: 'Own Back', text: 'Back text' }
  assert.equal(faceUp(false, front, own, 'x').name, 'Own Back')
})

test('without a card record only the fallback name shows', () => {
  const info = buildLocationInfo(loc({ revealed: true }), ctx({ front: null }))
  assert.deepEqual(info.face, { name: '01113', known: false, traits: null, text: null, flavor: null })
  assert.deepEqual(info.shroud, { printed: 1, current: 1, x: false })
  assert.equal(info.clues.printed, null)
})

test('shroud modifiers follow the engine order', () => {
  const m = (tag, contents) => ({ type: { tag, contents } })
  assert.equal(currentShroud(2, [m('ShroudModifier', 1), m('ShroudModifier', -2)]), 1)
  assert.equal(currentShroud(2, [m('ShroudModifier', -5)]), 0)
  assert.equal(currentShroud(2, [m('BaseShroud', 4), m('ShroudModifier', 1)]), 5)
  assert.equal(currentShroud(2, [m('ShroudModifier', 3), m('SetShroud', 1)]), 1)
  assert.equal(currentShroud(2, [m('OtherModifier', 'ShroudCannotBeReduced'), m('ShroudModifier', -1), m('ShroudModifier', 1)]), 3)
})

test('forced enter effects are spotted, other forced effects are not', () => {
  assert.equal(hasEnterEffect('<b>Forced</b> - After you enter the Cellar: Take 1 damage.'), true)
  assert.equal(hasEnterEffect('<b>Forced</b> - At the end of the round: place 1 doom.'), false)
  assert.equal(hasEnterEffect('[action]: Investigate. You may enter...'), false)
  assert.equal(hasEnterEffect(null), false)
})

test('move and investigate are read from the ability actions, the rest kept in order', () => {
  const choices = [{ index: 4, contents: ability('Investigate') }, { index: 7, contents: ability('Move') }, { index: 9, contents: { ability: { type: { tag: 'FastAbility' } } } }]
  const info = buildLocationInfo(loc({ revealed: true }), ctx({ choices, selectIndex: 2 }))
  assert.equal(info.actions.move.index, 7)
  assert.equal(info.actions.investigate.index, 4)
  assert.deepEqual(info.actions.other.map((c) => c.index), [9])
  assert.equal(info.targetable, true)
  assert.deepEqual(choiceAction({ tag: 'FightLabel' }), [])
})

test('here and adjacent are relative to the viewing investigator', () => {
  assert.equal(buildLocationInfo(loc(), ctx()).adjacent, true)
  assert.equal(buildLocationInfo(loc(), ctx({ myLocationId: 'l1' })).here, true)
  assert.equal(buildLocationInfo(loc(), ctx({ myLocationId: null })).adjacent, false)
})

test('a location ability that moves elsewhere or investigates its own way is not the basic action', () => {
  const special = (action) => ({ ability: { basic: false, type: { tag: 'ActionAbility', actions: { tag: 'SingleAction', contents: action } } } })
  const choices = [{ index: 1, contents: special('Move') }, { index: 2, contents: special('Investigate') }, { index: 5, contents: ability('Move') }]
  const info = buildLocationInfo(loc({ revealed: true }), ctx({ choices }))
  assert.equal(info.actions.move.index, 5)
  assert.equal(info.actions.investigate, null)
  assert.deepEqual(info.actions.other.map((c) => c.index), [1, 2])
})

test('per-player shroud is multiplied and an X shroud is flagged', () => {
  assert.deepEqual(buildLocationInfo(loc({ revealed: true, shroud: 2, shroudKind: 'PerPlayer' }), ctx()).shroud, { printed: 4, current: 4, x: false })
  assert.equal(buildLocationInfo(loc({ revealed: true, shroud: 0, shroudKind: 'ValueX' }), ctx()).shroud.x, true)
})

test('abilityActions reads printed and basic actions alike', () => {
  const ability = (basic, action) => ({ ability: { basic, type: { tag: 'ActionAbility', actions: { tag: 'SingleAction', contents: action } } } })
  assert.deepEqual(abilityActions(ability(false, 'Move')), ['Move'])
  assert.deepEqual(abilityActions(ability(true, 'Play')), ['Play'])
  assert.deepEqual(choiceAction(ability(false, 'Move')), [])
  assert.deepEqual(abilityActions({ ability: { type: { tag: 'ReactionAbility' } } }), [])
})

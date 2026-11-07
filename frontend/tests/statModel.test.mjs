import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import ts from 'typescript'

// statModel.ts is import-free on purpose so it can be loaded here without the Vue toolchain.
const source = readFileSync(new URL('../src/arkham/statModel.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { computeStat, effectsOfSource, blocksEntry, sourceEntityId } = await import(
  'data:text/javascript;base64,' + Buffer.from(js).toString('base64')
)

const from = (id) => ({ sourceTag: 'OtherSource', tag: 'TreacherySource', contents: id })
const m = (tag, contents, id = 't1') => ({ type: { tag, contents }, source: from(id) })
const skill = (tag, skillType, value, id = 'a1') => ({ type: { tag, skillType, value }, source: from(id) })

test('a stat with no modifiers is its printed value and has no trend', () => {
  const s = computeStat('fight', 3, [])
  assert.equal(s.current, 3)
  assert.equal(s.trend, null)
  assert.equal(s.bad, false)
  assert.deepEqual(s.parts, [])
})

test('enemy fight and evade add up and name their sources', () => {
  const s = computeStat('fight', 3, [m('EnemyFight', 1, 'a'), m('EnemyFight', 2, 'b'), m('EnemyEvade', 5, 'c')])
  assert.equal(s.current, 6)
  assert.deepEqual(s.parts.map((p) => [p.sourceId, p.delta]), [['a', 1], ['b', 2]])
  assert.equal(s.trend, 'up')
  assert.equal(s.bad, true)
})

test('a lowered enemy stat is good news, and the stat never goes below zero', () => {
  const s = computeStat('evade', 2, [m('EnemyEvade', -5)])
  assert.equal(s.current, 0)
  assert.equal(s.trend, 'down')
  assert.equal(s.bad, false)
})

test('a minimum on a modifier floors the result', () => {
  const s = computeStat('enemyHealth', 3, [m('HealthModifierWithMin', [-5, 1])])
  assert.equal(s.current, 1)
})

test('shroud follows the engine order: base, shifts, override', () => {
  assert.equal(computeStat('shroud', 2, [m('BaseShroud', 4), m('ShroudModifier', 1)]).current, 5)
  assert.equal(computeStat('shroud', 2, [m('ShroudModifier', 1), m('SetShroud', 0)]).current, 0)
  const raisedOnly = computeStat('shroud', 3, [m('OtherModifier', 'ShroudCannotBeReduced'), m('ShroudModifier', -1), m('ShroudModifier', 1)])
  assert.equal(raisedOnly.current, 4)
})

test('shroud matches the location info model for the same modifiers', () => {
  const mods = [m('BaseShroud', 4), m('ShroudModifier', 1), m('ShroudModifier', -3)]
  assert.equal(computeStat('shroud', 2, mods).current, 2)
})

test('investigator skills only take modifiers for their own skill, and raised skills are good news', () => {
  const mods = [skill('SkillModifier', 'SkillCombat', 2), skill('SkillModifier', 'SkillAgility', -1), skill('SetSkillValue', 'SkillWillpower', 1)]
  const combat = computeStat('combat', 3, mods)
  assert.equal(combat.current, 5)
  assert.equal(combat.bad, false)
  assert.equal(computeStat('agility', 3, mods).current, 2)
  assert.equal(computeStat('agility', 3, mods).bad, true)
  assert.equal(computeStat('willpower', 4, mods).current, 1)
  assert.equal(computeStat('intellect', 4, mods).current, 4)
})

test('the source of a modifier is found through wrapper sources', () => {
  assert.equal(sourceEntityId(from('x')), 'x')
  assert.equal(sourceEntityId({ sourceTag: 'AbilitySource', contents: [from('y'), 1] }), 'y')
  assert.equal(sourceEntityId({ sourceTag: 'ProxySource', source: from('p'), originalSource: from('o') }), 'o')
  assert.equal(sourceEntityId(undefined), null)
})

test('effects are grouped by the card that granted them', () => {
  const mods = [m('ShroudModifier', 2, 'fog'), m('OtherModifier', 'CannotBeEnteredBy', 'wood'), m('EnemyFight', 1, 'other')]
  assert.deepEqual(effectsOfSource(mods, 'fog'), [{ kind: 'stat', stat: 'shroud', delta: 2, mode: 'add' }])
  assert.deepEqual(effectsOfSource(mods, 'wood'), [{ kind: 'flag', flag: 'CannotBeEnteredBy' }])
  assert.deepEqual(effectsOfSource(mods, 'nobody'), [])
})

test('a barricade or a locked door blocks entry', () => {
  assert.equal(blocksEntry([m('OtherModifier', 'CannotBeEnteredBy')]), true)
  assert.equal(blocksEntry([m('CannotEnter', 'inv1')]), true)
  assert.equal(blocksEntry([m('ShroudModifier', 1)]), false)
  assert.equal(blocksEntry(undefined), false)
})

test('investigator stats count lasting asset bonuses but not ones an effect hands out for a turn or an attack', () => {
  const lasting = skill('SkillModifier', 'SkillWillpower', 1, 'rosary')
  const oneAttack = { ...skill('SkillModifier', 'SkillCombat', 2, 'gun'), effect: 'e1' }
  const perAction = { type: { tag: 'ActionSkillModifier', action: 'Fight', skillType: 'SkillCombat', value: 1 }, source: from('gun') }
  const mods = [lasting, oneAttack, perAction, m('HealthModifier', 1, 'armor'), { ...m('SanityModifier', 2, 'charm'), effect: 'e2' }]
  assert.equal(computeStat('willpower', 3, mods).current, 4)
  assert.equal(computeStat('combat', 3, mods).current, 3)
  assert.equal(computeStat('health', 7, mods).current, 8)
  assert.equal(computeStat('sanity', 7, mods).current, 7)
})

test('an enemy keeps counting effect modifiers, since they really change the enemy', () => {
  assert.equal(computeStat('fight', 3, [{ ...m('EnemyFight', 1), effect: 'e1' }]).current, 4)
})

import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import ts from 'typescript'

// actionConfirmPolicy.ts is import-free on purpose so it can be loaded here without the Vue toolchain.
const source = readFileSync(new URL('../src/arkham/actionConfirmPolicy.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { decideActionConfirm, playProvokes } = await import(
  'data:text/javascript;base64,' + Buffer.from(js).toString('base64')
)

const playerWindow = { tag: 'PlayerWindowChooseOne', isPlayerWindow: true }
const card = (id, cardCode) => ({ tag: 'PlayerCard', contents: { id, cardCode } })
const TEXT = {
  '01088': 'Gain 3 resources.',
  '01022': 'Fast. Play after you defeat an enemy.',
  '01051': '<b>Fight.</b> This attack uses [agility] instead of [combat].',
}

function input(overrides = {}) {
  return {
    question: playerWindow,
    choice: undefined,
    playerId: 'p1',
    investigators: {
      roland: { id: 'roland', playerId: 'p1', hand: [card('c1', '01088'), card('c2', '01022'), card('c3', '01051')], engagedEnemies: [] },
      daisy: { id: 'daisy', playerId: 'p2', hand: [card('c9', '01088')], engagedEnemies: ['ghoul'] },
    },
    enemies: { ghoul: { id: 'ghoul', exhausted: false }, rat: { id: 'rat', exhausted: true } },
    cardText: (code) => TEXT[code] ?? null,
    confirmPlays: true,
    warnAttacksOfOpportunity: true,
    ...overrides,
  }
}

const engaged = (ids) => (base) => ({
  ...base,
  investigators: { ...base.investigators, roland: { ...base.investigators.roland, engagedEnemies: ids } },
})
const playChoice = (id) => ({ tag: 'TargetLabel', target: { tag: 'CardIdTarget', contents: id } })
const basicAction = (action) => ({
  tag: 'AbilityLabel',
  investigatorId: 'roland',
  ability: { type: { tag: 'ActionAbility', actions: { tag: 'SingleAction', contents: action } } },
})

test('asks before playing a card from hand, with no warning when nothing is engaged', () => {
  assert.deepEqual(decideActionConfirm(input({ choice: playChoice('c1') })), {
    kind: 'play', investigatorId: 'roland', cardCode: '01088', provokingEnemyIds: [],
  })
})

test('a hand card offered outside the player window is a target, not a play', () => {
  assert.equal(decideActionConfirm(input({ choice: playChoice('c1'), question: { tag: 'ChooseOne' } })), null)
})

test('sees through a labelled player window', () => {
  const question = { tag: 'QuestionLabel', question: playerWindow }
  assert.equal(decideActionConfirm(input({ choice: playChoice('c1'), question }))?.kind, 'play')
})

test('another seat\'s hand card is not this player\'s play', () => {
  assert.equal(decideActionConfirm(input({ choice: playChoice('c9') })), null)
})

test('playing a non-Fast card while engaged with a ready enemy warns about it', () => {
  const result = decideActionConfirm(engaged(['ghoul', 'rat'])(input({ choice: playChoice('c1') })))
  assert.deepEqual(result.provokingEnemyIds, ['ghoul'])
})

test('Fast cards and Fight events do not provoke', () => {
  for (const id of ['c2', 'c3']) {
    const result = decideActionConfirm(engaged(['ghoul'])(input({ choice: playChoice(id) })))
    assert.deepEqual(result.provokingEnemyIds, [])
  }
})

test('with play confirmation off, a play only asks when it provokes', () => {
  const base = input({ choice: playChoice('c1'), confirmPlays: false })
  assert.equal(decideActionConfirm(base), null)
  assert.equal(decideActionConfirm(engaged(['ghoul'])(base))?.kind, 'play')
})

test('a provoking basic action warns only while a ready enemy is engaged', () => {
  assert.equal(decideActionConfirm(input({ choice: basicAction('Investigate') })), null)
  assert.equal(decideActionConfirm(engaged(['rat'])(input({ choice: basicAction('Investigate') }))), null)
  assert.deepEqual(decideActionConfirm(engaged(['ghoul'])(input({ choice: basicAction('Move') }))), {
    kind: 'action', investigatorId: 'roland', cardCode: null, provokingEnemyIds: ['ghoul'],
  })
})

test('fight, evade, resign and parley never provoke', () => {
  for (const action of ['Fight', 'Evade', 'Resign', 'Parley']) {
    assert.equal(decideActionConfirm(engaged(['ghoul'])(input({ choice: basicAction(action) }))), null)
  }
})

test('nested actions are flattened', () => {
  const choice = {
    ...basicAction('Fight'),
    ability: { type: { tag: 'ActionAbility', actions: { tag: 'AndActions', contents: [{ tag: 'SingleAction', contents: 'Activate' }, { tag: 'SingleAction', contents: 'Fight' }] } } },
  }
  assert.equal(decideActionConfirm(engaged(['ghoul'])(input({ choice }))), null)
})

test('drawing a card and taking a resource provoke', () => {
  const draw = { tag: 'ComponentLabel', component: { tag: 'InvestigatorDeckComponent', investigatorId: 'roland' } }
  const resource = { tag: 'ComponentLabel', component: { tag: 'InvestigatorComponent', investigatorId: 'roland', tokenType: 'ResourceToken' } }
  const clue = { tag: 'ComponentLabel', component: { tag: 'InvestigatorComponent', investigatorId: 'roland', tokenType: 'ClueToken' } }
  assert.equal(decideActionConfirm(engaged(['ghoul'])(input({ choice: draw })))?.kind, 'action')
  assert.equal(decideActionConfirm(engaged(['ghoul'])(input({ choice: resource })))?.kind, 'action')
  assert.equal(decideActionConfirm(engaged(['ghoul'])(input({ choice: clue }))), null)
})

test('warnings off and plays off asks nothing', () => {
  const off = { confirmPlays: false, warnAttacksOfOpportunity: false }
  assert.equal(decideActionConfirm(engaged(['ghoul'])(input({ choice: playChoice('c1'), ...off }))), null)
  assert.equal(decideActionConfirm(engaged(['ghoul'])(input({ choice: basicAction('Move'), ...off }))), null)
})

test('unknown card text is treated as provoking', () => {
  assert.equal(playProvokes(null), true)
  assert.equal(playProvokes('<b>Fast</b>. Play when...'), false)
  assert.equal(playProvokes('Uses (4 ammo). [action] Fight.'), true)
})

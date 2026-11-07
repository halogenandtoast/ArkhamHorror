import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import ts from 'typescript'

// autoAnswerPolicy.ts is import-free on purpose so it can be loaded here without the Vue toolchain.
const source = readFileSync(new URL('../src/arkham/autoAnswerPolicy.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { decideAutoAnswer } = await import('data:text/javascript;base64,' + Buffer.from(js).toString('base64'))

const chooseOne = (extra = {}) => ({ tag: 'ChooseOne', ...extra })
const encounterDeck = { tag: 'TargetLabel', target: { tag: 'EncounterDeckTarget' } }
const label = (text) => ({ tag: 'Label', label: text })

test('draws from the encounter deck when it is the only place to draw from', () => {
  assert.deepEqual(decideAutoAnswer(chooseOne(), [encounterDeck]), { index: 0, reason: 'encounterDraw' })
})

test('applies skill test results when that is the only button', () => {
  assert.deepEqual(decideAutoAnswer(chooseOne(), [{ tag: 'SkillTestApplyResultsButton' }]), {
    index: 0,
    reason: 'applyResults',
  })
})

test('starts the skill test when nothing can be committed', () => {
  assert.deepEqual(decideAutoAnswer(chooseOne(), [{ tag: 'StartSkillTestButton' }]), {
    index: 0,
    reason: 'startSkillTest',
  })
})

test('takes the one valid choice and ignores invalid ones', () => {
  const choices = [{ tag: 'InvalidLabel', label: 'x' }, label('Take 1 horror')]
  assert.deepEqual(decideAutoAnswer(chooseOne(), choices), { index: 1, reason: 'singleChoice' })
})

test('unwraps a labelled question', () => {
  const q = { tag: 'QuestionLabel', label: 'Choose', question: chooseOne() }
  assert.deepEqual(decideAutoAnswer(q, [label('Continue')]), { index: 0, reason: 'singleChoice' })
})

test('leaves a real decision to the player', () => {
  assert.equal(decideAutoAnswer(chooseOne(), [label('A'), label('B')]), null)
  assert.equal(decideAutoAnswer(chooseOne(), [{ tag: 'StartSkillTestButton' }, { tag: 'CardLabel' }]), null)
  assert.equal(decideAutoAnswer(chooseOne(), [{ tag: 'AbilityLabel' }, { tag: 'SkipTriggersButton' }]), null)
})

test('closes a player window when only end turn or done is left', () => {
  const win = chooseOne({ isPlayerWindow: true })
  assert.deepEqual(decideAutoAnswer(win, [{ tag: 'EndTurnButton' }]), { index: 0, reason: 'closeWindow' })
  assert.deepEqual(decideAutoAnswer(win, [{ tag: 'InvalidLabel' }, { tag: 'Done' }]), { index: 1, reason: 'closeWindow' })
})

test('keeps a player window open while anything can still be played', () => {
  const win = chooseOne({ isPlayerWindow: true })
  assert.equal(decideAutoAnswer(win, [{ tag: 'EndTurnButton' }, { tag: 'AbilityLabel' }]), null)
  assert.equal(decideAutoAnswer(win, [label('Continue')]), null)
})

test('never answers story, flavor, or non choose-one questions', () => {
  assert.equal(decideAutoAnswer({ tag: 'Read' }, [label('Continue')]), null)
  assert.equal(decideAutoAnswer(chooseOne(), [{ tag: 'Info' }, label('Continue')]), null)
  assert.equal(decideAutoAnswer({ tag: 'ChooseN' }, [label('A')]), null)
  assert.equal(decideAutoAnswer({ tag: 'ChooseDeck' }, []), null)
})

test('waits for the player while revealed cards are on screen', () => {
  assert.equal(decideAutoAnswer(chooseOne(), [{ tag: 'Done', label: 'done' }], { hasRevealedCards: true }), null)
})

test('unknown lone choices stop as before', () => {
  assert.equal(decideAutoAnswer(chooseOne(), [{ tag: 'EndTurnButton' }]), null)
  assert.equal(decideAutoAnswer(chooseOne(), [{ tag: 'ChaosTokenLabel' }]), null)
  assert.equal(decideAutoAnswer(chooseOne(), [{ tag: 'SomethingNew' }]), null)
  assert.equal(decideAutoAnswer(null, [label('A')]), null)
})

// Damage assignment arrives as QuestionWithSource > QuestionLabel > ChooseOne.
const assignQuestion = (text) => ({
  tag: 'QuestionWithSource',
  source: { tag: 'GameSource' },
  question: { tag: 'QuestionLabel', label: text, question: chooseOne() },
})
const invToken = (tokenType, investigatorId = 'i1', tag = 'ComponentLabel') => ({
  tag,
  component: { tag: 'InvestigatorComponent', investigatorId, tokenType },
})
const assetToken = (tokenType) => ({
  tag: 'ComponentLabel',
  component: { tag: 'AssetComponent', assetId: 'a1', tokenType },
})

test('assigns a lone damage token through the source and label wrappers', () => {
  assert.deepEqual(decideAutoAnswer(assignQuestion('Assign 2 damage'), [invToken('DamageToken')]), {
    index: 0,
    reason: 'assignDamage',
  })
})

test('assigns damage before horror when only the investigator can take either', () => {
  const choices = [invToken('HorrorToken'), invToken('DamageToken')]
  assert.deepEqual(decideAutoAnswer(assignQuestion('Assign 2 damage and 1 horror'), choices), {
    index: 1,
    reason: 'assignDamage',
  })
  assert.deepEqual(decideAutoAnswer(assignQuestion('Assign 1 horror'), [invToken('HorrorToken')]), {
    index: 0,
    reason: 'assignDamage',
  })
})

test('stops when an asset could soak, another investigator is offered, or the label is not an assignment', () => {
  const label2 = assignQuestion('Assign 1 damage and 1 horror')
  assert.equal(decideAutoAnswer(label2, [invToken('DamageToken'), assetToken('DamageToken')]), null)
  assert.equal(decideAutoAnswer(label2, [invToken('DamageToken'), invToken('HorrorToken', 'i2')]), null)
  assert.equal(decideAutoAnswer(label2, [invToken('DamageToken'), invToken('HorrorToken', 'i1', 'AuxiliaryComponentLabel')]), null)
  // Healing uses the same labels but has no "Assign" label: a real choice.
  const heal = { tag: 'QuestionWithSource', source: {}, question: chooseOne() }
  assert.equal(decideAutoAnswer(heal, [invToken('DamageToken'), invToken('HorrorToken')]), null)
})

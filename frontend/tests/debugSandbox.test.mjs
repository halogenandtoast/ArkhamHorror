import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* The debug sandbox's calculator decides pass/fail on its own, outside any
real skill test, so its rules are pinned here against the ones the engine
uses. The message builders are pinned too: they are hand-written JSON that
the backend's derived decoders (Arkham.Debug.SkillTest, Arkham.Message.ChaosBag)
must accept, and nothing else checks that shape. */
async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/debugSandbox.ts')
}

const token = (face, value, extra = {}) => ({ face, value, autoFail: false, autoSuccess: false, ...extra })

test('a test passes on a tie and reports the margin', async (t) => {
  const { sandboxResult } = await load(t)
  assert.deepEqual(sandboxResult([token('MinusOne', -1)], 4, 3), { kind: 'pass', total: 3, by: 0 })
  assert.deepEqual(sandboxResult([token('PlusOne', 1)], 4, 3), { kind: 'pass', total: 5, by: 2 })
})

test('ties fail when told to', async (t) => {
  const { sandboxResult } = await load(t)
  assert.deepEqual(sandboxResult([token('Zero', 0)], 3, 3, true), { kind: 'fail', total: 3, by: 0 })
})

test('the modified skill never drops below zero', async (t) => {
  const { sandboxResult } = await load(t)
  assert.deepEqual(sandboxResult([token('MinusEight', -8)], 2, 1), { kind: 'fail', total: 0, by: 1 })
})

test('several tokens add up', async (t) => {
  const { sandboxResult } = await load(t)
  const tokens = [token('MinusTwo', -2), token('BlessToken', 2), token('Skull', -1)]
  assert.deepEqual(sandboxResult(tokens, 4, 3), { kind: 'pass', total: 3, by: 0 })
})

test('auto-fail beats auto-success', async (t) => {
  const { sandboxResult } = await load(t)
  const tokens = [token('ElderSign', null, { autoSuccess: true }), token('AutoFail', null, { autoFail: true })]
  assert.deepEqual(sandboxResult(tokens, 10, 0), { kind: 'autoFail' })
  assert.deepEqual(sandboxResult([tokens[0]], 0, 10), { kind: 'autoSuccess' })
})

test('two frost tokens are an automatic failure', async (t) => {
  const { sandboxResult } = await load(t)
  assert.deepEqual(sandboxResult([token('FrostToken', -1)], 5, 1), { kind: 'pass', total: 4, by: 3 })
  assert.deepEqual(
    sandboxResult([token('FrostToken', -1), token('FrostToken', -1)], 5, 1),
    { kind: 'autoFail' },
  )
})

test('an unknown value waits for the player, and an override fills it in', async (t) => {
  const { sandboxResult, withOverride } = await load(t)
  const unknown = token('Tablet', null)
  assert.deepEqual(sandboxResult([unknown], 3, 2), { kind: 'unknown' })
  assert.deepEqual(sandboxResult([withOverride(unknown, -3)], 3, 2), { kind: 'fail', total: 0, by: 2 })
  // an empty override leaves the suggestion alone
  assert.equal(withOverride(unknown, undefined), unknown)
  assert.equal(withOverride(unknown, Number.NaN), unknown)
})

test('an override replaces an automatic result', async (t) => {
  const { sandboxResult, withOverride } = await load(t)
  const autoFail = token('AutoFail', null, { autoFail: true })
  assert.deepEqual(sandboxResult([withOverride(autoFail, 0)], 2, 2), { kind: 'pass', total: 2, by: 0 })
})

test('reveal and return messages match the backend constructors', async (t) => {
  const { revealMessage, returnMessage } = await load(t)
  assert.deepEqual(revealMessage('01001', 2), {
    tag: 'ChaosBagMessage',
    contents: { tag: 'DebugRevealChaosTokens_', contents: ['01001', 2] },
  })
  // never asks the engine for zero or a fraction of a token
  assert.deepEqual(revealMessage('01001', 0).contents.contents, ['01001', 1])
  assert.deepEqual(revealMessage('01001', 2.7).contents.contents, ['01001', 2])
  assert.deepEqual(returnMessage(), { tag: 'ChaosBagMessage', contents: { tag: 'DebugReturnChaosTokens_' } })
})

test('a skill test form encodes the DebugSkillTestSpec record', async (t) => {
  const { beginSkillTestMessage } = await load(t)
  const message = beginSkillTestMessage({
    investigator: '01001',
    skill: { kind: 'one', skill: 'SkillCombat' },
    action: 'Fight',
    target: { tag: 'EnemyTarget', contents: 'enemy-1' },
    sourceCard: 'card-1',
    difficulty: { kind: 'target' },
  })
  assert.deepEqual(message, {
    tag: 'DebugBeginSkillTest',
    contents: {
      investigator: '01001',
      skill: { tag: 'DebugTestOneSkill', contents: 'SkillCombat' },
      action: 'Fight',
      target: { tag: 'EnemyTarget', contents: 'enemy-1' },
      sourceCard: 'card-1',
      difficulty: { tag: 'DebugDifficultyOfTarget' },
    },
  })
})

test('combined, fixed and plain tests encode their own shapes', async (t) => {
  const { beginSkillTestMessage } = await load(t)
  const base = { investigator: '01001', action: null, target: null, sourceCard: null }
  const combined = beginSkillTestMessage({
    ...base,
    skill: { kind: 'combined', skills: ['SkillWillpower', 'SkillIntellect'] },
    difficulty: { kind: 'fixed', value: 4 },
  })
  assert.deepEqual(combined.contents.skill, { tag: 'DebugTestCombined', contents: ['SkillWillpower', 'SkillIntellect'] })
  assert.deepEqual(combined.contents.difficulty, { tag: 'DebugDifficultyFixed', contents: 4 })
  assert.equal(combined.contents.action, null)
  assert.equal(combined.contents.target, null)

  const fixed = beginSkillTestMessage({ ...base, skill: { kind: 'fixed', value: 3 }, difficulty: { kind: 'fixed', value: -2 } })
  assert.deepEqual(fixed.contents.skill, { tag: 'DebugTestFixed', contents: 3 })
  // a negative difficulty is clamped rather than sent
  assert.deepEqual(fixed.contents.difficulty, { tag: 'DebugDifficultyFixed', contents: 0 })
})

test('the form says what is missing before it can start', async (t) => {
  const { formProblem, targetKindFor } = await load(t)
  const form = {
    investigator: '01001',
    skill: { kind: 'one', skill: 'SkillIntellect' },
    action: 'Investigate',
    target: null,
    sourceCard: null,
    difficulty: { kind: 'fixed', value: 2 },
  }
  assert.equal(targetKindFor('Investigate'), 'LocationTarget')
  assert.equal(targetKindFor('Fight'), 'EnemyTarget')
  assert.equal(targetKindFor('Parley'), null)
  assert.equal(targetKindFor(null), null)
  assert.equal(formProblem(form), 'target')
  // the wrong kind of target is as good as none
  assert.equal(formProblem({ ...form, target: { tag: 'EnemyTarget', contents: 'e' } }), 'target')
  assert.equal(formProblem({ ...form, target: { tag: 'LocationTarget', contents: 'l' } }), null)
  assert.equal(formProblem({ ...form, action: null, difficulty: { kind: 'target' } }), 'difficulty')
  assert.equal(formProblem({ ...form, action: null, skill: { kind: 'combined', skills: [] } }), 'skill')
  assert.equal(formProblem({ ...form, investigator: '' }), 'investigator')
})

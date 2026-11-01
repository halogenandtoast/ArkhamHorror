import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import test from 'node:test'
import ts from 'typescript'

const source = await readFile(new URL('../src/arkham/standaloneSettingsSubmission.ts', import.meta.url), 'utf8')
const { outputText } = ts.transpileModule(source, {
  compilerOptions: { module: ts.ModuleKind.ES2022, target: ts.ScriptTarget.ES2022 },
})
const { createStandaloneSettingsSubmission } = await import(
  `data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`
)

const choosing = { question: { player: { tag: 'ChooseDeck' } } }
const settings = { question: { player: { tag: 'PickScenarioSettings' } } }

test('advances using HTTP state when no websocket update is delivered', async () => {
  const calls = []
  const submit = createStandaloneSettingsSubmission({
    playerId: 'player',
    save: async (value) => { calls.push(['save', value]) },
    fetch: async () => { calls.push(['fetch']); return choosing },
  })
  assert.equal(await submit([]), choosing)
  assert.deepEqual(calls, [['save', []], ['fetch']])
})

test('a failed refresh retries the read without answering the next question', async () => {
  let saves = 0
  let reads = 0
  const submit = createStandaloneSettingsSubmission({
    playerId: 'player',
    save: async () => { saves++ },
    fetch: async () => {
      if (++reads === 1) throw new Error('connection lost')
      return choosing
    },
  })
  await assert.rejects(submit([]), /connection lost/)
  assert.equal(await submit([]), choosing)
  assert.equal(saves, 1)
})

test('a lost PUT response checks whether the answer committed before retrying', async () => {
  let saves = 0
  const submit = createStandaloneSettingsSubmission({
    playerId: 'player',
    save: async () => { saves++; throw new Error('response lost') },
    fetch: async () => choosing,
  })
  await assert.rejects(submit([]), /response lost/)
  assert.equal(await submit([]), choosing)
  assert.equal(saves, 1)
})

test('retries an unsaved answer only while its settings question is still parked', async () => {
  let saves = 0
  let reads = 0
  const submit = createStandaloneSettingsSubmission({
    playerId: 'player',
    save: async () => { if (++saves === 1) throw new Error('offline') },
    fetch: async () => ++reads === 1 ? settings : choosing,
  })
  await assert.rejects(submit([]), /offline/)
  assert.equal(await submit([]), choosing)
  assert.equal(saves, 2)
})

test('does not resend when the recovery read also fails', async () => {
  let saves = 0
  const submit = createStandaloneSettingsSubmission({
    playerId: 'player',
    save: async () => { saves++; throw new Error('offline') },
    fetch: async () => { throw new Error('still offline') },
  })
  await assert.rejects(submit([]), /offline/)
  await assert.rejects(submit([]), /still offline/)
  assert.equal(saves, 1)
})

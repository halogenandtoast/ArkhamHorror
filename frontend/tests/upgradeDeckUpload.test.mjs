import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import { resolve } from 'node:path'
import test from 'node:test'
import ts from 'typescript'

async function importTsModule(path) {
  const source = await readFile(path, 'utf8')
  const { outputText } = ts.transpileModule(source, {
    compilerOptions: {
      module: ts.ModuleKind.ES2022,
      target: ts.ScriptTarget.ES2022,
      verbatimModuleSyntax: true,
    },
    fileName: path,
  })

  return import(`data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`)
}

const modulePath = resolve('src/arkham/upgradeDeckUpload.ts')

function recorder() {
  const calls = []
  return {
    calls,
    actions: {
      setModel: (value) => calls.push(['model', value]),
      setDeckList: (value) => calls.push(['deckList', value]),
      setDeckUrl: (value) => calls.push(['deckUrl', value]),
      setDeck: (value) => calls.push(['deck', value]),
      setDeckInvestigator: (value) => calls.push(['deckInvestigator', value]),
      upgrade: () => calls.push(['upgrade']),
    },
  }
}

test('uploaded upgrade deck JSON is applied and submitted', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const deck = {
    id: '6454',
    url: 'https://arkhamdb.com/api/public/deck/6454',
    name: 'Roland upgrade',
    investigator_code: '1001',
    investigator_name: 'Roland Banks',
    slots: { '01016': 1 },
  }
  const { calls, actions } = recorder()

  const result = loadUpgradeDeckFromJsonText(JSON.stringify(deck), actions)

  assert.deepEqual(result, { ok: true })
  assert.deepEqual(
    calls.map(([name]) => name),
    ['model', 'deckList', 'deckUrl', 'deck', 'deckInvestigator', 'upgrade'],
  )
  assert.deepEqual(calls[1][1], deck)
  assert.equal(calls[2][1], deck.url)
  assert.equal(calls[4][1], deck.investigator_code)
})

// A real ArkhamDB export has a NUMERIC id and no url at all; requiring either rejected every
// exported file and the upload silently did nothing.
test('an ArkhamDB export with a numeric id and no url is accepted', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const deck = {
    id: 2599352,
    name: 'Roland upgrade',
    date_creation: '2026-07-01T00:00:00+00:00',
    investigator_code: '01001',
    investigator_name: 'Roland Banks',
    slots: { '01016': 1 },
    sideSlots: {},
    taboo_id: null,
  }
  const { calls, actions } = recorder()

  const result = loadUpgradeDeckFromJsonText(JSON.stringify(deck), actions)

  assert.deepEqual(result, { ok: true })
  assert.deepEqual(
    calls.map(([name]) => name),
    ['model', 'deckList', 'deckUrl', 'deck', 'deckInvestigator', 'upgrade'],
  )
  const submitted = calls[1][1]
  assert.equal(submitted.id, '2599352')
  assert.equal(submitted.url, null)
  assert.deepEqual(submitted.slots, deck.slots)
  assert.equal(calls[2][1], null)
  assert.equal(calls[4][1], '01001')
})

// arkham.build exports a string id, still with no url.
test('an arkham.build export without a url is accepted', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const deck = {
    id: '5a2f9e7c-1c6a-4a2f-9a1e-1f2b3c4d5e6f',
    name: 'Roland upgrade',
    investigator_code: '01001',
    investigator_name: 'Roland Banks',
    slots: { '01016': 1 },
  }
  const { calls, actions } = recorder()

  const result = loadUpgradeDeckFromJsonText(JSON.stringify(deck), actions)

  assert.deepEqual(result, { ok: true })
  assert.equal(calls[1][1].url, null)
  assert.equal(calls[4][1], '01001')
})

// The upgrade must target the investigator the deck is actually for, not the base front.
test('an alternate front in meta is used as the deck investigator', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const deck = {
    id: 1,
    name: 'Parallel Roland',
    investigator_code: '01001',
    investigator_name: 'Roland Banks',
    meta: JSON.stringify({ alternate_front: '90024' }),
    slots: { '01016': 1 },
  }
  const { calls, actions } = recorder()

  const result = loadUpgradeDeckFromJsonText(JSON.stringify(deck), actions)

  assert.deepEqual(result, { ok: true })
  assert.equal(calls[4][1], '90024')
})

test('an empty alternate front falls back to the investigator code', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const deck = {
    id: 1,
    name: 'Roland',
    investigator_code: '01001',
    investigator_name: 'Roland Banks',
    meta: JSON.stringify({ alternate_front: '' }),
    slots: { '01016': 1 },
  }
  const { calls, actions } = recorder()

  const result = loadUpgradeDeckFromJsonText(JSON.stringify(deck), actions)

  assert.deepEqual(result, { ok: true })
  assert.equal(calls[4][1], '01001')
})

test('invalid uploaded upgrade deck JSON is reported', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const { calls, actions } = recorder()

  const result = loadUpgradeDeckFromJsonText('{', actions)

  assert.deepEqual(result, { ok: false, reason: 'invalidJson' })
  assert.deepEqual(calls, [])
})

test('uploaded upgrade deck JSON without investigator code is reported', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const { calls, actions } = recorder()
  const deck = {
    id: '6454',
    url: 'https://arkhamdb.com/api/public/deck/6454',
    name: 'Roland upgrade',
    investigator_name: 'Roland Banks',
    slots: { '01016': 1 },
  }

  const result = loadUpgradeDeckFromJsonText(JSON.stringify(deck), actions)

  assert.deepEqual(result, { ok: false, reason: 'notADecklist' })
  assert.deepEqual(calls, [])
})

// The 404 body arkham.build returns for a missing share must still be refused.
test('an error body is not treated as a decklist', async () => {
  const { loadUpgradeDeckFromJsonText, isUsableDecklist } = await importTsModule(modulePath)
  const { calls, actions } = recorder()

  const result = loadUpgradeDeckFromJsonText('{"message":"No share was found for this deck"}', actions)

  assert.deepEqual(result, { ok: false, reason: 'notADecklist' })
  assert.deepEqual(calls, [])
  assert.equal(isUsableDecklist({ message: 'No share was found for this deck' }), false)
  assert.equal(isUsableDecklist({ investigator_code: '01001', slots: { '01016': 'x' } }), false)
})

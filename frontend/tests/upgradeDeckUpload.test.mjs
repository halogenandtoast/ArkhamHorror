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
  const calls = []

  const loaded = loadUpgradeDeckFromJsonText(JSON.stringify(deck), {
    setModel: (value) => calls.push(['model', value]),
    setDeckList: (value) => calls.push(['deckList', value]),
    setDeckUrl: (value) => calls.push(['deckUrl', value]),
    setDeck: (value) => calls.push(['deck', value]),
    setDeckInvestigator: (value) => calls.push(['deckInvestigator', value]),
    upgrade: () => calls.push(['upgrade']),
  })

  assert.equal(loaded, true)
  assert.deepEqual(
    calls.map(([name]) => name),
    ['model', 'deckList', 'deckUrl', 'deck', 'deckInvestigator', 'upgrade'],
  )
  assert.deepEqual(calls[1][1], deck)
  assert.equal(calls[2][1], deck.url)
  assert.equal(calls[4][1], deck.investigator_code)
})

test('invalid uploaded upgrade deck JSON is ignored', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const calls = []

  const loaded = loadUpgradeDeckFromJsonText('{', {
    setModel: (value) => calls.push(['model', value]),
    setDeckList: (value) => calls.push(['deckList', value]),
    setDeckUrl: (value) => calls.push(['deckUrl', value]),
    setDeck: (value) => calls.push(['deck', value]),
    setDeckInvestigator: (value) => calls.push(['deckInvestigator', value]),
    upgrade: () => calls.push(['upgrade']),
  })

  assert.equal(loaded, false)
  assert.deepEqual(calls, [])
})

test('uploaded upgrade deck JSON without investigator code is ignored', async () => {
  const { loadUpgradeDeckFromJsonText } = await importTsModule(modulePath)
  const calls = []
  const deck = {
    id: '6454',
    url: 'https://arkhamdb.com/api/public/deck/6454',
    name: 'Roland upgrade',
    investigator_name: 'Roland Banks',
    slots: { '01016': 1 },
  }

  const loaded = loadUpgradeDeckFromJsonText(JSON.stringify(deck), {
    setModel: (value) => calls.push(['model', value]),
    setDeckList: (value) => calls.push(['deckList', value]),
    setDeckUrl: (value) => calls.push(['deckUrl', value]),
    setDeck: (value) => calls.push(['deck', value]),
    setDeckInvestigator: (value) => calls.push(['deckInvestigator', value]),
    upgrade: () => calls.push(['upgrade']),
  })

  assert.equal(loaded, false)
  assert.deepEqual(calls, [])
})

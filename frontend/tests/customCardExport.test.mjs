import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* Every custom card belongs to a set, so every import has to land in one --
including a file that predates sets, or a single card pasted on its own. What
set that is decides whether an import replaces something you already have or
quietly makes a second copy of it beside the first, so the fallbacks are pinned
here rather than left to whatever the file happens to carry. */
async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/customCardLibrary.ts')
}

const card = (title, setName) => ({
  def: {
    cardCode: `*${title.toLowerCase()}0`,
    name: { title },
    cardType: 'AssetType',
    ...(setName === undefined ? {} : { meta: { set: setName } }),
  },
  art: null,
})

test('an export names the set it came from', async (t) => {
  const { exportCards, EXPORT_VERSION } = await load(t)

  const file = await exportCards([card('One', 'Zealot')], {
    id: 'set-1',
    name: 'Zealot',
    sourceCode: 'pack-1',
    cardCount: 1,
    updatedAt: '2026-01-01T00:00:00Z',
  })

  assert.equal(file.version, EXPORT_VERSION)
  assert.deepEqual(file.set, { name: 'Zealot', sourceCode: 'pack-1' })
  assert.equal(file.cards.length, 1)
})

test('an export of loose cards names no set', async (t) => {
  const { exportCards } = await load(t)

  const file = await exportCards([card('One', 'Zealot')])
  assert.equal(file.set, undefined)
})

test('an import reads the set the file names', async (t) => {
  const { parseCardExport } = await load(t)

  const parsed = parseCardExport(
    JSON.stringify({
      version: 2,
      set: { name: 'Zealot', sourceCode: 'pack-1' },
      cards: [card('One', 'Zealot')],
    }),
    'whatever.json',
  )

  assert.equal(parsed.name, 'Zealot')
  assert.equal(parsed.sourceCode, 'pack-1')
  assert.equal(parsed.cards.length, 1)
})

test('a file from before sets falls back to what its cards claim', async (t) => {
  const { parseCardExport } = await load(t)

  // Version 1: cards only, each naming its set the old way.
  const parsed = parseCardExport(
    JSON.stringify({ version: 1, cards: [card('One', 'Dunwich'), card('Two', 'Dunwich')] }),
    'ignored',
  )
  assert.equal(parsed.name, 'Dunwich')
  assert.equal(parsed.sourceCode, null)
  assert.equal(parsed.cards.length, 2)
})

test('a file that names no set anywhere falls back to the file name', async (t) => {
  const { parseCardExport } = await load(t)

  assert.equal(parseCardExport(JSON.stringify({ cards: [card('One')] }), 'my-cards').name, 'my-cards')
  // A blank set name is no name at all.
  assert.equal(parseCardExport(JSON.stringify({ cards: [card('One', '   ')] }), 'my-cards').name, 'my-cards')
})

test('a single card pasted on its own still imports', async (t) => {
  const { parseCardExport } = await load(t)

  const parsed = parseCardExport(JSON.stringify(card('Lonely', 'Zealot')), 'lonely')
  assert.equal(parsed.cards.length, 1)
  assert.equal(parsed.name, 'Zealot')
})

test('anything without a card code is not a card', async (t) => {
  const { parseCardExport } = await load(t)

  const parsed = parseCardExport(
    JSON.stringify({ cards: [card('Real', 'Zealot'), { def: { name: { title: 'Junk' } }, art: null }] }),
    'file',
  )
  assert.equal(parsed.cards.length, 1)
  assert.equal(parsed.cards[0].def.name.title, 'Real')
})

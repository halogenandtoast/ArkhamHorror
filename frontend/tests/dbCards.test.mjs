import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createPinia, setActivePinia } from 'pinia'
import { createServer } from 'vite'

const shippedCards = (lang) =>
  JSON.parse(readFileSync(new URL(`../public/cards/cards_${lang}.json`, import.meta.url), 'utf8'))

// `fetchDbCards` checks the status and the content type, so a bare `{ json }` is
// not enough of a Response to get past it.
const respondWith = (cards) => async () => ({
  ok: true,
  status: 200,
  statusText: 'OK',
  headers: { get: () => 'application/json' },
  json: async () => cards,
})

const withStore = async (t, cards) => {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())

  const { useDbCardStore } = await server.ssrLoadModule('/src/stores/dbCards.ts')

  const originalFetch = globalThis.fetch
  globalThis.fetch = respondWith(cards)
  t.after(() => {
    globalThis.fetch = originalFetch
  })

  setActivePinia(createPinia())
  const store = useDbCardStore()
  await store.fetchDbCards('en')
  return store
}

test('unsuffixed runtime codes resolve split-card front translations', async (t) => {
  const dagonFront = { code: '07330a', name: '达贡', real_name: 'Dagon' }
  const dagonBack = { code: '07330b', name: '达贡', real_name: 'Dagon' }
  const hydraFront = { code: '07331a', name: '海德拉', real_name: 'Hydra' }
  const hydraBack = { code: '07331b', name: '海德拉', real_name: 'Hydra' }

  const store = await withStore(t, [dagonFront, dagonBack, hydraFront, hydraBack])

  assert.equal(store.getDbCard('07330')?.code, dagonFront.code)
  assert.equal(store.getDbCard('07330b')?.code, dagonBack.code)
  assert.equal(store.getDbCard('07331')?.code, hydraFront.code)
  assert.equal(store.getDbCard('07331b')?.code, hydraBack.code)
})

test('a record filed under a "b" code wins its own key whatever the file order', async (t) => {
  const front = { code: '52023', name: 'Dianne Devine', real_name: 'Dianne Devine', double_sided: false }
  const back = { code: '52023b', name: 'Sickening Reality', real_name: 'Sickening Reality', double_sided: false }

  // The alias used to overwrite the real record, so which one answered came down
  // to where the two cards happened to sit in the file.
  const store = await withStore(t, [back, front])

  assert.equal(store.getDbCard('52023b')?.real_name, 'Sickening Reality')
  assert.equal(store.getDbCard('52023')?.real_name, 'Dianne Devine')
})

test('the alias still describes a double-sided card\'s back', async (t) => {
  const scenario = { code: '51020', name: 'Return to The Miskatonic Museum', real_name: 'Return to The Miskatonic Museum', double_sided: true }

  const store = await withStore(t, [scenario])

  assert.equal(store.getDbCard('51020b')?.code, '51020')
})

test('a Masked Carnevale-Goer never resolves to the enemy it hides', async (t) => {
  const store = await withStore(t, shippedCards('en'))

  // The engine gives each goer the `b` face of the enemy it hides (82017b-82021b);
  // ArkhamDB records the shared printed card once, as 82017b. Anything reached
  // through the alias is a different, single-sided card, which is the overlay's
  // cue to look the face up by its own name instead of describing it with the front.
  for (const code of ['82017b', '82018b', '82019b', '82020b', '82021b']) {
    const record = store.getDbCard(code)
    if (record.code === code) {
      assert.equal(record.real_name, 'Masked Carnevale-Goer')
    } else {
      assert.equal(record.double_sided, false, `${code} must not look like a real back face`)
    }
  }

  assert.equal(store.getDbCard('82017b').real_name, 'Masked Carnevale-Goer')
})

test('the goers are findable by the name the engine gives every one of them', async (t) => {
  const store = await withStore(t, shippedCards('zh-cn'))

  const goer = store.getDbCardByRealName('Masked Carnevale-Goer')
  assert.equal(goer?.code, '82017b')
  assert.equal(goer.name, '戴面具的狂欢节行人')
})

test('a name more than one card answers to resolves to nothing', async (t) => {
  const store = await withStore(t, shippedCards('en'))

  assert.equal(store.getDbCardByRealName('Legs of Atlach-Nacha'), null)
})

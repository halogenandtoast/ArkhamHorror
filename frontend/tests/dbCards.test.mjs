import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createPinia, setActivePinia } from 'pinia'
import { createServer } from 'vite'

test('unsuffixed runtime codes resolve split-card front translations', async (t) => {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())

  const { useDbCardStore } = await server.ssrLoadModule('/src/stores/dbCards.ts')
  const dagonFront = { code: '07330a', name: '达贡', real_name: 'Dagon' }
  const dagonBack = { code: '07330b', name: '达贡', real_name: 'Dagon' }
  const hydraFront = { code: '07331a', name: '海德拉', real_name: 'Hydra' }
  const hydraBack = { code: '07331b', name: '海德拉', real_name: 'Hydra' }
  const originalFetch = globalThis.fetch
  globalThis.fetch = async () => ({
    json: async () => [dagonFront, dagonBack, hydraFront, hydraBack],
  })
  t.after(() => {
    globalThis.fetch = originalFetch
  })

  setActivePinia(createPinia())
  const store = useDbCardStore()
  await store.fetchDbCards('en')

  assert.equal(store.getDbCard('07330')?.code, dagonFront.code)
  assert.equal(store.getDbCard('07330b')?.code, dagonBack.code)
  assert.equal(store.getDbCard('07331')?.code, hydraFront.code)
  assert.equal(store.getDbCard('07331b')?.code, hydraBack.code)
})

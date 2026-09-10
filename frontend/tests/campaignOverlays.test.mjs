import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'
import { createServer } from 'vite'

const overlay = {
  id: 'circus-ex-mortis:rougarou', name: 'Circus Ex Mortis', scenario: 'c81001',
  available: true, active: true, xpCost: 0,
  cardReplacements: { 'c81019': 'c:circus-ex-mortis:019c', 'c81029': 'c:circus-ex-mortis:029c' },
}

test('campaign overlays update both card-image paths and clear on leaving the game', async t => {
  const previous = globalThis.localStorage
  globalThis.localStorage = { getItem: () => null, setItem: () => {} }
  t.after(() => { globalThis.localStorage = previous })
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)), appType: 'custom', logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  const { createPinia, setActivePinia } = await import('pinia')
  const { computed } = await import('vue')
  setActivePinia(createPinia())
  const { setCampaignOverlays, campaignOverlayDecoder } = await server.ssrLoadModule('/src/arkham/campaignOverlays.ts')
  const { cardImage } = await server.ssrLoadModule('/src/arkham/cardImages.ts')
  const { imgsrc } = await server.ssrLoadModule('/src/arkham/helpers.ts')
  assert.deepEqual(await campaignOverlayDecoder.decodeToPromise(overlay), overlay)
  const image = computed(() => cardImage('81019'))
  assert.match(image.value, /cards\/81019\.avif$/)
  setCampaignOverlays([overlay])
  assert.match(image.value, /homebrew\/circus-ex-mortis\/cards\/019c\.avif$/)
  assert.match(cardImage('c81029'), /homebrew\/circus-ex-mortis\/cards\/029c\.avif$/)
  assert.match(imgsrc('cards/81019.avif'), /homebrew\/circus-ex-mortis\/cards\/019c\.avif$/)
  assert.match(cardImage('81001'), /cards\/81001\.avif$/)
  setCampaignOverlays([{ ...overlay, available: false }])
  assert.match(image.value, /homebrew\/circus-ex-mortis\/cards\/019c\.avif$/)
  setCampaignOverlays([{ ...overlay, active: false }])
  assert.match(image.value, /cards\/81019\.avif$/)
  setCampaignOverlays([overlay])
  setCampaignOverlays([])
  assert.match(image.value, /cards\/81019\.avif$/)
})

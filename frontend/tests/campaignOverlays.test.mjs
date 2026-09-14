import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'
import { createServer } from 'vite'

const overlay = {
  id: 'circus-ex-mortis:rougarou', name: 'Circus Ex Mortis', scenario: 'c81001',
  available: true, xpCost: 0,
}

test('a campaign overlay decodes off the campaign payload', async t => {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)), appType: 'custom', logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  const { campaignOverlayDecoder } = await server.ssrLoadModule('/src/arkham/campaignOverlays.ts')
  assert.deepEqual(await campaignOverlayDecoder.decodeToPromise(overlay), overlay)
})

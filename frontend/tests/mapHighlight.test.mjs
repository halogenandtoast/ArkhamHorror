import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

test('location highlight width compensates for map zoom', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())
  const { mapHighlightBorderWidth } = await server.ssrLoadModule('/src/arkham/mapHighlight.ts')

  assert.equal(mapHighlightBorderWidth(0.25), '8px')
  assert.equal(mapHighlightBorderWidth(1), '2px')
  assert.equal(mapHighlightBorderWidth(6), `${2 / 6}px`)
})

import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

test('target decoder accepts a BothTarget with two nested targets', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())

  const { targetDecoder } = await server.ssrLoadModule('/src/arkham/types/Target.ts')
  const payload = {
    tag: 'BothTarget',
    contents: [
      {
        tag: 'LocationTarget',
        contents: '7042d939-b028-49bd-81d6-19b948ed35e5',
      },
      {
        tag: 'LocationTarget',
        contents: 'da2a4ea4-1503-4105-87cb-3c64e9e449da',
      },
    ],
  }

  const decoded = await targetDecoder.decodePromise(payload)

  assert.equal(decoded.tag, payload.tag)
  assert.deepEqual(
    decoded.contents.map(({ tag, contents }) => ({ tag, contents })),
    payload.contents,
  )
})

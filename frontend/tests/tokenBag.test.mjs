import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

test('custom chaos bag decoding and inspection', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())
  const { readTokenBag } = await server.ssrLoadModule('/src/arkham/types/TokenBag.ts')
  const { storyMetaDecoder } = await server.ssrLoadModule('/src/arkham/types/Story.ts')
  const skull = { bagTokenId: 'skull-id', bagTokenFace: 'Skull' }
  const tablet = { bagTokenId: 'tablet-id', bagTokenFace: 'Tablet' }

  await t.test('new story state survives the decoder, including its debug override', async () => {
    const meta = await storyMetaDecoder.decodePromise({
      bagTokens: [skull], bagSetAside: [tablet], bagCurrentToken: null,
      bagCancelNext: true, bagDebugNext: 'Skull',
    })
    assert.deepEqual(readTokenBag(meta), {
      tokens: [{ id: 'skull-id', face: 'Skull' }],
      setAside: [{ id: 'tablet-id', face: 'Tablet' }],
      currentToken: null, cancelNext: true, debugNext: 'Skull',
    })
  })

  for (const prefix of ['infestation', 'predation']) {
    await t.test(`legacy ${prefix} state is inspectable before any engine messages`, async () => {
      const meta = await storyMetaDecoder.decodePromise({
        [`${prefix}Tokens`]: [{ [`${prefix}TokenId`]: 'skull-id', [`${prefix}TokenFace`]: 'Skull' }],
        [`${prefix}SetAside`]: [],
        [`${prefix}CurrentToken`]: { [`${prefix}TokenId`]: 'tablet-id', [`${prefix}TokenFace`]: 'Tablet' },
      })
      assert.deepEqual(readTokenBag(meta), {
        tokens: [{ id: 'skull-id', face: 'Skull' }], setAside: [],
        currentToken: { id: 'tablet-id', face: 'Tablet' }, cancelNext: false, debugNext: null,
      })
    })
  }

  await t.test('an exhausted bag remains inspectable', () => {
    assert.deepEqual(readTokenBag({ bagTokens: [], bagSetAside: [], bagCurrentToken: null }), {
      tokens: [], setAside: [], currentToken: null, cancelNext: false, debugNext: null,
    })
  })

  await t.test('unrelated metadata is not mistaken for a bag', () => {
    assert.equal(readTokenBag({ count: 3 }), null)
    assert.equal(readTokenBag(null), null)
    assert.equal(readTokenBag(['Skull']), null)
  })

  await t.test('custom Fury token faces retain their slugs', () => {
    const bag = readTokenBag({ bagTokens: [{ bagTokenId: 'moon-id', bagTokenFace: ':circus-ex-mortis:moon' }] })
    assert.equal(bag.tokens[0].face, ':circus-ex-mortis:moon')
  })
})

import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* The debug seal/release messages are hand-written JSON the backend's derived
decoders must accept (Arkham.Message `UnsealChaosToken`, `DebugSealChaosToken`,
`Run`, Arkham.Message.ChaosBag `ReturnChaosTokensToPool_`), so their shapes are
pinned here, along with which faces may go back to a token pool. */
async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/debugSeals.ts')
}

const bless = { id: 'token-1', face: 'BlessToken' }

const record = {
  chaosTokenId: 'token-1',
  chaosTokenFace: 'BlessToken',
  chaosTokenRevealedBy: null,
  chaosTokenCancelled: false,
  chaosTokenSealed: true,
}

test('releasing sends the whole token record', async (t) => {
  const { unsealMessage } = await load(t)
  assert.deepEqual(unsealMessage(bless), { tag: 'UnsealChaosToken', contents: record })
})

test('releasing to the pool unseals, then returns the token to the pool, as one step', async (t) => {
  const { releaseToPoolMessage } = await load(t)
  assert.deepEqual(releaseToPoolMessage(bless), {
    tag: 'Run',
    contents: [
      { tag: 'UnsealChaosToken', contents: record },
      { tag: 'ChaosBagMessage', contents: { tag: 'ReturnChaosTokensToPool_', contents: [record] } },
    ],
  })
})

test('sealing names the token by id and the card by target', async (t) => {
  const { sealMessage } = await load(t)
  assert.deepEqual(sealMessage('token-1', { tag: 'TreacheryTarget', contents: 'treachery-1' }), {
    tag: 'DebugSealChaosToken',
    contents: ['token-1', { tag: 'TreacheryTarget', contents: 'treachery-1' }],
  })
})

test('only pool faces can be released to the pool', async (t) => {
  const { isPooledFace } = await load(t)
  for (const face of ['BlessToken', 'CurseToken', 'FrostToken', 'BloodToken', ':circus-ex-mortis:moon']) {
    assert.equal(isPooledFace(face), true, face)
  }
  for (const face of ['Zero', 'MinusOne', 'Skull', 'AutoFail', 'ElderSign']) {
    assert.equal(isPooledFace(face), false, face)
  }
})

test('the picker groups the bag by face, in bag order', async (t) => {
  const { groupByFace } = await load(t)
  const tokens = [
    { id: 'a', face: 'Skull' },
    { id: 'b', face: 'MinusOne' },
    { id: 'c', face: 'Skull' },
    { id: 'd', face: 'PlusOne' },
    { id: 'e', face: 'BlessToken' },
  ]
  const groups = groupByFace(tokens)
  assert.deepEqual(groups.map((g) => g.face), ['PlusOne', 'MinusOne', 'Skull', 'BlessToken'])
  assert.deepEqual(groups.find((g) => g.face === 'Skull').tokens.map((t) => t.id), ['a', 'c'])
  assert.deepEqual(groupByFace([]), [])
})

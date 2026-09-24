import assert from 'node:assert/strict'
import { resolve } from 'node:path'
import test from 'node:test'
import { createServer } from 'vite'

const cardId = 'cf53b4dd-4e1e-4b0f-9b2c-1a1d0f1d6c3a'

const card = {
  tag: 'EncounterCard',
  contents: { tag: 'CardContents', id: cardId, cardCode: 'c01163', tokens: {} },
}

const gameWith = (messages) => ({
  investigators: {},
  focusedCards: [card],
  foundCards: {},
  scenario: null,
  question: {
    player: {
      tag: 'ChooseOneAtATime',
      choices: [{ tag: 'TargetLabel', target: { tag: 'CardIdTarget', contents: cardId }, messages }],
    },
  },
})

test('put-back panel recognises only the put-back-on-top question', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())

  const { putBackInAnyOrderPicks } = await server.ssrLoadModule('/src/arkham/putBackInAnyOrder.ts')

  const putBack = putBackInAnyOrderPicks(gameWith([{ tag: 'AddFocusedToTopOfDeck', contents: [] }]), 'player')
  assert.equal(putBack?.length, 1)
  assert.equal(putBack[0].id, cardId)
  assert.equal(putBack[0].index, 0)

  // Discarding the rest of a search targets the same cards; it must not be
  // mistaken for a put-back.
  assert.equal(putBackInAnyOrderPicks(gameWith([{ tag: 'AddToEncounterDiscard', contents: [] }]), 'player'), null)

})

test('a target label keeps the messages it will run', async (t) => {
  const server = await createServer({
    root: resolve('.'),
    server: { middlewareMode: true },
    appType: 'custom',
    logLevel: 'silent',
  })
  t.after(() => server.close())

  const { targetLabelDecoder } = await server.ssrLoadModule('/src/arkham/types/Message.ts')
  const decoded = await targetLabelDecoder.decodePromise({
    tag: 'TargetLabel',
    target: { tag: 'CardIdTarget', contents: cardId },
    messages: [{ tag: 'AddFocusedToTopOfDeck', contents: [] }],
  })

  assert.deepEqual(decoded.messages, [{ tag: 'AddFocusedToTopOfDeck', contents: [] }])
})

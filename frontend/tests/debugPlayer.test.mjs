import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* The investigator and campaign debug tools send hand-written JSON that the
backend's derived decoders must accept (Arkham.Message `DrawCards`,
`DiscardFromHand`, `SufferTrauma`, `HealTrauma`, `ReportXp`, `GainXP`,
`SpendXP`, `Record`, `RecordCount`, `ForgetRecord`), so the shapes
are pinned here, along with how custom campaign log entries round-trip. */
async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/debugPlayer.ts')
}

test('drawing is a real draw from the investigator deck', async (t) => {
  const { drawCardsMessage } = await load(t)
  const message = drawCardsMessage('01001', 2)
  assert.equal(message.tag, 'DrawCards')
  const [iid, draw] = message.contents
  assert.equal(iid, '01001')
  assert.deepEqual(draw.cardDrawDeck, { tag: 'InvestigatorDeck', contents: '01001' })
  assert.equal(draw.cardDrawAmount, 2)
  assert.equal(draw.cardDrawKind, 'StandardCardDraw')
  assert.deepEqual(draw.cardDrawState, { tag: 'UnresolvedCardDraw' })
  // never asks for zero or a fraction of a card
  assert.equal(drawCardsMessage('01001', 0).contents[1].cardDrawAmount, 1)
  assert.equal(drawCardsMessage('01001', 2.6).contents[1].cardDrawAmount, 2)
})

test('a random discard takes one card from hand to the discard pile', async (t) => {
  const { discardRandomMessage } = await load(t)
  assert.deepEqual(discardRandomMessage('01001'), {
    tag: 'DiscardFromHand',
    contents: {
      discardStrategy: 'DiscardRandom',
      discardInvestigator: '01001',
      discardSource: { tag: 'GameSource' },
      discardTarget: null,
      discardFilter: { tag: 'AnyCard', contents: [] },
      discardAmount: 1,
      discardThen: null,
      discardDestination: 'ToDiscardPile',
      discardBatchCards: [],
    },
  })
})

test('trauma goes on the right track', async (t) => {
  const { sufferTraumaMessage, healTraumaMessage } = await load(t)
  assert.deepEqual(sufferTraumaMessage('01001', 'physical'), { tag: 'SufferTrauma', contents: ['01001', 1, 0] })
  assert.deepEqual(sufferTraumaMessage('01001', 'mental'), { tag: 'SufferTrauma', contents: ['01001', 0, 1] })
  assert.deepEqual(healTraumaMessage('01001', 'mental', 2), { tag: 'HealTrauma', contents: ['01001', 0, 2] })
})

test('gaining XP is reported to the breakdown, spending is its own message', async (t) => {
  const { gainXpMessage, spendXpMessage } = await load(t)
  assert.deepEqual(gainXpMessage('01001'), {
    tag: 'Run',
    contents: [
      {
        tag: 'ReportXp',
        contents: [
          {
            tag: 'InvestigatorGainXp',
            investigator: '01001',
            details: { source: 'XpFromCardEffect', sourceName: 'Debug adjustment', amount: 1 },
          },
        ],
      },
      { tag: 'GainXP', contents: ['01001', { tag: 'GameSource' }, 1] },
    ],
  })
  assert.deepEqual(spendXpMessage('01001'), { tag: 'SpendXP', contents: ['01001', 1] })
})

test('a custom entry keeps exactly what was typed', async (t) => {
  const { customLogKey, customLogText } = await load(t)
  const text = "Agnes's Dr. Armitage owes us. Twice!"
  const key = customLogKey(`  ${text} `)
  assert.deepEqual(key, { tag: 'HomebrewCampaignLogKey', contents: `custom.${text}` })
  assert.equal(customLogText(key), text)
  assert.equal(customLogText({ tag: 'HomebrewCampaignLogKey', contents: 'darkMatter.SomeKey' }), null)
  assert.equal(customLogText({ tag: 'NightOfTheZealotKey', contents: 'YourHouseIsStillStanding' }), null)
  assert.equal(customLogText(null), null)
})

test('only free-text keys outside the running homebrew campaign are editable', async (t) => {
  const { isEditableLogKey, editableLogText } = await load(t)
  const custom = { tag: 'HomebrewCampaignLogKey', contents: 'custom.A note' }
  const cardRecorded = { tag: 'HomebrewCampaignLogKey', contents: 'The lodge remembers' }
  const campaignOwn = { tag: 'HomebrewCampaignLogKey', contents: 'darkMatter.TheShipSank' }
  const official = { tag: 'NightOfTheZealotKey', contents: 'YourHouseIsStillStanding' }
  assert.equal(isEditableLogKey(custom, 'darkMatter'), true)
  assert.equal(isEditableLogKey(cardRecorded, 'darkMatter'), true)
  assert.equal(isEditableLogKey(campaignOwn, 'darkMatter'), false)
  assert.equal(isEditableLogKey(campaignOwn), true)
  assert.equal(isEditableLogKey(official), false)
  assert.equal(editableLogText(custom), 'A note')
  assert.equal(editableLogText(cardRecorded), 'The lodge remembers')
})

test('campaign log edits are single steps', async (t) => {
  const { recordMessage, recordCountMessage, forgetRecordMessage, renameNoteMessage, renameCountMessage, customLogKey } =
    await load(t)
  const old = customLogKey('Old note')
  assert.deepEqual(recordMessage(old), { tag: 'Record', contents: old })
  assert.deepEqual(recordCountMessage(old, 3.4), { tag: 'RecordCount', contents: [old, 3] })
  assert.deepEqual(forgetRecordMessage(old), { tag: 'ForgetRecord', contents: old })
  assert.deepEqual(renameNoteMessage(old, 'New note'), {
    tag: 'Run',
    contents: [
      { tag: 'ForgetRecord', contents: old },
      { tag: 'Record', contents: customLogKey('New note') },
    ],
  })
  assert.deepEqual(renameCountMessage(old, 'Favours', 2), {
    tag: 'Run',
    contents: [
      { tag: 'ForgetRecord', contents: old },
      { tag: 'RecordCount', contents: [customLogKey('Favours'), 2] },
    ],
  })
})

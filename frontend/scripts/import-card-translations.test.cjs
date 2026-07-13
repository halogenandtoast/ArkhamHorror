const assert = require('node:assert/strict')
const test = require('node:test')

const { buildLocalizedCards } = require('./import-card-translations.cjs')

test('merges translated card text and localized metadata onto the English export', () => {
  const cards = [
    {
      code: '01001',
      name: 'Roland Banks',
      text: 'English text',
      real_name: 'Roland Banks',
      pack_code: 'core',
      pack_name: 'Core Set',
      type_code: 'investigator',
      type_name: 'Investigator',
      faction_code: 'guardian',
      faction_name: 'Guardian',
      encounter_code: 'rats',
      encounter_name: 'Rats',
    },
    { code: '01002', name: 'Daisy Walker', real_name: 'Daisy Walker' },
  ]
  const translations = new Map([
    [
      '01001',
      {
        code: '01001',
        name: '罗兰·班克斯',
        text: '简体中文文本',
        deck_limit: 99,
      },
    ],
  ])
  const metadata = {
    packs: new Map([['core', '核心游戏']]),
    types: new Map([['investigator', '调查员']]),
    factions: new Map([['guardian', '守护者']]),
    encounters: new Map([['rats', '鼠群']]),
  }

  const result = buildLocalizedCards(cards, translations, metadata)

  assert.equal(result.cards[0].name, '罗兰·班克斯')
  assert.equal(result.cards[0].text, '简体中文文本')
  assert.equal(result.cards[0].real_name, 'Roland Banks')
  assert.equal(result.cards[0].pack_name, '核心游戏')
  assert.equal(result.cards[0].type_name, '调查员')
  assert.equal(result.cards[0].faction_name, '守护者')
  assert.equal(result.cards[0].encounter_name, '鼠群')
  assert.notEqual(result.cards[0].deck_limit, 99)
  assert.equal(result.cards[1].name, 'Daisy Walker')
  assert.deepEqual(result.stats, { total: 2, translated: 1, reused: 0, fallback: 1, unused: 0 })
})

test('reuses a translation for an equivalent reprint with a different card code', () => {
  const shared = {
    name: 'Beat Cop',
    real_name: 'Beat Cop',
    text: '',
    real_text: '',
    traits: 'Ally. Police.',
    real_traits: 'Ally. Police.',
    type_code: 'asset',
    xp: 0,
  }
  const result = buildLocalizedCards(
    [
      { ...shared, code: '01018' },
      { ...shared, code: '01518', pack_code: 'rcore', text: null, real_text: null },
    ],
    new Map([['01018', { code: '01018', name: '巡警', text: '简体中文文本', traits: '盟友. 警察.' }]]),
    {},
  )

  assert.equal(result.cards[1].name, '巡警')
  assert.equal(result.cards[1].text, '简体中文文本')
  assert.deepEqual(result.stats, { total: 2, translated: 1, reused: 1, fallback: 0, unused: 0 })
})

test('reuses an unsuffixed translation for a split card-side code', () => {
  const result = buildLocalizedCards(
    [{ code: '86024b', name: 'Hub Dimension', text: 'English text' }],
    new Map([['86024', { code: '86024', name: '次元枢纽', text: '简体中文文本' }]]),
    {},
  )

  assert.equal(result.cards[0].name, '次元枢纽')
  assert.deepEqual(result.stats, { total: 1, translated: 0, reused: 1, fallback: 0, unused: 0 })
})

test('reports translations that do not match a card in the English export', () => {
  const result = buildLocalizedCards(
    [{ code: '01001', name: 'Roland Banks' }],
    new Map([
      ['01001', { code: '01001', name: '罗兰·班克斯' }],
      ['missing', { code: 'missing', name: '不存在' }],
    ]),
    {},
  )

  assert.equal(result.stats.unused, 1)
})

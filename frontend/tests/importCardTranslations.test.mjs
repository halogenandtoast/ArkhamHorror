import assert from 'node:assert/strict'
import test from 'node:test'

import importer from '../scripts/import-card-translations.cjs'

const { buildLocalizedCards, parseArgs } = importer

test('the Simplified Chinese importer writes a separate zh-cn locale by default', () => {
  assert.deepEqual(parseArgs([]), { sourceLocale: 'zh-cn', outputLang: 'zh-cn', slim: false })
})

test('translation importer options require a value', () => {
  for (const option of ['--source', '--source-locale', '--output-lang']) {
    assert.throws(() => parseArgs([option]), new RegExp(`${option} requires a value`))
    assert.throws(() => parseArgs([option, '--slim']), new RegExp(`${option} requires a value`))
  }
})

test('translated text is merged without replacing gameplay metadata', () => {
  const cards = [
    {
      code: '01001',
      name: 'Roland Banks',
      text: 'English text',
      real_name: 'Roland Banks',
      pack_code: 'core',
      pack_name: 'Core Set',
    },
  ]
  const translations = new Map([
    ['01001', { code: '01001', name: '罗兰·班克斯', text: '简体中文文本', deck_limit: 99 }],
  ])
  const metadata = { packs: new Map([['core', '基础游戏']]) }

  const result = buildLocalizedCards(cards, translations, metadata)

  assert.equal(result.cards[0].name, '罗兰·班克斯')
  assert.equal(result.cards[0].text, '简体中文文本')
  assert.equal(result.cards[0].real_name, 'Roland Banks')
  assert.equal(result.cards[0].pack_name, '基础游戏')
  assert.notEqual(result.cards[0].deck_limit, 99)
})

test('equivalent reprints reuse an existing Simplified Chinese translation', () => {
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
      { ...shared, code: '01518', text: null, real_text: null },
    ],
    new Map([['01018', { code: '01018', name: '巡警', text: '简体中文文本' }]]),
  )

  assert.equal(result.cards[1].name, '巡警')
  assert.deepEqual(result.stats, { total: 2, translated: 1, reused: 1, fallback: 0, unused: 0 })
})

test('split card sides reuse an unsuffixed translation', () => {
  const result = buildLocalizedCards(
    [{ code: '86024b', name: 'Hub Dimension', text: 'English text' }],
    new Map([['86024', { code: '86024', name: '次元枢纽', text: '简体中文文本' }]]),
  )

  assert.equal(result.cards[0].name, '次元枢纽')
})

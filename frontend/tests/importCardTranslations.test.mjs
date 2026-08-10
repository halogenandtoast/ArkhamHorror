import assert from 'node:assert/strict'
import test from 'node:test'

import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

import importer from '../scripts/import-card-translations.cjs'

const { buildLocalizedCards, loadOverrides, parseArgs } = importer

const frontendRoot = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')

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
  assert.deepEqual(result.stats, {
    total: 2,
    translated: 1,
    reused: 1,
    fallback: 0,
    unused: 0,
    overridden: 0,
  })
})

test('split card sides reuse an unsuffixed translation', () => {
  const result = buildLocalizedCards(
    [{ code: '86024b', name: 'Hub Dimension', text: 'English text' }],
    new Map([['86024', { code: '86024', name: '次元枢纽', text: '简体中文文本' }]]),
  )

  assert.equal(result.cards[0].name, '次元枢纽')
})

test('a local override replaces the upstream translation for that card only', () => {
  const result = buildLocalizedCards(
    [
      { code: '09676b', name: 'Sympathy Pain', text: 'English text', real_name: 'Sympathy Pain' },
      { code: '09676d', name: 'Familial Pain', text: 'English text', real_name: 'Familial Pain' },
    ],
    new Map([
      ['09676b', { code: '09676b', name: '家庭分离之痛', text: '换到了一只小猫' }],
      ['09676d', { code: '09676d', name: '怜悯同情之痛', text: '分担了深沉的痛苦' }],
    ]),
    {},
    new Map([['09676b', { code: '09676b', name: '怜悯同情之痛', text: '分担了深沉的痛苦' }]]),
  )

  assert.equal(result.cards[0].name, '怜悯同情之痛')
  assert.equal(result.cards[0].text, '分担了深沉的痛苦')
  assert.equal(result.cards[0].real_name, 'Sympathy Pain')
  assert.equal(result.cards[1].name, '怜悯同情之痛')
  assert.equal(result.stats.overridden, 1)
})

test('an override never rides along on a reprint matched by identity', () => {
  const shared = {
    name: 'Beat Cop',
    real_name: 'Beat Cop',
    text: '',
    real_text: '',
    type_code: 'asset',
    xp: 0,
  }
  const result = buildLocalizedCards(
    [
      { ...shared, code: '01018' },
      { ...shared, code: '01518' },
    ],
    new Map([['01018', { code: '01018', name: '巡警' }]]),
    {},
    new Map([['01018', { code: '01018', name: '街头巡警' }]]),
  )

  assert.equal(result.cards[0].name, '街头巡警')
  assert.equal(result.cards[1].name, '巡警')
  assert.equal(result.stats.overridden, 1)
})

test('the checked-in zh-cn overrides only touch translated fields', () => {
  const overrides = loadOverrides(frontendRoot, 'zh-cn')
  assert.ok(overrides.size > 0)

  const english = new Map(
    JSON.parse(fs.readFileSync(path.join(frontendRoot, 'public', 'cards_en.json'), 'utf8')).map(
      (card) => [card.code, card],
    ),
  )
  for (const code of overrides.keys()) {
    assert.ok(english.has(code), `override ${code} does not match any English card`)
  }
})

test('loadOverrides returns an empty map for a locale with no override file', () => {
  assert.equal(loadOverrides(frontendRoot, 'not-a-locale').size, 0)
})

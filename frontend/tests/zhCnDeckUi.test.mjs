import assert from 'node:assert/strict'
import { fileURLToPath, URL } from 'node:url'
import { before, after, test } from 'node:test'
import { createServer } from 'vite'
import { createI18n } from 'vue-i18n'

let server
let zh
let en
let taboo
let restrictions
let localizedDeckError

before(async () => {
  server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    configLoader: 'runner', appType: 'custom', logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  const { loadLocaleMessages } = await server.ssrLoadModule('/src/locales/messages.ts')
  const messages = {
    'zh-cn': (await loadLocaleMessages('zh-cn')).messages,
    en: (await loadLocaleMessages('en')).messages,
  }
  const composer = (locale) => createI18n({
    legacy: false, locale, fallbackLocale: 'en', warnHtmlMessage: false, messages,
  }).global
  zh = composer('zh-cn')
  en = composer('en')
  taboo = await server.ssrLoadModule('/src/arkham/taboo.ts')
  restrictions = await server.ssrLoadModule('/src/arkham/deckRestrictions.ts')
  ;({ localizedDeckError } = await server.ssrLoadModule('/src/arkham/deckErrorMessages.ts'))
})

after(async () => { await server?.close() })

test('the loaded deck messages translate dynamic summaries without dropping their counts or names', () => {
  assert.equal(zh.te('deckUi.sections.main'), true)
  assert.equal(zh.t('deckUi.sections.main'), '主牌组')
  assert.equal(zh.t('deckUi.weaknessPool.productsSelected', { count: 12 }), '已选择12个产品')
  assert.equal(zh.t('deckUi.overlay.cardCount', { count: 3 }, 3), '3张卡牌')
  assert.equal(en.t('deckUi.overlay.cardCount', { count: 1 }, 1), '1 card')
  assert.equal(en.t('deckUi.overlay.cardCount', { count: 3 }, 3), '3 cards')
  assert.equal(zh.t('deckUi.sections.attachedFor', { name: '自定义卡牌' }), '自定义卡牌的附属卡牌')
  assert.equal(zh.t('deckUi.import.unknownCard', { code: 'custom:123' }), '未知卡牌：custom:123')
})

test('taboo dates preserve every version and calendar day in Chinese and English', () => {
  const versions = ['15', '16', '18', '19', '20', '21', '22', '23', '24', '25']
  for (const [index, version] of versions.entries()) {
    const key = `TabooList${version}`
    const text = taboo.displayTabooList(key, 'zh-cn')
    assert.match(text, /\d{4}年\d{1,2}月\d{1,2}日/)
    assert.equal(taboo.displayTabooId(index + 1, 'zh-cn'), text)
  }
  assert.equal(taboo.displayTabooId(1), '1.5 (Apr 23, 2019)')
  assert.equal(taboo.displayTabooId(10, 'zh-cn'), '2.5 (2026年2月21日)')
  assert.equal(taboo.displayTabooId(99, 'zh-cn', zh.t('deckUi.unknownTaboo')), '未知禁忌表')
})

test('challenge deck requirements translate while preserving investigator, card-count and group validation', () => {
  const { deckRequirementDescriptions, deckRestrictionError } = restrictions
  const roland = { investigator_code: '01001', slots: {} }
  const daisy = { investigator_code: '01002', slots: { '01031': 2, '01035': 1 } }
  assert.deepEqual(deckRequirementDescriptions('c90004', undefined, zh.t), [
    '本冒险需要黛西·沃克参与。',
    '黛西·沃克的牌组必须包含至少4张非弱点的典籍支援卡。',
  ])
  assert.equal(deckRestrictionError('90004', roland, [], undefined, zh.t), '本冒险需要黛西·沃克参与')
  assert.equal(deckRestrictionError('90004', roland), 'This scenario requires Daisy Walker')
  assert.match(deckRestrictionError('90004', daisy, [], undefined, zh.t), /（3\/4）$/)
  assert.equal(deckRestrictionError('90004', { ...daisy, slots: { '01031': 2, '01035': 2 } }, [], undefined, zh.t), null)
  assert.equal(deckRestrictionError('90004', roland, ['c01002'], undefined, zh.t), null)
  assert.equal(deckRestrictionError('90004', roland, [], undefined, zh.t, { isLastPlayer: false }), null)
  assert.match(deckRestrictionError('90094', { investigator_code: '04001', slots: {} }, [], undefined, zh.t), /平行版本/)
})

test('legacy server errors translate while retaining deck URLs, details and unknown diagnostics', () => {
  const url = 'https://arkhamdb.com/api/public/deck/12345'
  const message = `Could not fetch the deck at ${url}. If it has been deleted, point this deck at one that still exists.`
  assert.equal(localizedDeckError(message, zh.t), `无法获取${url}中的牌组。如果该牌组已被删除，请改用仍然有效的牌组链接。`)
  assert.equal(localizedDeckError(`Could not read the deck at ${url}: invalid JSON`, zh.t), `无法读取${url}中的牌组：invalid JSON`)
  assert.equal(localizedDeckError('This deck contains cards that are not implemented yet: c12345, c67890', zh.t), '此牌组包含尚未实现的卡牌：c12345, c67890')
  assert.equal(localizedDeckError(message, en.t), message)
  assert.equal(localizedDeckError('Unexpected custom diagnostic', zh.t), 'Unexpected custom diagnostic')
})

import assert from 'node:assert/strict'
import { readFileSync, readdirSync } from 'node:fs'
import { fileURLToPath, URL } from 'node:url'
import { before, after, test } from 'node:test'
import { createServer } from 'vite'
import { createI18n } from 'vue-i18n'
import { createSSRApp } from 'vue'
import { renderToString } from '@vue/server-renderer'

const root = fileURLToPath(new URL('..', import.meta.url))
const json = (path) => JSON.parse(readFileSync(new URL(path, import.meta.url), 'utf8'))
const homebrewScopes = new Set(readdirSync(new URL('../homebrew/', import.meta.url)).map(
  (name) => name.replace(/-([a-z])/g, (_, letter) => letter.toUpperCase()),
))
let server
let english
let chinese
let i18n
let tokenLabels

function leaves(value, prefix = '') {
  if (typeof value === 'string') return [[prefix, value]]
  if (!value || typeof value !== 'object') return []
  return Object.entries(value).flatMap(([key, child]) => leaves(child, prefix ? `${prefix}.${key}` : key))
}

before(async () => {
  server = await createServer({
    root, configLoader: 'runner', appType: 'custom', logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  const { loadLocaleMessages } = await server.ssrLoadModule('/src/locales/messages.ts')
  english = (await loadLocaleMessages('en')).messages
  chinese = (await loadLocaleMessages('zh-cn')).messages
  i18n = createI18n({
    legacy: false, locale: 'zh-cn', fallbackLocale: 'en', warnHtmlMessage: false,
    messages: { en: english, 'zh-cn': chinese },
  })
  tokenLabels = await server.ssrLoadModule('/src/arkham/tokenLabels.ts')
})

after(async () => { await server?.close() })

test('the active Simplified Chinese locale covers every core UI and official campaign message', () => {
  const available = new Set(leaves(chinese).map(([key]) => key))
  const missing = leaves(english).filter(([key]) =>
    !homebrewScopes.has(key.split('.')[0]) && !available.has(key),
  ).map(([key]) => key)
  assert.deepEqual(missing, [])
  assert.match(i18n.global.t('historyPanel.title'), /历史/)
  assert.equal(i18n.global.t('questionFilter.results', { count: 3, total: 24 }), '3/24张卡牌')
})

test('interface messages preserve formatting parameters and every added translation is loaded', () => {
  const en = new Map(leaves(json('../src/locales/en/interface.json')))
  const zh = new Map(leaves(json('../src/locales/zh-cn/interface.json')))
  assert.deepEqual([...zh.keys()].sort(), [...en.keys()].sort())
  const parameters = (text) => [...new Set([...text.matchAll(/\{([\w]+)\}/g)].map((match) => match[1]))].sort()
  for (const [key, value] of en) {
    assert.deepEqual(parameters(zh.get(key)), parameters(value), `${key} lost an interpolation`)
    assert.equal(i18n.global.te(`ui.${key}`), true, `${key} was not loaded`)
  }
  assert.equal(i18n.global.t('ui.cardsUnderTooltip', { label: '自定义名称', count: 4 }), '自定义名称（4），点击查看')
  assert.equal(i18n.global.t('ui.pageTitles.Deck'), '诡镇奇谈：牌组')
  assert.equal(i18n.global.t('ui.revokeKeyConfirm', { name: 'My agent' }), '撤销“My agent”？使用此密钥的工具将立即停止工作。')
})

test('literal interface references resolve to text rather than an overwritten message group', () => {
  const messages = new Map(leaves(chinese))
  function scan(directory) {
    for (const entry of readdirSync(directory, { withFileTypes: true })) {
      const path = `${directory}/${entry.name}`
      if (entry.isDirectory()) scan(path)
      else if (/\.(vue|ts)$/.test(entry.name)) {
        const source = readFileSync(path, 'utf8')
        for (const [, key] of source.matchAll(/(?:\$t|uiT|\bt)\(['"](ui\.[^'"]+)['"]/g)) {
          assert.equal(typeof messages.get(key), 'string', `${path} references ${key}, which is not a message`)
        }
      }
    }
  }
  scan(`${root}/src`)
})

test('card filter captions translate while the selected type and class identifiers stay unchanged', async () => {
  const { default: CardFilters } = await server.ssrLoadModule('/src/arkham/components/CardFilters.vue')
  const filter = { query: '', types: ['AssetType'], classes: ['Guardian'], traits: [], levels: [] }
  const card = { def: {
    cardCode: 'c:test:1', cardType: 'AssetType', name: { title: 'Custom title', subtitle: null },
    classSymbols: ['Guardian'], cardTraits: [], level: 0,
  } }
  const app = createSSRApp(CardFilters, { modelValue: filter, cards: [card], placeholder: '搜索' }).use(i18n)
  app.component('font-awesome-icon', { render: () => null })
  app.directive('tooltip', { getSSRProps: () => ({}) })
  const html = await renderToString(app)
  assert.match(html, /守护者/)
  assert.match(html, /支援（玩家卡背）/)
  assert.deepEqual(filter.types, ['AssetType'])
  assert.deepEqual(filter.classes, ['Guardian'])
})

test('the API key component renders its actual controls and scope descriptions in Chinese', async () => {
  const { default: ApiKeys } = await server.ssrLoadModule('/src/components/ApiKeys.vue')
  const html = await renderToString(createSSRApp(ApiKeys).use(i18n))
  assert.match(html, /API密钥/)
  assert.match(html, /读取我的自定义卡牌/)
  assert.match(html, /创建和修改我的自定义卡牌/)
  assert.match(html, /创建密钥/)
  assert.doesNotMatch(html, /What may it do|Read my custom cards|Create key/)
})

test('token display names translate without changing numeric faces or custom token identities', () => {
  const face = (value) => tokenLabels.localizedTokenFace(value, i18n.global.t)
  assert.equal(face('Skull'), '骷髅')
  assert.equal(face('ElderThingToken'), '古神')
  assert.equal(face('AutoFail'), '自动失败')
  assert.equal(face('MinusEight'), '−8')
  assert.equal(face(':custom:my-token'), 'custom / my-token')
  assert.equal(i18n.global.t('ui.tokenTreatedAs', { face: face('Skull'), faces: face('Cultist') }), '骷髅视为异教徒')
})

test('community scenario display names and campaign titles are available alongside official names', () => {
  for (const name of readdirSync(new URL('../homebrew/', import.meta.url))) {
    let scenarios
    try { scenarios = json(`../homebrew/${name}/scenarios.json`) } catch { continue }
    for (const scenario of scenarios) {
      assert.match(i18n.global.t(`scenarioNames.${scenario.id}`), /\p{Script=Han}/u, scenario.name)
    }
  }
})

test('generated card placeholders translate their UI labels while retaining authored names and codes', async () => {
  const registry = await server.ssrLoadModule('/src/locales/i18n.ts')
  const cards = await server.ssrLoadModule('/src/arkham/customCards.ts')
  registry.setI18n(i18n)
  const definition = { name: { title: 'My Card <1>', subtitle: null }, cardType: 'AssetType', cardTraits: [] }
  const svg = decodeURIComponent(cards.renderCardPlaceholder(definition).split(',').slice(1).join(','))
  assert.match(svg, /支援/)
  assert.match(svg, /My Card &lt;1&gt;/)
  assert.equal(definition.cardType, 'AssetType')
  const missing = decodeURIComponent(cards.renderMissingCard(':custom:abc').split(',').slice(1).join(','))
  assert.match(missing, /卡牌定义/)
  assert.match(missing, /缺失/)
  assert.match(missing, /:custom:abc/)
})

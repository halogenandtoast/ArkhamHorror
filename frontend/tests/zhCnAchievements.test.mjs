import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { fileURLToPath, URL } from 'node:url'
import { before, after, test } from 'node:test'
import { createServer } from 'vite'
import { createI18n } from 'vue-i18n'

const root = fileURLToPath(new URL('..', import.meta.url))
const readCatalog = (locale) => JSON.parse(readFileSync(
  new URL(`../src/locales/${locale}/gameBoard/achievements.json`, import.meta.url), 'utf8',
))
let server
let messages
let composer
let catalog

// Read the actual lazy-loaded messages. Merely checking the JSON files misses
// a catalog that exists on disk but was never imported by gameBoard.ts.
before(async () => {
  server = await createServer({
    root,
    configLoader: 'runner',
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  const { loadLocaleMessages } = await server.ssrLoadModule('/src/locales/messages.ts')
  const zh = await loadLocaleMessages('zh-cn')
  const en = await loadLocaleMessages('en')
  messages = zh.messages
  composer = createI18n({
    legacy: false, locale: 'zh-cn', fallbackLocale: 'en', warnHtmlMessage: false,
    messages: { 'zh-cn': zh.messages, en: en.messages },
  }).global
  catalog = await server.ssrLoadModule('/src/arkham/achievements.ts')
})

after(async () => { await server?.close() })

function leaves(value, prefix = '') {
  return typeof value === 'string' ? [[prefix, value]] : Object.entries(value).flatMap(
    ([key, child]) => leaves(child, prefix ? `${prefix}.${key}` : key),
  )
}

test('Simplified Chinese loads its achievement catalog before any achievement view opens', () => {
  const expected = readCatalog('zh-cn')
  assert.equal(messages.achievements?.pageTitle, expected.pageTitle)
  assert.deepEqual(Object.keys(messages.achievements.entries).sort(), Object.keys(expected.entries).sort())
  assert.equal(composer.t('achievements.toastTitle'), '解锁成就！')
  assert.equal(composer.t('achievements.progressLabel', { earned: 2, total: 10 }), '已获得2/10项成就')
})

test('official and homebrew achievement names, descriptions and checklists resolve in Chinese', () => {
  for (const entry of catalog.achievementCatalog) {
    const scope = catalog.achievementEntryScope(entry.tag)
    const keys = [`${scope}.name`, `${scope}.text`]
    for (const item of catalog.achievementChecklists[entry.tag] ?? []) keys.push(`${scope}.items.${item}`)
    for (const key of keys) {
      assert.ok(composer.te(key), `${key} is missing from the active locale`)
      assert.match(composer.t(key), /\p{Script=Han}/u, `${key} is not Chinese`)
    }
    const heading = catalog.achievementCampaignScope(entry.campaignId)
    assert.ok(composer.te(heading), `${heading} is missing`)
    assert.match(composer.t(heading), /\p{Script=Han}/u)
  }
})

test('achievement translations preserve every official message and formatting token', () => {
  const english = new Map(leaves(readCatalog('en')))
  const chinese = new Map(leaves(readCatalog('zh-cn')))
  assert.deepEqual([...chinese.keys()].sort(), [...english.keys()].sort())
  for (const [key, value] of english) {
    const tokens = (text) => [...text.matchAll(/\{([A-Za-z0-9_]+)\}/g)].map((m) => m[1]).sort()
    assert.deepEqual(tokens(chinese.get(key)), tokens(value), `${key} lost a formatting token`)
  }
})

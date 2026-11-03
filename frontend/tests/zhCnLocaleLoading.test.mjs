import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import fs from 'node:fs'
import path from 'node:path'
import { after, before, test } from 'node:test'
import { createServer } from 'vite'

const frontend = path.resolve(import.meta.dirname, '..')
let server
let english
let simplifiedChinese

before(async () => {
  server = await createServer({
    root: frontend,
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false, watch: null },
  })
  english = (await server.ssrLoadModule('/src/locales/en.ts')).default
  simplifiedChinese = (await server.ssrLoadModule('/src/locales/zh-cn.ts')).default
})

after(async () => {
  await server?.close()
})

function messageAt(messages, parts) {
  return parts.reduce((value, key) => value?.[key], messages)
}

function stringLeaves(value, parts = []) {
  if (typeof value === 'string') return [[parts, value]]
  if (!value || typeof value !== 'object') return []
  return Object.entries(value).flatMap(([key, child]) => stringLeaves(child, [...parts, key]))
}

// Check the actual Vite-loaded catalog, rather than only the JSON files on disk:
// translations can exist on disk while missing imports or flattened namespaces
// still make the running application fall back to English.
function assertTranslationsExposed(scope, file) {
  const translated = JSON.parse(fs.readFileSync(
    path.join(frontend, 'src/locales/zh-cn', file),
    'utf8',
  ))
  const prefix = scope.split('.')
  let checked = 0
  for (const [parts, value] of stringLeaves(translated)) {
    const fullPath = [...prefix, ...parts]
    // Locales may contain older or extra keys; only assert paths the English
    // catalog still uses, without requiring every new English key to be translated.
    if (typeof messageAt(english, fullPath) !== 'string') continue
    assert.equal(messageAt(simplifiedChinese, fullPath), value, fullPath.join('.'))
    checked += 1
  }
  assert.ok(checked > 0, scope + ' must expose existing Chinese translations')
}

test('Simplified Chinese exposes translated achievements, history, and modifiers', () => {
  assertTranslationsExposed('achievements', 'gameBoard/achievements.json')
  assertTranslationsExposed('historyPanel', 'gameBoard/history_panel.json')
  assertTranslationsExposed('ultimatumsAndBoons', 'gameBoard/ultimatumsAndBoons.json')
})

test('Simplified Chinese Hemlock Vale scenarios keep the namespaces used by the game', () => {
  for (const scenario of [
    'fateOfTheVale',
    'hemlockHouse',
    'theLongestNight',
    'theLostSister',
    'theSilentHeath',
    'theThingInTheDepths',
    'theTwistedHollow',
    'writtenInRock',
  ]) {
    assertTranslationsExposed(
      'theFeastOfHemlockVale.' + scenario,
      'theFeastOfHemlockVale/' + scenario + '.json',
    )
  }
})

test('Simplified Chinese Hemlock Vale scenarios preserve campaign-level translations', () => {
  assertTranslationsExposed('theFeastOfHemlockVale', 'theFeastOfHemlockVale/base.json')
})

test('Simplified Chinese exposes both Heart of the Elders parts and their return variants', () => {
  for (const campaign of ['theForgottenAge', 'returnToTheForgottenAge']) {
    for (const scenario of ['heartOfTheEldersPart1', 'heartOfTheEldersPart2']) {
      assertTranslationsExposed(
        campaign + '.' + scenario,
        'theForgottenAge/scenarios/' + scenario + '.json',
      )
    }
  }
})



test('Reference-sourced Chinese translations are exposed at their documented game paths', () => {
  const manifest = JSON.parse(fs.readFileSync(
    path.join(frontend, 'translation-reference/arkham-cards-data-zh-cn.json'),
    'utf8',
  ))
  const hash = (value) => createHash('sha256').update(value).digest('hex')
  assert.ok(manifest.messages.length > 0)

  for (const message of manifest.messages) {
    const label = message.runtimePath.join('.')
    const source = messageAt(english, message.runtimePath)
    const translation = messageAt(simplifiedChinese, message.runtimePath)
    assert.equal(typeof source, 'string', label + ' must have an English source')
    assert.equal(hash(source), message.englishSha256, label + ' English source changed')
    assert.equal(typeof translation, 'string', label + ' must expose its Chinese text')
    assert.equal(hash(translation), message.translationSha256, label + ' translation changed')
  }
})

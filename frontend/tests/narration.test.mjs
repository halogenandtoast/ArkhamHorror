import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync, existsSync } from 'node:fs'
import ts from 'typescript'

// narration.ts imports types only, so it loads here without the Vue toolchain.
const source = readFileSync(new URL('../src/arkham/narration.ts', import.meta.url), 'utf8')
const js = ts.transpile(source, { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 })
const { entryKeys, storyKeysInPlay, narrationTracks, defaultNarrationLanguage, readNarrationLanguage } =
  await import('data:text/javascript;base64,' + Buffer.from(js).toString('base64'))

const i18n = (key) => ({ tag: 'I18nEntry', key, variables: {} })
const read = (...body) => ({ tag: 'Read', flavorText: { title: null, body }, readChoices: { contents: [] }, readCards: null })

test('entryKeys walks composite, modified and list entries in reading order', () => {
  const body = [
    { tag: 'ModifyEntry', modifiers: ['ResolutionEntry'], entry: i18n('a') },
    { tag: 'CompositeEntry', entries: [i18n('b'), { tag: 'BasicEntry', text: 'x' }] },
    { tag: 'ListEntry', list: [{ entry: i18n('c'), nested: [{ entry: i18n('d'), nested: [] }] }] },
  ]
  assert.deepEqual(entryKeys(body), ['a', 'b', 'c', 'd'])
})

test('storyKeysInPlay sees any player\'s Read, unwraps labels and de-duplicates', () => {
  const questions = {
    lead: read(i18n('intro.body')),
    other: { tag: 'QuestionLabel', label: 'x', cards: [], question: read(i18n('intro.body')) },
    third: { tag: 'ChooseOne', choices: [] },
  }
  assert.deepEqual(storyKeysInPlay(questions), ['intro.body'])
  assert.deepEqual(storyKeysInPlay(null), [])
})

test('narrationTracks keeps only rendered keys and falls back to English', () => {
  const manifest = { version: 1, languages: { en: { a: 'h1', b: 'h2' }, vi: { a: 'h3' } } }
  assert.deepEqual(narrationTracks(['a', 'b', 'setup'], manifest, 'vi', '/n'), [
    { key: 'a', url: '/n/vi/a.mp3?v=h3' },
    { key: 'b', url: '/n/en/b.mp3?v=h2' },
  ])
  assert.deepEqual(narrationTracks(['a'], null, 'en'), [])
})

test('narration language defaults from the browser and rejects unknown stored values', () => {
  assert.equal(defaultNarrationLanguage(['vi-VN', 'en']), 'vi')
  assert.equal(defaultNarrationLanguage(['en-US']), 'en')
  assert.equal(readNarrationLanguage('vi', 'en'), 'vi')
  assert.equal(readNarrationLanguage('klingon', 'en'), 'en')
})

test('every manifest entry has its mp3 on disk', () => {
  const root = new URL('../public/audio/narration/', import.meta.url)
  const manifestUrl = new URL('manifest.json', root)
  if (!existsSync(manifestUrl)) return
  const manifest = JSON.parse(readFileSync(manifestUrl, 'utf8'))
  for (const [lang, keys] of Object.entries(manifest.languages)) {
    for (const key of Object.keys(keys)) {
      assert.ok(existsSync(new URL(`${lang}/${key}.mp3`, root)), `${lang}/${key}.mp3 missing`)
    }
  }
})

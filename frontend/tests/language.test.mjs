import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'
import { resolve } from 'node:path'
import test from 'node:test'
import ts from 'typescript'

async function importTsModule(path) {
  const source = await readFile(path, 'utf8')
  const { outputText } = ts.transpileModule(source, {
    compilerOptions: {
      module: ts.ModuleKind.ES2022,
      target: ts.ScriptTarget.ES2022,
      verbatimModuleSyntax: true,
    },
    fileName: path,
  })

  return import(`data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`)
}

const modulePath = resolve('src/locales/language.ts')

test('Simplified Chinese browser locales select zh-cn', async () => {
  const { preferredLanguage } = await importTsModule(modulePath)

  for (const locale of ['zh-CN', 'zh-SG', 'zh-Hans', 'zh-Hans-CN']) {
    assert.equal(preferredLanguage(locale), 'zh-cn')
  }
})

test('Traditional Chinese browser locales keep the existing zh language', async () => {
  const { preferredLanguage } = await importTsModule(modulePath)

  for (const locale of ['zh-TW', 'zh-HK', 'zh-MO', 'zh-Hant', 'zh-Hant-TW', 'zh-Hant-CN']) {
    assert.equal(preferredLanguage(locale), 'zh')
  }
})

test('regional non-Chinese locales use their base language', async () => {
  const { preferredLanguage } = await importTsModule(modulePath)

  assert.equal(preferredLanguage('fr-CA'), 'fr')
  assert.equal(preferredLanguage(''), 'en')
})

test('Simplified Chinese reuses the existing Chinese UI messages', async () => {
  const { uiLocaleFor } = await importTsModule(modulePath)

  assert.equal(uiLocaleFor('zh-cn'), 'zh')
  assert.equal(uiLocaleFor('zh'), 'zh')
  assert.equal(uiLocaleFor('de'), 'en')
})

test('bootstrap initializes Vue I18n with the normalized UI locale', async () => {
  const source = await readFile(resolve('src/main.ts'), 'utf8')

  assert.match(source, /locale:\s*currentLocale/)
})

test('language settings switch Vue I18n to the normalized UI locale', async () => {
  const source = await readFile(resolve('src/components/SettingsForm.vue'), 'utf8')

  assert.match(source, /locale\.value\s*=\s*uiLocale/)
})

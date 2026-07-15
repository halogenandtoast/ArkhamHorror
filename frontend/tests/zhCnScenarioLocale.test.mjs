import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import test from 'node:test'

const locales = path.resolve(import.meta.dirname, '../src/locales')
const english = path.join(locales, 'en')
const simplifiedChinese = path.join(locales, 'zh-cn')

function jsonFiles(directory) {
  return fs.readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const file = path.join(directory, entry.name)
    return entry.isDirectory() ? jsonFiles(file) : file.endsWith('.json') ? [file] : []
  })
}

function leafKeys(value, prefix = '') {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    return Object.entries(value).flatMap(([key, child]) =>
      leafKeys(child, prefix ? `${prefix}.${key}` : key),
    )
  }
  return [prefix]
}

const scenarioDirectories = fs.readdirSync(english, { withFileTypes: true })
  .filter((entry) => entry.isDirectory() && entry.name !== 'gameBoard')
  .map((entry) => entry.name)

test('Simplified Chinese scenario locale contains every English scenario file and key', () => {
  const missing = []

  for (const directory of scenarioDirectories) {
    for (const englishFile of jsonFiles(path.join(english, directory))) {
      const relative = path.relative(english, englishFile)
      const localizedFile = path.join(simplifiedChinese, relative)
      if (!fs.existsSync(localizedFile)) {
        missing.push(`${relative} (file)`)
        continue
      }

      const englishKeys = leafKeys(JSON.parse(fs.readFileSync(englishFile, 'utf8')))
      const localizedKeys = new Set(leafKeys(JSON.parse(fs.readFileSync(localizedFile, 'utf8'))))
      for (const key of englishKeys) {
        if (!localizedKeys.has(key)) missing.push(`${relative}:${key}`)
      }
    }
  }

  assert.deepEqual(missing, [])
})

test('Simplified Chinese scenario locale keeps locale keys language-neutral', () => {
  const translatedKeys = []

  for (const directory of scenarioDirectories) {
    for (const file of jsonFiles(path.join(simplifiedChinese, directory))) {
      const relative = path.relative(simplifiedChinese, file)
      for (const key of leafKeys(JSON.parse(fs.readFileSync(file, 'utf8')))) {
        if (/\p{Script=Han}/u.test(key)) translatedKeys.push(`${relative}:${key}`)
      }
    }
  }

  assert.deepEqual(translatedKeys, [])
})

test('Simplified Chinese locale does not load campaign text from the English locale', () => {
  const files = [path.join(locales, 'zh-cn.ts'), ...jsonFiles(simplifiedChinese)]
  const englishImports = files
    .filter((file) => file.endsWith('.ts'))
    .flatMap((file) => fs.readFileSync(file, 'utf8').match(/@\/locales\/en\/(?:standalone|brethrenOfAsh|theFeastOfHemlockVale|theDrownedCity|darkMatter|circusExMortis)[^'\"]*/g) ?? [])

  assert.deepEqual(englishImports, [])
})

test('Simplified Chinese scenario strings do not fall back to English source text', () => {
  const untranslated = []

  for (const directory of scenarioDirectories) {
    for (const englishFile of jsonFiles(path.join(english, directory))) {
      const relative = path.relative(english, englishFile)
      const localizedFile = path.join(simplifiedChinese, relative)
      if (!fs.existsSync(localizedFile)) continue

      const source = JSON.parse(fs.readFileSync(englishFile, 'utf8'))
      const localized = JSON.parse(fs.readFileSync(localizedFile, 'utf8'))
      const collect = (value, translation, key = '') => {
        if (typeof value === 'string') {
          if (value === translation && !value.includes('@:') && /[A-Za-z]{3}/.test(value)) untranslated.push(`${relative}:${key}`)
          return
        }
        if (Array.isArray(value)) value.forEach((entry, index) => collect(entry, translation?.[index], `${key}[${index}]`))
        else if (value && typeof value === 'object') Object.entries(value).forEach(([name, entry]) => collect(entry, translation?.[name], key ? `${key}.${name}` : name))
      }
      collect(source, localized)
    }
  }

  assert.deepEqual(untranslated, [])
})

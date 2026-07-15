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

test('Simplified Chinese locale does not import text from the English locale', () => {
  const files = [path.join(locales, 'zh-cn.ts'), ...jsonFiles(simplifiedChinese)]
  const englishImports = files
    .filter((file) => file.endsWith('.ts'))
    .flatMap((file) => fs.readFileSync(file, 'utf8').match(/@\/locales\/en\/[^'\"]*/g) ?? [])

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

test('Simplified Chinese scenario markup uses Chinese sentence endings and valid paragraph starts', () => {
  const malformed = []

  for (const directory of scenarioDirectories) {
    for (const file of jsonFiles(path.join(simplifiedChinese, directory))) {
      const relative = path.relative(simplifiedChinese, file)
      const collect = (value, key = '') => {
        if (typeof value === 'string') {
          if (/[。！？…]\.(?:<\/(?:p|li)>|$)/.test(value)) {
            malformed.push(`${relative}:${key} (duplicate sentence ending)`)
          }
          if (/<p>\s*<p>/i.test(value)) malformed.push(`${relative}:${key} (nested paragraph start)`)
          return
        }
        if (Array.isArray(value)) value.forEach((entry, index) => collect(entry, `${key}[${index}]`))
        else if (value && typeof value === 'object') {
          Object.entries(value).forEach(([name, entry]) => collect(entry, key ? `${key}.${name}` : name))
        }
      }
      collect(JSON.parse(fs.readFileSync(file, 'utf8')))
    }
  }

  assert.deepEqual(malformed, [])
})

test('Simplified Chinese scenario strings preserve source formatting tokens', () => {
  const mismatches = []
  const tokens = (value) => {
    const escapedBraces = /\{'\{'\}([A-Za-z][A-Za-z0-9]*)\{'\}'\}/g
    const escapedTokens = [...value.matchAll(escapedBraces)].map((match) => `icon:${match[1]}`)
    const remaining = value.replace(escapedBraces, '')
    const interpolationTokens = [...remaining.matchAll(/\{([A-Za-z][A-Za-z0-9]*)\}/g)]
      .map((match) => `value:${match[1]}`)
    return [...escapedTokens, ...interpolationTokens]
      .sort()
  }

  for (const directory of scenarioDirectories) {
    for (const englishFile of jsonFiles(path.join(english, directory))) {
      const relative = path.relative(english, englishFile)
      const localizedFile = path.join(simplifiedChinese, relative)
      if (!fs.existsSync(localizedFile)) continue

      const collect = (value, translation, key = '') => {
        if (typeof value === 'string') {
          if (typeof translation === 'string' && translation.startsWith('@:')) return
          const sourceTokens = tokens(value)
          const localizedTokens = typeof translation === 'string' ? tokens(translation) : []
          const availableTokens = [...localizedTokens]
          const missingTokens = sourceTokens.filter((token) => {
            const index = availableTokens.indexOf(token)
            if (index === -1) return true
            availableTokens.splice(index, 1)
            return false
          })
          if (missingTokens.length > 0) {
            mismatches.push(`${relative}:${key} (${[...new Set(missingTokens)].join(', ')})`)
          }
          return
        }
        if (Array.isArray(value)) {
          value.forEach((entry, index) => collect(entry, translation?.[index], `${key}[${index}]`))
        } else if (value && typeof value === 'object') {
          Object.entries(value).forEach(([name, entry]) =>
            collect(entry, translation?.[name], key ? `${key}.${name}` : name),
          )
        }
      }
      collect(
        JSON.parse(fs.readFileSync(englishFile, 'utf8')),
        JSON.parse(fs.readFileSync(localizedFile, 'utf8')),
      )
    }
  }

  assert.deepEqual(mismatches, [])
})

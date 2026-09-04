#!/usr/bin/env node

const fs = require('fs')
const path = require('path')
const { spawnSync } = require('child_process')
const { isDeepStrictEqual } = require('util')

const TRANSLATED_FIELDS = [
  'name',
  'subname',
  'text',
  'traits',
  'flavor',
  'slot',
  'back_name',
  'back_subname',
  'back_text',
  'back_traits',
  'back_flavor',
  'customization_text',
  'customization_change',
]

const METADATA_FIELDS = [
  ['pack_code', 'pack_name', 'packs'],
  ['type_code', 'type_name', 'types'],
  ['subtype_code', 'subtype_name', 'subtypes'],
  ['faction_code', 'faction_name', 'factions'],
  ['faction2_code', 'faction2_name', 'factions'],
  ['faction3_code', 'faction3_name', 'factions'],
  ['encounter_code', 'encounter_name', 'encounters'],
  ['cycle_code', 'cycle_name', 'cycles'],
]

function buildLocalizedCards(englishCards, translations, metadata = {}, overrides = new Map()) {
  const used = new Set()
  let translated = 0
  let reused = 0
  let overridden = 0

  const identityFor = (card) =>
    JSON.stringify([
      card.real_name ?? card.name ?? null,
      card.subname || '',
      card.real_text || card.text || '',
      card.real_traits || card.traits || '',
      card.back_name || '',
      card.back_text || '',
      card.type_code ?? null,
      card.faction_code ?? null,
      card.xp ?? null,
      card.customization_text || '',
    ])

  const reusableTranslations = new Map()
  for (const card of englishCards) {
    const translation = translations.get(card.code)
    if (translation) reusableTranslations.set(identityFor(card), translation)
  }

  const cards = englishCards.map((card) => {
    const localized = { ...card }
    const directTranslation = translations.get(card.code)
    const sideTranslation =
      !directTranslation && card.code.endsWith('b')
        ? translations.get(card.code.slice(0, -1))
        : undefined
    const translation =
      directTranslation ?? sideTranslation ?? reusableTranslations.get(identityFor(card))

    if (translation) {
      used.add(translation.code)
      if (directTranslation) translated += 1
      else reused += 1

      for (const field of TRANSLATED_FIELDS) {
        if (translation[field] !== undefined) localized[field] = translation[field]
      }
    }

    for (const [codeField, nameField, metadataKey] of METADATA_FIELDS) {
      const code = localized[codeField]
      const name = metadata[metadataKey]?.get(code)
      if (name !== undefined) localized[nameField] = name
    }

    // Applied last, and only by exact code, so a local correction always wins over upstream
    // and never rides along on a reprint matched by identityFor.
    const override = overrides.get(card.code)
    if (override) {
      overridden += 1
      for (const field of TRANSLATED_FIELDS) {
        if (override[field] !== undefined) localized[field] = override[field]
      }
    }

    return localized
  })

  return {
    cards,
    stats: {
      total: englishCards.length,
      translated,
      reused,
      fallback: englishCards.length - translated - reused,
      unused: translations.size - used.size,
      overridden,
    },
  }
}

function jsonFilesUnder(root) {
  const files = []
  for (const entry of fs.readdirSync(root, { withFileTypes: true })) {
    const target = path.join(root, entry.name)
    if (entry.isDirectory()) files.push(...jsonFilesUnder(target))
    else if (entry.isFile() && entry.name.endsWith('.json')) files.push(target)
  }
  return files.sort()
}

// Upstream duplicates a pack file into a second cycle directory often enough that an
// all-or-nothing conflict check leaves whole locales unimportable (fr has been wedged on 06168
// since 2021). Merge field by field instead: a value identical to the English one is an
// untranslated placeholder and loses to a real translation. What survives that is a genuine
// upstream data bug, and only an override -- which wins anyway -- may silence it.
function loadTranslations(localeRoot, englishCards = [], overrides = new Map()) {
  const english = new Map(englishCards.map((card) => [card.code, card]))
  const translations = new Map()
  const sources = new Map()

  for (const file of jsonFilesUnder(path.join(localeRoot, 'pack'))) {
    const cards = JSON.parse(fs.readFileSync(file, 'utf8'))
    if (!Array.isArray(cards)) throw new Error(`${file} must contain a JSON array`)

    for (const card of cards) {
      if (!card.code) throw new Error(`${file} contains a card without a code`)
      const existing = translations.get(card.code)
      if (!existing) {
        translations.set(card.code, { ...card })
        sources.set(card.code, file)
        continue
      }
      if (isDeepStrictEqual(existing, card)) continue

      const englishCard = english.get(card.code) ?? {}
      for (const field of TRANSLATED_FIELDS) {
        const incoming = card[field]
        if (incoming === undefined || incoming === existing[field]) continue
        if (existing[field] === undefined) {
          existing[field] = incoming
          continue
        }

        const incomingIsEnglish = incoming === englishCard[field]
        const existingIsEnglish = existing[field] === englishCard[field]
        if (existingIsEnglish && !incomingIsEnglish) existing[field] = incoming
        else if (incomingIsEnglish && !existingIsEnglish) continue
        else if (!overrides.has(card.code)) {
          throw new Error(
            `Conflicting translations for card ${card.code} field "${field}": ` +
              `${sources.get(card.code)} and ${file}. Fix it upstream, or add ${card.code} to ` +
              `translation-overrides/<locale>.json to pick a winner.`,
          )
        }
      }
    }
  }
  return translations
}

// Local corrections layered on top of upstream, so a bad translation we have already fixed does
// not come back on the next import. See translation-overrides/README.md.
function loadOverrides(root, sourceLocale) {
  const overrides = new Map()
  const file = path.join(root, 'translation-overrides', `${sourceLocale}.json`)
  if (!fs.existsSync(file)) return overrides

  const cards = JSON.parse(fs.readFileSync(file, 'utf8'))
  if (!Array.isArray(cards)) throw new Error(`${file} must contain a JSON array`)

  for (const card of cards) {
    if (!card.code) throw new Error(`${file} contains an override without a code`)
    if (overrides.has(card.code)) throw new Error(`${file} overrides ${card.code} twice`)

    const unknown = Object.keys(card).filter(
      (field) => field !== 'code' && !TRANSLATED_FIELDS.includes(field),
    )
    if (unknown.length) {
      throw new Error(`${file} override ${card.code} has untranslatable fields: ${unknown.join(', ')}`)
    }

    overrides.set(card.code, card)
  }
  return overrides
}

function loadMetadata(localeRoot) {
  const metadata = {}
  for (const key of ['packs', 'types', 'subtypes', 'factions', 'encounters', 'cycles']) {
    const file = path.join(localeRoot, `${key}.json`)
    if (!fs.existsSync(file)) continue
    const entries = JSON.parse(fs.readFileSync(file, 'utf8'))
    metadata[key] = new Map(entries.map(({ code, name }) => [code, name]))
  }
  return metadata
}

function parseArgs(args) {
  const options = { sourceLocale: 'zh-cn', outputLang: 'zh-cn', slim: false }
  const readValue = (option, index) => {
    const value = args[index]
    if (!value || value.startsWith('-')) throw new Error(`${option} requires a value`)
    return value
  }

  for (let i = 0; i < args.length; i += 1) {
    const arg = args[i]
    if (arg === '--source') options.source = readValue(arg, ++i)
    else if (arg === '--source-locale') options.sourceLocale = readValue(arg, ++i)
    else if (arg === '--output-lang') options.outputLang = readValue(arg, ++i)
    else if (arg === '--slim') options.slim = true
    else if (arg === '--help' || arg === '-h') options.help = true
    else throw new Error(`Unknown argument: ${arg}`)
  }
  return options
}

function main() {
  const root = path.join(__dirname, '..')
  const options = parseArgs(process.argv.slice(2))
  if (options.help) {
    console.log(
      'Usage: node scripts/import-card-translations.cjs [--source PATH] [--source-locale zh-cn] [--output-lang zh-cn] [--slim]',
    )
    return
  }

  const source = path.resolve(
    options.source ||
      process.env.ARKHAMDB_JSON_DATA_DIR ||
      path.join(root, '..', '..', 'arkhamdb-json-data'),
  )
  const localeRoot = path.join(source, 'translations', options.sourceLocale)
  if (!fs.existsSync(localeRoot)) throw new Error(`Translation locale not found: ${localeRoot}`)

  const englishFile = path.join(root, 'public', 'cards_en.json')
  const outputFile = path.join(root, 'public', `cards_${options.outputLang}.json`)
  const englishCards = JSON.parse(fs.readFileSync(englishFile, 'utf8'))
  const metadata = loadMetadata(localeRoot)
  const overrides = loadOverrides(root, options.sourceLocale)
  const translations = loadTranslations(localeRoot, englishCards, overrides)
  const { cards, stats } = buildLocalizedCards(englishCards, translations, metadata, overrides)

  fs.writeFileSync(outputFile, `${JSON.stringify(cards, null, 2)}\n`)
  console.log(
    `${options.sourceLocale} -> ${options.outputLang}: ${stats.translated}/${stats.total} translated, ` +
      `${stats.reused} reprints reused, ${stats.fallback} English fallbacks, ` +
      `${stats.unused} unused, ${stats.overridden} locally overridden -> ${outputFile}`,
  )

  if (options.slim) {
    const result = spawnSync(process.execPath, ['scripts/slim-cards.cjs'], {
      cwd: root,
      stdio: 'inherit',
    })
    if (result.status !== 0) process.exit(result.status ?? 1)
  }
}

if (require.main === module) main()

module.exports = { buildLocalizedCards, loadMetadata, loadOverrides, loadTranslations, parseArgs }

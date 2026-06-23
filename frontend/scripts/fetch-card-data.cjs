#!/usr/bin/env node
/*
 * Downloads ArkhamDB card metadata exports into frontend/public/cards_*.json.
 *
 * English lives at arkhamdb.com; translations use the language subdomain
 * (for example zh.arkhamdb.com). After refreshing, pass --slim or run
 * `npm run slim-cards` to regenerate frontend/public/cards/cards_*.json.
 *
 * Usage:
 *   node scripts/fetch-card-data.cjs
 *   node scripts/fetch-card-data.cjs --langs en,zh --slim
 *
 * Environment:
 *   ARKHAMDB_CARD_LANGS=en,zh,fr   Override languages to fetch
 *   ARKHAMDB_TIMEOUT_MS=60000      Per-request timeout
 */

const fs = require('fs')
const path = require('path')
const { spawnSync } = require('child_process')

const DEFAULT_LANGS = [
  'en',
  'de',
  'es',
  'fr',
  'it',
  'ko',
  'pl',
  'po',
  'ru',
  'uk',
  'zh',
]

// Local app historically uses cards_po.json. ArkhamDB's Portuguese subdomain is pt.
const HOST_LANG = {
  en: '',
  po: 'pt',
}

const args = process.argv.slice(2)
const hasArg = (name) => args.includes(name)
const valueArg = (name) => {
  const i = args.indexOf(name)
  return i === -1 ? null : args[i + 1]
}

if (hasArg('--help') || hasArg('-h')) {
  console.log(`Usage: node scripts/fetch-card-data.cjs [--langs en,zh,fr] [--slim]\n\nDownloads ArkhamDB /api/public/cards/?encounter=1 exports to frontend/public/cards_*.json.\n`)
  process.exit(0)
}

const root = path.join(__dirname, '..')
const publicDir = path.join(root, 'public')
const timeoutMs = Number(process.env.ARKHAMDB_TIMEOUT_MS || 60000)
const langs = (valueArg('--langs') || process.env.ARKHAMDB_CARD_LANGS || DEFAULT_LANGS.join(','))
  .split(',')
  .map((s) => s.trim())
  .filter(Boolean)

function urlForLang(lang) {
  const hostLang = HOST_LANG[lang] ?? lang
  const host = hostLang ? `${hostLang}.arkhamdb.com` : 'arkhamdb.com'
  return `https://${host}/api/public/cards/?encounter=1`
}

async function fetchJson(lang) {
  const url = urlForLang(lang)
  const controller = new AbortController()
  const timer = setTimeout(() => controller.abort(), timeoutMs)

  try {
    process.stdout.write(`${lang}: fetching ${url} ... `)
    const response = await fetch(url, {
      signal: controller.signal,
      headers: { accept: 'application/json' },
    })

    if (!response.ok) {
      throw new Error(`HTTP ${response.status} ${response.statusText}`)
    }

    const text = await response.text()
    const data = JSON.parse(text)
    if (!Array.isArray(data)) throw new Error('response was not a JSON array')

    const pretty = `${JSON.stringify(data, null, 2)}\n`
    const out = path.join(publicDir, `cards_${lang}.json`)
    const tmp = `${out}.tmp`
    fs.writeFileSync(tmp, pretty)
    fs.renameSync(tmp, out)

    console.log(`${data.length} cards -> ${path.relative(root, out)}`)
  } finally {
    clearTimeout(timer)
  }
}

async function main() {
  fs.mkdirSync(publicDir, { recursive: true })

  const failures = []
  for (const lang of langs) {
    try {
      await fetchJson(lang)
    } catch (error) {
      failures.push([lang, error])
      console.error(`failed: ${error.message}`)
    }
  }

  if (failures.length > 0) {
    console.error('\nFailed languages:')
    for (const [lang, error] of failures) console.error(`  ${lang}: ${error.message}`)
    process.exitCode = 1
    return
  }

  if (hasArg('--slim')) {
    console.log('\nRegenerating slim card metadata...')
    const result = spawnSync(process.execPath, ['scripts/slim-cards.cjs'], {
      cwd: root,
      stdio: 'inherit',
    })
    if (result.status !== 0) process.exit(result.status ?? 1)
  } else {
    console.log('\nDone. Run `npm run slim-cards` to regenerate frontend/public/cards/cards_*.json.')
  }
}

main().catch((error) => {
  console.error(error)
  process.exit(1)
})

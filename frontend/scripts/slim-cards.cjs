// Reads frontend/public/cards_*.json (the full ArkhamDB exports) and writes
// trimmed copies to frontend/public/cards/cards_*.json containing only the
// fields the frontend actually consumes (see ArkhamDBCard in
// src/stores/dbCards.ts). Each output is also pre-compressed to .gz so
// nginx can serve it via gzip_static.
//
// Run after refreshing the source card files:
//   npm run slim-cards

const fs = require('fs')
const path = require('path')
const zlib = require('zlib')

const KEEP_FIELDS = [
  'code',
  'name',
  'xp',
  'subname',
  'traits',
  'text',
  'back_name',
  'back_traits',
  'back_text',
  'customization_text',
  'flavor',
  'back_flavor',
  'faction_name',
  'faction2_name',
  'faction3_name',
  'faction_code',
  'type_name',
  'pack_name',
  'real_name',
  'real_traits',
  'real_text',
  'type_code',
  'is_unique',
  'double_sided',
  'encounter_code',
]

const publicDir = path.join(__dirname, '..', 'public')
const outDir = path.join(publicDir, 'cards')

if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true })

const sources = fs
  .readdirSync(publicDir)
  .filter((f) => /^cards_[a-z]+\.json$/.test(f))

if (sources.length === 0) {
  console.error('No cards_*.json source files found in', publicDir)
  process.exit(1)
}

let totalIn = 0
let totalOut = 0
let totalGz = 0

for (const file of sources) {
  const src = path.join(publicDir, file)
  const raw = fs.readFileSync(src, 'utf8').trim()
  if (!raw) {
    console.log(`${file}: empty source, skipping`)
    continue
  }
  const cards = JSON.parse(raw)
  const slim = cards.map((c) => {
    const o = {}
    for (const k of KEEP_FIELDS) if (c[k] !== undefined) o[k] = c[k]
    return o
  })
  const json = JSON.stringify(slim)
  const dst = path.join(outDir, file)
  fs.writeFileSync(dst, json)
  const gz = zlib.gzipSync(json, { level: 9 })
  fs.writeFileSync(dst + '.gz', gz)

  const inSize = fs.statSync(src).size
  totalIn += inSize
  totalOut += json.length
  totalGz += gz.length

  const pct = ((gz.length / inSize) * 100).toFixed(1)
  console.log(
    `${file}: ${inSize} -> ${json.length} bytes (slim), ${gz.length} bytes (gz, ${pct}% of original)`,
  )
}

console.log(
  `\nTotal: ${totalIn} -> ${totalOut} bytes (slim), ${totalGz} bytes (gz)`,
)

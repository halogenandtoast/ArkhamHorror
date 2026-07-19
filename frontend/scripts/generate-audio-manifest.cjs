#!/usr/bin/env node
//
// Generates a manifest of available card sound cues under frontend/public/audio/cards/.
//
// Numbered variants (e.g. "Cultist (1).wav", "Cultist (2).wav", ...) are grouped
// under a single key so the client can pick one at random each time, instead of
// always playing the same file. The number of variants is never hardcoded
// anywhere else — just drop more "Name (N).ext" files into the right folder and
// re-run this script to pick them up.
//
// Usage: node scripts/generate-audio-manifest.cjs
//
const fs = require('fs')
const path = require('path')

const publicDir = path.join(__dirname, '../public')
const audioDir = path.join(publicDir, 'audio')
const cardsDir = path.join(audioDir, 'cards')
const outputFile = path.join(audioDir, 'manifest.json')

const AUDIO_EXT = /\.(ogg|mp3|wav)$/i
const VARIANT_SUFFIX = /\s*\(\d+\)$/

function walkDir(dir) {
  const results = []
  if (!fs.existsSync(dir)) return results

  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.name === '.DS_Store') continue
    const fullPath = path.join(dir, entry.name)
    if (entry.isDirectory()) {
      results.push(...walkDir(fullPath))
    } else if (AUDIO_EXT.test(entry.name)) {
      results.push(fullPath)
    }
  }
  return results
}

// Group files by "<dir>/<base name without variant suffix or extension>" so
// that e.g. cards/Generic/Cultist/Cultist (1..13).wav all land under the same
// key: cards/Generic/Cultist/Cultist
const manifest = {}

for (const absPath of walkDir(cardsDir)) {
  const relPath = path.relative(audioDir, absPath).split(path.sep).join('/')
  const dir = path.dirname(relPath)
  const base = path.basename(relPath, path.extname(relPath)).replace(VARIANT_SUFFIX, '')
  const key = `${dir}/${base}`
  if (!manifest[key]) manifest[key] = []
  manifest[key].push(relPath)
}

const sortedManifest = {}
let totalFiles = 0
for (const key of Object.keys(manifest).sort()) {
  sortedManifest[key] = manifest[key].sort()
  totalFiles += manifest[key].length
}

fs.writeFileSync(outputFile, JSON.stringify(sortedManifest, null, 2) + '\n')
console.log(`Wrote ${totalFiles} audio files across ${Object.keys(sortedManifest).length} cues to ${outputFile}`)

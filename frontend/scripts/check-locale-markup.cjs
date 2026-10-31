#!/usr/bin/env node
/* Report locale strings whose list markup does not balance.
 *
 * The two hard checks live in tests/localeMarkup.test.mjs: no stray bullet
 * glyphs, and no <ul> opening with bare text. This script covers the wider,
 * softer family -- <ul>/</ul> and <li>/</li> counts that disagree. Most of
 * those render acceptably because browsers auto-close the tags, so this is a
 * backlog report, not a gate: it always exits 0.
 *
 * Run with: npm run check:locale-markup
 */
const { readdirSync, readFileSync } = require('node:fs')
const { join, relative, resolve } = require('node:path')

const localeRoot = resolve(__dirname, '..', 'src', 'locales')

function jsonFilesUnder(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name)
    return entry.isDirectory() ? jsonFilesUnder(path) : path.endsWith('.json') ? [path] : []
  })
}

function count(haystack, needle) {
  return haystack.split(needle).length - 1
}

function problems(value) {
  const found = []
  const openUl = count(value, '<ul>')
  const closeUl = count(value, '</ul>')
  if (openUl !== closeUl) found.push(`ul ${openUl}/${closeUl}`)
  const openLi = (value.match(/<li\b/g) || []).length
  const closeLi = count(value, '</li>')
  if (openLi !== closeLi) found.push(`li ${openLi}/${closeLi}`)
  return found
}

function walk(node, path, report) {
  if (typeof node === 'string') {
    if (!node.includes('<')) return
    const found = problems(node)
    if (found.length) report.push({ path, found })
  } else if (Array.isArray(node)) {
    node.forEach((child, i) => walk(child, `${path}[${i}]`, report))
  } else if (node && typeof node === 'object') {
    for (const [key, child] of Object.entries(node)) walk(child, `${path}.${key}`, report)
  }
}

let total = 0
for (const file of jsonFilesUnder(localeRoot)) {
  let parsed
  try {
    parsed = JSON.parse(readFileSync(file, 'utf8'))
  } catch (error) {
    console.log(`${relative(localeRoot, file)}  UNPARSEABLE: ${error.message}`)
    continue
  }
  const report = []
  walk(parsed, '', report)
  for (const { path, found } of report) {
    console.log(`${relative(localeRoot, file)}  ${path}  [${found.join(', ')}]`)
    total += 1
  }
}

console.log(
  total === 0
    ? '\nNo unbalanced list markup found.'
    : `\n${total} locale string(s) with unbalanced list markup (report only; not a failure).`,
)

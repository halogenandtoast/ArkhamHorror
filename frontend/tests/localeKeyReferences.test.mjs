import assert from 'node:assert/strict'
import { readdirSync, readFileSync } from 'node:fs'
import { join, relative, resolve } from 'node:path'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* The backend writes player-facing choice labels as i18n tokens -- `Label
"$cards.label.mysteriesRemain.placeClue" [...]`. The frontend hands the token
straight to vue-i18n (`handleI18n` in src/arkham/i18n.ts), and vue-i18n returns
the key itself when it does not resolve. So a key that exists on only one side
of the fence is not a silent fallback: the raw key is what the player reads.

That is issue #5951 -- Mysteries Remain offered
"cards.label.mysteriesremain.placecluefrombank" as a choice, because the May 2026
i18n migration landed the Haskell literal (`placeClueFromBank`) and the locale
text (`placeClue`) in two commits 71 seconds apart. Six more cards shipped the
same way, from the same commit, and went unnoticed for months.

Resolution is checked against the real merged `en` message object rather than
the raw JSON files, because `src/locales/en.ts` is where namespaces are actually
assembled -- it aliases `label.cards` onto `cards.label`, spreads some files to
the top level and nests others, and stitches each campaign directory together
through its own aggregator. Reimplementing any of that here would invent
failures the app never has. */
const projectRoot = fileURLToPath(new URL('..', import.meta.url))
const library = resolve(projectRoot, '../backend/arkham-api/library/Arkham')

/* Only absolute tokens: a leading `$`, a namespace, and at least one dot. Keys
 * written relative to a `scope`/`campaignI18n` wrapper are resolved on the
 * backend and cannot be checked from the literal alone. A trailing `.` is
 * sentence punctuation, not part of the key. */
const i18nToken = /\$[A-Za-z0-9_]+\.[A-Za-z0-9_.]+/g

function haskellFiles(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const file = join(directory, entry.name)
    return entry.isDirectory() ? haskellFiles(file) : file.endsWith('.hs') ? [file] : []
  })
}

function lineNumber(source, index) {
  return source.slice(0, index).split('\n').length
}

function backendReferences() {
  const references = []

  for (const file of haskellFiles(library)) {
    const source = readFileSync(file, 'utf8')
    for (const match of source.matchAll(i18nToken)) {
      references.push({
        key: match[0].slice(1).replace(/\.+$/, ''),
        at: `${relative(library, file)}:${lineNumber(source, match.index)}`,
      })
    }
  }

  return references
}

async function englishMessages(t) {
  const server = await createServer({
    root: projectRoot,
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return (await server.ssrLoadModule('/src/locales/en.ts')).default
}

function lookup(messages, key) {
  return key
    .split('.')
    .reduce((node, segment) => (node && typeof node === 'object' ? node[segment] : undefined), messages)
}

test('every i18n key the backend references resolves in the en locale', async (t) => {
  const messages = await englishMessages(t)

  /* Guard the guard: if en.ts ever failed to load, or `lookup` started handing
   * back truthy junk, every key below would "resolve" and the test would pass
   * while checking nothing. */
  assert.equal(
    typeof lookup(messages, 'cards.label.mysteriesRemain.placeClue'),
    'string',
    'en.ts did not load, or the cards namespace moved',
  )
  assert.equal(
    lookup(messages, 'cards.label.mysteriesRemain.noSuchKeyExists'),
    undefined,
    'lookup resolved a key that does not exist',
  )

  const unresolved = backendReferences()
    .filter(({ key }) => typeof lookup(messages, key) !== 'string')
    .map(({ key, at }) => `${at} $${key}`)

  assert.deepEqual([...new Set(unresolved)].sort(), [])
})

import assert from 'node:assert/strict'
import { readdirSync, readFileSync } from 'node:fs'
import { join, relative, resolve } from 'node:path'
import test from 'node:test'

const localeRoot = resolve('src/locales')

function jsonFilesUnder(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name)
    return entry.isDirectory() ? jsonFilesUnder(path) : path.endsWith('.json') ? [path] : []
  })
}

test('locale markup uses complete HTML closing tags', () => {
  const malformedTags = []
  const incompleteClosingTag = /(?<!<)\/(p|div|ul|ol|li|span|b|i|strong|em|blockquote)>/g

  for (const file of jsonFilesUnder(localeRoot)) {
    const contents = readFileSync(file, 'utf8')
    for (const match of contents.matchAll(incompleteClosingTag)) {
      malformedTags.push(`${relative(localeRoot, file)}:${match.index} /${match[1]}>`)
    }
  }

  assert.deepEqual(malformedTags, [])
})

/* A translator localized the key path inside a `@:` link, not just the text
 * around it: `investigatorSetup.` became `investigator设置。`. vue-i18n still
 * resolves the outer message, so `fallbackLocale` never fires -- the link
 * resolves to '' and the modal renders empty, with nothing in the console. */
test('locale message links keep ASCII key paths', () => {
  const localizedLinks = []
  const nonAsciiLinkKey = /@:[A-Za-z0-9_.]*[^\x20-\x7E]/g

  for (const file of jsonFilesUnder(localeRoot)) {
    const contents = readFileSync(file, 'utf8')
    for (const match of contents.matchAll(nonAsciiLinkKey)) {
      localizedLinks.push(`${relative(localeRoot, file)}:${match.index} ${match[0]}`)
    }
  }

  assert.deepEqual(localizedLinks, [])
})

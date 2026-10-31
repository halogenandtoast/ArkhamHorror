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

/* A PDF-extraction artefact: the FFG bullet glyph survives text extraction as a
 * standalone "Æ", and the campaign-log entry that should have opened the <ul>
 * ends up stranded inside the preceding <p> next to it (issue #5939, The
 * Devourer Below resolution 1). Matched only when whitespace-delimited, so a
 * legitimate ligature inside a word is never flagged. */
test('locale markup has no stray bullet-glyph artifacts', () => {
  const artifacts = []
  const strandedGlyph = /(?<=\s)Æ(?=\s)/g

  for (const file of jsonFilesUnder(localeRoot)) {
    const contents = readFileSync(file, 'utf8')
    for (const match of contents.matchAll(strandedGlyph)) {
      artifacts.push(`${relative(localeRoot, file)}:${match.index}`)
    }
  }

  assert.deepEqual(artifacts, [])
})

/* The other half of #5939: a <ul> whose first child is bare text, either because
 * the opening <li> is missing or because a </ul> was typed as <ul>. The entry
 * then renders outside its bullet, which is what the player sees. */
test('locale markup opens every list with a list item', () => {
  const strayText = []
  const listOpeningWithText = /<ul>(?!\s*(?:<li|<ul))/g

  for (const file of jsonFilesUnder(localeRoot)) {
    const contents = readFileSync(file, 'utf8')
    for (const match of contents.matchAll(listOpeningWithText)) {
      strayText.push(`${relative(localeRoot, file)}:${match.index}`)
    }
  }

  assert.deepEqual(strayText, [])
})

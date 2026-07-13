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

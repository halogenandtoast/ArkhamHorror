import assert from 'node:assert/strict'
import test from 'node:test'
import fs from 'node:fs'
import { fileURLToPath, URL } from 'node:url'

/* The English modifier glossary against the engine's own constructor list.
 *
 * Every modifier the UI cannot draw as a glyph and a number falls back to
 * `modifierTag.<Constructor>`. A miss renders the raw Haskell name on screen,
 * which is how `en` came to carry none of these at all while `ko` carried 432.
 * Nothing else catches that, because a missing key is not a type error and the
 * fallback "works".
 */

const root = fileURLToPath(new URL('..', import.meta.url))
const MODIFIER_HS = fileURLToPath(
  new URL('../../backend/arkham-api/library/Arkham/Modifier.hs', import.meta.url),
)
const TRAIT_HS = fileURLToPath(
  new URL('../../backend/arkham-api/library/Arkham/Trait.hs', import.meta.url),
)

/* Every constructor of `data ModifierType`. A constructor does not always
 * follow its own `|` -- a Haddock can sit between them -- so comments come out
 * before the alternatives are split. */
function engineConstructors(source) {
  const start = source.indexOf('data ModifierType')
  assert.notEqual(start, -1, 'ModifierType not found in Modifier.hs')
  const end = source.indexOf('\n  deriving', start)
  let block = source.slice(start, end)

  block = block.replace(/\{-[\s\S]*?-\}/g, ' ')
  block = block.replace(/--[^\n]*/g, ' ')
  block = block.slice(block.indexOf('\n'))
  block = block.replace(/\{[^{}]*\}/g, ' ')

  const names = new Set()
  for (const m of block.matchAll(/[|=]\s*([A-Z][A-Za-z0-9_']*)/g)) names.add(m[1])
  return names
}

test('en has a label for every ModifierType constructor', (t) => {
  if (!fs.existsSync(MODIFIER_HS)) {
    t.skip('backend checkout not present')
    return
  }

  const constructors = engineConstructors(fs.readFileSync(MODIFIER_HS, 'utf8'))
  assert.ok(constructors.size > 400, `expected the full constructor list, got ${constructors.size}`)

  const { modifierTag } = JSON.parse(
    fs.readFileSync(new URL('./src/locales/en/modifiers.json', `file://${root}`), 'utf8'),
  )

  const missing = [...constructors].filter((c) => !(c in modifierTag)).sort()
  assert.deepEqual(
    missing,
    [],
    `modifierTag is missing ${missing.length} constructor(s); add them to src/locales/en/modifiers.json`,
  )

  const blank = Object.entries(modifierTag).filter(([, v]) => typeof v !== 'string' || !v.trim())
  assert.deepEqual(blank, [], 'every modifierTag entry needs a non-empty label')
})

test('en labels the modifier groups the panel can emit', () => {
  const messages = JSON.parse(
    fs.readFileSync(new URL('./src/locales/en/modifiers.json', `file://${root}`), 'utf8'),
  )

  /* Kept in step with MODIFIER_GROUP_ORDER in src/arkham/modifierDisplay.ts. */
  const groups = [
    'stats',
    'skills',
    'combat',
    'chaos',
    'actions',
    'economy',
    'cards',
    'movement',
    'keywords',
    'permissions',
    'other',
  ]

  for (const group of groups) {
    assert.ok(messages.modifierGroup?.[group], `modifierGroup.${group} is missing`)
  }

  for (const key of ['none', 'count', 'countPlural', 'more', 'open', 'source']) {
    assert.ok(messages.modifierPanel?.[key], `modifierPanel.${key} is missing`)
  }
})

/* The trait glossary is generated from the engine's `Arkham.Trait`, so it can
 * silently fall behind when a trait is added there. The wire value is the bare
 * constructor, so a missing key renders "AncientOne" rather than failing. */
test('en has a label for every core trait the engine can send', (t) => {
  if (!fs.existsSync(TRAIT_HS)) {
    t.skip('backend checkout not present')
    return
  }

  const hs = fs.readFileSync(TRAIT_HS, 'utf8')
  const block = hs.slice(hs.indexOf('data Trait'), hs.indexOf('deriving stock (Show, Eq, Generic'))
  const ctors = [...block.matchAll(/^\s*[=|]\s*([A-Z][A-Za-z0-9_]*)/gm)]
    .map((m) => m[1])
    .filter((n) => n !== 'HomebrewTrait')

  assert.ok(ctors.length > 300, `only found ${ctors.length} traits -- parser drifted`)

  const { trait } = JSON.parse(
    fs.readFileSync(new URL('./src/locales/en/traits.json', `file://${root}`), 'utf8'),
  )
  const missing = ctors.filter((c) => !trait?.[c])
  assert.deepEqual(missing, [], `traits missing from en: ${missing.slice(0, 10).join(', ')}`)

  // A multi-word trait must come out spaced, or the formatting bug is back.
  assert.equal(trait.AncientOne, 'Ancient One')
})

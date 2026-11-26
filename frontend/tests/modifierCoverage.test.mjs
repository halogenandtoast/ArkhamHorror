import assert from 'node:assert/strict'
import test from 'node:test'
import fs from 'node:fs'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* Every modifier the engine can emit, through the real rendering path.
 *
 * Spot-checking a card or two proves nothing about 471 constructors. This runs
 * the whole list and asserts the three things that would make a row useless on
 * screen:
 *
 *   1. it draws no glyph at all,
 *   2. it says nothing -- no number AND no words, the blank-pill bug the old
 *      renderer had for 35 decoded tags,
 *   3. it says `CannotLeaveYourHand` instead of "Cannot leave your hand".
 *
 * Point 3 is why the glossary exists; without this test a constructor added to
 * the engine reaches players as raw Haskell and nothing fails.
 */

const MODIFIER_HS = fileURLToPath(
  new URL('../../backend/arkham-api/library/Arkham/Modifier.hs', import.meta.url),
)

function engineConstructors(source) {
  const start = source.indexOf('data ModifierType')
  const end = source.indexOf('\n  deriving', start)
  let block = source.slice(start, end)
  block = block.replace(/\{-[\s\S]*?-\}/g, ' ')
  block = block.replace(/--[^\n]*/g, ' ')
  block = block.slice(block.indexOf('\n'))
  block = block.replace(/\{[^{}]*\}/g, ' ')
  const names = new Set()
  for (const m of block.matchAll(/[|=]\s*([A-Z][A-Za-z0-9_']*)/g)) names.add(m[1])
  return [...names]
}

async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/modifierDisplay.ts')
}

/* The real `en` messages, resolved the way vue-i18n would. */
function enGlossary() {
  const messages = JSON.parse(
    fs.readFileSync(new URL('../src/locales/en/modifiers.json', import.meta.url), 'utf8'),
  )
  const te = (key) => {
    const [ns, tag] = key.split('.')
    return Boolean(messages[ns] && messages[ns][tag])
  }
  const t = (key) => {
    const [ns, tag] = key.split('.')
    return messages[ns]?.[tag] ?? key
  }
  return { te, t }
}

/* An unsplit multi-word constructor: "CannotLeaveYourHand", not "Blocked" and
 * not "XP modifier". A lowercase letter butted against a capital is the
 * hallmark of camel case that was never split; a leading acronym is not. */
const UNSPLIT_CAMEL = /[a-z][A-Z]/

test('every engine modifier renders as something a player can read', async (t) => {
  if (!fs.existsSync(MODIFIER_HS)) {
    t.skip('backend checkout not present')
    return
  }

  const { describeModifier, modifierLabel } = await load(t)
  const { te, t: translate } = enGlossary()
  const constructors = engineConstructors(fs.readFileSync(MODIFIER_HS, 'utf8'))
  assert.ok(constructors.length > 400, `expected the full list, got ${constructors.length}`)

  const noGlyph = []
  const silent = []
  const rawName = []

  for (const tag of constructors) {
    // How a nullary modifier actually arrives: the decoder's catch-all keeps
    // the constructor name and drops any payload.
    const display = describeModifier({ type: { tag: 'OtherModifier', contents: tag } })
    const label = modifierLabel(display.tag, te, translate)

    if (!display.glyph || !display.glyph.kind) noGlyph.push(tag)

    const hasNumber = display.op !== 'none' && display.value !== undefined
    if (!hasNumber && !label.trim()) silent.push(tag)

    // A label still carrying an internal capital was never split into words.
    if (UNSPLIT_CAMEL.test(label)) rawName.push(`${tag} -> ${label}`)
  }

  assert.deepEqual(noGlyph, [], 'these modifiers would draw nothing')
  assert.deepEqual(silent, [], 'these modifiers would render as an empty pill')
  assert.deepEqual(rawName, [], 'these modifiers would show a raw Haskell constructor')
})

test('every modifier lands in a real group, and the common ones need no words', async (t) => {
  if (!fs.existsSync(MODIFIER_HS)) {
    t.skip('backend checkout not present')
    return
  }

  const { describeModifier, MODIFIER_GROUP_ORDER } = await load(t)
  const constructors = engineConstructors(fs.readFileSync(MODIFIER_HS, 'utf8'))
  const groups = new Set(MODIFIER_GROUP_ORDER)

  const counts = {}
  for (const tag of constructors) {
    const d = describeModifier({ type: { tag: 'OtherModifier', contents: tag } })
    assert.ok(groups.has(d.group), `${tag} landed in unknown group ${d.group}`)
    counts[d.group] = (counts[d.group] ?? 0) + 1
  }

  // Nothing should be a dumping ground: if most modifiers fall to 'other' the
  // name-shape rules in `inferred` have stopped matching the engine.
  const other = counts.other ?? 0
  assert.ok(
    other < constructors.length / 2,
    `${other} of ${constructors.length} modifiers fell through to 'other'`,
  )
})

test('the modifiers carrying a payload are drawn with it, not described', async (t) => {
  if (!fs.existsSync(MODIFIER_HS)) {
    t.skip('backend checkout not present')
    return
  }

  const { describeModifier } = await load(t)
  const decoder = fs.readFileSync(
    new URL('../src/arkham/types/Modifier.ts', import.meta.url),
    'utf8',
  )

  /* Everything the decoder keeps a number for must draw that number rather
   * than fall back to a sentence -- that is the whole language-agnostic
   * claim. Read the tables straight out of the decoder so the two cannot
   * drift apart. */
  const numericTags = [
    ...decoder
      .slice(decoder.indexOf('NUMERIC_MODIFIER_TAGS = ['), decoder.indexOf('] as const', decoder.indexOf('NUMERIC_MODIFIER_TAGS = [')))
      .matchAll(/'([A-Za-z]+)'/g),
  ].map((m) => m[1])

  assert.ok(numericTags.length > 5, 'could not read NUMERIC_MODIFIER_TAGS')

  for (const tag of numericTags) {
    const d = describeModifier({ type: { tag, contents: 2 } })
    // Magnitude, not sign: `FewerActions 2` is correctly drawn as -2, because
    // the constructor names a reduction.
    assert.equal(Math.abs(d.value), 2, `${tag} dropped its number`)
    assert.notEqual(d.op, 'none', `${tag} drew no operator`)
  }

  /* The ones a glyph and a number fully describe must ask for no words at
   * all -- these are the rows that read the same in every language. (A
   * modifier can carry a number and still need one, e.g. "+2 targets".) */
  const wordless = [
    { tag: 'EnemyFight', contents: 2 },
    { tag: 'EnemyEvade', contents: 2 },
    { tag: 'HealthModifier', contents: 2 },
    { tag: 'SanityModifier', contents: 2 },
    { tag: 'ShroudModifier', contents: 2 },
    { tag: 'DamageDealt', contents: 1 },
    { tag: 'HorrorDealt', contents: 1 },
    { tag: 'HandSize', contents: -2 },
    { tag: 'AdditionalResources', contents: 2 },
    { tag: 'SkillModifier', skillType: 'SkillCombat', value: 1 },
    { tag: 'SetSkillValue', skillType: 'SkillAgility', value: 4 },
    { tag: 'AddKeyword', contents: 'Retaliate' },
  ]
  for (const type of wordless) {
    const d = describeModifier({ type })
    assert.equal(d.needsLabel, false, `${type.tag} should need no words`)
  }
})

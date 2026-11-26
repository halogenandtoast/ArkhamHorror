import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* What a modifier turns into on screen.
 *
 * The rule the whole design rests on: a modifier that a glyph and a number can
 * describe must NOT ask for words (`needsLabel: false`), because those rows are
 * the ones that read the same in every language. Everything else must ask for
 * them, so it gets a translated sentence rather than a bare icon.
 *
 * The second rule is that a number-shaped modifier whose number never arrived
 * -- its constructor is missing from the decoder tables in types/Modifier.ts --
 * must fall back to words instead of drawing a glyph with nothing beside it.
 */
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

const mod = (type) => ({ type })

test('a modifier with a glyph and a number needs no words', async (t) => {
  const { describeModifier } = await load(t)

  const cases = [
    [{ tag: 'HealthModifier', contents: 2 }, { group: 'stats', value: 2, op: 'delta' }],
    [{ tag: 'EnemyFight', contents: 2 }, { group: 'stats', value: 2, op: 'delta' }],
    [{ tag: 'EnemyEvade', contents: -1 }, { group: 'stats', value: -1, op: 'delta' }],
    [{ tag: 'ShroudModifier', contents: 1 }, { group: 'stats', value: 1, op: 'delta' }],
    [{ tag: 'SkillModifier', skillType: 'SkillCombat', value: 1 }, { group: 'skills', value: 1, op: 'delta' }],
    [{ tag: 'SetSkillValue', skillType: 'SkillAgility', value: 4 }, { group: 'skills', value: 4, op: 'set' }],
    [{ tag: 'DamageDealt', contents: 1 }, { group: 'combat', value: 1, op: 'delta' }],
    [{ tag: 'HandSize', contents: -3 }, { group: 'cards', value: -3, op: 'delta' }],
    [{ tag: 'DoubleDifficulty' }, { group: 'combat', value: 2, op: 'scale' }],
  ]

  for (const [type, expected] of cases) {
    const d = describeModifier(mod(type))
    assert.equal(d.needsLabel, false, `${type.tag} should be drawable without words`)
    assert.equal(d.group, expected.group, `${type.tag} group`)
    assert.equal(d.op, expected.op, `${type.tag} op`)
    assert.equal(d.value, expected.value, `${type.tag} value`)
  }
})

test('the `Int (Min Int)` pair shows the change, not the floor', async (t) => {
  const { describeModifier } = await load(t)

  const d = describeModifier(mod({ tag: 'HealthModifierWithMin', contents: [-1, 1] }))
  assert.equal(d.value, -1)
  assert.equal(d.group, 'stats')
  assert.equal(d.needsLabel, false)
})

test('a number-shaped modifier with no number falls back to words', async (t) => {
  const { describeModifier } = await load(t)

  // AdditionalActions is `Text Source Int`; with no decoder for it the payload
  // is dropped and only the tag survives.
  const d = describeModifier(mod({ tag: 'OtherModifier', contents: 'AdditionalActions' }))
  assert.equal(d.op, 'none', 'must not draw an operator it has no number for')
  assert.equal(d.value, undefined)
  assert.equal(d.needsLabel, true)
})

test('a nullary modifier is read through the OtherModifier catch-all', async (t) => {
  const { describeModifier, modifierTag } = await load(t)

  const d = describeModifier(mod({ tag: 'OtherModifier', contents: 'CannotBeDamaged' }))
  assert.equal(modifierTag(mod({ tag: 'OtherModifier', contents: 'CannotBeDamaged' })), 'CannotBeDamaged')
  assert.equal(d.group, 'permissions')
  assert.equal(d.polarity, 'ban')
  assert.equal(d.needsLabel, false, 'a crossed-out damage token says this on its own')
})

test('an unknown constructor is still grouped and still asks for words', async (t) => {
  const { describeModifier } = await load(t)

  const banned = describeModifier(mod({ tag: 'OtherModifier', contents: 'CannotSomethingBrandNew' }))
  assert.equal(banned.group, 'permissions')
  assert.equal(banned.polarity, 'ban')
  assert.equal(banned.needsLabel, true)

  const unknown = describeModifier(mod({ tag: 'OtherModifier', contents: 'ZzzUnrecognised' }))
  assert.equal(unknown.group, 'other')
  assert.equal(unknown.needsLabel, true)
  assert.ok(unknown.glyph, 'every row draws something')
})

test('sections come out in a fixed order, stats first', async (t) => {
  const { groupModifierRows, modifierRows } = await load(t)

  const game = { enemies: {}, assets: {}, locations: {}, investigators: {} }
  const modifiers = [
    { type: { tag: 'OtherModifier', contents: 'CannotLeaveYourHand' }, source: { tag: 'TestSource' } },
    { type: { tag: 'SkillModifier', skillType: 'SkillCombat', value: 1 }, source: { tag: 'TestSource' } },
    { type: { tag: 'HealthModifier', contents: 2 }, source: { tag: 'TestSource' } },
  ]

  const sections = groupModifierRows(modifierRows(modifiers, game))
  assert.deepEqual(sections.map((s) => s.group), ['stats', 'skills', 'permissions'])
})

test("a card's own printed text is not restated back at the player", async (t) => {
  const { presentableModifiers } = await load(t)

  const kept = presentableModifiers([
    // `modifySelf a [CannotLeaveYourHand]` -- the card says this; a row for it
    // adds nothing, least of all beside the magnified card text.
    { type: { tag: 'OtherModifier', contents: 'CannotLeaveYourHand' }, printed: true },
    // Same shape, but a computed value: "shroud +1 per Witch here" is the
    // printed rule, "+3" is not.
    { type: { tag: 'ShroudModifier', contents: 3 }, printed: true },
    // Conditional self-modifier: its presence says the condition is met now.
    { type: { tag: 'OtherModifier', contents: 'CannotBeEvaded' }, printed: false },
    // Someone else's doing -- always worth showing.
    { type: { tag: 'OtherModifier', contents: 'CannotLeaveYourHand' } },
  ])

  assert.deepEqual(
    kept.map((m) => `${m.type.contents}${m.printed === true ? ' (printed)' : ''}`),
    ['3 (printed)', 'CannotBeEvaded', 'CannotLeaveYourHand'],
  )
})

test('a server that does not send `printed` shows everything, as before', async (t) => {
  const { presentableModifiers } = await load(t)

  const kept = presentableModifiers([
    { type: { tag: 'OtherModifier', contents: 'CannotLeaveYourHand' } },
    { type: { tag: 'OtherModifier', contents: 'CannotBeDamaged' } },
  ])
  assert.equal(kept.length, 2)
})

test('a UI-only modifier is not listed; it is already on screen as itself', async (t) => {
  const { presentableModifiers } = await load(t)

  const kept = presentableModifiers([
    { type: { tag: 'UIModifier', contents: 'Oversized' } },
    { type: { tag: 'HealthModifier', contents: 2 } },
  ])
  assert.equal(kept.length, 1)
  assert.equal(kept[0].type.tag, 'HealthModifier')
})

test('the pip wears the most urgent polarity in the list', async (t) => {
  const { dominantPolarity, describeModifier } = await load(t)

  const rows = [
    { display: describeModifier(mod({ tag: 'SkillModifier', skillType: 'SkillCombat', value: 1 })) },
    { display: describeModifier(mod({ tag: 'OtherModifier', contents: 'CannotBeDamaged' })) },
  ]
  assert.equal(dominantPolarity(rows), 'ban')
})

test('a stat reading pairs the printed number with the one in force', async (t) => {
  const { enemyStatReadings, hasAlteredStat } = await load(t)

  const readings = enemyStatReadings({
    printedFight: 1, modifiedFight: 3,
    printedHealth: 1, modifiedHealth: 3,
    printedEvade: 3, modifiedEvade: 3,
  })

  // Evade is unchanged, so it gets no line at all: this is a modifier panel,
  // and "Evade 3" against a card printing 3 is a restatement of the card.
  assert.deepEqual(
    readings.map(({ key, printed, modified }) => ({ key, printed, modified })),
    [
      { key: 'fight', printed: 1, modified: 3 },
      { key: 'health', printed: 1, modified: 3 },
    ],
  )
  // Each reading carries its own glyph and glossary key, so one renderer draws
  // enemy stats, investigator skills and location shroud alike.
  assert.deepEqual(readings.map((r) => r.glyph), [
    { kind: 'stat', stat: 'fight' },
    { kind: 'stat', stat: 'health' },
  ])
  assert.equal(hasAlteredStat(readings), true)
  assert.equal(
    hasAlteredStat(enemyStatReadings({ printedEvade: 3, modifiedEvade: 3 })),
    false,
    'an unchanged stat produces no reading, so there is nothing altered',
  )
})

test('a stat the card prints as a dash is not drawn as a number', async (t) => {
  const { enemyStatReadings } = await load(t)

  // The server resolves ValueStar/ValueX to 0; only the client knows it was a
  // dash, from the calculation tag it still receives.
  const readings = enemyStatReadings(
    { printedFight: 0, modifiedFight: 0, printedHealth: 1, modifiedHealth: 3 },
    { fight: true },
  )
  assert.deepEqual(readings.map((r) => r.key), ['health'])
})

test('a stat is withheld entirely rather than half-reported', async (t) => {
  const { enemyStatReadings, locationShroudReading } = await load(t)

  // A server older than these fields, or an enemy whose fight was suppressed
  // because the active investigator was missing.
  assert.deepEqual(enemyStatReadings({ printedFight: 1, modifiedFight: null }), [])
  assert.deepEqual(enemyStatReadings({}), [])
  assert.deepEqual(locationShroudReading({ printedShroud: null, modifiedShroud: null }), [])
})

test('a composite enemy\'s bookkeeping never reaches the panel', async (t) => {
  const { presentableModifiers } = await load(t)

  // Cthulhu puts these on each facet so the four act as one enemy; the facet's
  // own card says nothing about them. They go whether or not they carry a
  // number, and whether or not they are also flagged printed.
  const facet = presentableModifiers([
    { ...mod({ tag: 'OtherModifier', contents: 'CannotAttackDuringEnemyPhase' }), subsumed: true },
    { ...mod({ tag: 'AddKeyword', contents: 'Massive' }), subsumed: true },
    { ...mod({ tag: 'AddTrait', contents: 'AncientOne' }), subsumed: true },
    // Something genuinely put on the facet survives.
    mod({ tag: 'EnemyFight', contents: 2 }),
  ])
  assert.deepEqual(facet.map((m) => m.type.tag ?? m.type), ['EnemyFight'])

  // And the other direction: Cthulhu itself cannot be attacked only because
  // you attack a facet instead.
  assert.deepEqual(
    presentableModifiers([
      { ...mod({ tag: 'OtherModifier', contents: 'CannotBeAttacked' }), subsumed: true },
    ]),
    [],
  )

  // A server older than the field sends nothing, and nothing is hidden.
  assert.equal(
    presentableModifiers([mod({ tag: 'OtherModifier', contents: 'CannotBeAttacked' })]).length,
    1,
  )
})

test('an unmodified stat produces no reading at all', async (t) => {
  const { enemyStatReadings, locationShroudReading, investigatorSkillReadings } = await load(t)

  assert.deepEqual(
    locationShroudReading({ printedShroud: 4, modifiedShroud: 4 }),
    [],
    'an unmodified shroud has nothing to say in a modifier panel',
  )
  assert.deepEqual(
    enemyStatReadings({
      printedFight: 2, modifiedFight: 2,
      printedHealth: 3, modifiedHealth: 3,
      printedEvade: 1, modifiedEvade: 1,
    }),
    [],
  )
  assert.deepEqual(
    investigatorSkillReadings({
      willpower: 3, intellect: 3, combat: 3, agility: 3,
      modifiedWillpower: 3, modifiedIntellect: 3, modifiedCombat: 3, modifiedAgility: 3,
    }),
    [],
  )
})

test('investigator skills and location shroud use the same reading shape', async (t) => {
  const { investigatorSkillReadings, locationShroudReading } = await load(t)

  const skills = investigatorSkillReadings({
    willpower: 3, intellect: 4, combat: 1, agility: 5,
    modifiedWillpower: 3, modifiedIntellect: 5, modifiedCombat: 1, modifiedAgility: 6,
  })
  // Only the two that moved; willpower and combat are unchanged.
  assert.deepEqual(skills.map((r) => [r.key, r.printed, r.modified]), [
    ['intellect', 4, 5], ['agility', 5, 6],
  ])
  assert.deepEqual(skills[0].glyph, { kind: 'skill', skill: 'intellect' })

  const shroud = locationShroudReading({ printedShroud: 2, modifiedShroud: 4 })
  assert.deepEqual(shroud.map((r) => [r.key, r.printed, r.modified]), [['shroud', 2, 4]])
  assert.deepEqual(shroud[0].glyph, { kind: 'stat', stat: 'shroud' })
})

test('every entity with a modifiable printed value reads out the same way', async (t) => {
  const {
    enemyStatReadings,
    investigatorSkillReadings,
    locationShroudReading,
    assetStatReadings,
    agendaThresholdReading,
  } = await load(t)

  // One shape across the board, so one renderer draws them all.
  const sets = [
    enemyStatReadings({ printedHealth: 1, modifiedHealth: 3 }),
    investigatorSkillReadings({
      willpower: 3, intellect: 3, combat: 3, agility: 3,
      modifiedWillpower: 4, modifiedIntellect: 3, modifiedCombat: 3, modifiedAgility: 3,
    }),
    locationShroudReading({ printedShroud: 2, modifiedShroud: 4 }),
    assetStatReadings({ printedHealth: 3, modifiedHealth: 4, printedSanity: 2, modifiedSanity: 2 }),
    agendaThresholdReading({ printedDoomThreshold: 6, modifiedDoomThreshold: 5 }),
  ]

  for (const readings of sets) {
    assert.ok(readings.length > 0)
    for (const r of readings) {
      assert.equal(typeof r.key, 'string')
      assert.equal(typeof r.labelKey, 'string')
      assert.equal(typeof r.printed, 'number')
      assert.equal(typeof r.modified, 'number')
      assert.ok(r.glyph && typeof r.glyph.kind === 'string', 'every reading draws something')
    }
  }

  assert.deepEqual(agendaThresholdReading({ printedDoomThreshold: 6, modifiedDoomThreshold: 5 })[0].glyph, {
    kind: 'token',
    token: 'doom',
  })
  assert.deepEqual(assetStatReadings({ printedSanity: 2, modifiedSanity: 3 }).map((r) => r.key), ['sanity'])
})

test('a server that sends no resolved skills reports no skill line', async (t) => {
  const { investigatorSkillReadings } = await load(t)

  // Caught in the browser: failing over to 0 made every skill read "0" with a
  // negative drift against an older server. Absent must mean absent.
  assert.deepEqual(
    investigatorSkillReadings({
      willpower: 3, intellect: 3, combat: 4, agility: 2,
      modifiedWillpower: null, modifiedIntellect: null, modifiedCombat: null, modifiedAgility: null,
    }),
    [],
  )
})

test('the wire form survives a round trip through a data attribute', async (t) => {
  const { toWireModifiers, wireModifierRows } = await load(t)

  const game = { enemies: {}, assets: {}, locations: {}, investigators: {} }
  const wire = toWireModifiers(
    [{ type: { tag: 'EnemyFight', contents: 2 }, source: { tag: 'TestSource' } }],
    game,
  )

  const rows = wireModifierRows(JSON.parse(JSON.stringify(wire)))
  assert.equal(rows.length, 1)
  assert.equal(rows[0].display.tag, 'EnemyFight')
  assert.equal(rows[0].display.value, 2)
})

test('a preview that cannot scroll cuts the list and counts what it dropped', async (t) => {
  const { groupModifierRows, limitModifierSections, wireModifierRows } = await load(t)

  const many = wireModifierRows([
    { type: { tag: 'EnemyFight', contents: 1 } },
    { type: { tag: 'EnemyEvade', contents: 1 } },
    { type: { tag: 'HealthModifier', contents: 2 } },
    { type: { tag: 'CannotAttack' } },
    { type: { tag: 'CannotBeEvaded' } },
  ])
  const sections = groupModifierRows(many)
  const all = sections.reduce((n, s) => n + s.rows.length, 0)
  assert.equal(all, 5, 'every row should land in some group')

  // No limit, a limit nothing reaches, and a nonsense one all mean "draw it all".
  for (const limit of [undefined, 99, 0, -1]) {
    const kept = limitModifierSections(sections, limit)
    assert.equal(kept.hidden, 0, `limit ${limit} should hide nothing`)
    assert.equal(kept.sections, sections, `limit ${limit} should not copy the list`)
  }

  const cut = limitModifierSections(sections, 2)
  assert.equal(cut.hidden, 3)
  assert.equal(cut.sections.reduce((n, s) => n + s.rows.length, 0), 2)
  // Taken off the front, in the order the groups already put them: the rows a
  // player most needs survive the cut.
  assert.deepEqual(
    cut.sections.flatMap((s) => s.rows).map((r) => r.display.tag),
    sections.flatMap((s) => s.rows).slice(0, 2).map((r) => r.display.tag),
  )
  // A group the cut lands inside keeps its heading and its first rows, rather
  // than being dropped whole.
  assert.ok(cut.sections.every((s) => s.rows.length > 0), 'no empty section survives')
})

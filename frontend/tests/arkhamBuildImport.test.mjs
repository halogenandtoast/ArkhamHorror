import assert from 'node:assert/strict'
import test from 'node:test'
import { fileURLToPath, URL } from 'node:url'

import { createServer } from 'vite'

/* Importing an arkham.build card pool and later importing a DECK built on
arkham.build against those same cards have to agree on one thing: the card
code. arkham.build names a card by a bare UUID; this app only ever recognizes
a custom card by a `*`-prefixed code (isCustomCardCode), both in the frontend
lookups and in the backend's deck validation. So the importer derives a code
from the UUID, and the deck normalizer derives it again from the same UUID --
and if those two derivations ever drift apart, a deck silently loses every
custom card in it.

That agreement is what these tests pin. The field mapping is checked alongside
it because the same module owns both. */
async function load(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/arkhamBuildImport.ts')
}

async function loadCustomCards(t) {
  const server = await createServer({
    root: fileURLToPath(new URL('..', import.meta.url)),
    appType: 'custom',
    logLevel: 'silent',
    server: { middlewareMode: true, hmr: false },
  })
  t.after(() => server.close())
  return server.ssrLoadModule('/src/arkham/customCards.ts')
}

const PACK = 'Return to the Zealot'

const investigator = {
  code: 'd545b38e-f3cf-486a-a026-12ed94b33453',
  position: 1,
  name: 'Roland Banks',
  subname: 'The Fed',
  type_code: 'investigator',
  faction_code: 'guardian',
  skill_willpower: 3,
  skill_intellect: 3,
  skill_combat: 4,
  skill_agility: 2,
  health: 9,
  sanity: 5,
  is_unique: true,
  traits: 'Agency. Detective.',
  image_url: 'https://example.test/roland-front.jpg',
  back_image_url: 'https://example.test/roland-back.jpg',
}

const asset = {
  code: '43433775-620b-4e10-8583-253eb3f3e6c2',
  position: 2,
  name: "Roland's .38 Special",
  type_code: 'asset',
  faction_code: 'neutral',
  cost: 3,
  xp: 0,
  skill_willpower: 1,
  skill_combat: 1,
  skill_wild: 1,
  slot: 'Hand',
  traits: 'Item. Weapon. Firearm.',
  is_unique: true,
  image_url: 'https://example.test/38-special.jpg',
}

const weaknessTreachery = {
  code: '3497e84b-9698-4d54-937c-65096eb6c82c',
  position: 3,
  name: 'Cover Up',
  type_code: 'treachery',
  faction_code: 'neutral',
  subtype_code: 'weakness',
  traits: 'Task.',
}

const weaknessEnemy = {
  code: '4ac58d97-9521-4609-bcc0-a1d88d494289',
  position: 124,
  name: 'Issith',
  type_code: 'enemy',
  faction_code: 'neutral',
  subtype_code: 'weakness',
  enemy_fight: 3,
  health: 3,
  enemy_evade: 2,
  enemy_damage: 1,
  enemy_horror: 1,
  traits: 'Humanoid. Monster. Serpent.',
}

const poolFile = JSON.stringify({
  meta: { name: PACK, code: 'cae9cd37-4621-4e52-958c-40a250f730b6' },
  data: { cards: [investigator, asset, weaknessTreachery, weaknessEnemy] },
})

test('a card pool export yields its pack name and every card', async (t) => {
  const { parseArkhamBuildCards } = await load(t)

  const { packName, cards } = parseArkhamBuildCards(poolFile)
  assert.equal(packName, PACK)
  assert.equal(cards.length, 4)
})

test('a pack carries its own id, so a renamed pack still replaces its set', async (t) => {
  const { parseArkhamBuildCards } = await load(t)

  const { packCode } = parseArkhamBuildCards(poolFile)
  assert.equal(packCode, 'cae9cd37-4621-4e52-958c-40a250f730b6')

  // Same pack, renamed: the id is what says it is the same set.
  const renamed = JSON.stringify({
    meta: { name: 'Return to the Zealot (revised)', code: 'cae9cd37-4621-4e52-958c-40a250f730b6' },
    data: { cards: [asset] },
  })
  assert.equal(parseArkhamBuildCards(renamed).packCode, packCode)

  // A file with no pack id at all says so rather than inventing one.
  assert.equal(parseArkhamBuildCards(JSON.stringify({ cards: [asset] })).packCode, null)
})

test('a bare card array and a lone card both parse', async (t) => {
  const { parseArkhamBuildCards } = await load(t)

  assert.equal(parseArkhamBuildCards(JSON.stringify({ cards: [asset] })).cards.length, 1)
  assert.equal(parseArkhamBuildCards(JSON.stringify(asset)).cards.length, 1)
  // Something that is not a card export at all yields nothing rather than a card.
  assert.equal(parseArkhamBuildCards(JSON.stringify({ nope: true })).cards.length, 0)
})

test('an investigator keeps its stats, class and both faces', async (t) => {
  const { arkhamBuildCardToCustomCard } = await load(t)

  const { def, art } = arkhamBuildCardToCustomCard(investigator, PACK)
  assert.equal(def.cardType, 'InvestigatorType')
  assert.deepEqual(def.name, { title: 'Roland Banks', subtitle: 'The Fed' })
  assert.deepEqual(def.classSymbols, ['Guardian'])
  assert.equal(def.unique, true)
  assert.deepEqual(def.cardTraits, ['Agency', 'Detective'])
  // On an investigator `skill_*` is a printed stat, not a count of icons.
  assert.deepEqual(def.skills, [])
  assert.equal(def.meta.willpower, 3)
  assert.equal(def.meta.combat, 4)
  assert.equal(def.meta.health, 9)
  assert.equal(def.meta.sanity, 5)
  assert.equal(art, investigator.image_url)
  assert.equal(def.meta.backArt, investigator.back_image_url)
})

test('a player asset keeps cost, icons and slot', async (t) => {
  const { arkhamBuildCardToCustomCard } = await load(t)

  const { def } = arkhamBuildCardToCustomCard(asset, PACK)
  assert.equal(def.cardType, 'AssetType')
  assert.deepEqual(def.cost, { tag: 'StaticCost', contents: 3 })
  assert.deepEqual(def.slots, ['HandSlot'])
  // Here `skill_*` IS a count of printed icons -- one pip each, wild last.
  assert.deepEqual(def.skills, [
    { tag: 'SkillIcon', contents: 'SkillWillpower' },
    { tag: 'SkillIcon', contents: 'SkillCombat' },
    { tag: 'WildIcon', contents: [] },
  ])
})

test('a weakness lands on the player-side type for its kind', async (t) => {
  const { arkhamBuildCardToCustomCard } = await load(t)

  const treachery = arkhamBuildCardToCustomCard(weaknessTreachery, PACK).def
  assert.equal(treachery.cardType, 'PlayerTreacheryType')
  assert.equal(treachery.cardSubType, 'Weakness')
  // A weakness has no class of its own, so none is claimed for it.
  assert.equal(treachery.classSymbols, undefined)

  const enemy = arkhamBuildCardToCustomCard(weaknessEnemy, PACK).def
  assert.equal(enemy.cardType, 'PlayerEnemyType')
  assert.deepEqual(enemy.fight, { tag: 'Static', contents: 3 })
  assert.deepEqual(enemy.health, { tag: 'Static', contents: 3 })
  assert.deepEqual(enemy.evade, { tag: 'Static', contents: 2 })
  assert.deepEqual(enemy.healthDamage, { tag: 'Static', contents: 1 })
  assert.deepEqual(enemy.sanityDamage, { tag: 'Static', contents: 1 })
})

test('ability text is never imported -- there is no field for it', async (t) => {
  const { arkhamBuildCardToCustomCard } = await load(t)

  const { def } = arkhamBuildCardToCustomCard(
    { ...asset, text: 'Spend 1 ammo: Fight.', flavor: 'Bang.' },
    PACK,
  )
  const blob = JSON.stringify(def)
  assert.ok(!blob.includes('Spend 1 ammo'))
  assert.ok(!blob.includes('Bang.'))
})

test('a card code is derived from the arkham.build id, not minted fresh', async (t) => {
  const { arkhamBuildCardToCustomCard } = await load(t)

  const once = arkhamBuildCardToCustomCard(asset, PACK).def.cardCode
  const again = arkhamBuildCardToCustomCard(asset, PACK).def.cardCode
  // Re-importing the same file has to land on the same code, or it would
  // duplicate the card instead of updating it.
  assert.equal(once, again)
  // isCustomCardCode is a `*` prefix check, and the engine's CardCode equality
  // treats a trailing a/b/c/d as a card side, so the code ends in a digit.
  assert.ok(once.startsWith('*'))
  assert.match(once, /^\*[0-9a-f]{32}0$/)
})

test('a deck names the same cards the import stored', async (t) => {
  const { arkhamBuildCardToCustomCard, normalizeArkhamBuildDeckCodes } = await load(t)

  const deck = normalizeArkhamBuildDeckCodes({
    investigator_code: investigator.code,
    slots: { [asset.code]: 1, [weaknessTreachery.code]: 1, '01001': 2 },
    sideSlots: { [weaknessEnemy.code]: 1 },
  })

  assert.equal(deck.investigator_code, arkhamBuildCardToCustomCard(investigator, PACK).def.cardCode)
  for (const card of [asset, weaknessTreachery]) {
    const code = arkhamBuildCardToCustomCard(card, PACK).def.cardCode
    assert.ok(code in deck.slots, `${card.name} is not in the deck under ${code}`)
  }
  assert.ok(arkhamBuildCardToCustomCard(weaknessEnemy, PACK).def.cardCode in deck.sideSlots)
  // Quantities ride along untouched.
  assert.equal(deck.slots['01001'], 2)
})

test('an official card code is left exactly as it was', async (t) => {
  const { normalizeArkhamBuildDeckCodes } = await load(t)

  const deck = normalizeArkhamBuildDeckCodes({
    investigator_code: '01001',
    slots: { '01006': 2, '60505': 1, c01007: 1 },
  })
  assert.equal(deck.investigator_code, '01001')
  assert.deepEqual(deck.slots, { '01006': 2, '60505': 1, c01007: 1 })
})

test('an alternate front is rewritten inside the meta string', async (t) => {
  const { arkhamBuildCardToCustomCard, normalizeArkhamBuildDeckCodes } = await load(t)

  const deck = normalizeArkhamBuildDeckCodes({
    investigator_code: investigator.code,
    slots: {},
    meta: JSON.stringify({ alternate_front: investigator.code, faction_selected: 'guardian' }),
  })

  const meta = JSON.parse(deck.meta)
  assert.equal(meta.alternate_front, arkhamBuildCardToCustomCard(investigator, PACK).def.cardCode)
  // Everything else in meta survives the rewrite.
  assert.equal(meta.faction_selected, 'guardian')
})

/* A pack's cards arrive with a short hex id rather than a dashed UUID. Both the
 * import and the deck have to derive the same code from it, or the deck names a
 * card the library holds under a different code and validation rejects it as
 * unimplemented -- reported as a bare `Unknown card: c1C8082CF`. */
test('a short arkham.build id matches the code its card was imported under', async (t) => {
  const { arkhamBuildCardToCustomCard, normalizeArkhamBuildDeckCodes } = await load(t)

  const shortId = '1C8082CF'
  const card = arkhamBuildCardToCustomCard({ ...asset, code: shortId }, PACK)

  const deck = normalizeArkhamBuildDeckCodes({
    investigator_code: '01001',
    slots: { [shortId]: 2 },
  })

  // The deck now names the card by the same code the library stored it under,
  // and case does not matter: the deck spells it upper, the code is lower.
  assert.deepEqual(Object.keys(deck.slots), [card.def.cardCode])
  assert.equal(card.def.cardCode, '*1c8082cf0')
})

test('an undashed 32-character uuid is translated too', async (t) => {
  const { arkhamBuildCardToCustomCard, normalizeArkhamBuildDeckCodes } = await load(t)

  const bare = '43433775620b4e108583253eb3f3e6c2'
  const card = arkhamBuildCardToCustomCard({ ...asset, code: bare }, PACK)
  const deck = normalizeArkhamBuildDeckCodes({ investigator_code: '01001', slots: { [bare]: 1 } })

  assert.deepEqual(Object.keys(deck.slots), [card.def.cardCode])
})

/* The two derivations agree because there is only one of them: the deck side
asks `isArkhamBuildCardId` what counts as an id, and the importer derives the
code from whatever it is given. This pins them to each other -- if the deck side
ever stops translating a shape the importer accepts, a whole pack of cards goes
missing from every deck that names them. */
test('the deck rewrite translates exactly the ids the importer recognises', async (t) => {
  const { arkhamBuildCardToCustomCard, normalizeArkhamBuildDeckCodes } = await load(t)
  const { isArkhamBuildCardId } = await loadCustomCards(t)

  const codes = [
    'd545b38e-f3cf-486a-a026-12ed94b33453', // dashed uuid
    '43433775620b4e108583253eb3f3e6c2', // bare 32-hex
    '1C8082CF', // short id, upper case
    '01001', // ArkhamDB
    '60101', // ArkhamDB, six digits
    '*1c8082cf0', // already this app's own code
    'not-an-id',
    '',
  ]

  // One at a time: a slots object with several keys reorders numeric-looking
  // ones, and it is which code came out that matters, not where.
  for (const code of codes) {
    const deck = normalizeArkhamBuildDeckCodes({ investigator_code: '01001', slots: { [code]: 1 } })
    const [out] = Object.keys(deck.slots)

    assert.equal(
      out !== code,
      isArkhamBuildCardId(code),
      `${JSON.stringify(code)}: the deck rewrite and the id predicate disagree`,
    )
    // And when it is translated, it lands on the code an import would give it.
    if (out !== code) {
      assert.equal(out, arkhamBuildCardToCustomCard({ ...asset, code }, PACK).def.cardCode)
    }
  }
})

/* The mini is cut from the card face in a browser, with a canvas. Outside one
there is nothing to cut with, and the cards come back exactly as they went in
rather than half-filled. */
test('portrait cropping leaves cards alone when it cannot run', async (t) => {
  const { arkhamBuildCardToCustomCard, attachInvestigatorPortraits } = await load(t)

  const cards = [
    arkhamBuildCardToCustomCard(investigator, PACK),
    arkhamBuildCardToCustomCard(asset, PACK),
  ]
  const cut = await attachInvestigatorPortraits(cards)

  assert.equal(cut, 0)
  assert.equal(cards[0].def.meta.portrait, undefined)
  assert.equal(cards[0].def.meta.portraitBack, undefined)
  // The face it would have been cut from is untouched either way.
  assert.equal(cards[0].art, investigator.image_url)
})

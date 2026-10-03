import { img } from '@/assets'
import type { Tagged } from '@/types'

export const cssName = (s: unknown) => String(s).replace(/[^a-zA-Z0-9_-]/g, '-')
export const slug = (t: unknown) =>
  String(t)
    .toLowerCase()
    .replace(/'/g, '')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '')
export const humanize = (s: unknown) => String(s).replace(/([a-z])([A-Z])/g, '$1 $2')
export const show = (v: unknown): string =>
  typeof v === 'string'
    ? v
    : v && typeof v === 'object' && 'tag' in v
      ? (v as Tagged).tag + ((v as Tagged).contents !== undefined ? ' ' + JSON.stringify((v as Tagged).contents) : '')
      : JSON.stringify(v)

export const TOKEN = (n: string) => img(`tokens/${n}.webp`)

/* The symbols the cards print, as glyphs of the AHRDIcons font. Use them through
Icon.vue rather than pasting the characters, which are in a private use area. */
export const ICON: Record<string, string> = {
  lore: '\uf490',
  influence: '\uf491',
  observation: '\uf492',
  strength: '\uf493',
  will: '\uf494',
  items: '\uf495',
  special: '\uf496',
  spells: '\uf497',
  allies: '\uf498',
  damage: '\uf499',
  horror: '\uf49a',
  money: '\uf49b',
  remnant: '\uf49c',
  'remove-doom': '\uf49d',
  doom: '\uf49e',
  clue: '\uf49f',
  monster: '\uf4a0',
  headline: '\uf4a1',
  'gate-burst': '\uf4a2',
  reckoning: '\uf4a3',
  residential: '\uf4af',
  scenic: '\uf4c0',
  bridge: '\uf4c1',
  'train-platform': '\uf4c2',
  'country-road': '\uf4c3',
  'ferry-terminal': '\uf4c4',
  move: '\uf4c7',
  'spread-terror': '\uf4c8',
}
/* Card copy names a skill in a handful of set ways -- "test lore", "(will)",
"lore -1", "+2 strength", "an observation test", "in place of influence", "may
focus will". Only those are the game's skill; everywhere else "will" is a verb
and "influence" is a noun, so the words are left alone. */
const SKILL_WORDS = 'lore|will|strength|observation|influence'
const SKILL_SENSE = new RegExp(
  [
    `\\b(?:tests?|testing|tested|focus|focuses|focused|focusing)\\s+(?:a\\s+|an\\s+|one\\s+)?(${SKILL_WORDS})\\b`,
    `\\bin place of\\s+(${SKILL_WORDS})\\b`,
    `\\(\\s*(${SKILL_WORDS})\\s*\\)`,
    `(?:[+\\-\u2212]\\d+)\\s*(${SKILL_WORDS})\\b`,
    `\\b(${SKILL_WORDS})\\s*(?=[+\\-\u2212]\\d)`,
    `\\b(${SKILL_WORDS})\\s+tests?\\b`,
    `\\b(${SKILL_WORDS})\\s+modifier\\b`,
    `\\bthan\\s+(${SKILL_WORDS})\\b`,
    // a skill listed beside another is one too: "a will or observation test"
    `\\b(${SKILL_WORDS})\\b(?=\\s*(?:,|/|\\bor\\b|\\band\\b)\\s*(?:${SKILL_WORDS})\\b)`,
    `(?<=\\b(?:${SKILL_WORDS})\\s{0,3}(?:,|/|\\bor\\b|\\band\\b)\\s{0,3})(${SKILL_WORDS})\\b`,
  ].join('|'),
  'gi',
)

export type TextPart = { text: string } | { icon: string; word: string }
/** Card copy split into its words and the skill icons that stand in for them. */
export function skillParts(copy: string): TextPart[] {
  const parts: TextPart[] = []
  let at = 0
  for (const m of copy.matchAll(SKILL_SENSE)) {
    const word = m.slice(1).find(Boolean)
    if (!word || m.index === undefined) continue
    /* the match carries its context ("test lore"), and only the skill itself becomes
    the icon -- except for a skill standing alone in brackets, where the icon is what
    the card prints and the brackets go with the word */
    const bracketed = /^\(\s*[a-z]+\s*\)$/i.test(m[0])
    const start = bracketed ? m.index : m.index + m[0].toLowerCase().lastIndexOf(word.toLowerCase())
    const end = bracketed ? m.index + m[0].length : start + word.length
    if (start > at) parts.push({ text: copy.slice(at, start) })
    parts.push({ icon: word.toLowerCase(), word })
    at = end
  }
  if (at < copy.length) parts.push({ text: copy.slice(at) })
  return parts
}

// a skill's icon, by the name the engine uses
export const SKILL_ICON: Record<string, string> = {
  Lore: 'lore',
  Influence: 'influence',
  Observation: 'observation',
  Strength: 'strength',
  Will: 'will',
}

export const MYTHOS: Record<string, string> = {
  SpreadDoomToken: 'mythos-spread-doom',
  SpawnMonsterToken: 'mythos-spawn-monster',
  ReadHeadlineToken: 'mythos-read-headline',
  SpawnClueToken: 'mythos-spawn-clue',
  GateBurstToken: 'mythos-gate-burst',
  ReckoningToken: 'mythos-reckoning',
  BlankToken: 'mythos-blank',
  SpreadTerrorToken: 'mythos-spread-terror',
  WhiteMarkerToken: 'white-marker',
}
export const FOCUS: Record<string, string> = {
  Lore: 'focus-lore',
  Influence: 'focus-influence',
  Observation: 'focus-observation',
  Strength: 'focus-strength',
  Will: 'focus-willpower',
}
export const LOG_TOKEN: Record<string, string> = {
  SpreadDoomToken: 'Spread doom',
  SpawnMonsterToken: 'Spawn monster',
  ReadHeadlineToken: 'Read headline',
  SpawnClueToken: 'Spawn clue',
  GateBurstToken: 'Gate burst',
  ReckoningToken: 'Reckoning',
  BlankToken: 'Blank',
  SpreadTerrorToken: 'Spread terror',
}
/* What a mythos token does, keyed by the art it is drawn with. The tooltip reads
these; every other token keeps its plain name. */
export const TOKEN_TIP: Record<string, [string, string]> = {
  'mythos-spread-doom': ['Spread doom', 'The bottom event card is revealed and doom is placed on the spaces it names.'],
  'mythos-spawn-monster': ['Spawn monster', 'A monster is drawn from the monster cup and spawns.'],
  'mythos-read-headline': ['Read headline', 'The investigator who drew the token reads a headline card.'],
  'mythos-spawn-clue': ['Spawn clue', 'The top event card is revealed and a clue appears in its neighborhood.'],
  'mythos-gate-burst': ['Gate burst', 'The top event card is revealed and an anomaly bursts in its neighborhood.'],
  'mythos-reckoning': ['Reckoning', 'Every reckoning effect in play resolves, one source at a time.'],
  'mythos-blank': ['Blank', 'Nothing happens, unless a card reacts to drawing a blank.'],
  'mythos-spread-terror': ['Spread terror', 'Terror spreads through a neighborhood holding an unstable space.'],
  'white-marker': ['White marker', 'A marker a card has added to the cup; that card alone says what drawing it does.'],
}

// skill rows on the investigator sheet front, as fractions of the image
export const SKILL_ROWS: Record<string, number> = { Lore: 0.6, Influence: 0.685, Observation: 0.77, Strength: 0.853, Will: 0.935 }
export const SHROUDED = new Set([
  'haunting-dead',
  'screaming-haunt',
  'weeping-haunt',
  'cacophonous-haunt',
  'raging-poltergeist',
  'commanding-specter',
  'confounding-specter',
  'crashing-specter',
  'stalking-wraith',
  'sanguinous-wraith',
  'vomitous-wraith',
])
export const ROLE_CLASS: Record<string, string> = {
  Guardian: 'Guardian',
  Mystic: 'Mystic',
  Rogue: 'Rogue',
  Seeker: 'Seeker',
  Survivor: 'Survivor',
}
export const ANOMALY_BACKS: Record<string, string> = {
  'Temporal Fissure': 'temporal-fissures',
  'Fractured Reality': 'fractured-reality',
  'Nightmare Breach': 'nightmare-breach',
}
export const EXPANSION_NAMES: Record<string, string> = {
  CoreSet: 'Core Set',
  DeadOfNight: 'Dead of Night',
  UnderDarkWaves: 'Under Dark Waves',
  SecretsOfTheOrder: 'Secrets of the Order',
  RecursiveEchoes: 'Recursive Echoes',
}
export const expName = (e: string) => EXPANSION_NAMES[e] ?? e
// box cover per expansion: CoreSet -> core-set
export const expArt = (e: string) => String(e).replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()

export const GAME_MODES: [string, string, string][] = [
  ['StandardMode', 'Standard', 'The mythos cup as printed on the scenario sheet.'],
  ['StoryMode', 'Story', 'One doom token in the mythos cup becomes a blank.'],
  ['ChallengeMode', 'Challenge', 'One blank token in the mythos cup becomes a doom.'],
]

// only answers announce: the engine lists the phases it began while resolving one
export const PHASE_BANNERS: Record<string, [string, string]> = {
  ActionPhase: ['Action Phase', '#a87532'],
  MonsterPhase: ['Monster Phase', '#9f2929'],
  EncounterPhase: ['Encounter Phase', '#2f6b4f'],
  MythosPhase: ['Mythos Phase', '#7b4b91'],
}

export const TILE_W = 240
export const TILE_H = (TILE_W * 2) / Math.sqrt(3)
/* A tile's picture is wider than the hexagon it holds: the hexagon's points touch the top
and bottom of the picture, and a regular hexagon 907 across stands 1047 tall, not the 1000
the picture is. The hexagon is 0.9548 of the picture, so the picture is drawn this much
wider for the hexagon in it to come out TILE_W across -- and then its height is TILE_H,
which is what a hexagon that wide stands anyway. Drawn any other shape, the tiles no
longer meet the pieces laid between them. */
export const TILE_ART_W = (TILE_H * 907) / 1000
export const TILE_ART_H = TILE_H
/* A street's length along the join, and how wide it is across that -- the size it is on
the table, measured off the tabletop version by fitting this same art onto a photograph of
its board. The art is square and is drawn square; stretching it along the join is what used
to push the streets over the tiles they join. */
export const STREET_W = 0.4189 * TILE_W
export const STREET_H = (STREET_W * 813) / 808
/* A connector hangs off one edge instead of spanning two tiles, so what sets its size is
how deep it stands, not a street's length. The backend seats it as a piece this deep with a
fifth of that tucked behind the tile's edge (@connectorDepth@ and @connectorTab@ in
Tiles.hs), and that fifth is its tab: at this depth the tab is 0.068 long, which is exactly
how deep the notch in a tile's edge is, so the tab bottoms out in the notch just as the
piece's body comes up against the edge. Deeper and the tab's flared tip drives into the
notch's walls, which is what buried the piece's corners in the tile. The
box is as wide as the widest connector art and each picture is fitted inside it, which
keeps every connector's own proportions -- they run from 1.01:1 to 1.20:1 -- and brings
them all out at the same depth. */
export const CONNECTOR_D = 0.349 * TILE_W
export const CONNECTOR_W = 1.2 * CONNECTOR_D
/* A corner piece stands in the junction three tiles leave between them, which the backend
puts at the middle of their three centres. This is the size it is on the table, and it
checks out against the hole it has to fill: that middle is 0.773 of a tile from each
centre and their corners reach 0.562, so an arm has 0.211 to cross, and the art's arms run
to 0.509 of the piece's width. */
export const CORNER_W = 0.4158 * TILE_W
/* A portal spans the same gap but is drawn square and a little larger, also measured. */
export const PORTAL_W = 0.4409 * TILE_W
/* Where the piece's own middle sits inside its art, as a fraction of the art: the point
its three joining edges stand evenly round, which is not the middle of the picture. The
art is turned about its box, so this offset has to be taken out, turned with it. */
export const CORNER_ART_MIDDLE = { x: 0.4991, y: 0.4585 }
export const HUB_R = 0.135

export const DECK_KEYS = [
  'street',
  'anomaly',
  'ally',
  'spell',
  'item',
  'headline',
  'headlineDiscard',
  'monster',
  'event',
  'eventDiscard',
] as const
export const DECK_DEBUG: Record<string, string> = {
  ally: 'DeckAlly',
  spell: 'DeckSpell',
  item: 'DeckItem',
  street: 'DeckStreet',
  anomaly: 'DeckAnomaly',
  monster: 'DeckMonster',
  headline: 'DeckHeadline',
}
export const NEIGHBOURHOOD_KEY = 'neighborhood:'
// DeckNeighborhood carries an id, which makes aeson tag every case -- even the
// nullary ones, so they go as {tag} objects rather than bare strings
export const deckTag = (key: string): Tagged | undefined =>
  key.startsWith(NEIGHBOURHOOD_KEY)
    ? { tag: 'DeckNeighborhood', contents: key.slice(NEIGHBOURHOOD_KEY.length) }
    : DECK_DEBUG[key]
      ? { tag: DECK_DEBUG[key] }
      : undefined
export const DBG_SKILLS = ['Lore', 'Influence', 'Observation', 'Strength', 'Will']

export const TEST_STEPS: [string, string][] = [
  ['DeterminePool', 'Gather dice'],
  ['ManipulateDice', 'Reroll & adjust'],
  ['TestResolved', 'Resolve'],
]

export const archiveImage = (n: number, back: boolean) =>
  img(`archive/${String(n).padStart(3, '0')}${back ? 'b' : ''}.avif`)

export const sleep = (ms: number) => new Promise<void>((r) => setTimeout(r, ms))

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
export const STREET_W = 0.534 * TILE_W
export const STREET_H = 0.537 * TILE_W
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
  img(`archive/core/${String(n).padStart(3, '0')}${back ? 'b' : ''}.avif`)

export const sleep = (ms: number) => new Promise<void>((r) => setTimeout(r, ms))

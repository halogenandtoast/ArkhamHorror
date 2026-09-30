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
/* What a token is and what it does, keyed by the art it is drawn with. The
tooltip reads these; a token with no entry keeps its plain name. */
export const TOKEN_TIP: Record<string, [string, string]> = {
  'mythos-spread-doom': ['Spread doom', 'The bottom event card is revealed and doom is placed on the spaces it names.'],
  'mythos-spawn-monster': ['Spawn monster', 'A monster is drawn from the monster cup and spawns.'],
  'mythos-read-headline': ['Read headline', 'The investigator who drew the token reads a headline card.'],
  'mythos-spawn-clue': ['Spawn clue', 'The top event card is revealed and a clue appears in its neighborhood.'],
  'mythos-gate-burst': ['Gate burst', 'The top event card is revealed and an anomaly bursts in its neighborhood.'],
  'mythos-reckoning': ['Reckoning', 'Every reckoning effect in play resolves, one source at a time.'],
  'mythos-blank': ['Blank', 'Nothing happens, unless a card reacts to drawing a blank.'],
  'mythos-spread-terror': ['Spread terror', 'Terror spreads through a neighborhood holding an unstable space.'],
  clue: ['Clue', 'Investigators spend clues on scenario objectives and card effects.'],
  doom: ['Doom', 'The mythos places doom on spaces and the scenario sheet; scenarios turn on how much has gathered.'],
  damage: ['Damage', 'An investigator is defeated when damage reaches their health.'],
  horror: ['Horror', 'An investigator is defeated when horror reaches their sanity.'],
  money: ['Money', 'Spent to buy items and on card effects.'],
  remnant: ['Remnant', 'A keepsake spent on card effects, and traded between investigators.'],
  terror: ['Terror', "A neighborhood's terror level, which rises as terror spreads."],
  anomaly: ['Anomaly', 'A rift on a space; the scenario says what passing through it costs.'],
  'activation-active': ['Activation', 'This monster acts during the monster phase.'],
  'activation-inactive': ['Activation', 'This monster does not act during the monster phase.'],
  'first-player-active': ['Lead investigator', 'Takes the first turn and answers the questions the group is asked.'],
  'first-player-inactive': ['Lead investigator', 'Passes to the next investigator at the end of the round.'],
  'focus-lore': ['Lore focus', 'Spend it to reroll a die on a lore test.'],
  'focus-influence': ['Influence focus', 'Spend it to reroll a die on an influence test.'],
  'focus-observation': ['Observation focus', 'Spend it to reroll a die on an observation test.'],
  'focus-strength': ['Strength focus', 'Spend it to reroll a die on a strength test.'],
  'focus-willpower': ['Will focus', 'Spend it to reroll a die on a will test.'],
  'red-marker': ['Red marker', 'A scenario component; the scenario sheet says what it marks.'],
  'blue-marker': ['Blue marker', 'A scenario component; the scenario sheet says what it marks.'],
  'white-marker': ['White marker', 'A scenario component; the scenario sheet says what it marks.'],
  'green-marker': ['Green marker', 'A scenario component; the scenario sheet says what it marks.'],
  'marker-back': ['Marker', 'Face down: what it marks is not known yet.'],
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

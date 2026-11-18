// Display formatting for the facts a CardDef carries. Shared by the card list
// and the card details modal so both describe a card the same way.
import type { CardDef } from '@/arkham/types/CardDef'
import { isCustomCardCode } from '@/arkham/customCards'
import { homebrewBoxLabelKey, homebrewBoxName, homebrewCampaignScope } from '@/arkham/homebrewData'
import sets from '@/arkham/data/sets.json'
import { useDbCardStore, type ArkhamDBCard } from '@/stores/dbCards'
import { currentLocale, messagesFor } from '@/locales/i18n'

const viewerLanguage = () =>
  (typeof localStorage === 'undefined' ? null : localStorage.getItem('language')) || 'en'

/* A raw message from the viewer's locale, read straight from the loaded
 * messages rather than through t(): card names are data and may hold
 * characters vue-i18n would read as syntax. */
const localMessage = (path: string): unknown => {
  if (viewerLanguage() === 'en') return undefined
  let node: unknown = messagesFor(currentLocale())
  for (const part of path.split('.')) {
    if (!node || typeof node !== 'object') return undefined
    node = (node as Record<string, unknown>)[part]
  }
  return node
}

const localString = (path: string): string | null => {
  const value = localMessage(path)
  return typeof value === 'string' && value ? value : null
}

// Two defs with the same art are one physical card and render as a single
// tile/row; these three share art but are genuinely distinct cards.
const ungroupedWarOfTheOuterGodsCards = new Set(['c86038a', 'c86044a', 'c86049a'])

export const cardGroupKey = (card: CardDef) =>
  ungroupedWarOfTheOuterGodsCards.has(card.cardCode) ? card.cardCode : card.art

export const groupCards = (cards: CardDef[]) => {
  const grouped = new Map<string, { card: CardDef; count: number }>()

  for (const card of cards) {
    const key = cardGroupKey(card)
    const existing = grouped.get(key)
    if (existing) existing.count += 1
    else grouped.set(key, { card, count: 1 })
  }

  return Array.from(grouped.values())
}

/* The viewer's card-data record for a card, or null in English and for custom
 * cards. Homebrew cards have records too where arkham-cards-data translates them
 * (see loadHomebrewCards in scripts/import-card-translations.cjs). The CardDef itself stays
 * English -- filters, CSS classes and icons key off its values -- so only what
 * is shown goes through here. Reading the store makes callers reactive: a list
 * drawn before cards_<lang>.json arrives redraws translated once it does. */
const localizedDbCard = (card: CardDef): ArkhamDBCard | null => {
  if (viewerLanguage() === 'en' || isCustomCardCode(card.art)) return null
  const revisedFrom = card.meta?.revisedFrom
  return useDbCardStore().getDbCard(typeof revisedFrom === 'string' ? revisedFrom : card.art)
}

// A `b` art borrows its front's record (see buildCardIndex); that record keeps
// the back's text under back_*.
const isBorrowedBack = (card: CardDef, match: ArkhamDBCard) =>
  match.code !== card.art && card.art.endsWith('b')

/* A homebrew card no card data translates may still be named by its box:
 * `<scope>.cardNames.<number>` in the box's own locale folder, either a string
 * or { title, subtitle }. "063aa" falls back to "063" when only the printed
 * card is named. */
const homebrewTitleParts = (card: CardDef): { title: string; subtitle: string | null } | null => {
  const art = card.art.match(/^:([^:]+):(.+)$/)
  if (!art) return null
  const names = localMessage(`${homebrewCampaignScope(art[1])}.cardNames`)
  if (!names || typeof names !== 'object') return null
  const table = names as Record<string, unknown>
  const entry = table[art[2]] ?? table[art[2].replace(/[a-z]+$/, '')]
  if (typeof entry === 'string' && entry) return { title: entry, subtitle: card.name.subtitle }
  if (entry && typeof entry === 'object') {
    const { title, subtitle } = entry as { title?: string; subtitle?: string }
    if (title) return { title, subtitle: subtitle ?? card.name.subtitle }
  }
  return null
}

export const cardTitleParts = (card: CardDef): { title: string; subtitle: string | null } => {
  const match = localizedDbCard(card)
  if (!match) return homebrewTitleParts(card) ?? card.name
  const back = isBorrowedBack(card, match)
  return {
    title: (back && match.back_name) || match.name,
    subtitle: !back && match.subname ? match.subname : card.name.subtitle,
  }
}

export const cardName = (card: CardDef) => {
  const { title, subtitle } = cardTitleParts(card)
  return subtitle === null ? title : `${title}: ${subtitle}`
}

/* Class names as printed in the viewer's language, in the card's own order.
 * ArkhamDB names a card's classes faction, faction2, faction3. */
export const cardClassNames = (card: CardDef): string[] => {
  const match = localizedDbCard(card)
  const names = match ? [match.faction_name, match.faction2_name, match.faction3_name] : []
  return card.classSymbols.map((sym, i) => names[i] || localString(`classNames.${sym.toLowerCase()}`) || sym)
}

// The engine's card types, as ArkhamDB codes them.
const TYPE_CODES: Record<string, string> = {
  InvestigatorType: 'investigator',
  AssetType: 'asset',
  EncounterAssetType: 'asset',
  EventType: 'event',
  EncounterEventType: 'event',
  SkillType: 'skill',
  TreacheryType: 'treachery',
  PlayerTreacheryType: 'treachery',
  EnemyType: 'enemy',
  PlayerEnemyType: 'enemy',
  LocationType: 'location',
  EnemyLocationType: 'enemy_location',
  EnemyLocationCardType: 'enemy_location',
  ScenarioType: 'scenario',
  AgendaType: 'agenda',
  ActType: 'act',
  StoryType: 'story',
  KeyType: 'key',
}

/* Names the loaded card data already gives, for cards it has no record of
 * (homebrew mostly): type names by type code, and trait names by the English
 * trait. Built once per load of the card data. A trait takes the translation
 * most cards give it; a "translation" that is still Latin text is the card data
 * leaving the trait in English, and is not counted. */
type DerivedNames = { types: Map<string, string>; traits: Map<string, string> }
let derived: { source: ArkhamDBCard[]; names: DerivedNames } | null = null

// "Ancient One", "AncientOne" and "ancient one" are one trait.
const traitKey = (trait: string) => trait.replace(/[^A-Za-z0-9]/g, '').toLowerCase()
const splitTraits = (traits: string) => traits.split('.').map((s) => s.trim()).filter(Boolean)

const derivedNames = (): DerivedNames => {
  const cards = useDbCardStore().dbCards
  if (derived?.source === cards) return derived.names
  const types = new Map<string, string>()
  const votes = new Map<string, Map<string, number>>()
  for (const c of cards) {
    if (c.type_code && c.type_name && !types.has(c.type_code)) types.set(c.type_code, c.type_name)
    if (!c.real_traits || !c.traits) continue
    const english = splitTraits(c.real_traits)
    const local = splitTraits(c.traits)
    if (english.length !== local.length) continue
    english.forEach((trait, i) => {
      const name = local[i]!
      if (/^[\x20-\x7e]+$/.test(name)) return
      const key = traitKey(trait)
      const tally = votes.get(key) ?? new Map<string, number>()
      tally.set(name, (tally.get(name) ?? 0) + 1)
      votes.set(key, tally)
    })
  }
  const traits = new Map<string, string>()
  for (const [key, tally] of votes) {
    traits.set(key, [...tally].sort((a, b) => b[1] - a[1])[0]![0])
  }
  const names = { types, traits }
  derived = { source: cards, names }
  return names
}

/* `traitNames` in the locale wins (traits no printed card carries, and any the
 * card data gets wrong), then the card data's own usage. */
let traitTable: { source: unknown; map: Map<string, string> } | null = null
const translateTrait = (trait: string): string => {
  const table = localMessage('traitNames')
  if (!traitTable || traitTable.source !== table) {
    const map = new Map<string, string>()
    if (table && typeof table === 'object') {
      for (const [k, v] of Object.entries(table)) if (typeof v === 'string') map.set(traitKey(k), v)
    }
    traitTable = { source: table, map }
  }
  const key = traitKey(trait)
  return traitTable.map.get(key) ?? derivedNames().traits.get(key) ?? trait
}

export const levelText = (card: CardDef) => {
  if (!card.level || card.level === 0) return ''
  return ` (${card.level})`
}

export const cardCost = (card: CardDef) => {
  if (card.cost?.tag === 'StaticCost') return card.cost.contents
  if (card.cost?.tag === 'DynamicCost') return -2
  if (card.cost?.tag === 'DeferredCost') return -2
  if (card.cost?.tag === 'DiscardAmountCost') return -2
  return null
}

export const cardType = (card: CardDef) => {
  const match = localizedDbCard(card)
  if (match?.type_name) return match.type_name
  if (viewerLanguage() !== 'en') {
    const code = TYPE_CODES[card.cardType]
    const name = code && (localString(`cardTypeNames.${code}`) || derivedNames().types.get(code))
    if (name) return name
  }
  switch (card.cardType) {
    case 'PlayerTreacheryType': return 'Treachery'
    case 'PlayerEnemyType': return 'Enemy'
    default: return card.cardType.replace(/Type$/, '')
  }
}

export const cardTraits = (card: CardDef) => {
  const match = localizedDbCard(card)
  if (match) {
    const traits = isBorrowedBack(card, match) ? match.back_traits : match.traits
    if (traits) return traits.trim()
  }
  if (card.cardTraits.length === 0) return ''
  if (viewerLanguage() !== 'en') return `${card.cardTraits.map(translateTrait).join('. ')}.`
  return `${card.cardTraits.join('. ')}.`
}

export const cardIcons = (card: CardDef) => {
  return card.skills.map((s) => {
    if (s.tag === 'SkillIcon') {
      switch (s.contents) {
        case 'SkillWillpower': return 'willpower'
        case 'SkillIntellect': return 'intellect'
        case 'SkillCombat': return 'combat'
        case 'SkillAgility': return 'agility'
        default: return 'unknown'
      }
    }
    if (s.tag === 'WildIcon' || s.tag === 'WildMinusIcon') return 'wild'
    return 'unknown'
  })
}

const cardSetCache = new Map<string, (typeof sets)[number] | undefined>()

export const cardSet = (card: CardDef) => {
  const cached = cardSetCache.get(card.art)
  if (cached !== undefined || cardSetCache.has(card.art)) return cached

  const cardCode = parseInt(card.art)
  const set = sets.find((s) => cardCode >= s.min && cardCode <= s.max)
  cardSetCache.set(card.art, set)
  return set
}

/* ":dark-matter:021" -> { box: 'dark-matter', number: 21 }. A homebrew box
 * namespaces its cards rather than taking a range of printed numbers, so its
 * collection number is what follows the box name. */
const homebrewArt = (art: string) => {
  // A few cards carry a name instead of a number (":ages-unwound:xsoldier").
  const match = art.match(/^:([^:]+):(?:0*(\d+))?/)
  if (!match) return null
  return { box: match[1], number: match[2] === undefined ? null : parseInt(match[2]) }
}

// "The Dunwich Legacy 12" — the set name plus the card's collection number.
// `packName` overrides the set name for localized card data.
export const cardSetText = (card: CardDef, packName?: string) => {
  // A card you built has no collection number; the set it names for itself is
  // the library set it was built in.
  if (isCustomCardCode(card.art)) {
    const name = card.meta?.set
    return typeof name === 'string' && name ? name : 'Custom'
  }

  const homebrew = homebrewArt(card.art)
  if (homebrew) {
    const labelKey = homebrewBoxLabelKey(homebrew.box)
    const name = (labelKey && localString(labelKey)) || homebrewBoxName(homebrew.box)
    if (!name) return localString('cardsView.homebrew') || 'Homebrew'
    return homebrew.number === null ? name : `${name} ${homebrew.number}`
  }

  const setNumber = parseInt(card.art.slice(2))
  const setName = packName || localizedDbCard(card)?.pack_name || cardSet(card)?.name

  if (setName) return `${setName} ${setNumber % 500}`
  return 'Unknown'
}

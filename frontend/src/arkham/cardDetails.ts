// Display formatting for the facts a CardDef carries. Shared by the card list
// and the card details modal so both describe a card the same way.
import type { CardDef } from '@/arkham/types/CardDef'
import { isCustomCardCode } from '@/arkham/customCards'
import { homebrewBoxName } from '@/arkham/homebrewData'
import sets from '@/arkham/data/sets.json'
import { useDbCardStore, type ArkhamDBCard } from '@/stores/dbCards'

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
  const language = (typeof localStorage === 'undefined' ? null : localStorage.getItem('language')) || 'en'
  if (language === 'en' || isCustomCardCode(card.art)) return null
  const revisedFrom = card.meta?.revisedFrom
  return useDbCardStore().getDbCard(typeof revisedFrom === 'string' ? revisedFrom : card.art)
}

// A `b` art borrows its front's record (see buildCardIndex); that record keeps
// the back's text under back_*.
const isBorrowedBack = (card: CardDef, match: ArkhamDBCard) =>
  match.code !== card.art && card.art.endsWith('b')

export const cardTitleParts = (card: CardDef): { title: string; subtitle: string | null } => {
  const match = localizedDbCard(card)
  if (!match) return card.name
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
  if (!match) return card.classSymbols
  const names = [match.faction_name, match.faction2_name, match.faction3_name]
  return card.classSymbols.map((sym, i) => names[i] || sym)
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
  const match = art.match(/^:([^:]+):0*(\d+)/)
  if (!match) return null
  return { box: match[1], number: parseInt(match[2]) }
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
    const name = homebrewBoxName(homebrew.box)
    return name ? `${name} ${homebrew.number}` : 'Homebrew'
  }

  const setNumber = parseInt(card.art.slice(2))
  const setName = packName || localizedDbCard(card)?.pack_name || cardSet(card)?.name

  if (setName) return `${setName} ${setNumber % 500}`
  return 'Unknown'
}

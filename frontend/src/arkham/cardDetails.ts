// Display formatting for the facts a CardDef carries. Shared by the card list
// and the card details modal so both describe a card the same way.
import type { CardDef } from '@/arkham/types/CardDef'
import sets from '@/arkham/data/sets.json'

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

export const cardName = (card: CardDef) => {
  const subtitle = card.name.subtitle === null ? '' : `: ${card.name.subtitle}`
  return `${card.name.title}${subtitle}`
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
  switch (card.cardType) {
    case 'PlayerTreacheryType': return 'Treachery'
    case 'PlayerEnemyType': return 'Enemy'
    default: return card.cardType.replace(/Type$/, '')
  }
}

export const cardTraits = (card: CardDef) => {
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

// "The Dunwich Legacy 12" — the set name plus the card's collection number.
// `packName` overrides the set name for localized card data.
export const cardSetText = (card: CardDef, packName?: string) => {
  const setNumber = parseInt(card.art.slice(2))
  const setName = packName || cardSet(card)?.name

  if (setName) return `${setName} ${setNumber % 500}`
  return 'Unknown'
}

import { type Card as ArkhamCard } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'

/**
 * A "card pool" is a set-aside deck an investigator builds by picking from
 * their own deck -- Joe Diamond's hunch deck, Underworld Market's market deck,
 * Stick to the Plan's tactics/supplies. They all ask the same question shape,
 * so they all get the same picker: slots for the finished pool, the cards
 * already in it, and the candidates still to choose from.
 */
export type CardPool = {
  /** i18n key for the heading over the pool being built. */
  titleKey: string
  /** i18n key for the heading over the choosable cards. */
  candidatesKey: string
  /** Cards already committed to the pool. */
  chosen: (game: Game, playerId: string) => ArkhamCard[]
  /** Class color the picker and its modal chrome take. */
  accent: 'seeker' | 'rogue' | 'guardian'
}

const investigatorFor = (game: Game, playerId: string) =>
  Object.values(game.investigators).find((i) => i.playerId === playerId)

const controlledAsset = (game: Game, playerId: string, cardCode: string) => {
  const investigator = investigatorFor(game, playerId)
  if (!investigator) return null
  return Object.values(game.assets).find(
    (asset) => asset.cardCode === cardCode && asset.controller === investigator.id
  ) ?? null
}

// Keyed by the scope segment the backend uses in the question label, i.e. the
// `<pool>` of "cards.label.<pool>.<choice>".
const cardPools: Record<string, CardPool> = {
  joeDiamond: {
    titleKey: 'cardPool.joeDiamond.title',
    candidatesKey: 'cardPool.joeDiamond.candidates',
    // The hunch deck is built up by the very question we are answering, so its
    // current size is the honest "how far along am I" counter -- Unsolved Case
    // is already in it before the first pick.
    chosen: (game, playerId) =>
      investigatorFor(game, playerId)?.decks.find(([key]) => key === 'HunchDeck')?.[1] ?? [],
    accent: 'seeker',
  },
  underworldMarket2: {
    titleKey: 'cardPool.underworldMarket2.title',
    candidatesKey: 'cardPool.underworldMarket2.candidates',
    chosen: (game, playerId) => controlledAsset(game, playerId, 'c09077')?.marketDeck ?? [],
    accent: 'rogue',
  },
  stickToThePlan3: {
    titleKey: 'cardPool.stickToThePlan3.title',
    candidatesKey: 'cardPool.stickToThePlan3.candidates',
    chosen: (game, playerId) => controlledAsset(game, playerId, 'c03264')?.cardsUnderneath ?? [],
    accent: 'guardian',
  },
}

/**
 * Resolve a pool from a question label key such as
 * "cards.label.joeDiamond.chooseHunchCards".
 */
export function cardPoolForLabelKey(key: string): CardPool | null {
  const parts = key.split('.')
  const labelIndex = parts.indexOf('label')
  if (labelIndex === -1) return null
  return cardPools[parts[labelIndex + 1]] ?? null
}

/* Debug-only card moving: the frontend half of the backend `DebugMoveCard`
 * message (Arkham.Debug.CardDestination).
 *
 * Every debug drag/drop of a card goes through here rather than sending a
 * gameplay message like `PutOnTopOfDeck` directly. Those leave the card in the
 * zone it came from -- a card dragged out of the victory display onto a deck
 * ends up in both places, which is exactly the duplication bug #5662 produced on
 * its own. `DebugMoveCard` obtains the card first, so it only ever exists once.
 */
import { ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import { PLAYER_CARD_TYPES } from '@/arkham/customCards'
import type { CardDef } from '@/arkham/types/CardDef'
import { type Card, type CardContents, toCardContents } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'

export type DeckSignifier =
  | { tag: 'InvestigatorDeck'; contents: string }
  | { tag: 'InvestigatorDiscard'; contents: string }
  | { tag: 'EncounterDeck' }
  | { tag: 'EncounterDiscard' }
  | { tag: 'EncounterDeckByKey'; contents: string }
  | { tag: 'ScenarioDeckByKey'; contents: string }

export type DebugDeckPosition = 'DebugDeckTop' | 'DebugDeckBottom' | 'DebugDeckShuffle'

export type DebugCardDestination =
  | { tag: 'DebugCardRemovedFromGame' }
  | { tag: 'DebugCardSetAside' }
  | { tag: 'DebugCardDiscard' }
  | { tag: 'DebugCardHand'; contents: string }
  | { tag: 'DebugCardDeck'; contents: [DeckSignifier, DebugDeckPosition] }

export const removedFromGame: DebugCardDestination = { tag: 'DebugCardRemovedFromGame' }
export const setAside: DebugCardDestination = { tag: 'DebugCardSetAside' }
export const discarded: DebugCardDestination = { tag: 'DebugCardDiscard' }
export const toHand = (investigatorId: string): DebugCardDestination =>
  ({ tag: 'DebugCardHand', contents: investigatorId })
export const toDeck = (deck: DeckSignifier, position: DebugDeckPosition): DebugCardDestination =>
  ({ tag: 'DebugCardDeck', contents: [deck, position] })

export function debugMoveCard(gameId: string, cardId: string, destination: DebugCardDestination) {
  return useDebug().send(gameId, { tag: 'DebugMoveCard', contents: [cardId, destination] })
}

/* Where a card is being dropped. Decks and their discards share a back, so they
 * share a rule; a hand accepts anything, which is what debug does today. */
export type DebugDropTarget =
  | 'hand'
  | 'playerDeck'
  | 'playerDiscard'
  | 'encounterDeck'
  | 'encounterDiscard'

/* A drag payload carries only a card id, so a drop target has to look the card
 * back up to know anything about it. */
export function resolveCard(game: Game, cardId: string): Card | undefined {
  return game.cards[cardId]
}

export function cardDefFor(cards: CardDef[], card: Card | CardContents): CardDef | undefined {
  const cardCode = toCardContents(card).cardCode
  return cards.find(def => def.cardCode === cardCode)
}

export function isPlayerCardDef(def: CardDef): boolean {
  return PLAYER_CARD_TYPES.includes(def.cardType)
}

/* Whether a card may legally be put where it is being dropped.
 *
 * A deck holds cards that are identical from the back, so the two things that
 * disqualify a card are having no deck back at all (locations, acts, agendas and
 * the other double-sided cards) and having the wrong one (a player card cannot
 * go in an encounter deck, and vice versa). This is not pedantry: the engine's
 * `PutCardOnTopOfDeck _ EncounterDeck` throws on a player card, so an
 * unchecked drop is a 500. The backend re-checks in `cardDefCanEnterDeck`.
 */
export function canMoveCardTo(def: CardDef | undefined, target: DebugDropTarget): boolean {
  if (!def) return false
  if (target === 'hand') return true
  if (def.doubleSided) return false
  const player = isPlayerCardDef(def)
  return target === 'playerDeck' || target === 'playerDiscard' ? player : !player
}

/* The id of the card currently being dragged.
 *
 * `dataTransfer.getData` returns '' during dragover in every browser -- the
 * payload is only readable on drop -- so a drop zone that wants to say "this one
 * can't go here" before the drop has no way to read it. Drag sources publish the
 * id here instead, and clear it when the drag ends.
 */
const draggedCardId = ref<string | null>(null)

export function beginCardDrag(cardId: string) {
  draggedCardId.value = cardId
  window.addEventListener('dragend', endCardDrag, { once: true })
  window.addEventListener('drop', endCardDrag, { once: true })
}

export function endCardDrag() {
  draggedCardId.value = null
}

/* Whether the in-flight drag, if any, would be accepted here. Used to light a
 * drop zone up (or mark it refused) while the card is still in the air. */
export function draggedCardAccepted(game: Game, cards: CardDef[], target: DebugDropTarget) {
  if (!draggedCardId.value) return null
  const card = resolveCard(game, draggedCardId.value)
  if (!card) return null
  return canMoveCardTo(cardDefFor(cards, card), target)
}

export { draggedCardId }

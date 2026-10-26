import { computed, inject, ref, toValue, type MaybeRefOrGetter, type Ref } from 'vue'
import { type Card, cardId } from '@/arkham/types/Card'

/* Carrying a card from a full-screen reveal to wherever it actually ended up:
 * a drawn player card into your hand, a revealed encounter card into the threat
 * area, onto a location, or into play.
 *
 * Both halves name the same card, and -- just as importantly -- must never hold
 * the name at the same time. A duplicate `view-transition-name` aborts the whole
 * transition, so the overlay names the card while it is up and the board element
 * only takes the name over inside the transition callback. Game.vue owns that
 * handover; `useCardFlight` is the receiving side.
 *
 * A card id can begin with a digit, which is not a valid custom-ident, hence the
 * prefix.
 */
export const cardFlightTransitionName = (card: Card | string) =>
  `cf-${typeof card === 'string' ? card : cardId(card)}`

/* Grouping name for the CSS that tunes the flight (see styles/base.css).
 * `view-transition-class` is how one rule reaches every card in flight without
 * the stylesheet having to know their ids. Browsers without it fall back to the
 * global `::view-transition-group(*)` timing, which is the same idea, slower. */
export const CARD_FLIGHT_TRANSITION_CLASS = 'card-flight'

/* The attribute that marks an element as somewhere a card can land. Game.vue
 * looks for it to decide whether a flight has a destination at all -- only the
 * DOM knows that, since a treachery may have resolved and gone, and a card drawn
 * by another seat is in a hand that is not on screen. Deliberately not reusing
 * `data-index`: Card.vue puts that on every card it renders, including the ones
 * inside the overlay, which would make the overlay its own destination. */
export const CARD_FLIGHT_ATTR = 'data-card-flight'

/* The attribute that marks a *pile* a card can land in: a discard, where only
 * the top card is ever rendered and the one that just arrived is usually not it
 * by the time a reveal is dismissed. Its value is the ids the pile is currently
 * holding, space separated, so the fallback can tell "this card went here" from
 * "a pile exists somewhere on screen".
 *
 * Separate from `CARD_FLIGHT_ATTR` because the two behave differently: a card's
 * own element hides while an overlay previews it, so the card is never on
 * screen twice. A pile must not -- hiding it would blank the whole discard
 * behind the overlay, and the pile is not a second picture of the flying card
 * anyway. It only takes a transition name, and only during the flight.
 */
export const CARD_FLIGHT_PILE_ATTR = 'data-card-flight-pile'

/* Injected as one object so a destination can tell the two phases apart.
 *
 * `previewed`: an overlay is showing this card full-screen. The board copy is
 * hidden so only one image of a card is ever on screen -- a placeholder holds
 * its slot instead.
 * `flying`: the view transition is running. The board copy is visible again and
 * carries the transition name, because a hidden element captures an empty
 * snapshot and would fly as nothing.
 */
export type CardFlightState = {
  previewed: Ref<ReadonlySet<string>>
  flying: Ref<ReadonlySet<string>>
}

export const CARD_FLIGHT_STATE = 'cardFlightState'

/* Style bindings for an element a card can land on. Returns undefined until
 * Game.vue puts this card id in flight, so the name exists on exactly one
 * element at a time.
 *
 * Bind it alongside `:[CARD_FLIGHT_ATTR]="cardId"`, and on an element that does
 * not already carry a `view-transition-name` -- Location.vue and Player.vue put
 * `enemy-<id>` on the Enemy root for board movement, so the flight goes on the
 * card frame inside it rather than fighting for the same element.
 */
export function useCardFlight(id: MaybeRefOrGetter<string | undefined>) {
  const state = useCardFlightState()
  return computed(() => cardFlightStyle(state, toValue(id)))
}

/* The same state, for a destination that renders a list and so cannot call
 * `useCardFlight` per item -- `inject` only runs during setup. Pair it with
 * `cardFlightStyle` in the template. */
export function useCardFlightState(): CardFlightState {
  return inject<CardFlightState>(CARD_FLIGHT_STATE, {
    previewed: ref(new Set()),
    flying: ref(new Set()),
  })
}

export function cardFlightStyle(state: CardFlightState, id: string | undefined) {
  if (!id) return undefined
  if (state.flying.value.has(id)) {
    return {
      viewTransitionName: cardFlightTransitionName(id),
      viewTransitionClass: CARD_FLIGHT_TRANSITION_CLASS,
    }
  }
  // `visibility`, not `display`: the slot has to keep its layout box, both so
  // the board does not reflow under the overlay and so the placeholder can be
  // measured from it.
  if (state.previewed.value.has(id)) return { visibility: 'hidden' as const }
  return undefined
}

/* Style bindings for a pile: the transition name while one of its cards is in
 * flight, and nothing otherwise. Deliberately not `cardFlightStyle`, which also
 * hides a previewed card. */
export function cardFlightPileStyle(state: CardFlightState, ids: string[]) {
  const flying = ids.find((id) => state.flying.value.has(id))
  if (!flying) return undefined
  return {
    viewTransitionName: cardFlightTransitionName(flying),
    viewTransitionClass: CARD_FLIGHT_TRANSITION_CLASS,
  }
}

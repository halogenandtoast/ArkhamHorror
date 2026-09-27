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

export const FLYING_CARD_IDS = 'flyingCardIds'

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
  const flying = inject<Ref<ReadonlySet<string>>>(FLYING_CARD_IDS, ref(new Set()))
  return computed(() => {
    const value = toValue(id)
    if (!value || !flying.value.has(value)) return undefined
    return {
      viewTransitionName: cardFlightTransitionName(value),
      viewTransitionClass: CARD_FLIGHT_TRANSITION_CLASS,
    }
  })
}

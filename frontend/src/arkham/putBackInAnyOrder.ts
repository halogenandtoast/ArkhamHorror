import { ref } from 'vue'
import { MessageType } from '@/arkham/types/Message'
import * as ArkhamGame from '@/arkham/types/Game'
import { revealedCards } from '@/arkham/types/Game'
import type { Game } from '@/arkham/types/Game'
import { toCardContents, type Card as ArkhamCard } from '@/arkham/types/Card'

// `PutBackInAnyOrder` asks one card at a time and runs `AddFocusedToTopOfDeck`
// for each pick, so the picks land bottom-first: the card chosen last ends up on
// top. The question itself carries no label, so it is recognised by the message
// its choices will run.
const PUT_ON_TOP = 'AddFocusedToTopOfDeck'

function runsPutOnTop(messages: unknown[] | undefined): boolean {
  return (messages ?? []).some((message) =>
    typeof message === 'object'
      && message !== null
      && (message as { tag?: unknown }).tag === PUT_ON_TOP
  )
}

export type PutBackPick = { card: ArkhamCard, id: string, index: number }

/* The cards still to be placed, in choice order, or null when the active
 * question is not a put-back. Every choice must be one, so a mixed question
 * (or a `DiscardRest`, which also targets focused cards) falls through to the
 * generic card list. */
export function putBackInAnyOrderPicks(game: Game, playerId: string): PutBackPick[] | null {
  const choices = ArkhamGame.choices(game, playerId)
  if (choices.length === 0) return null

  const byId = new Map(revealedCards(game, playerId).map((card) => [toCardContents(card).id, card]))
  const picks: PutBackPick[] = []

  for (let index = 0; index < choices.length; index += 1) {
    const choice = choices[index]
    if (choice.tag !== MessageType.TARGET_LABEL) return null
    if (choice.target.tag !== 'CardIdTarget') return null
    if (typeof choice.target.contents !== 'string') return null
    if (!runsPutOnTop(choice.messages)) return null

    const card = byId.get(choice.target.contents)
    if (!card) return null
    picks.push({ card, id: choice.target.contents, index })
  }

  return picks
}

/* The arrangement being built, as card ids in the order they were placed: the
 * first fills the bottom slot and the rest stack on top of it. That is also the
 * order it is answered in, since each choice puts its card on top of the deck.
 * Nothing is sent until it is submitted, so a card can be placed and taken back,
 * and the whole placement then goes out as one `OrderedAnswer` -- one action,
 * one undo step. */
export const putBackArrangement = ref<string[]>([])

/** True from the moment the arrangement is sent until the prompt goes away. */
export const putBackSubmitting = ref(false)

/** The cards this buffered state belongs to. A different set means a different
 * prompt, so nothing from the last one carries over. */
export const putBackSignature = ref('')

/** The ids on offer, order-independent: what identifies one prompt from another. */
export function putBackSignatureOf(picks: PutBackPick[]): string {
  return picks.map((pick) => pick.id).sort().join(',')
}

export function resetPutBack() {
  putBackArrangement.value = []
  putBackSignature.value = ''
  putBackSubmitting.value = false
}

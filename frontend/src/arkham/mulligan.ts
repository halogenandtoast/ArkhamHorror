import { ref } from 'vue'
import { MessageType } from '@/arkham/types/Message'
import * as ArkhamGame from '@/arkham/types/Game'
import type { Game } from '@/arkham/types/Game'
import { toCardContents, type Card as ArkhamCard } from '@/arkham/types/Card'

/* The mulligan is asked one card at a time: every card answered is discarded and
 * the same question comes back without it. Answering per click made the hand
 * flash on each round trip. The panel instead buffers the whole selection and
 * `advanceMulligan` (driven from the game view, which outlives the panel) sends
 * it as a quick chain of answers, without blanking the question in between, then
 * finishes with the "done" choice. */

export type MulliganPick = { card: ArkhamCard, id: string, index: number }

export type MulliganQuestion = {
  picks: MulliganPick[]
  doneIndex: number
}

function runs(messages: unknown[] | undefined, tag: string): boolean {
  return (messages ?? []).some((message) =>
    typeof message === 'object' && message !== null && (message as { tag?: unknown }).tag === tag
  )
}

/* The cards that can still be swapped plus the index of the "done" choice, or
 * null when the active question is not a mulligan. */
export function mulliganQuestion(game: Game, playerId: string): MulliganQuestion | null {
  const choices = ArkhamGame.choices(game, playerId)
  if (choices.length === 0) return null

  const investigator = Object.values(game.investigators).find((i) => i.playerId === playerId)
  if (!investigator) return null
  const hand = new Map(investigator.hand.map((card) => [toCardContents(card).id, card]))

  let doneIndex = -1
  const picks: MulliganPick[] = []

  for (let index = 0; index < choices.length; index += 1) {
    const choice = choices[index]
    if (choice.tag === MessageType.LABEL) {
      const messages = (choice as { messages?: unknown[] }).messages
      if (runs(messages, 'FinishedWithMulligan') || choice.label.includes('doneWithMulligan')) {
        doneIndex = index
        continue
      }
      return null
    }
    if (choice.tag !== MessageType.TARGET_LABEL) return null
    if (choice.target.tag !== 'CardIdTarget' || typeof choice.target.contents !== 'string') return null
    if (!runs(choice.messages, 'DiscardCard')) return null
    const card = hand.get(choice.target.contents)
    if (!card) return null
    picks.push({ card, id: choice.target.contents, index })
  }

  return doneIndex === -1 ? null : { picks, doneIndex }
}

/** Card ids the player has marked for replacement, in the order they were marked. */
export const mulliganSelected = ref<string[]>([])

/** True from the moment the selection is confirmed until the mulligan question goes away. */
export const mulliganRunning = ref(false)

/** Ids still to be discarded while running; the head is the next one to send. */
const queue = ref<string[]>([])

/** Signature of the question the last answer was sent against, to ignore echoes. */
let sentAgainst = ''

const signatureOf = (q: MulliganQuestion) => q.picks.map((p) => p.id).sort().join(',')

export function resetMulligan() {
  mulliganSelected.value = []
  mulliganRunning.value = false
  queue.value = []
  sentAgainst = ''
}

export function toggleMulligan(id: string) {
  if (mulliganRunning.value) return
  mulliganSelected.value = mulliganSelected.value.includes(id)
    ? mulliganSelected.value.filter((x) => x !== id)
    : [...mulliganSelected.value, id]
}

export function startMulligan() {
  if (mulliganRunning.value) return
  queue.value = [...mulliganSelected.value]
  mulliganRunning.value = true
  sentAgainst = ''
}

/* Sends the next answer of a confirmed mulligan, if one is due. `send` receives
 * the choice index. Safe to call on every question update: it only acts when a
 * mulligan is running and the question has moved on from the last answer. */
export function advanceMulligan(game: Game, playerId: string, send: (index: number) => void) {
  if (!mulliganRunning.value) return

  // Between answers the question can be momentarily empty; wait it out.
  if (ArkhamGame.choices(game, playerId).length === 0) return

  const q = mulliganQuestion(game, playerId)
  if (!q) return resetMulligan()

  const signature = signatureOf(q)
  if (signature === sentAgainst) return

  // Skip anything that is no longer on offer (e.g. an unswappable card).
  let next = queue.value[0]
  while (next !== undefined && !q.picks.some((p) => p.id === next)) {
    queue.value = queue.value.slice(1)
    next = queue.value[0]
  }

  sentAgainst = signature
  if (next === undefined) {
    send(q.doneIndex)
    return
  }
  queue.value = queue.value.slice(1)
  send(q.picks.find((p) => p.id === next)!.index)
}

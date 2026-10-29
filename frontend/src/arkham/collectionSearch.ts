import type { Game } from '@/arkham/types/Game'
import { choices } from '@/arkham/types/Game'
import type { Question } from '@/arkham/types/Question'

/** Detect collection searches so they render in their own modal, even during a skill test. */
export function isCollectionSearch(game: Game, playerId: string): boolean {
  let question: Question | undefined = game.question[playerId]
  let collectionSearch = false
  while (question) {
    if (question.tag === 'PayCostQuestion') return false
    if (question.tag === 'ChooseFromCollection') return question.cardCodes.length > 0
    if (question.label === '$questionFilter.collectionTitle') collectionSearch = true
    question = question.question
  }
  if (!collectionSearch) return false
  const offered = choices(game, playerId)
  return offered.length > 0 && offered.every((choice) => choice.tag === 'CardLabel')
}

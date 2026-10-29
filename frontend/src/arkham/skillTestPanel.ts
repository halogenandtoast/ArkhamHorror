import type { Game } from '@/arkham/types/Game'
import { choices } from '@/arkham/types/Game'
import type { Question } from '@/arkham/types/Question'

/** Fold test details only for an explicitly marked collection search.
 * The collection question remains visible outside the folded details. */
export function skillTestHasCollectionSearch(game: Game, playerId: string): boolean {
  if (!game.skillTest) return false
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

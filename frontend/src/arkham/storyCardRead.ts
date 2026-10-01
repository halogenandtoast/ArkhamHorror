import { MessageType } from '@/arkham/types/Message'
import * as ArkhamGame from '@/arkham/types/Game'
import type { Game } from '@/arkham/types/Game'
import { toCardContents, type Card as ArkhamCard } from '@/arkham/types/Card'

/* Being told to read a story card.
 *
 * `ReadStory` (Arkham/Game/Runner.hs) puts the card in focus and asks a
 * one-option prompt: a single `TargetLabel` on that card's id, whose messages
 * run `StoryMessage (ResolveStory ...)`. Like `PutBackInAnyOrder`, the question
 * carries no label of its own, so it is recognised by the message its choice
 * will run.
 *
 * That message is the whole discriminator. A plain flavour read -- a scenario
 * intro, interlude or resolution -- is a `Read` question with no card and no
 * choices of this shape, so it never matches and keeps its text spread.
 *
 * A story that visually replaces a board entity (an enemy, asset or location)
 * targets the story, not a card id, and focuses nothing: it does not match
 * either, and keeps being clicked in place.
 */
const RESOLVE_STORY = 'ResolveStory'

function isResolveStory(message: unknown): boolean {
  if (typeof message !== 'object' || message === null) return false
  const { tag, contents } = message as { tag?: unknown; contents?: unknown }
  if (tag === RESOLVE_STORY) return true
  // `Message`'s own encoding wraps it: {tag: StoryMessage, contents: {tag: ResolveStory}}.
  if (tag !== 'StoryMessage') return false
  if (typeof contents !== 'object' || contents === null) return false
  return (contents as { tag?: unknown }).tag === RESOLVE_STORY
}

export type StoryCardRead = { card: ArkhamCard; index: number }

/** The story card being read, or null when the active question is not one. */
export function storyCardRead(game: Game, playerId: string): StoryCardRead | null {
  const choices = ArkhamGame.choices(game, playerId)
  if (choices.length !== 1) return null

  const choice = choices[0]
  if (choice.tag !== MessageType.TARGET_LABEL) return null
  if (choice.target.tag !== 'CardIdTarget') return null
  if (typeof choice.target.contents !== 'string') return null
  if (!(choice.messages ?? []).some(isResolveStory)) return null

  const cardId = choice.target.contents
  const card = game.focusedCards.find((c) => toCardContents(c).id === cardId)
  return card ? { card, index: 0 } : null
}

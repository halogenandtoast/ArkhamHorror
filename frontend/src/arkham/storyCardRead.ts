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

export type StoryCardRead = { card: ArkhamCard; index: number; flip: boolean }

/* Whether this spotlight is a card turning over rather than a card being read.
 *
 * A two-part story is put aside at `Global` by its first part and resolved again
 * for its second, which is the moment it flips into whatever it becomes (a fate
 * card into an enemy or a location). Placement is the only thing that tells the
 * two apart: both are a focused card plus a one-option prompt, and the server
 * sends the same card either way. */
function flipsOver(game: Game, card: ArkhamCard): boolean {
  const placement = game.stories[toCardContents(card).cardCode]?.placement
  // `Global` has no tag of its own in the decoder: it arrives as the catch-all
  // with its constructor name as the contents, which is how Scenario.vue,
  // Player.vue and Enemy.vue all recognise it.
  return placement?.tag === 'OtherPlacement' && placement.contents === 'Global'
}

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
  return card ? { card, index: 0, flip: flipsOver(game, card) } : null
}

export type StoryCardChoice = { label: string; index: number }
export type StoryCardChoiceRead = { card: ArkhamCard; choices: StoryCardChoice[]; flip: boolean }

/* A story card that is asking something.
 *
 * A story whose card has been put on screen keeps it there for its own question
 * by wrapping the `chooseOne` in `focusCards`, so the choice is recognised by
 * that pairing: exactly one focused card, that card is a story in play, and
 * every choice is a plain label. A search popup focuses cards too, but it offers
 * card or target labels rather than text ones, so it never matches.
 */
export function storyCardChoiceRead(game: Game, playerId: string): StoryCardChoiceRead | null {
  const choices = ArkhamGame.choices(game, playerId)
  if (choices.length === 0) return null
  if (game.focusedCards.length !== 1) return null

  const card = game.focusedCards[0]
  // Story ids are card codes, so a focused card that is a story in play is one
  // lookup away.
  if (!(toCardContents(card).cardCode in game.stories)) return null

  const labels: StoryCardChoice[] = []
  for (const [index, choice] of choices.entries()) {
    if (choice.tag !== MessageType.LABEL) return null
    labels.push({ label: choice.label, index })
  }

  return { card, choices: labels, flip: flipsOver(game, card) }
}

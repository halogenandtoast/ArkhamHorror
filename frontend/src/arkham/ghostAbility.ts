import type { Game } from '@/arkham/types/Game'
import { MessageType, type Message } from '@/arkham/types/Message'

const GHOST_SOURCES = {
  TreacherySource: 'treacheries',
  AssetSource: 'assets',
  EnemySource: 'enemies',
  EventSource: 'events',
  SkillSource: 'skills',
  StorySource: 'stories',
} as const

/**
 * An ability whose source card has left play has nowhere on the board to carry its
 * button, so the choice modal has to open and show it on a ghost card -- Caught in the
 * Crossfire's later initiations resolve after it discards itself. #5743
 */
export function abilityNeedsGhostModal(game: Game, choice: Message): boolean {
  if (choice.tag !== MessageType.ABILITY_LABEL) return false
  const source = choice.ability.source
  if (!('contents' in source) || typeof source.contents !== 'string') return false
  const zone = GHOST_SOURCES[source.tag as keyof typeof GHOST_SOURCES]
  if (!zone) return false
  const entities = game[zone] as Record<string, unknown> | undefined
  return !entities || !(source.contents in entities)
}

/** The ghost card's image code for such an ability, if any. */
export function ghostCardCode(game: Game, choice: Message): string | null {
  if (!abilityNeedsGhostModal(game, choice)) return null
  return choice.tag === MessageType.ABILITY_LABEL ? choice.ability.cardCode : null
}

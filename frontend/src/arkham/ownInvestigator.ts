import type { Game } from '@/arkham/types/Game'
import type { Investigator } from '@/arkham/types/Investigator'

/** The investigator this player controls. Investigators are keyed by their own id, never by the player id. */
export function ownInvestigator(game: Game, playerId: string): Investigator | null {
  return Object.values(game.investigators).find((i) => i.playerId === playerId) ?? game.investigators[playerId] ?? null
}

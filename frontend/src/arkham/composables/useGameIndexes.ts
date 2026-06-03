import { computed, inject, type ComputedRef, type InjectionKey } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { Investigator } from '@/arkham/types/Investigator'

export interface GameIndexes {
  investigatorByPlayerId: ReadonlyMap<string, Investigator>
  storyIdsByLocation: ReadonlyMap<string, readonly string[]>
}

export const gameIndexesKey: InjectionKey<ComputedRef<GameIndexes>> = Symbol('gameIndexes')

const emptyIndexes: GameIndexes = {
  investigatorByPlayerId: new Map(),
  storyIdsByLocation: new Map(),
}

export function buildGameIndexes(game: Game | null): GameIndexes {
  if (!game) return emptyIndexes

  const investigatorByPlayerId = new Map<string, Investigator>()
  for (const investigator of Object.values(game.investigators)) {
    investigatorByPlayerId.set(investigator.playerId, investigator)
  }

  const enemyIds = new Set(Object.keys(game.enemies))
  const assetIds = new Set(Object.keys(game.assets))
  const storyIdsByLocation = new Map<string, string[]>()

  for (const story of Object.values(game.stories)) {
    const otherSide = story.otherSide?.contents
    if (typeof otherSide === 'string' && (enemyIds.has(otherSide) || assetIds.has(otherSide))) continue
    if (story.placement.tag !== 'AtLocation') continue
    if (story.otherSide?.contents === story.placement.contents) continue

    const stories = storyIdsByLocation.get(story.placement.contents)
    if (stories) {
      stories.push(story.id)
    } else {
      storyIdsByLocation.set(story.placement.contents, [story.id])
    }
  }

  return { investigatorByPlayerId, storyIdsByLocation }
}

export function useGameIndexes(game?: () => Game | null): ComputedRef<GameIndexes> {
  const provided = inject(gameIndexesKey, null)
  if (provided) return provided
  return computed(() => buildGameIndexes(game?.() ?? null))
}

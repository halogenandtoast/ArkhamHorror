import { computed, inject, type ComputedRef, type InjectionKey } from 'vue'
import * as ArkhamGame from '@/arkham/types/Game'
import type { Game } from '@/arkham/types/Game'
import type { Message } from '@/arkham/types/Message'
import type { Source } from '@/arkham/types/Source'

export const choicesByPlayerKey: InjectionKey<ComputedRef<ReadonlyMap<string, readonly Message[]>>> = Symbol('choicesByPlayer')
export const choicesSourceByPlayerKey: InjectionKey<ComputedRef<ReadonlyMap<string, Source | null>>> = Symbol('choicesSourceByPlayer')
export const choicesTooltipByPlayerKey: InjectionKey<ComputedRef<ReadonlyMap<string, string | null>>> = Symbol('choicesTooltipByPlayer')

export function useGameChoices(game: () => Game, playerId: () => string): ComputedRef<readonly Message[]> {
  const choicesByPlayer = inject(choicesByPlayerKey, null)
  return computed(() => choicesByPlayer?.value.get(playerId()) ?? ArkhamGame.choices(game(), playerId()))
}

export function useGameChoicesSource(game: () => Game, playerId: () => string): ComputedRef<Source | null> {
  const sourceByPlayer = inject(choicesSourceByPlayerKey, null)
  return computed(() => sourceByPlayer?.value.get(playerId()) ?? ArkhamGame.choicesSource(game(), playerId()))
}

export function useGameChoicesTooltip(game: () => Game, playerId: () => string): ComputedRef<string | null> {
  const tooltipByPlayer = inject(choicesTooltipByPlayerKey, null)
  return computed(() => tooltipByPlayer?.value.get(playerId()) ?? ArkhamGame.choicesTooltip(game(), playerId()))
}

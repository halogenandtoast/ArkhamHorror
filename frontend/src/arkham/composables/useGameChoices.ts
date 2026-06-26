import { computed, inject, ref, watch, onScopeDispose, type ComputedRef, type InjectionKey, type Ref } from 'vue'
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

// Like useGameChoicesSource, but holds the last source through brief gaps where
// the client clears `question` between steps (game "lock"). This keeps the
// source/actor highlight from blinking out and back when the source is unchanged.
export function useStickyChoicesSource(game: () => Game, playerId: () => string, holdMs = 600): Ref<Source | null> {
  const source = useGameChoicesSource(game, playerId)
  const sticky = ref<Source | null>(source.value)
  let timer: ReturnType<typeof setTimeout> | null = null

  watch(source, (next) => {
    if (next) {
      if (timer) { clearTimeout(timer); timer = null }
      sticky.value = next
    } else if (!timer) {
      timer = setTimeout(() => { sticky.value = null; timer = null }, holdMs)
    }
  })

  onScopeDispose(() => { if (timer) clearTimeout(timer) })
  return sticky
}

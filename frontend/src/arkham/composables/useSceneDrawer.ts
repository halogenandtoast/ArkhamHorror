import { useStorage } from '@vueuse/core'
import { computed, type Ref } from 'vue'
import { gameLocalStorageKey } from '@/arkham/localStorage'

/** Whether the play/threat area drawer of the scene view is open (remembered per game). */
export function useSceneDrawer(gameId: () => string): Ref<boolean> {
  const key = computed(() => gameLocalStorageKey(gameId(), 'scene-drawer'))
  return useStorage<boolean>(key, false)
}

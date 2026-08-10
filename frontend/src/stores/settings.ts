import { defineStore } from "pinia"
import { computed, ref } from "vue"
import { getGameLocalStorageItem, setGameLocalStorageItem } from '@/arkham/localStorage'
import { isDevBuild } from '@/arkham/displayRules'

const EPIC_MULTIPLAYER_KEY = 'epicMultiplayerEnabled'

export const useSettings = defineStore("settings", () => {
  const gameId = ref<string | null>(null)
  const splitView = ref(false)

  // Dev-only feature flag for Epic Multiplayer. Stored in localStorage, but
  // exposed as `isDevBuild() && stored` so a stale value can never enable it in
  // production builds.
  const epicMultiplayerStored = ref(localStorage.getItem(EPIC_MULTIPLAYER_KEY) === 'true')
  const epicMultiplayerEnabled = computed(() => isDevBuild() && epicMultiplayerStored.value)

  function setEpicMultiplayerEnabled(enabled: boolean) {
    epicMultiplayerStored.value = enabled
    localStorage.setItem(EPIC_MULTIPLAYER_KEY, String(enabled))
  }

  function toggleEpicMultiplayer() {
    setEpicMultiplayerEnabled(!epicMultiplayerStored.value)
  }

  function setGameId(id: string) {
    gameId.value = id
    const saved = getGameLocalStorageItem(id, 'splitView')
    splitView.value = saved === 'true'
  }

  function toggleSplitView() {
    splitView.value = !splitView.value
    if (gameId.value) {
      setGameLocalStorageItem(gameId.value, 'splitView', String(splitView.value))
    }
  }

  const showBonded = ref(false)

  function toggleShowBonded() {
    showBonded.value = !showBonded.value
  }
  return {
    splitView,
    toggleSplitView,
    showBonded,
    toggleShowBonded,
    setGameId,
    epicMultiplayerStored,
    epicMultiplayerEnabled,
    setEpicMultiplayerEnabled,
    toggleEpicMultiplayer,
  }
})

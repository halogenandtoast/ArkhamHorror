import { defineStore } from "pinia"
import { computed, ref } from "vue"
import { getGameLocalStorageItem, setGameLocalStorageItem } from '@/arkham/localStorage'
import { isDevBuild } from '@/arkham/displayRules'

const EPIC_MULTIPLAYER_KEY = 'epicMultiplayerEnabled'
const AI_INVESTIGATORS_KEY = 'aiInvestigatorsEnabled'

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

  // Dev-only feature flag for AI Investigators. Same shape as Epic Multiplayer:
  // stored in localStorage but exposed as `isDevBuild() && stored` so a stale
  // value can never enable it in production builds. WIP / does not work yet.
  const aiInvestigatorsStored = ref(localStorage.getItem(AI_INVESTIGATORS_KEY) === 'true')
  const aiInvestigatorsEnabled = computed(() => isDevBuild() && aiInvestigatorsStored.value)

  function setAiInvestigatorsEnabled(enabled: boolean) {
    aiInvestigatorsStored.value = enabled
    localStorage.setItem(AI_INVESTIGATORS_KEY, String(enabled))
  }

  function toggleAiInvestigators() {
    setAiInvestigatorsEnabled(!aiInvestigatorsStored.value)
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
    aiInvestigatorsStored,
    aiInvestigatorsEnabled,
    setAiInvestigatorsEnabled,
    toggleAiInvestigators,
  }
})

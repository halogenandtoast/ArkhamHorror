import { defineStore } from "pinia"
import { ref } from "vue"
import { getGameLocalStorageItem, setGameLocalStorageItem } from '@/arkham/localStorage'

export const useSettings = defineStore("settings", () => {
  const gameId = ref<string | null>(null)
  const splitView = ref(false)

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
  return { splitView, toggleSplitView, showBonded, toggleShowBonded, setGameId }
})

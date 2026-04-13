import { defineStore } from "pinia"
import { ref } from "vue"

export const useSettings = defineStore("settings", () => {
  const gameId = ref<string | null>(null)
  const splitView = ref(false)

  function setGameId(id: string) {
    gameId.value = id
    const saved = localStorage.getItem(`game:${id}:splitView`)
    splitView.value = saved === 'true'
  }

  function toggleSplitView() {
    splitView.value = !splitView.value
    if (gameId.value) {
      localStorage.setItem(`game:${gameId.value}:splitView`, String(splitView.value))
    }
  }

  const showBonded = ref(false)

  function toggleShowBonded() {
    showBonded.value = !showBonded.value
  }
  return { splitView, toggleSplitView, showBonded, toggleShowBonded, setGameId }
})

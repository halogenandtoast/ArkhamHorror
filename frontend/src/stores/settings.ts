import { defineStore } from "pinia"
import { ref } from "vue"

export const useSettings = defineStore("settings", () => {
  const splitView = ref(false)

  function toggleSplitView() {
    splitView.value = !splitView.value
  }

  const showBonded = ref(false)

  function toggleShowBonded() {
    showBonded.value = !showBonded.value
  }
  return { splitView, toggleSplitView, showBonded, toggleShowBonded }
})

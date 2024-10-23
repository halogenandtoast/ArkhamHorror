import { defineStore } from "pinia"
import { ref } from "vue"

export const useSettings = defineStore("settings", () => {
  const splitView = ref(false)

  function toggleSplitView() {
    splitView.value = !splitView.value
  }
  return { splitView, toggleSplitView }
})

import { reactive } from 'vue'

// Global, client-side master switch for the AI-investigator driver in Game.vue.
// Modeled on `useDebug()` (see debug.ts). `enabled` defaults to true so AI seats
// auto-drive as soon as a game with AI seats loads; the creator can `toggle()` to
// pause every AI seat at once (per-seat enable also exists via the backend
// `aiEnabled` flag / `SetAiEnabled`). This is purely a client kill-switch and has
// no effect when a game has no AI seats.
const ai = reactive({
  enabled: true,
  toggle: () => {
    ai.enabled = !ai.enabled
  },
})

export function useAi() {
  return ai
}

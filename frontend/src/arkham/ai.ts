import { reactive } from 'vue'

// Global, client-side master switch for the AI-investigator driver in Game.vue.
// Modeled on `useDebug()` (see debug.ts). `enabled` defaults to true so AI seats
// auto-drive as soon as a game with AI seats loads; the creator can `toggle()` to
// pause every AI seat at once (per-seat enable also exists via the backend
// `aiEnabled` flag / `SetAiEnabled`). This is purely a client kill-switch and has
// no effect when a game has no AI seats.
//
// `targeting` powers the dev-only "AI targeting mode" (see AiControlPanel): while
// on, hovering any board entity highlights it green and clicking opens a small
// directive menu (AiTargetMenu) that sets `selectedSeat`'s AI priority/focus.
// Normal play is untouched while `targeting` is off.
const ai = reactive({
  enabled: true,
  targeting: false,
  selectedSeat: null as string | null,
  toggle: () => {
    ai.enabled = !ai.enabled
  },
  setTargeting: (on: boolean, seat?: string | null) => {
    ai.targeting = on
    if (seat !== undefined) ai.selectedSeat = seat
  },
  stopTargeting: () => {
    ai.targeting = false
  },
})

export function useAi() {
  return ai
}

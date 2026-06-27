import { reactive } from 'vue'
import type { AiQuestion, StampedAiQuestion } from '@/arkham/types/AiQuestion'

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
//
// `questions` is the dev-only "AI asks questions" store. Each entry is stamped
// with the `scenarioSteps` version it arrived at so stale questions (undo, or
// advancing past their window) can be dropped. AI-target questions are
// auto-resolved by Game.vue and never linger here; human-target questions render
// in the non-blocking AiQuestionsPanel until answered or dismissed.
const ai = reactive({
  enabled: true,
  targeting: false,
  selectedSeat: null as string | null,
  questions: [] as StampedAiQuestion[],
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
  // Merge a fetched snapshot into the store, deduping by `id` (a re-fetch of the
  // same window must not double-list a question) and stamping each with the
  // version it arrived at. An incoming question replaces any existing one with
  // the same id (re-stamping it to the current version).
  mergeQuestions: (qs: AiQuestion[], version: number) => {
    const byId = new Map<string, StampedAiQuestion>()
    for (const q of ai.questions) byId.set(q.id, q)
    for (const q of qs) byId.set(q.id, { ...q, version })
    ai.questions = [...byId.values()]
  },
  dismissQuestion: (id: string) => {
    ai.questions = ai.questions.filter((q) => q.id !== id)
  },
  clearQuestions: () => {
    ai.questions = []
  },
  // Drop questions stamped at a version older than the current game state.
  clearStale: (currentVersion: number) => {
    ai.questions = ai.questions.filter((q) => q.version >= currentVersion)
  },
})

export function useAi() {
  return ai
}

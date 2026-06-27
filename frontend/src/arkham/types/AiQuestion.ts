// Non-blocking "AI asks questions" feature (dev-only). The backend exposes a
// snapshot of pending AI-originated questions over a plain GET; each option
// carries one or more RAW config Messages that are applied by replaying them on
// the debug/raw channel (`useDebug().send`), exactly like AiControlPanel sends
// AddAiPriority / SetAiFocusOverride.

// A raw config Message as the backend serializes it (tag + optional contents).
// `contents` is intentionally `unknown` — its shape varies per message tag and
// is only ever round-tripped back to the backend untouched.
export type AiMessage = {
  tag: string
  contents?: unknown
}

export type AiQuestionOption = {
  label: string
  messages: AiMessage[]
}

export type AiQuestion = {
  id: string
  kind: string
  fromPlayer: string
  fromInvestigator: string
  fromName: string
  toPlayer: string
  toInvestigator: string
  toName: string
  // When the target seat is itself an AI, the question is auto-resolved client
  // side using `aiAnswer` rather than rendered to a human.
  toIsAi: boolean
  prompt: string
  options: AiQuestionOption[]
  // The AI's chosen option index (into `options`) when the target is an AI, or
  // `null` when there is no precomputed answer.
  aiAnswer: number | null
}

// A question plus the `scenarioSteps` version it arrived at, so the store can
// drop questions that predate the current game state (undo / advancing past the
// window they belonged to).
export type StampedAiQuestion = AiQuestion & { version: number }

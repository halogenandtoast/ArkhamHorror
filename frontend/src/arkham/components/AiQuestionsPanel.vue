<script lang="ts" setup>
import { computed } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { AiQuestion, AiQuestionOption } from '@/arkham/types/AiQuestion'
import { useAi } from '@/arkham/ai'
import { useDebug } from '@/arkham/debug'

// Dev-only, NON-BLOCKING "AI asks questions" panel. Mounted by Game.vue behind
// the "AI Investigators" settings flag (+ presence of AI seats), pinned
// bottom-LEFT so it never overlaps AiControlPanel (bottom-right). It only renders
// human-target questions; AI-target ones are auto-resolved in Game.vue and never
// reach the store. Answering an option replays its RAW config Messages over the
// debug channel (the same path AiControlPanel uses) and dismisses the question.
const props = defineProps<{ game: Game }>()

const ai = useAi()
const debug = useDebug()

// Only human-target questions land here; AI-target ones are auto-resolved in
// Game.vue and removed before they would show.
const questions = computed<AiQuestion[]>(() => ai.questions.filter((q) => !q.toIsAi))

function answer(question: AiQuestion, option: AiQuestionOption) {
  for (const message of option.messages) {
    debug.send(props.game.id, message)
  }
  ai.dismissQuestion(question.id)
}

function dismiss(question: AiQuestion) {
  ai.dismissQuestion(question.id)
}
</script>

<template>
  <div v-if="questions.length > 0" class="ai-questions-panel">
    <div class="ai-questions-header">
      <span class="ai-questions-title">AI Questions <span class="ai-dev-pill">dev</span></span>
    </div>
    <div class="ai-questions-body">
      <div v-for="question in questions" :key="question.id" class="ai-question-card">
        <button
          type="button"
          class="ai-question-dismiss"
          title="Dismiss without answering"
          @click="dismiss(question)"
        >
          ×
        </button>
        <div class="ai-question-from">{{ question.fromName }} → {{ question.toName }}</div>
        <div class="ai-question-prompt">{{ question.prompt }}</div>
        <div class="ai-question-options">
          <button
            v-for="(option, idx) in question.options"
            :key="idx"
            type="button"
            class="ai-question-option"
            @click="answer(question, option)"
          >
            {{ option.label }}
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* Non-blocking: the wrapper has pointer-events:none so it never intercepts board
   clicks; only the actual question cards re-enable pointer events. No backdrop,
   no modal — play continues underneath. */
.ai-questions-panel {
  position: fixed;
  left: 12px;
  bottom: 12px;
  z-index: 9000;
  width: 320px;
  max-height: 70vh;
  overflow: auto;
  pointer-events: none;
  color: #fff;
  font-size: 12px;
}

.ai-questions-header {
  position: sticky;
  top: 0;
  display: flex;
  align-items: center;
  padding: 6px 4px;
  pointer-events: none;
}

.ai-questions-title {
  display: flex;
  align-items: center;
  gap: 6px;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  font-size: 11px;
  color: rgba(255, 255, 255, 0.85);
  text-shadow: 0 1px 3px rgba(0, 0, 0, 0.8);
}

.ai-dev-pill {
  font-size: 9px;
  letter-spacing: 0.06em;
  padding: 1px 5px;
  border-radius: 999px;
  border: 1px solid rgba(184, 134, 11, 0.55);
  background: rgba(184, 134, 11, 0.25);
  color: rgba(255, 226, 154, 0.95);
}

.ai-questions-body {
  display: grid;
  gap: 8px;
  padding: 2px;
}

.ai-question-card {
  position: relative;
  pointer-events: auto;
  border-radius: 12px;
  border: 1px solid rgba(255, 255, 255, 0.12);
  background: rgba(12, 14, 16, 0.92);
  box-shadow: 0 12px 30px rgba(0, 0, 0, 0.5);
  backdrop-filter: blur(6px);
  padding: 10px 10px 8px;
  display: grid;
  gap: 6px;
}

.ai-question-dismiss {
  position: absolute;
  top: 4px;
  right: 6px;
  border: none;
  background: none;
  color: rgba(255, 255, 255, 0.55);
  cursor: pointer;
  font-size: 16px;
  line-height: 1;
  padding: 0 2px;
}

.ai-question-dismiss:hover {
  color: #fff;
}

.ai-question-from {
  font-size: 10px;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.5);
  padding-right: 16px;
}

.ai-question-prompt {
  color: rgba(255, 255, 255, 0.92);
  line-height: 1.35;
}

.ai-question-options {
  display: grid;
  gap: 5px;
  margin-top: 2px;
}

.ai-question-option {
  width: 100%;
  text-align: left;
  border: 1px solid rgba(127, 184, 212, 0.6);
  background: rgba(127, 184, 212, 0.22);
  color: #cfe7f4;
  border-radius: 6px;
  padding: 6px 8px;
  cursor: pointer;
  font-size: 11px;
}

.ai-question-option:hover {
  background: rgba(127, 184, 212, 0.4);
}
</style>

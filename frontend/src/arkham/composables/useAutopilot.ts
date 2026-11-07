import { onBeforeUnmount, ref, watch, type Ref } from 'vue'
import type { Question } from '@/arkham/types/Question'
import type { Message } from '@/arkham/types/Message'
import { decideAutoAnswer, AUTO_ANSWER_DELAY_MS, type AutoAnswer } from '@/arkham/autoAnswerPolicy'

/* Answers the engine's non-questions (see autoAnswerPolicy) for the seat this
 * client is looking at, so phases run on without a click per step.
 *
 * Safety rails:
 * - only while nothing else is in flight or on screen (`blocked`);
 * - one submit per question: the step counter plus the chosen index must change
 *   before it answers again, so a slow server never gets a double answer;
 * - after an undo it stays out of the way until the game moves forward again,
 *   otherwise the restored question would be re-answered at once and undo would
 *   look broken;
 * - a run cap, in case some card ever loops a lone choice forever. */

const MAX_CONSECUTIVE_AUTO_ANSWERS = 60

export interface AutopilotOptions {
  enabled: Readonly<Ref<boolean>>
  question: Readonly<Ref<Question | null | undefined>>
  choices: Readonly<Ref<readonly Message[]>>
  /* The engine's step counter; it grows with every resolved answer. */
  step: Readonly<Ref<number | undefined>>
  /* True while something must not be answered over: a request in flight, an
   * overlay the player has to see, spectating, another seat's question. */
  blocked: Readonly<Ref<boolean>>
  hasRevealedCards: Readonly<Ref<boolean>>
  submit: (index: number) => void
  onCapReached?: () => void
}

export interface Autopilot {
  suspendUntilGameMoves: () => void
}

export function useAutopilot(options: AutopilotOptions): Autopilot {
  const { enabled, question, choices, step, blocked, hasRevealedCards, submit } = options

  let timer: ReturnType<typeof setTimeout> | null = null
  let lastSubmittedKey: string | null = null
  let consecutive = 0
  const suspendedAtStep = ref<number | null>(null)

  function clearTimer() {
    if (timer) clearTimeout(timer)
    timer = null
  }

  function currentDecision(): { answer: AutoAnswer; key: string } | null {
    if (!enabled.value || blocked.value || suspendedAtStep.value !== null) return null
    const answer = decideAutoAnswer(question.value, choices.value, {
      hasRevealedCards: hasRevealedCards.value,
    })
    if (!answer) return null
    return { answer, key: `${step.value ?? -1}:${answer.index}:${choices.value.length}` }
  }

  function schedule() {
    clearTimer()
    const decision = currentDecision()
    if (!decision) {
      // A question the player has to answer ends a run of automatic answers.
      if (question.value && !blocked.value) consecutive = 0
      return
    }
    if (decision.key === lastSubmittedKey) return
    if (consecutive >= MAX_CONSECUTIVE_AUTO_ANSWERS) {
      options.onCapReached?.()
      return
    }
    timer = setTimeout(() => {
      timer = null
      // Re-check: the board may have changed during the pause.
      const again = currentDecision()
      if (!again || again.key !== decision.key) return
      lastSubmittedKey = again.key
      consecutive += 1
      submit(again.answer.index)
    }, AUTO_ANSWER_DELAY_MS[decision.answer.reason])
  }

  watch([enabled, question, choices, step, blocked, hasRevealedCards, suspendedAtStep], schedule, {
    immediate: true,
  })

  // Lift the undo suspension once the game has moved past where the undo left it.
  watch(step, (now) => {
    const since = suspendedAtStep.value
    if (since === null || now === undefined) return
    if (since === -1) {
      suspendedAtStep.value = now
      return
    }
    if (now > since) suspendedAtStep.value = null
  })

  watch(enabled, (on) => {
    if (on) consecutive = 0
  })

  onBeforeUnmount(clearTimer)

  return {
    suspendUntilGameMoves() {
      clearTimer()
      lastSubmittedKey = null
      consecutive = 0
      // -1 until the undone state arrives; its step becomes the baseline.
      suspendedAtStep.value = -1
    },
  }
}

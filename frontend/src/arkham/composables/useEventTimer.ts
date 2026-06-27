import { computed } from 'vue'
import { storeToRefs } from 'pinia'
import { useTimestamp } from '@vueuse/core'
import { useEventStore } from '@/arkham/stores/event'
import { counterValue, TIME_LIMIT_MINUTES, TIMER_STARTED_AT } from '@/arkham/types/EpicEvent'

// Epic Multiplayer time-limit clock. Every value is derived reactively from the
// event store's sharedState (kept live by the SharedStateUpdate feed riding both
// the event websocket and each group's game websocket) plus a one-second wall
// clock. This is a pure read model with NO side effects: callers that need to fire
// the time-up endpoint own their own guarded watcher (see Game.vue /
// OrganizerDashboard.vue) so the firing path can't be duplicated by the display
// components that also read this clock.
export function useEventTimer() {
  const store = useEventStore()
  const { sharedState } = storeToRefs(store)
  // ms; ticks once a second and auto-cleans on unmount.
  const nowMs = useTimestamp({ interval: 1000 })

  // Configured limit in minutes; 0 means "no time limit / no countdown".
  const timeLimitMinutes = computed(() => counterValue(sharedState.value, TIME_LIMIT_MINUTES))
  // Epoch SECONDS the countdown started; 0 until the start barrier releases.
  const timerStartedAt = computed(() => counterValue(sharedState.value, TIMER_STARTED_AT))

  const hasTimeLimit = computed(() => timeLimitMinutes.value > 0)
  // A limit is set, but not every group has reached the start barrier yet.
  const barrierPending = computed(() => hasTimeLimit.value && timerStartedAt.value === 0)
  // The countdown is running.
  const countdownActive = computed(() => hasTimeLimit.value && timerStartedAt.value > 0)

  const remainingSeconds = computed<number | null>(() => {
    if (!countdownActive.value) return null
    const deadline = timerStartedAt.value + timeLimitMinutes.value * 60
    return Math.max(0, deadline - Math.floor(nowMs.value / 1000))
  })

  const timeUp = computed(
    () => countdownActive.value && remainingSeconds.value !== null && remainingSeconds.value <= 0,
  )

  // H:MM:SS once we're past an hour, otherwise MM:SS.
  const formatted = computed(() => {
    const total = remainingSeconds.value
    if (total === null) return ''
    const h = Math.floor(total / 3600)
    const m = Math.floor((total % 3600) / 60)
    const s = total % 60
    const mm = String(m).padStart(2, '0')
    const ss = String(s).padStart(2, '0')
    return h > 0 ? `${h}:${mm}:${ss}` : `${mm}:${ss}`
  })

  return {
    timeLimitMinutes,
    timerStartedAt,
    hasTimeLimit,
    barrierPending,
    countdownActive,
    remainingSeconds,
    timeUp,
    formatted,
  }
}

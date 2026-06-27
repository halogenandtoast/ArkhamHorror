<script lang="ts" setup>
import { computed } from 'vue'
import { useEventTimer } from '@/arkham/composables/useEventTimer'

// Shared countdown chip for the Epic Multiplayer time limit. Reads everything off
// the event store via useEventTimer, so it can be dropped into any surface that
// has the event loaded (OrganizerBar, PlayerEventBar, OrganizerDashboard). Renders
// nothing until the countdown is actually running.
const { countdownActive, formatted, remainingSeconds } = useEventTimer()

// Light up the last five minutes so players notice the clock is nearly out.
const urgent = computed(() => (remainingSeconds.value ?? Number.POSITIVE_INFINITY) <= 300)
</script>

<template>
  <div v-if="countdownActive" class="event-countdown" :class="{ urgent }">
    <span class="countdown-label">{{ $t('event.timeRemaining') }}</span>
    <span class="countdown-value">{{ formatted }}</span>
  </div>
</template>

<style scoped>
.event-countdown {
  display: flex;
  flex-direction: column;
  align-items: center;
  line-height: 1.1;
}

.countdown-label {
  text-transform: uppercase;
  letter-spacing: 0.05em;
  font-size: 0.62em;
  opacity: 0.65;
  white-space: nowrap;
}

.countdown-value {
  font-size: 1.3em;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
}

.event-countdown.urgent .countdown-value,
.event-countdown.urgent .countdown-label {
  color: var(--important, #d8a657);
  opacity: 1;
}
</style>

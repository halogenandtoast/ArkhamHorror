<script lang="ts" setup>
import { computed } from 'vue'
import { storeToRefs } from 'pinia'
import { useEventStore } from '@/arkham/stores/event'
import {
  actProgressValue,
  counterValue,
  hasActProgress,
  pendingActAdvance,
  TOTAL_INVESTIGATORS,
} from '@/arkham/types/EpicEvent'

// Shared-pool readouts for the epic bars: the global clue progress toward the
// viewed group's current act threshold. Kept deliberately general so a future
// shared doom pool can slot in alongside; only the act-clue pool is populated now.
// `currentActStage` is the sequence number (1/2/3) of the act in play, passed down
// from Game.vue which holds the game state.
const props = defineProps<{ currentActStage: number | null }>()

const store = useEventStore()
const { sharedState } = storeToRefs(store)

// Prefer the shared counter (per the contract); fall back to the decoded field.
const totalInvestigators = computed(
  () => counterValue(sharedState.value, TOTAL_INVESTIGATORS) || sharedState.value.sharedTotalInvestigators,
)

// Requirement for the current cycle. Guards divide-by-zero downstream.
const threshold = computed(() => 2 * totalInvestigators.value)

// The current act participates in the shared-clue pool only when a counter exists
// for its stage (Blob acts 1 & 3, not act 2) and we have a positive threshold.
const sharedClueActive = computed(
  () =>
    props.currentActStage !== null &&
    hasActProgress(sharedState.value, props.currentActStage) &&
    threshold.value > 0,
)

// Cumulative progress wrapped into the current cycle: 0 at the start of each cycle.
const sharedClues = computed(() => {
  if (!sharedClueActive.value || props.currentActStage === null) return 0
  return actProgressValue(sharedState.value, props.currentActStage) % threshold.value
})

// Soft indicator for players/spectators: the pool exceeded the threshold and the
// organizer must allocate which groups spend. No hard block — just a heads-up.
const awaitingOrganizer = computed(
  () =>
    props.currentActStage !== null &&
    pendingActAdvance(sharedState.value, props.currentActStage) > 0,
)
</script>

<template>
  <div v-if="sharedClueActive || awaitingOrganizer" class="shared-pools">
    <div v-if="sharedClueActive" class="shared-pool">
      <span class="pool-label">{{ $t('event.sharedClues') }}</span>
      <span class="pool-value">{{ sharedClues }} / {{ threshold }}</span>
    </div>
    <div v-if="awaitingOrganizer" class="awaiting-organizer" :title="$t('event.awaitingOrganizer')">
      <span class="awaiting-dot" aria-hidden="true"></span>
      <span class="awaiting-text">{{ $t('event.awaitingOrganizer') }}</span>
    </div>
  </div>
</template>

<style scoped>
.shared-pools {
  display: flex;
  align-items: center;
  gap: 18px;
}

.shared-pool {
  display: flex;
  flex-direction: column;
  align-items: center;
  line-height: 1.1;
}

.pool-label {
  text-transform: uppercase;
  letter-spacing: 0.05em;
  font-size: 0.62em;
  opacity: 0.65;
  white-space: nowrap;
}

.pool-value {
  font-size: 1.3em;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
}

.awaiting-organizer {
  display: flex;
  align-items: center;
  gap: 6px;
  max-width: 180px;
  color: var(--important, #d8a657);
  font-size: 0.72em;
  line-height: 1.05;
}

.awaiting-dot {
  flex: 0 0 auto;
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--important, #d8a657);
  animation: awaiting-pulse 1.4s ease-in-out infinite;
}

.awaiting-text {
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

@keyframes awaiting-pulse {
  0%, 100% { opacity: 0.4; }
  50% { opacity: 1; }
}
</style>

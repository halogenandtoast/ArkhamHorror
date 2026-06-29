<script lang="ts" setup>
import { computed } from 'vue'
import { storeToRefs } from 'pinia'
import { useEventStore } from '@/arkham/stores/event'
import {
  actProgressValue,
  counterValue,
  hasActProgress,
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

// The shared pool is per-cycle (the backend zeroes act-progress:N on advance), so
// its raw value IS the current progress toward the threshold. Cap the readout at
// the threshold so it reads cleanly (e.g. 0/Y right after an advance).
const sharedClues = computed(() => {
  if (!sharedClueActive.value || props.currentActStage === null) return 0
  return Math.min(actProgressValue(sharedState.value, props.currentActStage), threshold.value)
})
</script>

<template>
  <div v-if="sharedClueActive" class="shared-pools">
    <div class="shared-pool">
      <span class="pool-label">{{ $t('event.sharedClues') }}</span>
      <span class="pool-value">{{ sharedClues }} / {{ threshold }}</span>
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
</style>

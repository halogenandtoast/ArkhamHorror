<script lang="ts" setup>
import { computed } from 'vue'
import { storeToRefs } from 'pinia'
import { useEventStore } from '@/arkham/stores/event'
import { COUNTERMEASURES, counterValue } from '@/arkham/types/EpicEvent'

// Read-only in-game widget. Driven by the event store, which is fed
// SharedStateUpdate messages off this group's own game websocket (see Game.vue).
// Renders nothing until a shared-state update has been received, so ordinary
// (non-event) games are completely unaffected.
const store = useEventStore()
const { sharedState, received } = storeToRefs(store)

const countermeasures = computed(() => counterValue(sharedState.value, COUNTERMEASURES))
const totalInvestigators = computed(() => sharedState.value.sharedTotalInvestigators)
</script>

<template>
  <div v-if="received" class="shared-state-panel">
    <div class="metric">
      <span class="label">{{ $t('event.countermeasures') }}</span>
      <span class="value">{{ countermeasures }}</span>
    </div>
    <div class="metric">
      <span class="label">{{ $t('event.totalInvestigators') }}</span>
      <span class="value">{{ totalInvestigators }}</span>
    </div>
  </div>
</template>

<style scoped>
.shared-state-panel {
  position: fixed;
  bottom: 12px;
  left: 12px;
  z-index: 50;
  display: flex;
  gap: 16px;
  padding: 8px 14px;
  border-radius: 8px;
  background: rgba(0, 0, 0, 0.6);
  border: 1px solid var(--box-border, rgba(255, 255, 255, 0.15));
  color: #fff;
  font-size: 0.85em;
  pointer-events: none;
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.35);
}

.metric {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.metric .label {
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 0.7em;
  opacity: 0.7;
}

.metric .value {
  font-size: 1.4em;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
}
</style>

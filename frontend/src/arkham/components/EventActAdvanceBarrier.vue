<script lang="ts" setup>
// Blocking overlay shown over a parked group's board while the Epic Multiplayer act
// advance is awaiting the organizer's clue allocation (`awaiting-organizer:<stage>`).
// Purely presentational — Game.vue owns the show condition; when the gate clears the
// overlay simply lifts (like EventStartBarrier), surfacing the group's parked
// "Continue" question for the player to click. Sits above the board but below the
// event bars, so a waiting player (or the organizer) can still navigate.
//
// For the ORGANIZER (organizerEventId set) we surface a direct call-to-action to the
// dashboard, where the allocation happens — otherwise the organizer has no prompt
// telling them where to go. requestDashboardHub() suppresses the dashboard's
// frictionless redirect-back-to-seat for that one explicit navigation.
import { useEventStore } from '@/arkham/stores/event'

defineProps<{ organizerEventId?: string | null }>()

const store = useEventStore()
</script>

<template>
  <div class="event-act-advance-barrier" role="status" aria-live="polite">
    <div class="barrier-panel">
      <div class="barrier-spinner" aria-hidden="true"></div>
      <p class="barrier-message">{{ $t('event.awaitingOrganizer') }}</p>
      <RouterLink
        v-if="organizerEventId"
        class="barrier-dashboard-link"
        :to="`/events/${organizerEventId}`"
        @click="store.requestDashboardHub()"
      >
        {{ $t('event.openDashboardToAllocate') }}
      </RouterLink>
    </div>
  </div>
</template>

<style scoped>
.event-act-advance-barrier {
  position: fixed;
  inset: 0;
  z-index: 900;
  display: flex;
  align-items: center;
  justify-content: center;
  background: rgba(8, 10, 16, 0.82);
  backdrop-filter: blur(2px);
}

.barrier-panel {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 18px;
  padding: 32px 40px;
  text-align: center;
  color: #fff;
}

.barrier-spinner {
  width: 42px;
  height: 42px;
  border-radius: 50%;
  border: 3px solid rgba(255, 255, 255, 0.18);
  border-top-color: var(--important, #d8a657);
  animation: barrier-spin 0.9s linear infinite;
}

.barrier-message {
  margin: 0;
  font-family: teutonic, sans-serif;
  font-size: 1.5em;
  letter-spacing: 0.02em;
}

.barrier-dashboard-link {
  display: inline-block;
  padding: 10px 22px;
  border-radius: 6px;
  background: var(--select, #6a3d9a);
  color: #fff;
  font-family: teutonic, sans-serif;
  font-size: 1.05em;
  letter-spacing: 0.03em;
  text-decoration: none;
  cursor: pointer;
  transition: background 0.15s ease;
}

.barrier-dashboard-link:hover {
  background: var(--select-hover, #7d4cb5);
}

@keyframes barrier-spin {
  to {
    transform: rotate(360deg);
  }
}
</style>

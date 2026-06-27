<script lang="ts" setup>
// Blocking overlay shown over a group's board while the Epic Multiplayer start
// barrier is still closed (a time limit is set but not every group has reached
// "ready" yet). Purely presentational — Game.vue owns the show condition and the
// markEventReady call. Sits above the board but below the event bars so a waiting
// player can still navigate between groups / back to the dashboard.
</script>

<template>
  <div class="event-start-barrier" role="status" aria-live="polite">
    <div class="barrier-panel">
      <div class="barrier-spinner" aria-hidden="true"></div>
      <p class="barrier-message">{{ $t('event.waitingForGroups') }}</p>
    </div>
  </div>
</template>

<style scoped>
.event-start-barrier {
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
  border-top-color: var(--spooky-green, #6e8644);
  animation: barrier-spin 0.9s linear infinite;
}

.barrier-message {
  margin: 0;
  font-family: teutonic, sans-serif;
  font-size: 1.5em;
  letter-spacing: 0.02em;
}

@keyframes barrier-spin {
  to {
    transform: rotate(360deg);
  }
}
</style>

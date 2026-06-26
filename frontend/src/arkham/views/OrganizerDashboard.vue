<script lang="ts" setup>
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { storeToRefs } from 'pinia'
import { adjustEventCounter } from '@/arkham/api'
import { useEventStore } from '@/arkham/stores/event'
import { COUNTERMEASURES, counterValue } from '@/arkham/types/EpicEvent'

// Read-only organizer dashboard. Loads the event over REST, then subscribes to
// the event websocket (via the store) so the shared counter stays live as
// players act across the groups.
const props = defineProps<{ id: string }>()

const store = useEventStore()
const { event, sharedState, groupDigests, socketError } = storeToRefs(store)

const ready = ref(false)
const adjusting = ref(false)

const countermeasures = computed(() => counterValue(sharedState.value, COUNTERMEASURES))
const totalInvestigators = computed(() => sharedState.value.sharedTotalInvestigators)

async function adjust(amount: number) {
  if (adjusting.value) return
  adjusting.value = true
  try {
    // Fire-and-forget: the authoritative new value arrives back over the
    // websocket as a SharedStateUpdate, so we don't optimistically mutate here.
    await adjustEventCounter(props.id, COUNTERMEASURES, amount)
  } catch (e) {
    console.error(e)
  } finally {
    adjusting.value = false
  }
}

function groupStatusLabel(gameId: string | null): string {
  return gameId ? 'event.inProgress' : 'event.notStarted'
}

onMounted(async () => {
  await store.load(props.id)
  store.connect(props.id)
  ready.value = true
})

onUnmounted(() => {
  store.disconnect()
})
</script>

<template>
  <div class="organizer-dashboard">
    <header class="main-header">
      <h2>{{ event ? event.name : $t('event.organizerDashboard') }}</h2>
    </header>

    <p v-if="!ready" class="loading">{{ $t('event.loading') }}</p>

    <template v-else>
      <p v-if="socketError" class="socket-error">{{ $t('event.disconnected') }}</p>

      <section class="shared-bar">
        <div class="counter">
          <span class="label">{{ $t('event.countermeasures') }}</span>
          <div class="counter-controls">
            <button type="button" :aria-label="$t('event.decrement')" :disabled="adjusting" @click="adjust(-1)">
              −
            </button>
            <span class="value">{{ countermeasures }}</span>
            <button type="button" :aria-label="$t('event.increment')" :disabled="adjusting" @click="adjust(1)">
              +
            </button>
          </div>
        </div>
        <div class="metric">
          <span class="label">{{ $t('event.totalInvestigators') }}</span>
          <span class="value">{{ totalInvestigators }}</span>
        </div>
      </section>

      <section class="groups">
        <article v-for="group in groupDigests" :key="group.ordinal" class="group-card">
          <header class="group-header">
            <span class="ordinal">{{ $t('event.group', { ordinal: group.ordinal }) }}</span>
            <h3>{{ group.name }}</h3>
          </header>
          <dl class="group-stats">
            <div>
              <dt>{{ $t('event.investigators') }}</dt>
              <dd>{{ group.investigatorCount }}</dd>
            </div>
            <div>
              <dt>{{ $t('event.scenario') }}</dt>
              <dd>{{ $t(groupStatusLabel(group.gameId)) }}</dd>
            </div>
          </dl>
          <RouterLink
            v-if="group.gameId"
            class="open-link"
            :to="`/games/${group.gameId}/spectate`"
          >
            {{ $t('event.open') }}
          </RouterLink>
        </article>
      </section>
    </template>
  </div>
</template>

<style scoped>
.organizer-dashboard {
  width: 70vw;
  max-width: 98vw;
  min-width: 60vw;
  margin: 0 auto;
  color: #fff;
}

header.main-header {
  display: flex;
  align-items: center;
  margin-bottom: 16px;
}

h2 {
  color: var(--title);
  text-transform: uppercase;
  font-family: Teutonic;
  font-size: 2em;
  margin: 0;
  flex: 1;
}

.loading {
  opacity: 0.7;
}

.socket-error {
  color: var(--important, #d8a657);
  margin: 0 0 12px;
}

.shared-bar {
  display: flex;
  gap: 16px;
  align-items: stretch;
  margin-bottom: 20px;
  flex-wrap: wrap;
}

.counter,
.metric {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 14px 18px;
  border-radius: 8px;
  background: var(--box-background, rgba(0, 0, 0, 0.35));
  border: 1px solid var(--box-border, rgba(255, 255, 255, 0.15));
  min-width: 180px;
}

.label {
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 0.75em;
  opacity: 0.75;
}

.value {
  font-size: 1.8em;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
}

.metric {
  justify-content: center;
  align-items: center;
}

.counter-controls {
  display: flex;
  align-items: center;
  gap: 14px;
}

.counter-controls button {
  width: 40px;
  height: 40px;
  border-radius: 50%;
  border: 1px solid rgba(255, 255, 255, 0.2);
  background: rgba(255, 255, 255, 0.08);
  color: #fff;
  font-size: 1.4em;
  line-height: 1;
  cursor: pointer;
}

.counter-controls button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.16);
}

.counter-controls button:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.groups {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: 14px;
}

.group-card {
  display: flex;
  flex-direction: column;
  gap: 12px;
  padding: 16px;
  border-radius: 8px;
  background: var(--box-background, rgba(0, 0, 0, 0.35));
  border: 1px solid var(--box-border, rgba(255, 255, 255, 0.15));
}

.group-header .ordinal {
  text-transform: uppercase;
  font-size: 0.7em;
  letter-spacing: 0.08em;
  opacity: 0.6;
}

.group-header h3 {
  margin: 2px 0 0;
  color: var(--title);
  font-family: Teutonic;
}

.group-stats {
  display: flex;
  gap: 24px;
  margin: 0;
}

.group-stats div {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.group-stats dt {
  text-transform: uppercase;
  font-size: 0.68em;
  letter-spacing: 0.06em;
  opacity: 0.6;
}

.group-stats dd {
  margin: 0;
  font-size: 1.1em;
  font-weight: 600;
}

.open-link {
  align-self: flex-start;
  margin-top: auto;
  padding: 8px 14px;
  border-radius: 5px;
  background: rgba(110, 134, 64, 0.95);
  color: #fff;
  text-decoration: none;
  text-transform: uppercase;
  font-size: 0.8em;
  letter-spacing: 0.06em;
}

.open-link:hover {
  background: rgba(110, 134, 64, 1);
}
</style>

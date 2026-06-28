<script lang="ts" setup>
import { computed } from 'vue'
import { useRouter } from 'vue-router'
import { storeToRefs } from 'pinia'
import { useEventStore } from '@/arkham/stores/event'
import { useEpicHelpers } from '@/arkham/composables/useEpicHelpers'
import { COUNTERMEASURES, counterValue, type GroupDigest } from '@/arkham/types/EpicEvent'
import EventCountdown from '@/arkham/components/EventCountdown.vue'
import SharedPools from '@/arkham/components/SharedPools.vue'

// Organizer-only chrome shown at the top of a group's game view (below the app
// nav, above the board). Reads everything from the event store, which Game.vue
// loads from the ?event=<id> query param. Players never see this — Game.vue only
// mounts it when the loaded event marks the user as organizer.
// `spectate` is the CURRENT mode for this game view (true = organizer/spectate
// route, false = playing a seat). The bar is always present for the organizer; it
// uses the mode to decide what a tab/button does.
const props = defineProps<{
  eventId: string
  currentGameId: string
  spectate: boolean
  // Stage (1/2/3) of the act in play for the VIEWED group, from Game.vue.
  currentActStage: number | null
}>()

const router = useRouter()
const store = useEventStore()
const { event, sharedState, groupDigests } = storeToRefs(store)
const { groupLabel } = useEpicHelpers()

const countermeasures = computed(() => counterValue(sharedState.value, COUNTERMEASURES))
const totalInvestigators = computed(() => sharedState.value.sharedTotalInvestigators)

// Only groups whose game exists can be opened.
const switchableGroups = computed(() => groupDigests.value.filter((g) => g.gameId))

// Groups where this organizer also holds a seat — they can drop into play mode.
// Hide the button for the group they're already actively playing.
const seatedGroups = computed(() =>
  groupDigests.value.filter(
    (g) => g.gameId && g.youAreSeated && !(g.gameId === props.currentGameId && !props.spectate),
  ),
)

// Switch to the organizer (spectate) view of a group. No-op if we're already
// spectating that exact group; while PLAYING the current group, this pops back
// to organizer view of it.
function viewGroup(group: GroupDigest) {
  if (!group.gameId) return
  if (group.gameId === props.currentGameId && props.spectate) return
  router.push({ name: 'Spectate', params: { gameId: group.gameId }, query: { event: props.eventId } })
}

// Drop into PLAYER MODE for a seated group: the normal play route, event kept.
function playSeat(group: GroupDigest) {
  if (!group.gameId) return
  router.push({ name: 'Game', params: { gameId: group.gameId }, query: { event: props.eventId } })
}
</script>

<template>
  <div class="organizer-bar">
    <RouterLink class="dashboard-link" :to="`/events/${eventId}`" @click="store.requestDashboardHub()">
      <span class="back-arrow" aria-hidden="true">‹</span>
      <span class="dashboard-name">{{ event?.name ?? $t('event.organizerDashboard') }}</span>
    </RouterLink>

    <nav class="group-switcher">
      <button
        v-for="group in switchableGroups"
        :key="group.ordinal"
        type="button"
        class="group-tab"
        :class="{ active: group.gameId === currentGameId, playing: group.gameId === currentGameId && !spectate }"
        @click="viewGroup(group)"
      >
        <span class="tab-name">{{ groupLabel(group) }}</span>
        <span v-if="group.gameId === currentGameId && !spectate" class="tab-mode">{{ $t('event.playing') }}</span>
      </button>
    </nav>

    <div v-if="seatedGroups.length" class="bar-actions">
      <button
        v-for="group in seatedGroups"
        :key="group.ordinal"
        type="button"
        class="play-seat-btn"
        @click="playSeat(group)"
      >
        {{ seatedGroups.length > 1
          ? `${$t('event.playMySeat')} · ${groupLabel(group)}`
          : $t('event.playMySeat') }}
      </button>
    </div>

    <div class="bar-metrics">
      <SharedPools :current-act-stage="currentActStage" />
      <EventCountdown />
      <div class="bar-metric">
        <span class="bar-label">{{ $t('event.countermeasures') }}</span>
        <span class="bar-value">{{ countermeasures }}</span>
      </div>
      <div class="bar-metric">
        <span class="bar-label">{{ $t('event.totalInvestigators') }}</span>
        <span class="bar-value">{{ totalInvestigators }}</span>
      </div>
    </div>
  </div>
</template>

<style scoped>
.organizer-bar {
  position: relative;
  /* Sit above the start-barrier overlay (z-index 900) so the organizer can still
     navigate between groups while a group waits at the barrier. */
  z-index: 1000;
  flex: 0 0 auto;
  display: flex;
  align-items: stretch;
  gap: 16px;
  width: 100%;
  box-sizing: border-box;
  padding: 0 12px;
  background: var(--background-dark, #12151f);
  border-bottom: 1px solid var(--box-border, rgba(255, 255, 255, 0.12));
  color: #fff;
  font-size: 0.85em;
}

.dashboard-link {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 8px 4px;
  color: rgba(255, 255, 255, 0.8);
  text-decoration: none;
  white-space: nowrap;
}

.dashboard-link:hover {
  color: #fff;
}

.back-arrow {
  font-size: 1.4em;
  line-height: 1;
}

.dashboard-name {
  font-family: teutonic, sans-serif;
  font-size: 1.2em;
}

.group-switcher {
  display: flex;
  align-items: stretch;
  gap: 2px;
  flex: 1;
  min-width: 0;
  overflow-x: auto;
}

.group-tab {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 1px;
  background: transparent;
  border: 0;
  border-bottom: 3px solid transparent;
  color: rgba(255, 255, 255, 0.7);
  cursor: pointer;
  padding: 6px 14px;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  font-size: 0.9em;
  white-space: nowrap;
}

.group-tab:hover {
  background: rgba(255, 255, 255, 0.06);
  color: #fff;
}

.group-tab.active {
  color: #fff;
  border-bottom-color: var(--spooky-green, #6e8644);
  background: rgba(255, 255, 255, 0.04);
}

.group-tab.playing {
  border-bottom-color: var(--important, #d8a657);
}

.tab-mode {
  font-size: 0.62em;
  letter-spacing: 0.08em;
  color: var(--important, #d8a657);
  opacity: 0.9;
}

.bar-actions {
  display: flex;
  align-items: center;
  gap: 8px;
}

.play-seat-btn {
  background: var(--spooky-green, #6e8644);
  border: 0;
  border-radius: 4px;
  color: #fff;
  cursor: pointer;
  padding: 7px 14px;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  font-size: 0.82em;
  font-weight: 700;
  white-space: nowrap;
}

.play-seat-btn:hover {
  background: hsl(80, 35%, 32%);
}

.bar-metrics {
  display: flex;
  align-items: center;
  gap: 18px;
  margin-left: auto;
  padding-left: 12px;
}

.bar-metric {
  display: flex;
  flex-direction: column;
  align-items: center;
  line-height: 1.1;
}

.bar-label {
  text-transform: uppercase;
  letter-spacing: 0.05em;
  font-size: 0.62em;
  opacity: 0.65;
  white-space: nowrap;
}

.bar-value {
  font-size: 1.3em;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
}
</style>

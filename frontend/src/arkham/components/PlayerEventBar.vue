<script lang="ts" setup>
import { computed } from 'vue'
import { useRouter } from 'vue-router'
import { storeToRefs } from 'pinia'
import { useEventStore } from '@/arkham/stores/event'
import { useEpicHelpers } from '@/arkham/composables/useEpicHelpers'
import { COUNTERMEASURES, counterValue, type GroupDigest } from '@/arkham/types/EpicEvent'
import EventCountdown from '@/arkham/components/EventCountdown.vue'
import SharedPools from '@/arkham/components/SharedPools.vue'

// Player-facing group switcher shown at the top of a group's game view when that
// game belongs to an Epic Multiplayer event. It mirrors the organizer's switcher
// (OrganizerBar) but with player semantics: the player's OWN seated group opens
// in normal PLAY mode, every other group opens read-only in SPECTATE mode
// ("investigators may freely communicate between groups"). The organizer keeps
// OrganizerBar untouched; Game.vue only mounts this for non-organizer members and
// gates it behind the dev flag. Reads the sibling groups + shared counters from
// the event store, which Game.vue loads from the ?event=<id> query param and keeps
// live via the SharedStateUpdate feed riding on this group's game websocket.
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

// Only groups whose game has been created can be opened.
const switchableGroups = computed(() => groupDigests.value.filter((g) => g.gameId))

// We're viewing this group as a seated player (normal play, not spectating).
function isPlayingHere(group: GroupDigest): boolean {
  return group.gameId === props.currentGameId && !props.spectate
}

// We're spectating this group right now (read-only).
function isSpectatingHere(group: GroupDigest): boolean {
  return group.gameId === props.currentGameId && props.spectate
}

// Open a group. The player's own seated group opens in normal play mode; any
// other group opens read-only in spectate mode. No-op when the click would land
// us exactly where we already are.
function openGroup(group: GroupDigest) {
  if (!group.gameId) return
  if (group.youAreSeated) {
    if (isPlayingHere(group)) return
    router.push({ name: 'Game', params: { gameId: group.gameId }, query: { event: props.eventId } })
    return
  }
  if (isSpectatingHere(group)) return
  router.push({ name: 'Spectate', params: { gameId: group.gameId }, query: { event: props.eventId } })
}
</script>

<template>
  <div class="player-event-bar">
    <RouterLink class="dashboard-link" :to="`/events/${eventId}`">
      <span class="back-arrow" aria-hidden="true">‹</span>
      <span class="dashboard-name">{{ event?.name ?? $t('event.epicMultiplayer') }}</span>
    </RouterLink>

    <nav class="group-switcher" :title="$t('event.communicateHint')">
      <button
        v-for="group in switchableGroups"
        :key="group.ordinal"
        type="button"
        class="group-tab"
        :class="{
          active: group.gameId === currentGameId,
          playing: isPlayingHere(group),
          spectating: isSpectatingHere(group),
        }"
        @click="openGroup(group)"
      >
        <span class="tab-name">{{ groupLabel(group) }}</span>
        <span v-if="isPlayingHere(group)" class="tab-mode playing-mode">{{ $t('event.playing') }}</span>
        <span v-else-if="isSpectatingHere(group)" class="tab-mode">{{ $t('event.spectating') }}</span>
        <span v-else-if="!group.youAreSeated" class="tab-mode">{{ $t('event.spectate') }}</span>
      </button>
    </nav>

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
.player-event-bar {
  position: relative;
  /* Sit above the start-barrier overlay (z-index 900) so a waiting player can
     still switch groups / return to the dashboard. */
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
  border-bottom-color: var(--highlight, #4aa3c7);
  background: rgba(255, 255, 255, 0.04);
}

.group-tab.playing {
  border-bottom-color: var(--important, #d8a657);
}

.tab-mode {
  font-size: 0.62em;
  letter-spacing: 0.08em;
  color: var(--highlight, #4aa3c7);
  opacity: 0.85;
}

.tab-mode.playing-mode {
  color: var(--important, #d8a657);
  opacity: 0.9;
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

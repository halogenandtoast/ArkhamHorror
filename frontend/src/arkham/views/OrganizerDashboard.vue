<script lang="ts" setup>
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { storeToRefs } from 'pinia'
import { useClipboard } from '@vueuse/core'
import { deleteEvent, eventTimeUp, resolveEventAdvance } from '@/arkham/api'
import { useEventStore } from '@/arkham/stores/event'
import { useDbCardStore } from '@/stores/dbCards'
import { imgsrc, buildShareableUrl } from '@/arkham/helpers'
import { useEpicHelpers } from '@/arkham/composables/useEpicHelpers'
import { useEventTimer } from '@/arkham/composables/useEventTimer'
import {
  actContribution,
  activeAwaitingStage,
  counterValue,
  TOTAL_INVESTIGATORS,
  type GroupDigest,
} from '@/arkham/types/EpicEvent'
import EventCountdown from '@/arkham/components/EventCountdown.vue'

// Organizer dashboard. Loads the event over REST, then subscribes to the event
// websocket (via the store) so group state stays live as players act. Shared
// state (countermeasures etc.) lives only in the in-game organizer bar now.
const props = defineProps<{ id: string }>()

const router = useRouter()
const { t } = useI18n()
const store = useEventStore()
const dbStore = useDbCardStore()
const { event, groupDigests, socketError, sharedState } = storeToRefs(store)
const { groupLabel } = useEpicHelpers()

const ready = ref(false)
const deleting = ref(false)

// Shared countdown: when it hits 0, force the time-up resolution. Guarded so this
// view fires at most once; the endpoint is idempotent across clients.
const { timeUp } = useEventTimer()
let timeUpFired = false
watch(
  timeUp,
  (up) => {
    if (!up || timeUpFired) return
    timeUpFired = true
    eventTimeUp(props.id).catch((e) => console.error(e))
  },
  { immediate: true },
)

// role is 'organizer' whenever the user holds an organizer row, even if they play.
const isOrganizer = computed(() => event.value?.role === 'organizer')

// A user belongs to one group: once seated anywhere, they can't join elsewhere.
const committed = computed(() => groupDigests.value.some((g) => g.youAreSeated))

// --- Shared act-advance allocation ------------------------------------------
// When the pooled clues EXCEED the threshold the backend sets
// `awaiting-organizer:<stage>` and waits for the organizer to choose which groups
// spend. We detect the stage by scanning the shared counters, and require the
// chosen spends to total exactly `2 * total-investigators`, each within that
// group's contribution (`act-contribution:<stage>:<ordinal>`).
const awaitingStage = computed(() => activeAwaitingStage(sharedState.value))

const totalInvestigators = computed(
  () => counterValue(sharedState.value, TOTAL_INVESTIGATORS) || sharedState.value.sharedTotalInvestigators,
)
const advanceThreshold = computed(() => 2 * totalInvestigators.value)

// Each group's cap = its contribution to the pending stage's pool. Read straight
// from the shared counters (no GroupDigest field needed).
function groupCap(group: GroupDigest): number {
  const stage = awaitingStage.value
  if (stage === null) return 0
  return Math.max(0, actContribution(sharedState.value, stage, group.ordinal))
}

// Per-group spend, keyed by ordinal.
const spendByOrdinal = ref<Record<number, number>>({})
function spendFor(group: GroupDigest): number {
  return spendByOrdinal.value[group.ordinal] ?? 0
}

const totalSpend = computed(() =>
  groupDigests.value.reduce((sum, g) => sum + (Number(spendByOrdinal.value[g.ordinal]) || 0), 0),
)

// Sum of every group's contribution cap for the pending stage. Drives both the
// initial seed and a re-seed if contributions land in a later shared-state tick.
const capsTotal = computed(() =>
  awaitingStage.value === null ? 0 : groupDigests.value.reduce((sum, g) => sum + groupCap(g), 0),
)

// Greedily fill each group up to its contribution until the threshold is met.
function prefillAllocation() {
  let remaining = advanceThreshold.value
  const next: Record<number, number> = {}
  for (const g of groupDigests.value) {
    const take = Math.min(groupCap(g), Math.max(0, remaining))
    next[g.ordinal] = take
    remaining -= take
  }
  spendByOrdinal.value = next
}

// Seed a valid starting allocation the organizer can tweak. Keyed on the stage AND
// the caps total, with `immediate`, so it fills:
//   * on mount even when the dashboard is opened AFTER the gate was already set
//     (no stage transition fires otherwise), and
//   * again if a group's contribution arrives in a later shared-state tick.
// It only (re)seeds while nothing has been entered yet (totalSpend === 0), so live
// updates can never clobber the organizer's in-progress edits. Cleared when nothing
// is pending.
watch(
  [awaitingStage, capsTotal],
  () => {
    if (awaitingStage.value === null) {
      spendByOrdinal.value = {}
      return
    }
    if (totalSpend.value === 0 && capsTotal.value > 0) prefillAllocation()
  },
  { immediate: true },
)

// Valid when every group's spend is a whole number within [0, its contribution] and
// the total equals the threshold exactly.
const allocationValid = computed(() => {
  if (awaitingStage.value === null || advanceThreshold.value <= 0) return false
  if (totalSpend.value !== advanceThreshold.value) return false
  return groupDigests.value.every((g) => {
    const v = Number(spendByOrdinal.value[g.ordinal] ?? 0)
    return Number.isInteger(v) && v >= 0 && v <= groupCap(g)
  })
})

const submittingAllocation = ref(false)
async function submitAllocation() {
  const stage = awaitingStage.value
  if (stage === null || !allocationValid.value || submittingAllocation.value) return
  submittingAllocation.value = true
  const allocation = groupDigests.value.map((g) => ({ ordinal: g.ordinal, spend: spendFor(g) }))
  try {
    await resolveEventAdvance(props.id, stage, allocation)
    // Backend clears the gate + resets the pool; the event ws pushes the update,
    // which flips awaitingStage to null and tears down the panel.
  } catch (e) {
    console.error(e)
  } finally {
    submittingAllocation.value = false
  }
}

// Investigator card code may arrive with or without the engine 'c' prefix; the
// ArkhamDB lookup and portrait assets both use the bare code.
function bareCode(investigatorId: string): string {
  return investigatorId.replace(/^c/, '')
}
function investigatorName(investigatorId: string): string {
  const code = bareCode(investigatorId)
  return dbStore.getDbCard(code)?.name ?? code
}
function portraitSrc(investigatorId: string): string {
  return imgsrc(`portraits/${bareCode(investigatorId)}.jpg`)
}

// A group has room when fewer investigators are seated than there are seats.
function isJoinable(group: GroupDigest): boolean {
  return group.gameId !== null && group.investigatorCount < group.seatCount
}

// Absolute invite link to a group's existing join route (PUT
// arkham/games/:id/join under the hood). Same construction MultiplayerLobby.vue
// uses for shared multiplayer invites, so it works under hash-history routing.
function inviteUrl(group: GroupDigest): string {
  if (!group.gameId) return ''
  const resolved = router.resolve({ name: 'JoinGame', params: { gameId: group.gameId } })
  return buildShareableUrl(resolved.href)
}

const { copy } = useClipboard()
const copiedOrdinal = ref<number | null>(null)
let copiedTimer: ReturnType<typeof setTimeout> | null = null

async function copyInvite(group: GroupDigest) {
  const url = inviteUrl(group)
  if (!url) return
  await copy(url)
  copiedOrdinal.value = group.ordinal
  if (copiedTimer) clearTimeout(copiedTimer)
  copiedTimer = setTimeout(() => { copiedOrdinal.value = null }, 2000)
}

// Send the current user into the group's existing async join flow, carrying the
// event context so an organizer who seats themselves keeps the organizer bar.
function joinGroup(group: GroupDigest) {
  if (!group.gameId) return
  router.push({ name: 'JoinGame', params: { gameId: group.gameId }, query: { event: props.id } })
}

// Drop into the normal play route for a group the user is seated in, keeping the
// event context so the organizer bar follows them in.
function playSeat(group: GroupDigest) {
  if (!group.gameId) return
  router.push({ name: 'Game', params: { gameId: group.gameId }, query: { event: props.id } })
}

// Organizer-only: delete the event and all of its group games, then return home.
async function removeEvent() {
  if (deleting.value) return
  if (!window.confirm(t('event.confirmDelete'))) return
  deleting.value = true
  try {
    await deleteEvent(props.id)
    store.disconnect()
    router.push('/')
  } catch (e) {
    console.error(e)
    deleting.value = false
  }
}

onMounted(async () => {
  void dbStore.initDbCards()
  await store.load(props.id)

  // Frictionless entry: a seated organizer landing on the dashboard is dropped
  // straight into their group's game (where the in-game OrganizerBar gives them a
  // dashboard link + group switcher). The dashboard stays the hub — its link in
  // that bar sets `dashboardHubRequested`, which we consume here to SKIP this
  // redirect for that one explicit navigation, so the hub is always a click away.
  if (!store.consumeDashboardHubRequest() && isOrganizer.value) {
    const seat = groupDigests.value.find((g) => g.youAreSeated && g.gameId)
    if (seat?.gameId) {
      router.replace({ name: 'Game', params: { gameId: seat.gameId }, query: { event: props.id } })
      return
    }
  }

  store.connect(props.id)
  ready.value = true
})

onUnmounted(() => {
  if (copiedTimer) clearTimeout(copiedTimer)
  store.disconnect()
})
</script>

<template>
  <div class="organizer-dashboard">
    <header class="dash-header">
      <div class="dash-heading">
        <span class="dash-chip">{{ $t('event.epicMultiplayer') }}</span>
        <h2>{{ event ? event.name : $t('event.organizerDashboard') }}</h2>
      </div>
      <div class="dash-countdown">
        <EventCountdown />
      </div>
      <button
        v-if="isOrganizer && ready"
        type="button"
        class="delete-btn"
        :disabled="deleting"
        @click="removeEvent"
      >
        {{ $t('event.deleteEvent') }}
      </button>
    </header>

    <p v-if="!ready" class="loading">{{ $t('event.loading') }}</p>

    <template v-else>
      <p v-if="socketError" class="socket-error">{{ $t('event.disconnected') }}</p>

      <section v-if="isOrganizer && awaitingStage !== null" class="advance-panel">
        <header class="advance-header">
          <h3>{{ $t('event.allocateAdvance', { stage: awaitingStage }) }}</h3>
          <p class="advance-hint">{{ $t('event.allocateAdvanceHint', { threshold: advanceThreshold }) }}</p>
        </header>

        <div class="advance-groups">
          <div v-for="group in groupDigests" :key="group.ordinal" class="advance-group">
            <label class="advance-group-label" :for="`spend-${group.ordinal}`">
              <span class="advance-group-name">{{ groupLabel(group) }}</span>
              <span class="advance-group-clues">{{ $t('event.allocateAvailable', { count: groupCap(group) }) }}</span>
            </label>
            <input
              :id="`spend-${group.ordinal}`"
              v-model.number="spendByOrdinal[group.ordinal]"
              class="advance-input"
              type="number"
              min="0"
              :max="groupCap(group)"
              step="1"
            />
          </div>
        </div>

        <div class="advance-footer">
          <span class="advance-total" :class="{ invalid: totalSpend !== advanceThreshold }">
            {{ $t('event.allocateTotal', { total: totalSpend, threshold: advanceThreshold }) }}
          </span>
          <button
            type="button"
            class="advance-submit"
            :disabled="!allocationValid || submittingAllocation"
            @click="submitAllocation"
          >
            {{ $t('event.confirmAdvance') }}
          </button>
        </div>
      </section>

      <section class="groups">
        <div v-for="group in groupDigests" :key="group.ordinal" class="group-row">
          <div class="group-details">
            <div class="group-title">
              <span class="title">{{ groupLabel(group) }}</span>
              <div class="extra-details">
                <span class="seat-status">
                  {{ $t('event.seats', { count: group.investigatorCount, total: group.seatCount }) }}
                </span>
                <span class="status-badge" :class="isJoinable(group) ? 'joinable' : 'full'">
                  {{ isJoinable(group) ? $t('event.joinable') : $t('event.full') }}
                </span>
              </div>
            </div>

            <div class="group-subdetails">
              <div v-if="group.players.length" class="players">
                <div
                  v-for="(player, i) in group.players"
                  :key="`${player.username}-${i}`"
                  class="player"
                >
                  <div class="investigator-portrait-container">
                    <img
                      v-if="player.investigatorId"
                      class="investigator-portrait"
                      :src="portraitSrc(player.investigatorId)"
                      :alt="investigatorName(player.investigatorId)"
                    />
                    <span v-else class="portrait-placeholder" aria-hidden="true">?</span>
                  </div>
                  <div class="player-text">
                    <span class="username">{{ player.username }}</span>
                    <span class="investigator-name" :class="{ pending: !player.investigatorId }">
                      {{ player.investigatorId ? investigatorName(player.investigatorId) : $t('event.choosing') }}
                    </span>
                  </div>
                </div>
              </div>
              <p v-else class="empty-players">{{ $t('event.noPlayersYet') }}</p>

              <div class="group-controls">
                <RouterLink
                  v-if="group.gameId"
                  class="ctrl open-link"
                  :to="{ name: 'Spectate', params: { gameId: group.gameId }, query: { event: id } }"
                >
                  {{ $t('event.open') }}
                </RouterLink>
                <button
                  v-if="group.gameId && group.youAreSeated"
                  type="button"
                  class="ctrl play-btn"
                  @click="playSeat(group)"
                >
                  {{ $t('event.playMySeat') }}
                </button>
                <button
                  v-if="isJoinable(group) && (isOrganizer || !committed)"
                  type="button"
                  class="ctrl ghost-btn"
                  @click="copyInvite(group)"
                >
                  {{ copiedOrdinal === group.ordinal ? $t('event.inviteLinkCopied') : $t('event.copyInviteLink') }}
                </button>
                <button
                  v-if="isJoinable(group) && !committed"
                  type="button"
                  class="ctrl join-btn"
                  @click="joinGroup(group)"
                >
                  {{ $t('event.joinThisGroup') }}
                </button>
              </div>
            </div>
          </div>
        </div>
      </section>
    </template>
  </div>
</template>

<style scoped>
.organizer-dashboard {
  width: min(1100px, 92vw);
  margin: 0 auto;
  padding: 8px 0 40px;
  color: #fff;
}

.dash-header {
  display: flex;
  align-items: flex-start;
  gap: 16px;
  margin-bottom: 20px;
}

.dash-heading {
  flex: 1;
  min-width: 0;
}

.dash-countdown {
  display: flex;
  align-items: center;
  flex-shrink: 0;
  align-self: center;
  font-size: 1.2em;
}

.dash-chip {
  display: inline-block;
  padding: 2px 10px;
  border-radius: 999px;
  background: var(--spooky-green, #6e8644);
  color: #fff;
  font-size: 0.66em;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

h2 {
  color: var(--title);
  text-transform: uppercase;
  font-family: Teutonic;
  font-size: 2.1em;
  margin: 6px 0 0;
}

.loading {
  opacity: 0.7;
}

.socket-error {
  color: var(--important, #d8a657);
  margin: 0 0 12px;
}

/* Shared act-advance allocation panel */
.advance-panel {
  margin-bottom: 16px;
  padding: 14px 16px;
  border: 1px solid var(--important, #d8a657);
  border-radius: 6px;
  background: rgba(216, 166, 87, 0.08);
}

.advance-header h3 {
  margin: 0;
  color: var(--important, #d8a657);
  text-transform: uppercase;
  font-family: Teutonic;
  font-size: 1.3em;
}

.advance-hint {
  margin: 4px 0 12px;
  font-size: 0.9em;
  opacity: 0.85;
}

.advance-groups {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  gap: 10px;
}

.advance-group {
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 8px 10px;
  border-radius: 5px;
  background: rgba(0, 0, 0, 0.25);
}

.advance-group-label {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 8px;
}

.advance-group-name {
  font-weight: 600;
}

.advance-group-clues {
  font-size: 0.78em;
  opacity: 0.7;
  white-space: nowrap;
}

.advance-input {
  width: 100%;
  box-sizing: border-box;
  padding: 8px 10px;
  border-radius: 5px;
  border: 1px solid var(--box-border, rgba(255, 255, 255, 0.18));
  background: var(--background-dark, #12151f);
  color: #fff;
  font-variant-numeric: tabular-nums;
}

.advance-footer {
  display: flex;
  align-items: center;
  justify-content: flex-end;
  gap: 16px;
  margin-top: 12px;
}

.advance-total {
  font-variant-numeric: tabular-nums;
  font-weight: 700;
}

.advance-total.invalid {
  color: var(--delete, #d88);
}

.advance-submit {
  padding: 9px 18px;
  border-radius: 5px;
  border: 0;
  background: rgba(110, 134, 64, 0.95);
  color: #fff;
  cursor: pointer;
  text-transform: uppercase;
  font-size: 0.82em;
  font-weight: 700;
  letter-spacing: 0.06em;
}

.advance-submit:hover:not(:disabled) {
  background: rgba(110, 134, 64, 1);
}

.advance-submit:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

/* Groups — same two-band shape as GameRow.vue, with a subtle event/group rail. */
.groups {
  display: flex;
  flex-direction: column;
}

.group-row {
  position: relative;
  display: flex;
  color: var(--title);
  background:
    linear-gradient(120deg, rgba(110, 134, 64, 0.1), rgba(110, 134, 64, 0) 56%),
    var(--box-background);
  border: 1px solid color-mix(in srgb, var(--spooky-green) 28%, var(--box-border));
  border-radius: 3px;
  margin-bottom: 10px;
  overflow: hidden;
  transition: border-color 0.2s linear, transform 0.12s ease;
}

.group-row::before {
  content: '';
  position: absolute;
  inset: 0 auto 0 0;
  width: 5px;
  background: linear-gradient(180deg, rgba(210, 228, 158, 0.82), var(--spooky-green));
  opacity: 0.88;
}

.group-row:hover {
  border-color: var(--spooky-green);
  transform: translateY(-1px);
}

.group-details {
  flex: 1;
  display: flex;
  flex-direction: column;
  min-width: 0;
}

.group-title {
  display: flex;
  gap: 10px;
  flex-direction: row;
  align-items: center;
  padding: 10px 10px 10px 18px;
  border-bottom: 1px solid color-mix(in srgb, var(--spooky-green) 24%, var(--box-border));

  @media (max-width: 600px) {
    flex-direction: column;
    align-items: flex-start;
    font-size: 0.9em;
  }
}

.title {
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.6em;
  min-width: 0;
}

.extra-details {
  display: flex;
  gap: 10px;
  align-items: center;
}

.seat-status {
  padding: 5px 12px;
  background: rgba(0, 0, 0, 0.5);
  border-radius: 10px;
  font-size: 0.85em;
  font-variant-numeric: tabular-nums;
  text-transform: uppercase;
  white-space: nowrap;
}

.status-badge {
  padding: 3px 10px;
  border-radius: 999px;
  font-size: 0.7em;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  font-weight: 700;
  white-space: nowrap;
}

.status-badge.joinable {
  background: rgba(110, 134, 64, 0.25);
  color: rgba(180, 210, 120, 0.95);
  border: 1px solid rgba(110, 134, 64, 0.55);
}

.status-badge.full {
  background: rgba(255, 255, 255, 0.08);
  color: rgba(255, 255, 255, 0.6);
  border: 1px solid rgba(255, 255, 255, 0.15);
}

.group-subdetails {
  display: flex;
  align-items: center;
  gap: 12px;
  flex-wrap: wrap;
  background: rgba(255, 255, 255, 0.02);
  padding: 10px 10px 10px 18px;
}

.players {
  display: flex;
  flex: 1;
  flex-wrap: wrap;
  gap: 14px;
  min-width: 0;
}

.player {
  display: flex;
  align-items: center;
  gap: 8px;
}

.investigator-portrait-container {
  width: 50px;
  height: 50px;
  overflow: hidden;
  border-radius: 5px;
  flex-shrink: 0;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  background: var(--background-dark);
}

.investigator-portrait {
  width: 150px;
}

.portrait-placeholder {
  width: 50px;
  height: 50px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: rgba(255, 255, 255, 0.4);
  font-weight: 700;
  font-size: 1.4em;
}

.player-text {
  display: flex;
  flex-direction: column;
  min-width: 0;
}

.username {
  font-weight: 600;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.investigator-name {
  font-size: 0.82em;
  opacity: 0.8;
}

.investigator-name.pending {
  font-style: italic;
  opacity: 0.6;
}

.empty-players {
  margin: 0;
  flex: 1;
  font-size: 0.9em;
  opacity: 0.5;
  font-style: italic;
}

.group-controls {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 8px;
  margin-left: auto;
}

.ctrl {
  padding: 8px 14px;
  border-radius: 5px;
  border: 1px solid rgba(255, 255, 255, 0.18);
  cursor: pointer;
  text-transform: uppercase;
  font-size: 0.78em;
  letter-spacing: 0.06em;
  text-decoration: none;
  white-space: nowrap;
}

.ghost-btn,
.open-link {
  background: rgba(255, 255, 255, 0.06);
  color: #fff;
}

.ghost-btn:hover,
.open-link:hover {
  background: rgba(255, 255, 255, 0.14);
}

.join-btn,
.play-btn {
  background: rgba(110, 134, 64, 0.95);
  border-color: rgba(110, 134, 64, 0.95);
  color: #fff;
}

.join-btn:hover,
.play-btn:hover {
  background: rgba(110, 134, 64, 1);
}

/* Delete */
.delete-btn {
  flex-shrink: 0;
  padding: 9px 16px;
  border-radius: 5px;
  background: transparent;
  border: 1px solid var(--delete, #b04a4a);
  color: var(--delete, #d88);
  cursor: pointer;
  text-transform: uppercase;
  font-size: 0.78em;
  font-weight: 700;
  letter-spacing: 0.06em;
}

.delete-btn:hover:not(:disabled) {
  background: var(--delete, #b04a4a);
  color: #fff;
}

.delete-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}
</style>

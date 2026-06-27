<script lang="ts" setup>
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { storeToRefs } from 'pinia'
import { useClipboard } from '@vueuse/core'
import { deleteEvent } from '@/arkham/api'
import { useEventStore } from '@/arkham/stores/event'
import { useDbCardStore } from '@/stores/dbCards'
import { imgsrc } from '@/arkham/helpers'
import { type GroupDigest } from '@/arkham/types/EpicEvent'

// Organizer dashboard. Loads the event over REST, then subscribes to the event
// websocket (via the store) so group state stays live as players act. Shared
// state (countermeasures etc.) lives only in the in-game organizer bar now.
const props = defineProps<{ id: string }>()

const router = useRouter()
const { t } = useI18n()
const store = useEventStore()
const dbStore = useDbCardStore()
const { event, groupDigests, socketError } = storeToRefs(store)

const ready = ref(false)
const deleting = ref(false)

// role is 'organizer' whenever the user holds an organizer row, even if they play.
const isOrganizer = computed(() => event.value?.role === 'organizer')

// A user belongs to one group: once seated anywhere, they can't join elsewhere.
const committed = computed(() => groupDigests.value.some((g) => g.youAreSeated))

function groupLabel(group: GroupDigest): string {
  const name = group.name?.trim()
  return name ? name : t('event.group', { ordinal: group.ordinal + 1 })
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
  return window.location.origin + window.location.pathname + resolved.href
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

/* Groups — mirror the games-list row look (GameRow.vue) */
.groups {
  display: flex;
  flex-direction: column;
}

.group-row {
  display: flex;
  color: var(--title);
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 3px;
  margin-bottom: 10px;
  transition: border-color 0.2s linear;
}

.group-row:hover {
  border-color: var(--spooky-green);
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
  padding: 10px;
  border-bottom: 1px solid var(--box-border);

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
  padding: 10px;
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

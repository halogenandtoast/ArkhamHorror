<script setup lang="ts">
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import api from '@/api'
import type { RoomConnection, RoomDetails, RoomSweepResult } from './roomTypes'

const { t } = useI18n()
const props = defineProps<{ details: RoomDetails }>()
const emit = defineEmits<{ (e: 'update', details: RoomDetails): void }>()

const busy = ref<string | null>(null)
const message = ref<string | null>(null)

const details = computed(() => props.details)

/* The number the rooms list shows against what the pods actually hold. They
 * should match; when they do not, the excess is a dead replica's share of the
 * tally, which is what keeps a room out of the abandoned sweep forever. */
const tallyDrift = computed(() => {
  const recorded = details.value.tally.tallyClients
  if (recorded === null) return null
  return recorded - details.value.connections.length
})

const unanswered = computed(() =>
  details.value.connections.filter((c) => c.probeAnswered === false),
)

function duration(seconds: number | null): string {
  if (seconds === null) return '—'
  if (seconds < 60) return `${seconds}s`
  if (seconds < 3600) return `${Math.floor(seconds / 60)}m`
  return `${Math.floor(seconds / 3600)}h ${Math.floor((seconds % 3600) / 60)}m`
}

function probeLabel(c: RoomConnection): string {
  if (c.probeAnswered === null) return t('adminRooms.probeNotAsked')
  return c.probeAnswered ? t('adminRooms.probeAlive') : t('adminRooms.probeNoAnswer')
}

async function run<T>(label: string, call: () => Promise<{ data: T }>, done: (data: T) => void) {
  busy.value = label
  message.value = null
  try {
    const { data } = await call()
    done(data)
  } catch (e) {
    message.value = t('adminRooms.actionFailed', { action: label, error: String(e) })
  } finally {
    busy.value = null
  }
}

async function refresh() {
  await run(t('adminRooms.refresh'), () => api.get<RoomDetails>(`admin/rooms/${details.value.gameId}`), (d) =>
    emit('update', d),
  )
}

async function probe() {
  await run(t('adminRooms.probe'), () => api.post<RoomDetails>(`admin/rooms/${details.value.gameId}/probe`), (d) => {
    emit('update', d)
    const dead = d.connections.filter((c) => c.probeAnswered === false)
    const live = d.connections.filter((c) => c.probeAnswered === true)
    message.value = t('adminRooms.probeResult', { live: live.length, dead: dead.length })
  })
}

/* Spectators are not a reason to keep a room: once the seats are gone nothing
 * further happens on that board. A player socket that ANSWERS is, so the
 * server refuses on its own and this only has to report what it said. */
async function sweep(force: boolean) {
  const what = force ? t('adminRooms.forceClose') : t('adminRooms.sweep')
  if (force && !confirm(t('adminRooms.confirmForceClose'))) return
  await run(
    what,
    () => api.post<RoomSweepResult>(`admin/rooms/${details.value.gameId}/sweep${force ? '?force=true' : ''}`),
    (d) => {
      emit('update', d.room)
      message.value = d.detail
    },
  )
}
</script>

<template>
  <div class="details box">
    <header>
      <div class="heading">
        <h3>{{ details.gameName ?? t('adminRooms.noGameRow') }}</h3>
        <code>{{ details.gameId }}</code>
      </div>
      <div class="actions">
        <button :disabled="!!busy" @click="refresh">{{ t('adminRooms.refresh') }}</button>
        <button :disabled="!!busy" @click="probe">{{ t('adminRooms.probe') }}</button>
        <button :disabled="!!busy" class="sweep" @click="sweep(false)">{{ t('adminRooms.sweep') }}</button>
        <button :disabled="!!busy" class="force" @click="sweep(true)">{{ t('adminRooms.forceClose') }}</button>
      </div>
    </header>

    <p v-if="busy" class="note">{{ busy }}…</p>
    <p v-else-if="message" class="note">{{ message }}</p>

    <div class="stats">
      <div class="stat">
        <span class="label">{{ t('adminRooms.sockets') }}</span>
        <strong>{{ details.connections.length }}</strong>
        <small>{{ t('adminRooms.socketBreakdown', { players: details.playerConnections, watchers: details.spectatorConnections }) }}</small>
      </div>
      <div class="stat">
        <span class="label">{{ t('adminRooms.redisTally') }}</span>
        <strong>{{ details.tally.tallyClients ?? '—' }}</strong>
        <small>{{ t('adminRooms.tallyWatching', { count: details.tally.tallyWatchers ?? '—' }) }}</small>
      </div>
      <div class="stat" :class="{ warn: tallyDrift !== null && tallyDrift !== 0 }">
        <span class="label">{{ t('adminRooms.drift') }}</span>
        <strong>{{ tallyDrift === null ? '—' : tallyDrift }}</strong>
        <small>{{ t('adminRooms.driftHint') }}</small>
      </div>
      <div class="stat">
        <span class="label">{{ t('adminRooms.podsHolding') }}</span>
        <strong>{{ details.podsHoldingRoom.length }}</strong>
        <small>{{ t('adminRooms.podsAnswering', { count: details.podsAnswered.length }) }}</small>
      </div>
      <div class="stat">
        <span class="label">{{ t('adminRooms.gameLastUpdated') }}</span>
        <strong class="small">{{ details.gameUpdatedAt ?? '—' }}</strong>
      </div>
    </div>

    <p v-if="tallyDrift !== null && tallyDrift > 0" class="diagnosis">
      {{ t('adminRooms.driftDiagnosis', { count: tallyDrift }) }}
    </p>
    <p v-if="unanswered.length > 0" class="diagnosis">
      {{ t('adminRooms.unansweredDiagnosis', { count: unanswered.length }) }}
    </p>

    <h4>{{ t('adminRooms.connections') }}</h4>
    <div v-if="details.connections.length === 0" class="empty">
      {{ t('adminRooms.noConnections') }}
    </div>
    <table v-else>
      <thead>
        <tr>
          <th>{{ t('adminRooms.colWho') }}</th>
          <th>{{ t('adminRooms.colRole') }}</th>
          <th>{{ t('adminRooms.colPod') }}</th>
          <th>{{ t('adminRooms.colConnected') }}</th>
          <th>{{ t('adminRooms.colLastPong') }}</th>
          <th>{{ t('adminRooms.colQueued') }}</th>
          <th>{{ t('adminRooms.colProbe') }}</th>
        </tr>
      </thead>
      <tbody>
        <tr
          v-for="c in details.connections"
          :key="`${c.pod}-${c.connectionId}`"
          :class="{ dead: c.probeAnswered === false, closing: c.closing }"
        >
          <td>{{ c.username ?? (c.userId ? c.userId : t('adminRooms.anonymous')) }}</td>
          <td>{{ c.spectator ? t('adminRooms.spectator') : t('adminRooms.player') }}</td>
          <td><code>{{ c.pod }}</code></td>
          <td>{{ duration(c.connectedForSeconds) }}</td>
          <td>{{ t('adminRooms.ago', { duration: duration(c.lastPongSecondsAgo) }) }}</td>
          <td :class="{ warn: c.overflowed }">{{ c.queued }}{{ c.overflowed ? t('adminRooms.overflowed') : '' }}</td>
          <td>{{ probeLabel(c) }}</td>
        </tr>
      </tbody>
    </table>

    <h4>{{ t('adminRooms.seats') }}</h4>
    <div v-if="details.seats.length === 0" class="empty">{{ t('adminRooms.noSeats') }}</div>
    <ul v-else class="seats">
      <li v-for="seat in details.seats" :key="seat.userId" :class="{ absent: !seat.connected }">
        {{ seat.username }}
        <small>{{ seat.investigatorId }}</small>
        <span class="presence">{{ seat.connected ? t('adminRooms.connected') : t('adminRooms.notConnected') }}</span>
      </li>
    </ul>
  </div>
</template>

<style scoped>
.details {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

header {
  align-items: flex-end;
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  justify-content: space-between;
}

.heading {
  display: flex;
  flex-direction: column;
  gap: 4px;
  min-width: 0;
}

h3 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.4rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

h4 {
  color: color-mix(in srgb, var(--title) 70%, transparent);
  font-size: 0.74rem;
  font-weight: 800;
  letter-spacing: 0.09em;
  margin: 4px 0 0;
  text-transform: uppercase;
}

code {
  color: color-mix(in srgb, var(--title) 70%, transparent);
  font-size: 0.74rem;
  overflow-wrap: anywhere;
}

.actions {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

button {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--title);
  font-size: 0.78rem;
  font-weight: 700;
  padding: 7px 10px;
  text-transform: uppercase;
}

button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.06);
  color: white;
}

button:disabled {
  opacity: 0.5;
}

button.sweep {
  border-color: color-mix(in srgb, var(--spooky-green) 65%, var(--box-border));
}

button.force {
  border-color: color-mix(in srgb, var(--delete) 65%, var(--box-border));
  color: color-mix(in srgb, var(--delete) 35%, white);
}

.note {
  color: var(--title);
  margin: 0;
  opacity: 0.85;
}

.diagnosis {
  background: color-mix(in srgb, var(--delete) 12%, transparent);
  border-left: 3px solid color-mix(in srgb, var(--delete) 70%, transparent);
  color: var(--title);
  margin: 0;
  padding: 8px 10px;
}

.stats {
  display: grid;
  gap: 10px;
  grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
}

.stat {
  background: color-mix(in srgb, var(--background-dark) 55%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 70%, transparent);
  border-radius: 4px;
  display: flex;
  flex-direction: column;
  gap: 2px;
  padding: 8px 10px;
}

.stat.warn {
  border-color: color-mix(in srgb, var(--delete) 60%, var(--box-border));
}

.label {
  color: color-mix(in srgb, var(--title) 62%, transparent);
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

strong {
  color: var(--title);
  font-size: 1.25rem;
}

strong.small {
  font-size: 0.82rem;
  overflow-wrap: anywhere;
}

small {
  color: color-mix(in srgb, var(--title) 65%, transparent);
  font-size: 0.72rem;
}

table {
  border-collapse: collapse;
  font-size: 0.8rem;
  width: 100%;
}

th {
  color: color-mix(in srgb, var(--title) 62%, transparent);
  font-size: 0.7rem;
  letter-spacing: 0.07em;
  text-align: left;
  text-transform: uppercase;
}

th,
td {
  border-bottom: 1px solid color-mix(in srgb, var(--box-border) 55%, transparent);
  color: var(--title);
  padding: 6px 8px 6px 0;
}

tr.dead td {
  color: color-mix(in srgb, var(--delete) 30%, white);
}

tr.closing td {
  opacity: 0.6;
}

td.warn {
  color: color-mix(in srgb, var(--delete) 30%, white);
}

.seats {
  display: flex;
  flex-direction: column;
  gap: 6px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.seats li {
  align-items: baseline;
  color: var(--title);
  display: flex;
  gap: 8px;
}

.seats li.absent .presence {
  color: color-mix(in srgb, var(--delete) 30%, white);
}

.presence {
  font-size: 0.72rem;
  margin-left: auto;
  text-transform: uppercase;
}

.empty {
  color: var(--title);
  opacity: 0.7;
}

@media (max-width: 620px) {
  table {
    display: block;
    overflow-x: auto;
    white-space: nowrap;
  }
}
</style>

<script setup lang="ts">
import { computed, ref } from 'vue'
import api from '@/api'
import type { RoomConnection, RoomDetails, RoomSweepResult } from './roomTypes'

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
  if (c.probeAnswered === null) return 'not asked'
  return c.probeAnswered ? 'alive' : 'no answer'
}

async function run<T>(label: string, call: () => Promise<{ data: T }>, done: (data: T) => void) {
  busy.value = label
  message.value = null
  try {
    const { data } = await call()
    done(data)
  } catch (e) {
    message.value = `${label} failed: ${e}`
  } finally {
    busy.value = null
  }
}

async function refresh() {
  await run('Refresh', () => api.get<RoomDetails>(`admin/rooms/${details.value.gameId}`), (d) =>
    emit('update', d),
  )
}

async function probe() {
  await run('Probe', () => api.post<RoomDetails>(`admin/rooms/${details.value.gameId}/probe`), (d) => {
    emit('update', d)
    const dead = d.connections.filter((c) => c.probeAnswered === false)
    const live = d.connections.filter((c) => c.probeAnswered === true)
    message.value = `${live.length} answered, ${dead.length} did not.`
  })
}

/* Spectators are not a reason to keep a room: once the seats are gone nothing
 * further happens on that board. A player socket that ANSWERS is, so the
 * server refuses on its own and this only has to report what it said. */
async function sweep(force: boolean) {
  const what = force ? 'Force close' : 'Sweep'
  if (force && !confirm('Close this room regardless of what the probe says?')) return
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
        <h3>{{ details.gameName ?? 'No game row (deleted)' }}</h3>
        <code>{{ details.gameId }}</code>
      </div>
      <div class="actions">
        <button :disabled="!!busy" @click="refresh">Refresh</button>
        <button :disabled="!!busy" @click="probe">Probe</button>
        <button :disabled="!!busy" class="sweep" @click="sweep(false)">Sweep</button>
        <button :disabled="!!busy" class="force" @click="sweep(true)">Force close</button>
      </div>
    </header>

    <p v-if="busy" class="note">{{ busy }}…</p>
    <p v-else-if="message" class="note">{{ message }}</p>

    <div class="stats">
      <div class="stat">
        <span class="label">Sockets</span>
        <strong>{{ details.connections.length }}</strong>
        <small>{{ details.playerConnections }} player / {{ details.spectatorConnections }} watching</small>
      </div>
      <div class="stat">
        <span class="label">Redis tally</span>
        <strong>{{ details.tally.tallyClients ?? '—' }}</strong>
        <small>{{ details.tally.tallyWatchers ?? '—' }} watching</small>
      </div>
      <div class="stat" :class="{ warn: tallyDrift !== null && tallyDrift !== 0 }">
        <span class="label">Drift</span>
        <strong>{{ tallyDrift === null ? '—' : tallyDrift }}</strong>
        <small>tally minus real sockets</small>
      </div>
      <div class="stat">
        <span class="label">Pods holding it</span>
        <strong>{{ details.podsHoldingRoom.length }}</strong>
        <small>of {{ details.podsAnswered.length }} answering</small>
      </div>
      <div class="stat">
        <span class="label">Game last updated</span>
        <strong class="small">{{ details.gameUpdatedAt ?? '—' }}</strong>
      </div>
    </div>

    <p v-if="tallyDrift !== null && tallyDrift > 0" class="diagnosis">
      Redis counts {{ tallyDrift }} socket(s) nobody is holding — a replica died with them
      attached. That leftover reads as a seated player, which is why the abandoned-room
      sweep never closes this one. Sweeping clears the tally as well as the sockets.
    </p>
    <p v-if="unanswered.length > 0" class="diagnosis">
      {{ unanswered.length }} socket(s) did not answer the probe. Their clients are gone;
      the connections are half-open and will hold the room open indefinitely.
    </p>

    <h4>Connections</h4>
    <div v-if="details.connections.length === 0" class="empty">
      No pod is holding a socket for this game.
    </div>
    <table v-else>
      <thead>
        <tr>
          <th>Who</th>
          <th>Role</th>
          <th>Pod</th>
          <th>Connected</th>
          <th>Last pong</th>
          <th>Queued</th>
          <th>Probe</th>
        </tr>
      </thead>
      <tbody>
        <tr
          v-for="c in details.connections"
          :key="`${c.pod}-${c.connectionId}`"
          :class="{ dead: c.probeAnswered === false, closing: c.closing }"
        >
          <td>{{ c.username ?? (c.userId ? c.userId : 'anonymous') }}</td>
          <td>{{ c.spectator ? 'spectator' : 'player' }}</td>
          <td><code>{{ c.pod }}</code></td>
          <td>{{ duration(c.connectedForSeconds) }}</td>
          <td>{{ duration(c.lastPongSecondsAgo) }} ago</td>
          <td :class="{ warn: c.overflowed }">{{ c.queued }}{{ c.overflowed ? ' (overflowed)' : '' }}</td>
          <td>{{ probeLabel(c) }}</td>
        </tr>
      </tbody>
    </table>

    <h4>Seats</h4>
    <div v-if="details.seats.length === 0" class="empty">No players recorded for this game.</div>
    <ul v-else class="seats">
      <li v-for="seat in details.seats" :key="seat.userId" :class="{ absent: !seat.connected }">
        {{ seat.username }}
        <small>{{ seat.investigatorId }}</small>
        <span class="presence">{{ seat.connected ? 'connected' : 'not connected' }}</span>
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

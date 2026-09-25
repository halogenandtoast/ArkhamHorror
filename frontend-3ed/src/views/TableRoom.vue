<script setup lang="ts">
import { useWebSocket } from '@vueuse/core'
import { computed, onUnmounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import * as api from '@/api'
import { createGameContext, provideGame } from '@/game/context'
import GameScreen from '@/game/GameScreen.vue'
import Overlays from '@/game/Overlays.vue'
import { expName } from '@/game/util'
import type { Catalog, TableMessage } from '@/types'

const props = defineProps<{ id: string; catalog: Catalog }>()
const router = useRouter()
const ctx = createGameContext(props.id, props.catalog)
provideGame(ctx)

const tv = ctx.tv
const loadError = ref('')
const lobbyError = ref('')
const busy = ref(false)
let loaded = false
let closed = false

// open the feed first, then fetch; every reconnect refetches whatever was missed
async function refetch() {
  try {
    const t = await api.getTable(props.id)
    await ctx.apply(t, loaded ? 'quiet' : 'initial')
    loaded = true
    loadError.value = ''
  } catch (e) {
    if (!loaded) loadError.value = api.errorText(e)
  }
}
function onMessage(data: unknown) {
  if (typeof data !== 'string') return
  let msg: TableMessage
  try {
    msg = JSON.parse(data) as TableMessage
  } catch {
    return
  }
  if (msg.tag === 'TableUpdate') void ctx.apply(msg.contents, loaded ? 'live' : 'initial').then(() => (loaded = true))
  else if (msg.tag === 'TableClosed') leaveClosed()
}
const socket = useWebSocket(api.tableSocketUrl(props.id), {
  autoReconnect: { retries: () => !closed, delay: 2000 },
  onConnected: () => void refetch(),
  onMessage: (_ws, ev) => onMessage(ev.data),
})
// a socket that never opens (a proxy without websockets, say) must not leave the page blank
const fallback = setTimeout(() => {
  if (!loaded) void refetch()
}, 3000)
onUnmounted(() => {
  closed = true
  clearTimeout(fallback)
  socket.close()
})

function leaveClosed() {
  closed = true
  socket.close()
  void router.push({ path: '/', query: { closed: tv.value?.name ?? '1' } })
}

// ---- before the game starts -------------------------------------------------
const seats = computed(() => tv.value?.seats ?? [])
const filled = computed(() => seats.value.filter((s) => s.username).length)
const free = computed(() => seats.value.filter((s) => !s.username))
const allFilled = computed(() => seats.value.length > 0 && free.value.length === 0)
const started = computed(() => !!tv.value?.started)

async function act(f: () => Promise<unknown>) {
  if (busy.value) return
  busy.value = true
  try {
    const r = await f()
    if (r && typeof r === 'object' && 'version' in r) await ctx.apply(r as never, 'live')
    lobbyError.value = ''
  } catch (e) {
    lobbyError.value = api.errorText(e)
  } finally {
    busy.value = false
  }
}
const join = (seat?: number) => act(() => api.joinTable(props.id, seat))
const leaveSeat = (seat: number) => act(() => api.leaveTable(props.id, seat))
const start = () => act(() => api.startTable(props.id))
async function leaveTable() {
  await act(() => api.leaveTable(props.id))
  if (!lobbyError.value) void router.push('/')
}
async function closeTable() {
  if (busy.value) return
  busy.value = true
  try {
    await api.closeTable(props.id)
    closed = true
    void router.push('/')
  } catch (e) {
    const msg = api.errorText(e)
    lobbyError.value = msg
    ctx.error.value = msg
  } finally {
    busy.value = false
  }
}
const confirmClose = () => {
  if (confirm('Close this table for everyone?')) void closeTable()
}
</script>

<template>
  <template v-if="!tv">
    <header>
      <h1><RouterLink to="/" class="home-link">AH3e</RouterLink></h1>
    </header>
    <div class="screen">
      <p v-if="loadError" class="err">{{ loadError }}</p>
      <p v-else class="waiting">Loading table…</p>
      <RouterLink to="/">Back to the tables</RouterLink>
    </div>
  </template>
  <GameScreen v-else-if="started && tv.view" :on-close="closeTable" />
  <template v-else>
    <header>
      <h1><RouterLink to="/" class="home-link">AH3e</RouterLink> {{ tv.name }}</h1>
      <div class="pills">
        <span class="pill">{{ filled }}/{{ seats.length }} seated</span>
        <span class="pill">{{ tv.options.mode.replace('Mode', '') }} mode</span>
        <span class="pill">{{ tv.options.expansions.map(expName).join(', ') }}</span>
        <span v-if="tv.options.debug" class="pill">Debug</span>
      </div>
      <span style="flex: 1"></span>
      <span v-if="socket.status.value !== 'OPEN'" class="waiting">Reconnecting…</span>
    </header>
    <div class="screen">
      <header class="ng-header"><h2>{{ tv.name }}</h2></header>
      <p class="sub">Hosted by {{ tv.hostName }}. The game starts once every seat is taken and the host starts it.</p>
      <div class="ng">
        <div class="ng-card">
          <div class="ng-title">Seats</div>
          <ol class="seat-list">
            <li v-for="s in seats" :key="s.player" class="seat" :class="{ mine: ctx.isMine(s.player), open: !s.username }">
              <span class="seat-no">Seat {{ s.player }}</span>
              <span class="seat-user">
                <template v-if="s.username">{{ s.username }}<template v-if="s.username === tv.hostName"> (host)</template></template>
                <em v-else class="waiting">Open</em>
              </span>
              <button v-if="!s.username" :disabled="busy" @click="join(s.player)">Sit here</button>
              <button v-else-if="ctx.isMine(s.player)" :disabled="busy" @click="leaveSeat(s.player)">Leave seat</button>
            </li>
          </ol>
          <p class="ng-note">You may hold several seats to play more than one investigator.</p>
        </div>
        <div class="ng-actions table-actions">
          <button v-if="free.length" class="primary" :disabled="busy" @click="join()">
            {{ ctx.seated.value ? 'Take another seat' : 'Join the table' }}
          </button>
          <button
            v-if="ctx.isHost.value"
            class="primary"
            :disabled="busy || !allFilled"
            :title="allFilled ? 'Start the game' : 'Every seat needs a player first'"
            @click="start"
          >
            Start
          </button>
          <button v-if="ctx.isHost.value" :disabled="busy" @click="confirmClose">Close table</button>
          <button v-else-if="ctx.seated.value" :disabled="busy" @click="leaveTable">Leave table</button>
        </div>
        <div class="err">{{ lobbyError }}</div>
      </div>
    </div>
  </template>
  <Teleport to="body"><Overlays /></Teleport>
</template>

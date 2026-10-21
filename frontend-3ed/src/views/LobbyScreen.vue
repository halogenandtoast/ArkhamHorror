<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import * as api from '@/api'
import { img, isBroken, markBroken } from '@/assets'
import Overlays from '@/game/Overlays.vue'
import { expName } from '@/game/util'
import NewTableForm from '@/lobby/NewTableForm.vue'
import { catalog, loadCatalog, signOut, user } from '@/session'
import type { TableSummary } from '@/types'

const route = useRoute()
const router = useRouter()
const open = ref<TableSummary[]>([])
const mine = ref<TableSummary[]>([])
const error = ref('')
const loading = ref(true)
const showNew = ref(false)
const busy = ref<string | null>(null)
const closedNotice = computed(() => (typeof route.query.closed === 'string' ? route.query.closed : null))

async function refresh() {
  try {
    const r = await api.listTables()
    open.value = r.open
    mine.value = r.mine
    error.value = ''
  } catch (e) {
    error.value = api.errorText(e)
  } finally {
    loading.value = false
  }
}

const me = computed(() => user.value?.username ?? null)
const filled = (t: TableSummary) => t.seats.filter((s) => s.username).length
const hasFree = (t: TableSummary) => t.seats.some((s) => !s.username)
const isHost = (t: TableSummary) => t.hostName === me.value
const openToJoin = computed(() => open.value.filter((t) => !mine.value.some((m) => m.id === t.id)))
const seatsLabel = (t: TableSummary) => `${filled(t)}/${t.seats.length}`
// what the table is playing, when it was made for a scenario rather than asked in game
const scenarioOf = (t: TableSummary) =>
  catalog.value?.scenarios.find((sc) => sc.code === t.options.scenario) ?? null
const scenarioName = (t: TableSummary) => scenarioOf(t)?.name ?? null
const scenarioArt = (t: TableSummary) => {
  const sc = scenarioOf(t)
  return sc ? img(`scenarios/${sc.code}.webp`) : null
}
const extras = (t: TableSummary) => t.options.expansions.filter((e) => e !== 'CoreSet' && e !== scenarioOf(t)?.expansion)
const myUsers = (t: TableSummary) =>
  t.seats
    .filter((s) => s.username)
    .map((s) => s.username)
    .join(', ')

async function act(id: string, f: () => Promise<unknown>) {
  if (busy.value) return
  busy.value = id
  try {
    await f()
    error.value = ''
  } catch (e) {
    error.value = api.errorText(e)
  } finally {
    busy.value = null
    void refresh()
  }
}
const join = (t: TableSummary) =>
  act(t.id, async () => {
    await api.joinTable(t.id)
    void router.push(`/tables/${t.id}`)
  })
const leave = (t: TableSummary) => act(t.id, () => api.leaveTable(t.id))
// two tables made the same day carry the same default name, so the name alone
// cannot say which one this is about and closing a game cannot be undone
const closeDetail = (t: TableSummary) =>
  [scenarioName(t), `${t.seats.length} players`, `made ${new Date(t.createdAt).toLocaleString()}`]
    .filter(Boolean)
    .join(' \u00b7 ')
const close = (t: TableSummary) => {
  if (confirm(`Close "${t.name}" for everyone?\n\n${closeDetail(t)}\n\nThis cannot be undone.`))
    void act(t.id, () => api.closeTable(t.id))
}
const dismissNotice = () => void router.replace({ path: '/' })

// a debug file is an exported table; loading one makes a new table seated by whoever loads it
const fileInput = ref<HTMLInputElement | null>(null)
async function loadFile(e: Event) {
  const file = (e.target as HTMLInputElement).files?.[0]
  if (fileInput.value) fileInput.value.value = ''
  if (!file) return
  busy.value = 'import'
  try {
    const t = await api.importTable(JSON.parse(await file.text()))
    void router.push(`/tables/${t.id}`)
  } catch (err) {
    error.value = err instanceof SyntaxError ? "That file isn't a debug export" : api.errorText(err)
  } finally {
    busy.value = null
  }
}

let timer: ReturnType<typeof setInterval> | undefined
const onFocus = () => void refresh()
onMounted(() => {
  void refresh()
  void loadCatalog().catch((e) => (error.value = api.errorText(e)))
  timer = setInterval(() => {
    if (document.visibilityState === 'visible') void refresh()
  }, 10000)
  window.addEventListener('focus', onFocus)
})
onUnmounted(() => {
  clearInterval(timer)
  window.removeEventListener('focus', onFocus)
})
</script>

<template>
  <header>
    <h1>Arkham Horror 3e</h1>
    <span style="flex: 1"></span>
    <span class="pill">{{ me }}</span>
    <button @click="signOut">Sign out</button>
  </header>
  <div class="screen lobby">
    <p v-if="closedNotice" class="notice">
      The game {{ closedNotice === '1' ? '' : `"${closedNotice}" ` }}was closed by its host.
      <button class="log-close" aria-label="Dismiss" @click="dismissNotice">✕</button>
    </p>
    <div class="err">{{ error }}</div>

    <NewTableForm v-if="showNew && catalog" :catalog="catalog" @cancel="showNew = false" />
    <template v-else>
      <div class="lobby-hero">
        <div>
          <h2>Your games</h2>
        </div>
        <div class="lobby-hero-buttons">
          <button class="primary lobby-start" :disabled="!catalog" @click="showNew = true">New game</button>
          <button :disabled="busy === 'import'" @click="fileInput?.click()">
            {{ busy === 'import' ? 'Loading…' : 'Load game' }}
          </button>
          <input ref="fileInput" type="file" accept=".json,application/json" hidden @change="loadFile" />
        </div>
      </div>
      <section class="lobby-section">
        <h2>My games</h2>
        <p v-if="loading" class="waiting">Loading…</p>
        <p v-else-if="!mine.length" class="waiting">You aren't in any game yet.</p>
        <ul v-else class="table-list">
          <li v-for="t in mine" :key="t.id" class="table-row">
            <img
              v-if="scenarioArt(t) && !isBroken(scenarioArt(t)!)"
              class="table-art"
              :src="scenarioArt(t)!"
              alt=""
              @error="markBroken(scenarioArt(t)!)"
            />
            <div class="table-info">
              <RouterLink :to="`/tables/${t.id}`" class="table-name">{{ t.name }}</RouterLink>
              <div class="table-meta">
                <template v-if="scenarioName(t)">{{ scenarioName(t) }} · </template>
                {{ t.started ? 'In progress' : `Waiting · ${seatsLabel(t)} seated` }} · host {{ t.hostName }}
                <template v-if="extras(t).length"> · with {{ extras(t).map(expName).join(', ') }}</template>
              </div>
              <div class="table-meta">{{ myUsers(t) }}</div>
            </div>
            <div class="table-buttons">
              <button class="primary" @click="router.push(`/tables/${t.id}`)">{{ t.started ? 'Resume' : 'Open' }}</button>
              <button v-if="!t.started && !isHost(t)" :disabled="busy === t.id" @click="leave(t)">Leave</button>
              <button v-if="isHost(t)" :disabled="busy === t.id" @click="close(t)">Close</button>
            </div>
          </li>
        </ul>
      </section>

      <section class="lobby-section">
        <h2>Open games</h2>
        <p v-if="!loading && !openToJoin.length" class="waiting">No open games right now.</p>
        <ul v-else class="table-list">
          <li v-for="t in openToJoin" :key="t.id" class="table-row">
            <img
              v-if="scenarioArt(t) && !isBroken(scenarioArt(t)!)"
              class="table-art"
              :src="scenarioArt(t)!"
              alt=""
              @error="markBroken(scenarioArt(t)!)"
            />
            <div class="table-info">
              <span class="table-name">{{ t.name }}</span>
              <div class="table-meta">
                <template v-if="scenarioName(t)">{{ scenarioName(t) }} · </template>host {{ t.hostName }} ·
                {{ seatsLabel(t) }} seated · {{ t.options.mode.replace('Mode', '') }} mode<template
                  v-if="extras(t).length"
                >
                  · with {{ extras(t).map(expName).join(', ') }}</template
                >
              </div>
            </div>
            <div class="table-buttons">
              <button class="primary" :disabled="!hasFree(t) || busy === t.id" @click="join(t)">Join</button>
              <button @click="router.push(`/tables/${t.id}`)">View</button>
            </div>
          </li>
        </ul>
      </section>
    </template>
  </div>
  <Teleport to="body"><Overlays /></Teleport>
</template>

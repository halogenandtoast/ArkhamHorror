<script setup lang="ts">
/* What the nightly archive sweep has reclaimed.
 *
 * The whole mechanism is a bet that most of `arkham_steps` -- 94 of 98 GB, and
 * nothing ever pruned it -- belongs to games nobody is coming back to. These are
 * the numbers that say whether that is true and whether the sweep is still
 * running: games offloaded, step rows removed, bytes in the bucket, and how
 * deep the remaining backlog is.
 *
 * Plain strings rather than i18n, matching the rest of the admin panel: it has
 * exactly one reader.
 */
import { computed, onMounted, ref } from 'vue'
import * as Api from '@/arkham/api'

const stats = ref<Api.ArchiveStats | null>(null)
const loading = ref(true)
const running = ref(false)
const error = ref<string | null>(null)
const notice = ref<string | null>(null)

async function load() {
  try {
    stats.value = await Api.fetchArchiveStats()
    error.value = null
  } catch (e) {
    console.error(e)
    error.value = 'Unable to load archive stats.'
  } finally {
    loading.value = false
  }
}

/* The server starts a run a minute after every replica boots, so this is for
 * the times between deploys. A 409 means one already ran within the hour; the
 * only answer to that is to say so and offer to do it anyway. */
const canForce = ref(false)

async function runNow(force = false) {
  running.value = true
  notice.value = null
  error.value = null
  canForce.value = false
  try {
    await Api.runArchiveSweep(force)
    notice.value =
      'Sweep started. It works in batches until the backlog is empty, so this is hours, not minutes.'
  } catch (e) {
    const status = (e as { response?: { status?: number } }).response?.status
    if (status === 409) {
      error.value = 'A sweep has already run in the last hour.'
      canForce.value = true
    } else {
      error.value = 'Could not start a sweep.'
    }
    console.error(e)
  } finally {
    running.value = false
  }
}

/* Archiving a game you chose, which is how this gets done before -- or instead
 * of -- letting the nightly job pick. */
const gameId = ref('')
const archiving = ref(false)
const needsForce = ref(false)
const archiveResult = ref<string | null>(null)
const archiveError = ref<string | null>(null)

const uuidish = /^[0-9a-f-]{36}$/i

async function archiveGame(force = false) {
  const id = gameId.value.trim()
  archiveResult.value = null
  archiveError.value = null
  needsForce.value = false
  if (!uuidish.test(id)) {
    archiveError.value = "That does not look like a game id."
    return
  }
  archiving.value = true
  try {
    const r = await Api.archiveGameNow(id, force)
    archiveResult.value = `Archived "${r.game}": ${num(r.steps)} steps, ${bytes(r.bytes)} stored. Press Undo in that game to fetch it back.`
    gameId.value = ''
    await load()
  } catch (e) {
    const res = (e as { response?: { status?: number; data?: { error?: string } } }).response
    archiveError.value = res?.data?.error ?? 'Could not archive that game.'
    // 409 is the recency guard or an archive that already exists; only the
    // first of those is worth offering to override.
    needsForce.value = res?.status === 409 && (res.data?.error ?? '').includes('force=true')
    console.error(e)
  } finally {
    archiving.value = false
  }
}

/* The other half of the loop: put one back without having to go and press Undo
 * in the game. Same endpoint the player's prompt calls. */
const restoring = ref<string | null>(null)

async function restore(id: string) {
  restoring.value = id
  archiveError.value = null
  archiveResult.value = null
  try {
    await Api.unarchiveGame(id)
    archiveResult.value = 'Restore started. It runs in the background; refresh in a moment.'
  } catch (e) {
    archiveError.value = 'Could not start that restore.'
    console.error(e)
  } finally {
    restoring.value = null
  }
}

onMounted(load)

const num = (n: number) => n.toLocaleString()

/* Binary units, because this is storage and the bucket is billed by it. */
const bytes = (n: number) => {
  if (n < 1024) return `${n} B`
  const units = ['KiB', 'MiB', 'GiB', 'TiB']
  let value = n / 1024
  let unit = 0
  while (value >= 1024 && unit < units.length - 1) {
    value /= 1024
    unit += 1
  }
  return `${value.toFixed(value < 10 ? 1 : 0)} ${units[unit]}`
}

const when = (iso: string | null) => (iso ? new Date(iso).toLocaleString() : '—')

const backlog = computed(() => {
  const s = stats.value
  if (!s) return '—'
  // The count stops at the cap, so at the cap it is a floor, not a total.
  return s.candidates >= s.candidateCap ? `${num(s.candidateCap)}+` : num(s.candidates)
})

const lastSweep = computed(() => {
  const s = stats.value
  if (!s?.lastSweepAt) return 'never'
  const ms = s.lastSweepMs
  const took = ms === null ? '' : ` (took ${ms < 1000 ? `${ms}ms` : `${Math.round(ms / 1000)}s`})`
  return `${when(s.lastSweepAt)}${took}`
})
</script>

<template>
  <section class="admin-block">
    <header class="section-header">
      <h2>Archived Games</h2>
      <span v-if="stats && !stats.enabled" class="section-note">disabled — no bucket configured</span>
      <span v-else-if="stats" class="section-note">
        <template v-if="stats.sweepEnabled">
          nightly · {{ stats.afterMonths }}mo retention · {{ stats.batchSize }} games every
          {{ stats.pauseSeconds }}s until done
        </template>
        <template v-else>manual only — the nightly job is off</template>
        · {{ stats.bucket }}
      </span>
      <button
        v-if="stats?.enabled"
        class="run-button"
        :disabled="running"
        :title="stats.sweepEnabled
          ? 'Archive every game past the retention cutoff, in batches, until none are left'
          : 'The nightly job is off, but this still archives every game past the cutoff'"
        @click="runNow(false)"
      >
        {{ running ? 'Starting…' : 'Sweep the whole backlog' }}
      </button>
    </header>

    <div v-if="loading" class="empty box">Loading archive stats…</div>
    <template v-else-if="stats">
      <p v-if="error" class="empty box">
        {{ error }}
        <button v-if="canForce" class="run-button" :disabled="running" @click="runNow(true)">
          Run anyway
        </button>
      </p>
      <p v-if="notice" class="empty box">{{ notice }}</p>

      <div v-if="stats.enabled" class="pick box">
        <label for="archive-game-id">Archive one game</label>
        <p class="pick-hint">
          A game id — from the dashboard's game lists, or a game's admin page. This ignores the
          retention cutoff, so it is how you archive a game you have chosen, press Undo
          in it, and watch the history come back. Nothing is deleted until the uploaded
          copy has been downloaded again and checked.
        </p>
        <div class="pick-row">
          <input
            id="archive-game-id"
            v-model="gameId"
            type="text"
            placeholder="00000000-0000-0000-0000-000000000000"
            spellcheck="false"
            :disabled="archiving"
            @keyup.enter="archiveGame(false)"
          />
          <button class="run-button" :disabled="archiving || !gameId.trim()" @click="archiveGame(false)">
            {{ archiving ? 'Archiving…' : 'Archive it' }}
          </button>
        </div>
        <p v-if="archiveResult" class="pick-ok">{{ archiveResult }}</p>
        <p v-if="archiveError" class="pick-bad">
          {{ archiveError }}
          <button v-if="needsForce" class="run-button" :disabled="archiving" @click="archiveGame(true)">
            Archive it anyway
          </button>
        </p>
      </div>

      <div class="stats-grid">
        <div class="stat-card box">
          <span class="stat-label">Games Archived</span>
          <strong>{{ num(stats.archived) }}</strong>
        </div>
        <div class="stat-card box">
          <span class="stat-label">Steps Offloaded</span>
          <strong>{{ num(stats.stepsOffloaded) }}</strong>
        </div>
        <div class="stat-card box">
          <span class="stat-label">In The Bucket</span>
          <strong>{{ bytes(stats.bytesStored) }}</strong>
        </div>
        <div class="stat-card box">
          <span class="stat-label">Backlog</span>
          <strong>{{ backlog }}</strong>
        </div>
      </div>

      <dl class="facts">
        <div><dt>Last sweep</dt><dd>{{ lastSweep }}</dd></div>
        <div><dt>Archived in last 24h</dt><dd>{{ num(stats.archivedLastDay) }}</dd></div>
        <div><dt>Restored in last 7d</dt><dd>{{ num(stats.restoredLastWeek) }}</dd></div>
        <div><dt>Restores running</dt><dd>{{ num(stats.restoring) }}</dd></div>
        <div><dt>Brought back (total)</dt><dd>{{ num(stats.restored) }}</dd></div>
        <div><dt>Failed restores</dt><dd>{{ num(stats.failed) }}</dd></div>
        <div>
          <dt>Steps ever archived</dt>
          <dd>{{ num(stats.stepsEverArchived) }}</dd>
        </div>
      </dl>

      <table v-if="stats.recent.length" class="recent">
        <thead>
          <tr>
            <th>Game</th>
            <th>State</th>
            <th class="right">Steps</th>
            <th class="right">Size</th>
            <th>Last played</th>
            <th>Archived</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in stats.recent" :key="row.gameId">
            <td>
              <router-link :to="`/admin/games/${row.gameId}`">{{ row.gameName }}</router-link>
            </td>
            <td>
              <span class="state" :class="`state--${row.state}`">{{ row.state }}</span>
              <span v-if="row.error" class="row-error" :title="row.error">!</span>
            </td>
            <td class="right">{{ num(row.steps) }}</td>
            <td class="right">{{ bytes(row.bytes) }}</td>
            <td>{{ when(row.gameUpdatedAt) }}</td>
            <td>{{ when(row.archivedAt) }}</td>
            <td>
              <button
                v-if="row.state === 'archived' || row.state === 'failed'"
                class="run-button"
                :disabled="restoring === row.gameId"
                @click="restore(row.gameId)"
              >
                {{ restoring === row.gameId ? 'Starting…' : 'Restore' }}
              </button>
            </td>
          </tr>
        </tbody>
      </table>
      <div v-else class="empty box">Nothing has been archived yet.</div>
    </template>
    <p v-else class="empty box">{{ error }}</p>
  </section>
</template>

<style scoped>
/* Every admin page styles its own chrome: the shell's styles are scoped, so the
 * block and header shared with the other sections are restated here. */
.admin-block {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 14px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
}

.section-header {
  align-items: center;
  display: flex;
  gap: 12px;
}

.section-header h2 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.6rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.section-note {
  color: color-mix(in srgb, var(--title) 60%, transparent);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.empty {
  color: var(--title);
  opacity: 0.75;
}

.stats-grid {
  display: grid;
  gap: 10px;
  grid-template-columns: repeat(4, minmax(0, 1fr));
}

.stat-card {
  display: flex;
  flex-direction: column;
  gap: 6px;
  min-height: 82px;
  justify-content: center;
}

.stat-label {
  color: color-mix(in srgb, var(--title) 70%, transparent);
  font-size: 0.8rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.stat-card strong {
  color: var(--title);
  font-family: "Noto Sans", Avenir, Helvetica, Arial, sans-serif;
  font-size: 2.1rem;
  font-weight: 800;
  line-height: 1;
}

@media (max-width: 900px) {
  .stats-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

@media (max-width: 520px) {
  .stats-grid {
    grid-template-columns: 1fr;
  }
}

.run-button {
  margin-left: auto;
  padding: 6px 12px;
  border-radius: 6px;
  border: 1px solid rgba(255, 255, 255, 0.18);
  background: rgba(255, 255, 255, 0.14);
  color: var(--title);
  cursor: pointer;
  font-size: 0.8rem;
}

.run-button:hover:not(:disabled) {
  border-color: rgba(255, 255, 255, 0.45);
}

.run-button:disabled {
  cursor: progress;
  opacity: 0.6;
}

.pick {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.pick label {
  color: color-mix(in srgb, var(--title) 70%, transparent);
  font-size: 0.8rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.pick-hint {
  margin: 0;
  font-size: 0.8rem;
  opacity: 0.75;
  max-width: 70ch;
}

.pick-row {
  display: flex;
  gap: 8px;
  align-items: center;
  flex-wrap: wrap;
}

.pick-row input {
  flex: 1 1 24ch;
  min-width: 0;
  padding: 6px 10px;
  border-radius: 6px;
  border: 1px solid rgba(255, 255, 255, 0.18);
  background: rgba(0, 0, 0, 0.25);
  color: var(--title);
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 0.82rem;
}

.pick-ok,
.pick-bad {
  margin: 0;
  font-size: 0.85rem;
  display: flex;
  gap: 10px;
  align-items: center;
  flex-wrap: wrap;
}

.pick-bad {
  color: #e74c3c;
}

.facts {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 4px 18px;
  margin: 0;
}

.facts div {
  display: flex;
  justify-content: space-between;
  gap: 10px;
  padding: 4px 0;
  border-bottom: 1px solid color-mix(in srgb, var(--box-border) 40%, transparent);
}

.facts dt {
  color: color-mix(in srgb, var(--title) 70%, transparent);
  font-size: 0.82rem;
}

.facts dd {
  margin: 0;
  color: var(--title);
  font-variant-numeric: tabular-nums;
}

.recent {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.82rem;
}

.recent th {
  text-align: left;
  padding: 6px 8px;
  color: color-mix(in srgb, var(--title) 70%, transparent);
  border-bottom: 1px solid var(--box-border);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
}

.recent td {
  padding: 6px 8px;
  border-bottom: 1px solid color-mix(in srgb, var(--box-border) 40%, transparent);
  font-variant-numeric: tabular-nums;
}

.right {
  text-align: right;
}

.state {
  text-transform: uppercase;
  font-size: 0.7rem;
  letter-spacing: 0.06em;
  padding: 2px 6px;
  border-radius: 4px;
  background: rgba(255, 255, 255, 0.1);
}

.state--restoring {
  background: rgba(255, 255, 255, 0.22);
}

.state--failed {
  background: rgba(192, 57, 43, 0.5);
}

.row-error {
  margin-left: 6px;
  color: #e74c3c;
  cursor: help;
  font-weight: 700;
}
</style>

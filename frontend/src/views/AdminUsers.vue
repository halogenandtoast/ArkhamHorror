<script setup lang="ts">
/* Finding an account, what it has been let into, and what it has been playing.
 *
 * A feature rolls out a few accounts at a time, so the page opens on the list of
 * accounts that already have one -- short by definition, and the answer to "who
 * is in so far". Searching replaces it with the match, which is how somebody new
 * is found. Clearing the box puts the first list back.
 *
 * The search matches username and email alike, and the email is shown as well as
 * the name: a row that matched on one has to show the thing it matched.
 *
 * A checkbox writes immediately and sends the whole set, so what is on screen is
 * what the account ends up with. Admins are shown as already having everything,
 * because the server treats them that way -- a checkbox that looked off while
 * the feature worked would be a lie about the gate.
 *
 * Games load only when a row is opened, because reporting them means decoding
 * every game blob on the account and almost every row on screen is not the one
 * being asked about. Active and finished are split on what the server says
 * rather than recomputed here, so a game too broken to decode still lands in a
 * list instead of disappearing from both.
 */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import GameRow from '@/arkham/components/GameRow.vue'

const { t } = useI18n()
const K = 'adminUsers.'

const allFeatures = ref<string[]>([])
const rows = ref<Api.AdminUser[]>([])
const loading = ref(true)
const error = ref<string | null>(null)
const status = ref<string | null>(null)
const busy = ref<number | null>(null)

const query = ref('')
const searching = ref(false)
/* What the shown list is: the standing grants, or the result of a search. The
 * two read differently -- an empty search means "nobody by that name", an empty
 * grant list means "nobody is in yet" -- so the page has to know which it has. */
const showingSearch = ref(false)

const granted = computed(() => rows.value.filter((r) => r.features.length > 0))

async function load(q?: string) {
  loading.value = true
  error.value = null
  try {
    const data = await Api.fetchAdminUsers(q)
    allFeatures.value = data.allFeatures
    rows.value = data.users
    showingSearch.value = !!q
  } catch (e) {
    console.error(e)
    error.value = t(`${K}loadFailed`)
  } finally {
    loading.value = false
  }
}

onMounted(() => load())

async function search() {
  const q = query.value.trim()
  searching.value = true
  status.value = null
  try {
    await load(q || undefined)
  } finally {
    searching.value = false
  }
}

async function clearSearch() {
  query.value = ''
  status.value = null
  await load()
}

/* Keyed by account id rather than held on the row, so a search that replaces
 * the rows does not have to carry what was already fetched -- and reopening an
 * account that was open before the search still shows it without a refetch. */
const games = ref<Record<number, Api.AdminUserGames>>({})
const gamesOpen = ref<number[]>([])
const gamesLoading = ref<number[]>([])
const gamesError = ref<Record<number, string>>({})

const isOpen = (row: Api.AdminUser) => gamesOpen.value.includes(row.id)

/* The two headings a row's games are split under. Split on the server's answer
 * rather than on the game state here, so an entry too broken to decode is still
 * in one of them. */
const gameGroups = computed(() => [
  { active: true, heading: t(`${K}activeGames`), empty: t(`${K}noActiveGames`) },
  { active: false, heading: t(`${K}inactiveGames`), empty: t(`${K}noInactiveGames`) },
])

function inGroup(entries: Api.AdminUserGame[], active: boolean) {
  return entries.filter((entry) => entry.active === active)
}

function formatDate(at: string) {
  return new Date(at).toLocaleString()
}

async function toggleGames(row: Api.AdminUser) {
  if (isOpen(row)) {
    gamesOpen.value = gamesOpen.value.filter((id) => id !== row.id)
    return
  }

  gamesOpen.value = [...gamesOpen.value, row.id]
  if (games.value[row.id]) return

  gamesLoading.value = [...gamesLoading.value, row.id]
  delete gamesError.value[row.id]
  try {
    games.value[row.id] = await Api.fetchAdminUserGames(row.id)
  } catch (e) {
    console.error(e)
    gamesError.value[row.id] = t(`${K}gamesFailed`)
  } finally {
    gamesLoading.value = gamesLoading.value.filter((id) => id !== row.id)
  }
}

function has(row: Api.AdminUser, feature: string) {
  return row.admin || row.features.includes(feature)
}

async function toggle(row: Api.AdminUser, feature: string) {
  if (row.admin) return
  const next = row.features.includes(feature)
    ? row.features.filter((f) => f !== feature)
    : [...row.features, feature]

  busy.value = row.id
  error.value = null
  status.value = null
  try {
    const updated = await Api.updateUserFeatures(row.id, next)
    rows.value = rows.value.map((r) => (r.id === updated.id ? updated : r))
    status.value = updated.features.includes(feature)
      ? t(`${K}granted`, { feature, username: updated.username })
      : t(`${K}revoked`, { feature, username: updated.username })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}saveFailed`)
  } finally {
    busy.value = null
  }
}
</script>

<template>
  <section class="admin-block">
    <header class="section-header">
      <h2>{{ t(`${K}title`) }}</h2>
      <span v-if="!showingSearch" class="count-badge">{{ granted.length }}</span>
    </header>

    <p class="section-note">{{ t(`${K}note`) }}</p>

    <p v-if="status" class="status">{{ status }}</p>
    <p v-if="error" class="error">{{ error }}</p>

    <form class="search" @submit.prevent="search">
      <label for="user-search">{{ t(`${K}searchLabel`) }}</label>
      <div class="search-row">
        <input
          id="user-search"
          v-model="query"
          type="text"
          :placeholder="t(`${K}searchPlaceholder`)"
          @keydown.stop
        />
        <button type="submit" class="publish" :disabled="searching">
          {{ searching ? t(`${K}searching`) : t(`${K}search`) }}
        </button>
        <button v-if="showingSearch" type="button" class="ghost" @click="clearSearch">
          {{ t(`${K}showGranted`) }}
        </button>
      </div>
    </form>

    <p v-if="loading" class="empty box">{{ t(`${K}loading`) }}</p>
    <p v-else-if="!rows.length" class="empty box">
      {{ showingSearch ? t(`${K}noMatches`) : t(`${K}noneGranted`) }}
    </p>

    <ul v-else class="list">
      <li v-for="row in rows" :key="row.id" class="row">
        <div class="head">
          <p class="meta">
            <strong class="username">{{ row.username }}</strong>
            <span class="email">{{ row.email }}</span>
            <span v-if="row.admin" class="tag admin">{{ t(`${K}admin`) }}</span>
          </p>
          <button
            type="button"
            class="ghost"
            :aria-expanded="isOpen(row)"
            @click="toggleGames(row)"
          >
            {{ isOpen(row) ? t(`${K}hideGames`) : t(`${K}showGames`) }}
          </button>
        </div>

        <div class="features">
          <label v-for="feature in allFeatures" :key="feature" class="feature">
            <input
              type="checkbox"
              :checked="has(row, feature)"
              :disabled="row.admin || busy === row.id"
              @change="toggle(row, feature)"
            />
            <span>{{ t(`${K}feature.${feature}`) }}</span>
          </label>
        </div>

        <p v-if="row.admin" class="hint">{{ t(`${K}adminImplicit`) }}</p>

        <div v-if="isOpen(row)" class="games">
          <p v-if="gamesLoading.includes(row.id)" class="empty box">{{ t(`${K}gamesLoading`) }}</p>
          <p v-else-if="gamesError[row.id]" class="error">{{ gamesError[row.id] }}</p>
          <template v-else-if="games[row.id]">
            <p v-if="!games[row.id].games.length" class="empty box">{{ t(`${K}noGames`) }}</p>
            <template v-else>
              <section
                v-for="group in gameGroups"
                :key="String(group.active)"
                class="game-group"
              >
                <header class="group-header">
                  <h3>{{ group.heading }}</h3>
                  <span class="count-badge">{{ inGroup(games[row.id].games, group.active).length }}</span>
                </header>
                <p v-if="!inGroup(games[row.id].games, group.active).length" class="hint">
                  {{ group.empty }}
                </p>
                <div v-else class="game-list">
                  <div
                    v-for="entry in inGroup(games[row.id].games, group.active)"
                    :key="entry.game.id"
                    class="game-entry"
                  >
                    <GameRow v-if="entry.game.tag === 'game'" :game="entry.game" :admin="true" />
                    <p v-else class="error">{{ entry.game.name }} &mdash; {{ entry.game.error }}</p>
                    <p class="played">{{ t(`${K}lastPlayed`, { at: formatDate(entry.updatedAt) }) }}</p>
                  </div>
                </div>
              </section>
              <p v-if="games[row.id].total > games[row.id].games.length" class="hint">
                {{ t(`${K}gamesTruncated`, {
                  shown: games[row.id].games.length,
                  total: games[row.id].total,
                }) }}
              </p>
            </template>
          </template>
        </div>
      </li>
    </ul>
  </section>
</template>

<style scoped>
/* The admin shell's styles are scoped, so the panel chrome is restated here
   rather than inherited. Same chrome as the other admin panels. */
.admin-block {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
  display: flex;
  flex-direction: column;
  gap: 12px;
  padding: 14px;
}

.section-header {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
}

.section-header h2 {
  color: var(--title);
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.6rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.count-badge {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--spooky-green);
  border-left-width: 4px;
  border-radius: 3px;
  color: color-mix(in srgb, var(--spooky-green) 78%, white);
  display: inline-flex;
  font-size: 0.78rem;
  font-weight: 800;
  justify-content: center;
  line-height: 1;
  min-width: 2.1em;
  padding: 5px 7px;
}

.section-note {
  color: color-mix(in srgb, var(--title) 62%, transparent);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  margin: 0;
  text-transform: uppercase;
}

.status,
.error {
  border-radius: 4px;
  font-size: 0.9rem;
  margin: 0;
  padding: 8px 10px;
}

.status {
  background: color-mix(in srgb, var(--spooky-green-dark) 60%, transparent);
  color: white;
}

.error {
  background: color-mix(in srgb, #7a2020 65%, transparent);
  color: #ffdede;
}

.empty {
  color: var(--title);
  margin: 0;
  opacity: 0.75;
  padding: 10px;
}

.hint {
  color: color-mix(in srgb, var(--title) 55%, transparent);
  font-size: 0.8rem;
  margin: 0;
}

/* ----------------------------------------------------------------- search --- */

.search {
  border: 1px solid var(--box-border);
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 12px;
}

.search > label {
  color: color-mix(in srgb, var(--title) 72%, transparent);
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.search-row {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}

input[type="text"] {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  box-sizing: border-box;
  color: var(--title);
  flex: 1;
  font-family: "Noto Sans", Avenir, Helvetica, Arial, sans-serif;
  font-size: 0.95rem;
  min-width: 12em;
  padding: 8px;
}

input[type="text"]:focus {
  border-color: var(--spooky-green);
  outline: none;
}

button {
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.82rem;
  font-weight: 800;
  letter-spacing: 0.06em;
  padding: 8px 12px;
  text-transform: uppercase;
}

button:disabled {
  cursor: not-allowed;
  opacity: 0.45;
}

.publish {
  background: var(--spooky-green-dark);
  border: 1px solid var(--spooky-green);
  color: white;
}

.publish:hover:not(:disabled) {
  background: var(--spooky-green);
}

.ghost {
  background: transparent;
  border: 1px solid var(--box-border);
  color: color-mix(in srgb, var(--title) 80%, transparent);
}

.ghost:hover {
  border-color: var(--spooky-green);
  color: white;
}

/* ------------------------------------------------------------------- list --- */

.list {
  display: flex;
  flex-direction: column;
  gap: 12px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.row {
  border: 1px solid var(--box-border);
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 12px;
}

.head {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}

.meta {
  align-items: center;
  color: color-mix(in srgb, var(--title) 62%, transparent);
  display: flex;
  flex: 1;
  flex-wrap: wrap;
  font-size: 0.82rem;
  gap: 8px;
  margin: 0;
}

.username {
  color: color-mix(in srgb, var(--spooky-green) 80%, white);
  font-size: 1rem;
  font-weight: 800;
}

.email {
  color: color-mix(in srgb, var(--title) 58%, transparent);
  font-size: 0.82rem;
  overflow-wrap: anywhere;
}

.tag.admin {
  background: #2f4668;
  border-radius: 3px;
  color: #cfe0ff;
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.06em;
  padding: 3px 6px;
  text-transform: uppercase;
}

.features {
  display: flex;
  flex-wrap: wrap;
  gap: 14px;
}

.feature {
  align-items: center;
  color: var(--title);
  cursor: pointer;
  display: flex;
  font-size: 0.9rem;
  gap: 6px;
}

.feature input:disabled {
  cursor: not-allowed;
}

.feature input:disabled + span {
  opacity: 0.6;
}

/* ------------------------------------------------------------------ games --- */

.games {
  border-top: 1px solid color-mix(in srgb, var(--box-border) 70%, transparent);
  display: flex;
  flex-direction: column;
  gap: 12px;
  padding-top: 12px;
}

.game-group {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.group-header {
  align-items: center;
  display: flex;
  gap: 10px;
}

.group-header h3 {
  color: color-mix(in srgb, var(--title) 80%, transparent);
  font-size: 0.78rem;
  font-weight: 800;
  letter-spacing: 0.08em;
  margin: 0;
  text-transform: uppercase;
}

.game-list {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.game-entry {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.played {
  color: color-mix(in srgb, var(--title) 52%, transparent);
  font-size: 0.75rem;
  margin: 0;
}

@media (max-width: 620px) {
  .search-row {
    align-items: stretch;
    flex-direction: column;
  }
}
</style>

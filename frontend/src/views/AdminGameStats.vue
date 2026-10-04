<script setup lang="ts">
/* Play statistics, broken down by campaign and standalone.
 *
 * Everything here is read off a materialized view the server maintains, because
 * the underlying facts live inside a 28kB jsonb blob per game that costs ~0.7ms
 * each to crack open. So the numbers are as of the last rebuild, and the header
 * says when that was and offers to do it again.
 *
 * Charting follows the ordinal rule: player count and difficulty are *ordered*
 * categories, so they take a one-hue light-to-dark ramp rather than four
 * unrelated hues -- which is also the only way the mix bar stays readable to a
 * red-green colourblind viewer. The Arkham class colours were the obvious first
 * choice and were rejected: rogue green against seeker orange is ΔE 2.5 under
 * protanopia, i.e. the same colour. Every segment is additionally labelled, so
 * identity never rests on colour alone.
 */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import {
  achievementCampaignScope,
  achievementCatalog,
  achievementEntryScope,
} from '@/arkham/achievements'

const { t, te } = useI18n()
const K = 'adminStats.'

const stats = ref<Api.GameStats | null>(null)
const loading = ref(true)
const refreshing = ref(false)
const error = ref<string | null>(null)

async function load() {
  loading.value = true
  error.value = null
  try {
    stats.value = await Api.fetchGameStats()
  } catch (e) {
    console.error(e)
    error.value = t(`${K}loadFailed`)
  } finally {
    loading.value = false
  }
}

async function rebuild() {
  refreshing.value = true
  error.value = null
  try {
    stats.value = await Api.refreshGameStats()
  } catch (e) {
    console.error(e)
    error.value = t(`${K}refreshFailed`)
  } finally {
    refreshing.value = false
  }
}

onMounted(load)

// --------------------------------------------------------------- formatting ---

/* A code's title, falling back to the code. Homebrew ids carry a leading ':'
 * that is an internal marker, not part of any name. */
const nameOf = (code: string) =>
  stats.value?.names[code] ?? stats.value?.names[code.replace(/^:/, '')] ?? code

const pct = (part: number, whole: number) => (whole > 0 ? (part / whole) * 100 : 0)
const pctLabel = (part: number, whole: number) =>
  whole > 0 ? `${Math.round(pct(part, whole))}%` : '—'

const num = (n: number) => n.toLocaleString()

const staleness = computed(() => {
  const at = stats.value?.refreshedAt
  if (!at) return null
  const ms = Date.now() - new Date(at).getTime()
  const hours = Math.floor(ms / 3_600_000)
  if (hours < 1) return t(`${K}freshMinutes`, { n: Math.max(1, Math.floor(ms / 60_000)) })
  if (hours < 48) return t(`${K}freshHours`, { n: hours })
  return t(`${K}freshDays`, { n: Math.floor(hours / 24) })
})

// ------------------------------------------------------------------- series ---

/* The ordinal ramp, light to dark. Validated as a ramp against this surface:
 * one hue, monotone lightness, visible gaps, dark end clearing the background. */
const RAMP = ['#dbe7bd', '#b9cd8e', '#94ab5f', '#6e8139']

/* A single sequential fill for the one-series magnitude bars. The mid step, so a
 * lone bar reads at the same weight as the middle of a mix bar. */
const FILL = '#94ab5f'

type Segment = { label: string; value: number; color: string }

/* The top bucket is "4 or more", not "4": production has a handful of 5- and
 * 6-player games, and the server folds them in here rather than giving three
 * games their own column. Labelled 4+ so the bar does not claim they are 4. */
const playerLabel = (n: number) =>
  n >= 4 ? t(`${K}nPlayerPlus`, { n: 4 }) : t(`${K}nPlayer`, { n })

const playerSegments = (row: {
  players1: number
  players2: number
  players3: number
  players4: number
}): Segment[] =>
  [row.players1, row.players2, row.players3, row.players4]
    .map((value, i) => ({ label: playerLabel(i + 1), value, color: RAMP[i] }))
    .filter((s) => s.value > 0)

const difficultySegments = (row: {
  easy: number
  standard: number
  hard: number
  expert: number
}): Segment[] =>
  [
    { label: t(`${K}difficulty.Easy`), value: row.easy, color: RAMP[0] },
    { label: t(`${K}difficulty.Standard`), value: row.standard, color: RAMP[1] },
    { label: t(`${K}difficulty.Hard`), value: row.hard, color: RAMP[2] },
    { label: t(`${K}difficulty.Expert`), value: row.expert, color: RAMP[3] },
  ].filter((s) => s.value > 0)

const segmentTotal = (segments: Segment[]) => segments.reduce((a, s) => a + s.value, 0)

// -------------------------------------------------------------------- tables ---

const campaigns = computed(() => stats.value?.campaigns ?? [])

const overallPlayers = computed<Segment[]>(() =>
  (stats.value?.playerCounts ?? []).map((c) => ({
    label: playerLabel(Number(c.key)),
    value: c.count,
    color: RAMP[Math.min(3, Math.max(0, Number(c.key) - 1))],
  })),
)

const overallDifficulty = computed<Segment[]>(() => {
  const order = ['Easy', 'Standard', 'Hard', 'Expert']
  const rows = stats.value?.difficulties ?? []
  return order
    .map((key, i) => ({
      label: t(`${K}difficulty.${key}`),
      value: rows.find((r) => r.key === key)?.count ?? 0,
      color: RAMP[i],
    }))
    .filter((s) => s.value > 0)
})

/* Side stories, split into the ones taken on their own and the ones that are not
 * side stories at all -- a standalone run of a campaign scenario. Both are
 * interesting, and lumping them together answers neither question. */
const sideStoriesSolo = computed(() =>
  (stats.value?.standalones ?? []).filter((s) => s.isSideStory),
)
const standaloneScenarios = computed(() =>
  (stats.value?.standalones ?? []).filter((s) => !s.isSideStory),
)

/* Which scenarios go badly. Sorted by success rate ascending, so the top of the
 * list is the wall people hit. A floor on plays, because one loss out of one play
 * is not a 0% success rate, it is no data. */
const MIN_PLAYS = 5
const hardest = computed(() =>
  (stats.value?.scenarioOutcomes ?? [])
    .filter((s) => s.played >= MIN_PLAYS)
    .map((s) => ({ ...s, rate: pct(s.won, s.played) }))
    .sort((a, b) => a.rate - b.rate),
)

const mostPlayedScenarios = computed(() =>
  [...(stats.value?.scenarioOutcomes ?? [])].sort((a, b) => b.played - a.played).slice(0, 20),
)

/* The catalogue knows every achievement's title and which campaign it belongs to;
 * the server only knows the tag. Joining here keeps the backend out of the
 * business of naming things the frontend already names. */
const catalogByTag = new Map(achievementCatalog.map((e) => [e.tag as string, e]))

const achievementName = (tag: string) => {
  const key = `${achievementEntryScope(tag)}.name`
  return te(key) ? t(key) : tag
}

const achievementCampaign = (tag: string) => {
  const entry = catalogByTag.get(tag)
  if (!entry) return null
  const key = achievementCampaignScope(entry.campaignId)
  return te(key) ? t(key) : entry.campaignId
}

/* Rarest first: an achievement nobody has is the one worth looking at, either
 * because it is brutal or because it is unreachable. */
const achievements = computed(() => {
  const rows = stats.value?.achievements ?? []
  return [...rows]
    .map((a) => ({
      ...a,
      name: achievementName(a.id),
      campaign: achievementCampaign(a.id),
    }))
    .sort((a, b) => a.earned - b.earned || a.name.localeCompare(b.name))
})

const monthlyMax = computed(() =>
  Math.max(1, ...(stats.value?.monthly ?? []).map((m) => m.count)),
)

// ------------------------------------------------------- per-campaign detail ---

const selected = ref<string | null>(null)

const campaignOptions = computed(() =>
  campaigns.value.map((c) => ({ value: c.id, label: nameOf(c.id) })),
)

const selectedCampaign = computed(
  () => campaigns.value.find((c) => c.id === selected.value) ?? campaigns.value[0] ?? null,
)

const selectedId = computed(() => selectedCampaign.value?.id ?? null)

/* Side stories taken during this campaign, commonest first. */
const sideStoriesHere = computed(() =>
  (stats.value?.sideStoriesInCampaigns ?? [])
    .filter((s) => s.campaignId === selectedId.value)
    .sort((a, b) => b.count - a.count),
)

/* How far runs of this campaign get.
 *
 * Shown cumulatively -- "completed at least N" -- because that is the shape that
 * answers "where do people stop". The raw per-N counts read backwards: more runs
 * sit on the final scenario than on the second-to-last, simply because finishing
 * is where a run comes to rest. */
const progressHere = computed(() => {
  const rows = (stats.value?.campaignProgress ?? []).filter(
    (p) => p.campaignId === selectedId.value,
  )
  if (!rows.length) return []
  const maxN = Math.max(...rows.map((r) => r.scenarios))
  const total = rows.reduce((a, r) => a + r.games, 0)
  const atLeast = (n: number) =>
    rows.filter((r) => r.scenarios >= n).reduce((a, r) => a + r.games, 0)
  return Array.from({ length: maxN }, (_, i) => {
    const n = i + 1
    return { scenarios: n, games: atLeast(n), share: pct(atLeast(n), total) }
  })
})
</script>

<template>
  <section class="admin-block">
    <header class="section-header">
      <h2>{{ t(`${K}title`) }}</h2>
      <p v-if="stats" class="freshness">
        <template v-if="!stats.populated">{{ t(`${K}neverBuilt`) }}</template>
        <template v-else-if="staleness">
          {{ t(`${K}asOf`, { ago: staleness }) }}
          <span v-if="stats.refreshDurationMs !== null" class="cost">
            {{ t(`${K}tookMs`, { ms: num(stats.refreshDurationMs) }) }}
          </span>
        </template>
      </p>
      <button type="button" :disabled="refreshing" @click="rebuild">
        {{ refreshing ? t(`${K}rebuilding`) : t(`${K}rebuild`) }}
      </button>
    </header>

    <p v-if="error" class="error">{{ error }}</p>
    <p v-if="loading" class="empty box">{{ t(`${K}loading`) }}</p>

    <template v-else-if="stats">
      <p v-if="!stats.populated" class="notice box">
        {{ t(`${K}buildPrompt`) }}
      </p>

      <template v-if="stats.populated">
        <!-- A count is a number, not a chart. -->
        <div class="tiles">
          <div class="tile">
            <span class="tile-label">{{ t(`${K}totalGames`) }}</span>
            <strong>{{ num(stats.totals.games) }}</strong>
          </div>
          <div class="tile">
            <span class="tile-label">{{ t(`${K}campaignGames`) }}</span>
            <strong>{{ num(stats.totals.campaignGames) }}</strong>
            <small>{{ pctLabel(stats.totals.campaignGames, stats.totals.games) }}</small>
          </div>
          <div class="tile">
            <span class="tile-label">{{ t(`${K}standaloneGames`) }}</span>
            <strong>{{ num(stats.totals.standaloneGames) }}</strong>
            <small>{{ pctLabel(stats.totals.standaloneGames, stats.totals.games) }}</small>
          </div>
          <div class="tile">
            <span class="tile-label">{{ t(`${K}finished`) }}</span>
            <strong>{{ num(stats.totals.finished) }}</strong>
            <small>{{ pctLabel(stats.totals.finished, stats.totals.games) }}</small>
          </div>
          <div class="tile">
            <span class="tile-label">{{ t(`${K}neverStarted`) }}</span>
            <strong>{{ num(stats.totals.neverStarted) }}</strong>
            <small>{{ pctLabel(stats.totals.neverStarted, stats.totals.games) }}</small>
          </div>
          <div class="tile">
            <span class="tile-label">{{ t(`${K}players`) }}</span>
            <strong>{{ num(stats.totals.players) }}</strong>
          </div>
        </div>

        <!-- Two ordered mixes, side by side. Each is one stacked bar with every
             segment labelled, so neither depends on telling the hues apart. -->
        <div class="mixes">
          <figure class="mix">
            <figcaption>{{ t(`${K}playerMix`) }}</figcaption>
            <div
              class="bar stacked"
              role="img"
              :aria-label="overallPlayers.map((s) => `${s.label} ${s.value}`).join(', ')"
            >
              <span
                v-for="s in overallPlayers"
                :key="s.label"
                class="seg"
                :style="{
                  width: `${pct(s.value, segmentTotal(overallPlayers))}%`,
                  background: s.color,
                }"
                :title="`${s.label}: ${num(s.value)}`"
              />
            </div>
            <ul class="legend">
              <li v-for="s in overallPlayers" :key="s.label">
                <i :style="{ background: s.color }" />
                {{ s.label }}
                <b>{{ pctLabel(s.value, segmentTotal(overallPlayers)) }}</b>
                <span class="muted">{{ num(s.value) }}</span>
              </li>
            </ul>
          </figure>

          <figure class="mix">
            <figcaption>{{ t(`${K}difficultyMix`) }}</figcaption>
            <div
              class="bar stacked"
              role="img"
              :aria-label="overallDifficulty.map((s) => `${s.label} ${s.value}`).join(', ')"
            >
              <span
                v-for="s in overallDifficulty"
                :key="s.label"
                class="seg"
                :style="{
                  width: `${pct(s.value, segmentTotal(overallDifficulty))}%`,
                  background: s.color,
                }"
                :title="`${s.label}: ${num(s.value)}`"
              />
            </div>
            <ul class="legend">
              <li v-for="s in overallDifficulty" :key="s.label">
                <i :style="{ background: s.color }" />
                {{ s.label }}
                <b>{{ pctLabel(s.value, segmentTotal(overallDifficulty)) }}</b>
                <span class="muted">{{ num(s.value) }}</span>
              </li>
            </ul>
          </figure>

          <figure class="mix">
            <figcaption>{{ t(`${K}variantMix`) }}</figcaption>
            <ul class="plain">
              <li v-for="v in stats.variants" :key="v.key">
                {{ t(`${K}variant.${v.key}`) }}
                <b>{{ pctLabel(v.count, stats.totals.games) }}</b>
                <span class="muted">{{ num(v.count) }}</span>
              </li>
            </ul>
          </figure>
        </div>
      </template>

      <!-- ------------------------------------------------------- campaigns --->
      <template v-if="stats.populated">
        <h3>{{ t(`${K}campaignsHeading`) }}</h3>
        <div class="scroll">
          <table>
            <thead>
              <tr>
                <th>{{ t(`${K}col.campaign`) }}</th>
                <th class="n">{{ t(`${K}col.runs`) }}</th>
                <th class="n">{{ t(`${K}col.finished`) }}</th>
                <th class="n">{{ t(`${K}col.abandonedBeforeStart`) }}</th>
                <th class="n">{{ t(`${K}col.scenariosPlayed`) }}</th>
                <th class="n">{{ t(`${K}col.scenarioSuccess`) }}</th>
                <th class="mixcol">{{ t(`${K}col.playerMix`) }}</th>
                <th class="mixcol">{{ t(`${K}col.difficultyMix`) }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="c in campaigns" :key="c.id">
                <th scope="row">
                  <span class="name">{{ nameOf(c.id) }}</span>
                  <code>{{ c.id }}</code>
                </th>
                <td class="n">{{ num(c.games) }}</td>
                <td class="n">
                  {{ num(c.finished) }}
                  <small>{{ pctLabel(c.finished, c.games) }}</small>
                </td>
                <td class="n">
                  {{ num(c.neverStarted) }}
                  <small>{{ pctLabel(c.neverStarted, c.games) }}</small>
                </td>
                <td class="n">{{ num(c.scenariosPlayed) }}</td>
                <td class="n">
                  {{ pctLabel(c.scenariosWon, c.scenariosPlayed) }}
                  <small>{{ num(c.scenariosWon) }}/{{ num(c.scenariosPlayed) }}</small>
                </td>
                <td class="mixcol">
                  <div class="bar stacked small">
                    <span
                      v-for="s in playerSegments(c)"
                      :key="s.label"
                      class="seg"
                      :style="{
                        width: `${pct(s.value, segmentTotal(playerSegments(c)))}%`,
                        background: s.color,
                      }"
                      :title="`${s.label}: ${num(s.value)}`"
                    />
                  </div>
                </td>
                <td class="mixcol">
                  <div class="bar stacked small">
                    <span
                      v-for="s in difficultySegments(c)"
                      :key="s.label"
                      class="seg"
                      :style="{
                        width: `${pct(s.value, segmentTotal(difficultySegments(c)))}%`,
                        background: s.color,
                      }"
                      :title="`${s.label}: ${num(s.value)}`"
                    />
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
        <p class="footnote">{{ t(`${K}mixFootnote`) }}</p>
      </template>

      <!-- --------------------------------------------- one campaign, closer --->
      <template v-if="stats.populated && campaigns.length">
        <h3>{{ t(`${K}campaignDetailHeading`) }}</h3>
        <!-- A select rather than a segmented control: there are as many options
             as there are campaigns, and a sixteen-wide row of tabs is unreadable
             at any width. -->
        <label class="picker">
          <span class="sr-only">{{ t(`${K}pickCampaign`) }}</span>
          <select :value="selectedId ?? ''" @change="selected = ($event.target as HTMLSelectElement).value">
            <option v-for="o in campaignOptions" :key="o.value" :value="o.value">
              {{ o.label }}
            </option>
          </select>
        </label>

        <div v-if="selectedCampaign" class="detail">
          <figure class="panel">
            <figcaption>{{ t(`${K}progressHeading`) }}</figcaption>
            <p class="sub">{{ t(`${K}progressSub`) }}</p>
            <ul v-if="progressHere.length" class="bars">
              <li v-for="p in progressHere" :key="p.scenarios">
                <span class="row-label">{{ t(`${K}atLeastN`, { n: p.scenarios }) }}</span>
                <span class="track">
                  <span class="fill" :style="{ width: `${p.share}%`, background: FILL }" />
                </span>
                <span class="value">{{ num(p.games) }}</span>
                <span class="muted">{{ Math.round(p.share) }}%</span>
              </li>
            </ul>
            <p v-else class="muted">{{ t(`${K}noProgress`) }}</p>
          </figure>

          <figure class="panel">
            <figcaption>{{ t(`${K}sideStoriesHeading`) }}</figcaption>
            <p class="sub">{{ t(`${K}sideStoriesSub`) }}</p>
            <ul v-if="sideStoriesHere.length" class="bars">
              <li v-for="s in sideStoriesHere" :key="s.scenarioId">
                <span class="row-label">{{ nameOf(s.scenarioId) }}</span>
                <span class="track">
                  <span
                    class="fill"
                    :style="{
                      width: `${pct(s.count, sideStoriesHere[0].count)}%`,
                      background: FILL,
                    }"
                  />
                </span>
                <span class="value">{{ num(s.count) }}</span>
                <span class="muted">{{ pctLabel(s.count, selectedCampaign.games) }}</span>
              </li>
            </ul>
            <p v-else class="muted">{{ t(`${K}noSideStories`) }}</p>
          </figure>
        </div>
      </template>

      <!-- --------------------------------------------------- hardest first --->
      <template v-if="stats.populated && hardest.length">
        <h3>{{ t(`${K}hardestHeading`) }}</h3>
        <p class="sub">{{ t(`${K}hardestSub`, { n: MIN_PLAYS }) }}</p>
        <ul class="bars wide">
          <li v-for="s in hardest.slice(0, 25)" :key="s.id">
            <span class="row-label">{{ nameOf(s.id) }}</span>
            <span class="track">
              <span class="fill" :style="{ width: `${s.rate}%`, background: FILL }" />
            </span>
            <span class="value">{{ Math.round(s.rate) }}%</span>
            <span class="muted">{{ num(s.won) }}/{{ num(s.played) }}</span>
          </li>
        </ul>
      </template>

      <!-- ------------------------------------------- standalones & side --->
      <template v-if="stats.populated">
        <h3>{{ t(`${K}sideStoryHeading`) }}</h3>
        <div class="scroll">
          <table>
            <thead>
              <tr>
                <th>{{ t(`${K}col.scenario`) }}</th>
                <th class="n">{{ t(`${K}col.plays`) }}</th>
                <th class="n">{{ t(`${K}col.finished`) }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="s in sideStoriesSolo" :key="s.id">
                <th scope="row">
                  <span class="name">{{ nameOf(s.id) }}</span>
                  <code>{{ s.id }}</code>
                </th>
                <td class="n">{{ num(s.games) }}</td>
                <td class="n">
                  {{ num(s.finished) }}
                  <small>{{ pctLabel(s.finished, s.games) }}</small>
                </td>
              </tr>
              <tr v-if="!sideStoriesSolo.length">
                <td colspan="3" class="muted">{{ t(`${K}noSideStoryPlays`) }}</td>
              </tr>
            </tbody>
          </table>
        </div>

        <h3>{{ t(`${K}standaloneHeading`) }}</h3>
        <p class="sub">{{ t(`${K}standaloneSub`) }}</p>
        <div class="scroll">
          <table>
            <thead>
              <tr>
                <th>{{ t(`${K}col.scenario`) }}</th>
                <th class="n">{{ t(`${K}col.plays`) }}</th>
                <th class="n">{{ t(`${K}col.finished`) }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="s in standaloneScenarios.slice(0, 30)" :key="s.id">
                <th scope="row">
                  <span class="name">{{ nameOf(s.id) }}</span>
                  <code>{{ s.id }}</code>
                </th>
                <td class="n">{{ num(s.games) }}</td>
                <td class="n">
                  {{ num(s.finished) }}
                  <small>{{ pctLabel(s.finished, s.games) }}</small>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </template>

      <!-- ----------------------------------------------- most played scens --->
      <template v-if="stats.populated && mostPlayedScenarios.length">
        <h3>{{ t(`${K}mostPlayedHeading`) }}</h3>
        <ul class="bars wide">
          <li v-for="s in mostPlayedScenarios" :key="s.id">
            <span class="row-label">{{ nameOf(s.id) }}</span>
            <span class="track">
              <span
                class="fill"
                :style="{
                  width: `${pct(s.played, mostPlayedScenarios[0].played)}%`,
                  background: FILL,
                }"
              />
            </span>
            <span class="value">{{ num(s.played) }}</span>
          </li>
        </ul>
      </template>

      <!-- -------------------------------------------------------- activity --->
      <template v-if="stats.populated && stats.monthly.length">
        <h3>{{ t(`${K}activityHeading`) }}</h3>
        <div class="months" role="img" :aria-label="t(`${K}activityHeading`)">
          <div v-for="m in stats.monthly" :key="m.key" class="month">
            <span
              class="column"
              :style="{ height: `${(m.count / monthlyMax) * 100}%`, background: FILL }"
              :title="`${m.key}: ${num(m.count)}`"
            />
            <span class="month-label">{{ m.key.slice(2) }}</span>
          </div>
        </div>
      </template>

      <!-- ---------------------------------------------------- achievements --->
      <h3>{{ t(`${K}achievementsHeading`) }}</h3>
      <p class="sub">
        {{ t(`${K}achievementsSub`, { n: num(stats.achievementUsers) }) }}
      </p>
      <ul v-if="achievements.length" class="bars wide">
        <li v-for="a in achievements" :key="a.id">
          <span class="row-label">
            {{ a.name }}
            <em v-if="a.campaign">{{ a.campaign }}</em>
          </span>
          <span class="track">
            <span
              class="fill"
              :style="{
                width: `${pct(a.earned, Math.max(1, stats.achievementUsers))}%`,
                background: FILL,
              }"
            />
          </span>
          <span class="value">{{ num(a.earned) }}</span>
          <span class="muted">
            {{ pctLabel(a.earned, stats.achievementUsers) }}
            <template v-if="a.inProgress">
              · {{ t(`${K}inProgressN`, { n: num(a.inProgress) }) }}
            </template>
          </span>
        </li>
      </ul>
      <p v-else class="muted">{{ t(`${K}noAchievements`) }}</p>
    </template>
  </section>
</template>

<style scoped lang="scss">
.admin-block {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
  color: var(--title);
  display: flex;
  flex-direction: column;
  gap: 14px;
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
  font-family: teutonic, sans-serif;
  font-size: 1.6rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.freshness {
  color: color-mix(in srgb, var(--title) 65%, transparent);
  flex: 1;
  font-size: 0.78rem;
  margin: 0;
}

.cost {
  opacity: 0.7;
}

h3 {
  border-top: 1px solid var(--box-border);
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.2rem;
  margin: 6px 0 0;
  padding-top: 12px;
  text-transform: uppercase;
}

.sub,
.footnote {
  color: color-mix(in srgb, var(--title) 60%, transparent);
  font-size: 0.76rem;
  margin: -6px 0 0;
  max-width: 80ch;
}

button {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.35rem 0.75rem;

  &:hover:not(:disabled) {
    background: rgba(255, 255, 255, 0.14);
    border-color: var(--spooky-green);
  }

  &:disabled {
    cursor: default;
    opacity: 0.5;
  }
}

.error {
  background: color-mix(in srgb, var(--survivor) 18%, transparent);
  border-radius: 4px;
  color: color-mix(in srgb, var(--survivor) 75%, white);
  font-size: 0.85rem;
  margin: 0;
  padding: 0.5rem 0.7rem;
}

.empty,
.notice {
  color: var(--title);
  margin: 0;
  opacity: 0.8;
}

/* A count is a number. No chart. */
.tiles {
  display: grid;
  gap: 10px;
  grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
}

.tile {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 10px 12px;
}

.tile-label {
  color: color-mix(in srgb, var(--title) 68%, transparent);
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.tile strong {
  font-family: 'Noto Sans', sans-serif;
  font-size: 1.7rem;
  font-weight: 800;
  line-height: 1;
}

.tile small {
  color: color-mix(in srgb, var(--title) 60%, transparent);
  font-size: 0.74rem;
}

.mixes {
  display: grid;
  gap: 14px;
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
}

.mix {
  margin: 0;
}

figcaption {
  color: color-mix(in srgb, var(--title) 80%, transparent);
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.06em;
  margin-bottom: 8px;
  text-transform: uppercase;
}

/* Thin marks, rounded ends, and a surface-coloured gap between segments so two
   adjacent fills never read as one. */
.bar {
  background: rgba(255, 255, 255, 0.06);
  border-radius: 4px;
  display: flex;
  gap: 2px;
  height: 14px;
  overflow: hidden;
  width: 100%;
}

.bar.small {
  height: 10px;
  min-width: 90px;
}

.seg {
  display: block;
  min-width: 2px;

  &:first-child {
    border-radius: 4px 0 0 4px;
  }

  &:last-child {
    border-radius: 0 4px 4px 0;
  }
}

.legend,
.plain {
  display: flex;
  flex-direction: column;
  gap: 3px;
  list-style: none;
  margin: 8px 0 0;
  padding: 0;
}

.legend li,
.plain li {
  align-items: center;
  display: flex;
  font-size: 0.78rem;
  gap: 6px;
}

.legend i {
  border-radius: 2px;
  flex: none;
  height: 9px;
  width: 9px;
}

.legend b,
.plain b {
  margin-left: auto;
}

.muted {
  color: color-mix(in srgb, var(--title) 55%, transparent);
}

.scroll {
  overflow-x: auto;
}

table {
  border-collapse: collapse;
  font-size: 0.8rem;
  width: 100%;
}

thead th {
  border-bottom: 1px solid var(--box-border);
  color: color-mix(in srgb, var(--title) 65%, transparent);
  font-size: 0.68rem;
  font-weight: 700;
  letter-spacing: 0.06em;
  padding: 6px 8px;
  text-align: left;
  text-transform: uppercase;
  white-space: nowrap;
}

tbody th,
tbody td {
  border-bottom: 1px solid color-mix(in srgb, var(--box-border) 50%, transparent);
  padding: 7px 8px;
  text-align: left;
  vertical-align: middle;
}

tbody th {
  font-weight: 600;
  white-space: nowrap;
}

tbody th code {
  color: color-mix(in srgb, var(--title) 45%, transparent);
  font-size: 0.7rem;
  margin-left: 6px;
}

.n {
  text-align: right !important;
  white-space: nowrap;
}

td.n small {
  color: color-mix(in srgb, var(--title) 55%, transparent);
  display: block;
  font-size: 0.7rem;
}

.mixcol {
  min-width: 110px;
  width: 14%;
}

.picker select {
  background: var(--background-dark);
  background-image: var(--select-caret);
  background-position: right 10px center;
  background-repeat: no-repeat;
  background-size: var(--select-caret-size);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  font-size: 0.85rem;
  padding: 0.4rem 2rem 0.4rem 0.6rem;
  width: min(100%, 24rem);
  appearance: none;

  &:hover {
    border-color: var(--spooky-green);
  }
}

.sr-only {
  height: 1px;
  margin: -1px;
  overflow: hidden;
  position: absolute;
  width: 1px;
  clip: rect(0, 0, 0, 0);
}

/* The campaign an achievement belongs to, under its name: two achievements can
   share a title across campaigns, and the tag was the only thing telling them
   apart. */
.row-label em {
  color: color-mix(in srgb, var(--title) 45%, transparent);
  display: block;
  font-size: 0.68rem;
  font-style: normal;
  overflow: hidden;
  text-overflow: ellipsis;
}

.detail {
  display: grid;
  gap: 14px;
  grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
}

.panel {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  margin: 0;
  padding: 12px;
}

.panel .sub {
  margin: -4px 0 8px;
}

/* One series, so no legend: the caption names it. Values are direct-labelled
   rather than hovered-for. */
.bars {
  display: flex;
  flex-direction: column;
  gap: 5px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.bars li {
  align-items: center;
  display: grid;
  font-size: 0.78rem;
  gap: 8px;
  grid-template-columns: minmax(7rem, 14rem) 1fr auto auto;
  min-height: 18px;
}

.bars.wide li {
  grid-template-columns: minmax(10rem, 22rem) 1fr auto auto;
}

.row-label {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.track {
  background: rgba(255, 255, 255, 0.06);
  border-radius: 4px;
  display: block;
  height: 10px;
  min-width: 40px;
  overflow: hidden;
}

.fill {
  border-radius: 4px;
  display: block;
  height: 100%;
  min-width: 2px;
}

.value {
  font-variant-numeric: tabular-nums;
  font-weight: 700;
  text-align: right;
}

.bars .muted {
  font-size: 0.72rem;
  text-align: right;
  white-space: nowrap;
}

/* Change over time: one series of discrete months. */
.months {
  align-items: flex-end;
  display: flex;
  gap: 3px;
  height: 120px;
}

.month {
  align-items: center;
  display: flex;
  flex: 1;
  flex-direction: column;
  height: 100%;
  justify-content: flex-end;
  min-width: 0;
}

.column {
  border-radius: 4px 4px 0 0;
  min-height: 2px;
  width: 100%;
}

.month-label {
  color: color-mix(in srgb, var(--title) 50%, transparent);
  font-size: 0.62rem;
  margin-top: 4px;
  white-space: nowrap;
}

@media (max-width: 700px) {
  .bars li,
  .bars.wide li {
    grid-template-columns: 1fr auto;

    .track {
      grid-column: 1 / -1;
    }
  }

  .month-label {
    display: none;
  }
}
</style>

<script setup lang="ts">
/* The tracker: everything filed, by anyone.
 *
 * Public on purpose. The first thing someone does before filing is look for
 * what they are about to report, so the list has to be searchable by anyone and
 * has to default to open work -- a page that opens on 456 finished cards
 * answers a question nobody asked.
 *
 * Filters live in the URL query, so a filtered list is a link somebody can be
 * sent, and the back button walks through filters rather than out of the page.
 */
import { computed, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import * as Api from '@/arkham/api'
import ReportRow from '@/components/ReportRow.vue'
import { useReportsStore } from '@/stores/reports'

const route = useRoute()
const router = useRouter()

const page = ref<Api.ReportsPage | null>(null)
const loading = ref(true)
const failed = ref(false)

const param = (key: string) => {
  const value = route.query[key]
  return typeof value === 'string' && value !== '' ? value : undefined
}

const filters = computed<Api.ReportFilters>(() => ({
  kind: param('kind') as Api.ReportKind | undefined,
  status: (param('status') ?? 'open') as Api.ReportFilters['status'],
  q: param('q'),
  sort: (param('sort') ?? 'recent') as 'recent' | 'new',
  page: Number(param('page') ?? 1),
}))

const search = ref(param('q') ?? '')

const load = async () => {
  loading.value = true
  try {
    page.value = await Api.fetchReports(filters.value)
    failed.value = false
  } catch {
    failed.value = true
  } finally {
    loading.value = false
  }
}

watch(() => route.fullPath, load, { immediate: true })
watch(
  () => param('q') ?? '',
  (value) => (search.value = value),
)

/* Every filter change goes through the URL and comes back as a load, so there
 * is one path into the list and the address bar is never behind what is shown.
 * Anything but a page change starts again at page one. */
const apply = (changes: Record<string, string | undefined>) => {
  const query: Record<string, string> = {}
  for (const [key, value] of Object.entries({ ...route.query, ...changes })) {
    if (typeof value === 'string' && value !== '') query[key] = value
  }
  if (!('page' in changes)) delete query.page
  router.push({ query })
}

const KINDS: { value: string | undefined; label: string; icon: string }[] = [
  { value: undefined, label: 'reports.filter.everything', icon: 'list' },
  { value: 'bug', label: 'reports.kind.bug', icon: 'bug' },
  { value: 'feature', label: 'reports.kind.feature', icon: 'lightbulb' },
]

const totalPages = computed(() =>
  page.value ? Math.max(1, Math.ceil(page.value.total / page.value.pageSize)) : 1,
)

/* Your own reports as a group on top of everything.
 *
 * Already in hand: the store behind the nav badge polls the reports this
 * account follows, so the group costs no request, and it cannot disagree with
 * the badge about whether there is news.
 *
 * It sits above the filters because it is not subject to them -- a group that
 * emptied itself when somebody searched would read as "you filed nothing".
 */
const mine = useReportsStore()
const expanded = ref(false)
const FOLDED = 5

/* Archived reports are put away, not deleted, so the group has two buckets and
 * a button between them. A report archives itself once it is finished AND the
 * reporter has seen that it is -- see stores/reports.ts -- so the thing they
 * were told about is still on the list until they have read the answer, and
 * off it the moment they have. */
const showArchive = ref(false)

const byNews = (a: Api.Report, b: Api.Report) =>
  Number(b.unread) - Number(a.unread) || b.updatedAt.localeCompare(a.updatedAt)

const live = computed(() => [...mine.live].sort(byNews))
const archived = computed(() => [...mine.archived].sort(byNews))

const yours = computed(() => (showArchive.value ? archived.value : live.value))
const yoursShown = computed(() => (expanded.value ? yours.value : yours.value.slice(0, FOLDED)))

/* Folding is per bucket: five of one is not five of the other. */
watch(showArchive, () => (expanded.value = false))
</script>

<template>
  <div class="page-container">
    <div class="reports page-content">
      <header class="masthead">
        <h1>{{ $t('reports.title') }}</h1>
        <RouterLink class="file" :to="{ name: 'NewReport' }">
          <font-awesome-icon icon="pen-to-square" /> {{ $t('reports.file') }}
        </RouterLink>
      </header>

      <section v-if="live.length > 0 || archived.length > 0" class="yours">
        <header>
          <h2>{{ showArchive ? $t('reports.panel.archiveTitle') : $t('reports.panel.title') }}</h2>
          <button
            v-if="showArchive || archived.length > 0"
            class="archive-toggle"
            @click="showArchive = !showArchive"
          >
            <template v-if="showArchive">← {{ $t('reports.panel.title') }}</template>
            <template v-else>
              <font-awesome-icon icon="box-archive" />
              {{ $t('reports.panel.archive', { count: archived.length }) }}
            </template>
          </button>
        </header>

        <p v-if="yours.length === 0" class="all-clear">{{ $t('reports.panel.allClear') }}</p>

        <ul v-else class="list">
          <li v-for="report in yoursShown" :key="report.id">
            <ReportRow :report="report" />
          </li>
        </ul>

        <button v-if="yours.length > FOLDED" class="fold" @click="expanded = !expanded">
          {{ expanded ? $t('reports.panel.fewer') : $t('reports.panel.more', { count: yours.length - FOLDED }) }}
        </button>
      </section>

      <div class="filters">
        <div class="kinds">
          <button
            v-for="kind in KINDS"
            :key="kind.label"
            class="chip"
            :class="{ on: filters.kind === kind.value }"
            @click="apply({ kind: kind.value })"
          >
            <font-awesome-icon :icon="kind.icon" /> {{ $t(kind.label) }}
          </button>
        </div>

        <select :value="filters.status" @change="apply({ status: ($event.target as HTMLSelectElement).value })">
          <option value="open">{{ $t('reports.filter.open') }}</option>
          <option value="closed">{{ $t('reports.filter.closed') }}</option>
          <option value="all">{{ $t('reports.filter.all') }}</option>
          <option v-for="status in page?.statuses ?? []" :key="status" :value="status">
            {{ $t(`reports.status.${status}`, status) }}
          </option>
        </select>

        <select :value="filters.sort" @change="apply({ sort: ($event.target as HTMLSelectElement).value })">
          <option value="recent">{{ $t('reports.sort.recent') }}</option>
          <option value="new">{{ $t('reports.sort.new') }}</option>
        </select>

        <form class="search" @submit.prevent="apply({ q: search })">
          <input v-model="search" type="search" :placeholder="$t('reports.searchPlaceholder')" />
          <button type="submit"><font-awesome-icon icon="magnifying-glass" /></button>
        </form>
      </div>

      <p v-if="failed" class="empty">{{ $t('reports.loadFailed') }}</p>
      <p v-else-if="loading && !page" class="empty">{{ $t('reports.loading') }}</p>
      <p v-else-if="page && page.reports.length === 0" class="empty">{{ $t('reports.none') }}</p>

      <ul v-else-if="page" class="list" :class="{ stale: loading }">
        <li v-for="report in page.reports" :key="report.id">
          <ReportRow :report="report" />
        </li>
      </ul>

      <nav v-if="page && totalPages > 1" class="pages">
        <button :disabled="page.page <= 1" @click="apply({ page: String(page.page - 1) })">
          {{ $t('reports.previous') }}
        </button>
        <span>{{ $t('reports.pageOf', { page: page.page, pages: totalPages, total: page.total }) }}</span>
        <button :disabled="page.page >= totalPages" @click="apply({ page: String(page.page + 1) })">
          {{ $t('reports.next') }}
        </button>
      </nav>
    </div>
  </div>
</template>

<style scoped>
/* Filtering the list and folding your own group both change how tall the page
   is, and a scrollbar that appears and disappears with it shifts the column
   sideways under the pointer. Hold the gutter open either way. */
.page-container {
  scrollbar-gutter: stable;
}

.reports {
  align-content: start;
  color: var(--text);
  width: min(1100px, 94vw);
  display: grid;
  gap: 14px;
}

.masthead {
  align-items: center;
  display: flex;
  gap: 16px;
}

h1 {
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 2em;
  letter-spacing: 0.03em;
  margin: 0;
  text-transform: uppercase;
}

.file {
  background: var(--button-1);
  border-radius: 4px;
  color: var(--text);
  flex: none;
  font-weight: 600;
  padding: 8px 14px;
  text-decoration: none;
}

.file:hover {
  background: var(--button-1-highlight);
}

.filters {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

.kinds {
  display: flex;
  gap: 4px;
}

.chip {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.85em;
  padding: 7px 10px;
}

.chip.on {
  background: var(--button-1-muted);
  border-color: var(--button-1);
  color: var(--text);
}

select {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--text);
  padding: 7px 8px;
}

.search {
  display: flex;
  flex: 1;
  gap: 4px;
  min-width: 180px;
}

.search input {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--text);
  flex: 1;
  padding: 7px 10px;
}

.search button {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  padding: 7px 11px;
}

.empty {
  color: var(--title);
  margin: 0;
}

/* Your own reports, boxed off from the tracker proper: the same rows, but
   inset and under a rule, so it reads as a group on top of the list rather
   than as the first few entries of it. */
.yours {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  display: grid;
  gap: 8px;
  padding: 12px 14px;
}

.yours > header {
  align-items: baseline;
  display: flex;
  gap: 12px;
  justify-content: space-between;
}

.archive-toggle {
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  font-size: 0.82em;
  padding: 2px 0;
}

.archive-toggle:hover {
  color: var(--text);
}

.all-clear {
  color: var(--title);
  font-size: 0.9em;
  margin: 0;
}

.yours h2 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1em;
  letter-spacing: 0.08em;
  margin: 0;
  text-transform: uppercase;
}

.fold {
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  font-size: 0.82em;
  justify-self: start;
  padding: 2px 0;
  text-decoration: underline;
}

.list {
  display: grid;
  gap: 6px;
  list-style: none;
  margin: 0;
  padding: 0;
}

/* A reload leaves the old page in place and dims it, rather than emptying the
   list and bouncing the scroll position back to the top. */
.list.stale {
  opacity: 0.55;
}

.pages {
  align-items: center;
  color: var(--title);
  display: flex;
  gap: 12px;
  justify-content: center;
}

.pages button {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--text);
  cursor: pointer;
  padding: 6px 12px;
}

.pages button:disabled {
  cursor: default;
  opacity: 0.4;
}
</style>

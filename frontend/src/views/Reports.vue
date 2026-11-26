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
import ReportStatusTag from '@/components/ReportStatusTag.vue'

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

const when = (at: string) => new Date(at).toLocaleDateString()
</script>

<template>
  <div class="reports page-content">
    <header class="masthead">
      <h1>{{ $t('reports.title') }}</h1>
      <RouterLink class="file" :to="{ name: 'NewReport' }">
        <font-awesome-icon icon="pen-to-square" /> {{ $t('reports.file') }}
      </RouterLink>
    </header>

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
        <RouterLink class="row" :to="{ name: 'Report', params: { number: report.number } }">
          <font-awesome-icon
            class="kind"
            :icon="report.kind === 'feature' ? 'lightbulb' : 'bug'"
            :class="report.kind"
            :title="$t(`reports.kind.${report.kind}`)"
          />
          <span class="title">
            {{ report.title }}
            <span v-if="report.unread" class="news" :title="$t('reports.news')">!</span>
          </span>
          <ReportStatusTag :status="report.status" />
          <span class="meta">
            <span class="number">#{{ report.number }}</span>
            <span v-if="report.author">
              {{ report.authorAccount ? $t('reports.by', { who: report.author }) : $t('reports.byGithub', { who: report.author }) }}
            </span>
            <span>{{ when(report.createdAt) }}</span>
            <span v-if="report.blocking" class="blocking">
              <font-awesome-icon icon="ban" /> {{ $t('reports.blocking') }}
            </span>
            <span v-if="report.comments > 0" class="comments">
              <font-awesome-icon icon="comment" /> {{ report.comments }}
            </span>
            <span v-if="report.hidden" class="hidden-flag">{{ $t('reports.hiddenFlag') }}</span>
          </span>
        </RouterLink>
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
</template>

<style scoped>
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

.row {
  align-items: center;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--text);
  display: grid;
  gap: 4px 10px;
  grid-template-columns: auto minmax(0, 1fr) auto;
  padding: 10px 12px;
  text-decoration: none;
}

.row:hover {
  border-color: var(--button-1);
}

.kind {
  opacity: 0.8;
}

.kind.bug {
  color: var(--survivor);
}

.kind.feature {
  color: var(--rogue);
}

.title {
  font-weight: 600;
  overflow-wrap: anywhere;
}

.news {
  background: var(--survivor);
  border-radius: 50%;
  display: inline-grid;
  font-size: 0.72em;
  font-weight: 700;
  height: 1.15rem;
  margin-left: 6px;
  place-items: center;
  vertical-align: middle;
  width: 1.15rem;
}

.meta {
  color: var(--title);
  display: flex;
  flex-wrap: wrap;
  font-size: 0.78em;
  gap: 2px 12px;
  grid-column: 2 / -1;
}

.number {
  font-variant-numeric: tabular-nums;
  opacity: 0.8;
}

.blocking {
  color: var(--survivor);
}

.hidden-flag {
  color: var(--seeker);
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

@media (max-width: 700px) {
  /* Too narrow for a column of its own: the pill drops under the title rather
     than into the gutter beside the kind icon. */
  .row {
    grid-template-columns: auto minmax(0, 1fr);
  }

  .row > .status {
    grid-column: 2;
    justify-self: start;
  }
}
</style>

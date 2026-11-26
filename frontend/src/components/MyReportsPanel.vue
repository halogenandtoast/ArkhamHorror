<script setup lang="ts">
/* The home feed's panel for the reports you are following.
 *
 * The reason the tracker has a panel here at all: somebody who files a bug goes
 * back to playing, and the only place they reliably return to is this page. A
 * reply or a move that nobody ever sees may as well not have happened.
 */
import { computed } from 'vue'
import * as Api from '@/arkham/api'
import FeedPanel from '@/components/FeedPanel.vue'
import ReportStatusTag from '@/components/ReportStatusTag.vue'

const props = defineProps<{ reports: Api.Report[] }>()

/* Anything with news first, then the rest by what happened last. A short list:
 * the panel is a nudge towards the tracker, not the tracker. */
const shown = computed(() =>
  [...props.reports]
    .sort((a, b) => Number(b.unread) - Number(a.unread) || b.updatedAt.localeCompare(a.updatedAt))
    .slice(0, 4),
)

const unread = computed(() => props.reports.filter((report) => report.unread).length)
</script>

<template>
  <FeedPanel :title="$t('reports.panel.title')" icon="bug" :count="unread">
    <ul>
      <li v-for="report in shown" :key="report.id">
        <RouterLink :to="{ name: 'Report', params: { number: report.number } }">
          <span class="line">
            <span class="title">{{ report.title }}</span>
            <span v-if="report.unread" class="news">!</span>
          </span>
          <span class="meta">
            <ReportStatusTag :status="report.status" small />
            <span class="number">#{{ report.number }}</span>
            <span v-if="report.comments > 0"><font-awesome-icon icon="comment" /> {{ report.comments }}</span>
          </span>
        </RouterLink>
      </li>
    </ul>
    <RouterLink class="all" :to="{ name: 'Reports' }">{{ $t('reports.panel.all') }}</RouterLink>
  </FeedPanel>
</template>

<style scoped>
ul {
  display: flex;
  flex-direction: column;
  gap: 8px;
  list-style: none;
  margin: 0;
  padding: 0;
}

a {
  color: var(--text);
  display: grid;
  gap: 4px;
  text-decoration: none;
}

.line {
  align-items: baseline;
  display: flex;
  gap: 6px;
}

.title {
  font-size: 0.9em;
  overflow-wrap: anywhere;
}

a:hover .title {
  text-decoration: underline;
}

.news {
  background: var(--survivor);
  border-radius: 50%;
  flex: none;
  font-size: 0.68em;
  font-weight: 700;
  height: 1.1rem;
  display: inline-grid;
  place-items: center;
  width: 1.1rem;
}

.meta {
  align-items: center;
  color: var(--title);
  display: flex;
  flex-wrap: wrap;
  font-size: 0.75em;
  gap: 2px 8px;
}

.number {
  font-variant-numeric: tabular-nums;
}

.all {
  color: var(--title);
  display: inline-block;
  font-size: 0.82em;
  margin-top: 10px;
}
</style>

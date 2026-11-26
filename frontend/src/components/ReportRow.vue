<script setup lang="ts">
/* One line of the tracker.
 *
 * Shared because the list is now drawn twice on the same page -- your own
 * reports as a group on top, then everything -- and two copies of this markup
 * would drift apart the first time a field is added.
 */
import type { Report } from '@/arkham/api'
import ReportStatusTag from '@/components/ReportStatusTag.vue'

defineProps<{ report: Report }>()

const when = (at: string) => new Date(at).toLocaleDateString()
</script>

<template>
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
</template>

<style scoped>
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

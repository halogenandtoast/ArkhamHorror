<script setup lang="ts">
/* A report's board column, as a pill.
 *
 * Deliberately not magenta, cyan or gold at any size: those three say something
 * about a game waiting on you (see tokens.css), and a tracker is chrome.
 */
import { computed } from 'vue'
import type { ReportStatus } from '@/arkham/api'

const props = defineProps<{ status: ReportStatus; small?: boolean }>()

const COLORS: Record<string, string> = {
  'new': '#7b8794',
  'todo': '#5cb4fd',
  'in-progress': '#efa345',
  'blocked': '#ee4a53',
  'ready-for-deploy': '#ba81f2',
  'pending-deploy': '#e4d083',
  'done': '#48b14f',
  'declined': '#8a8a8a',
  'duplicate': '#8a7d5a',
}

/* A status the client has never heard of still draws, in grey, under its own
 * name: the server owns the vocabulary, and a new column should not need a
 * frontend release to be legible. */
const color = computed(() => COLORS[props.status] ?? '#7b8794')
</script>

<template>
  <span class="status" :class="{ small }" :style="{ '--status': color }">
    {{ $t(`reports.status.${status}`, status) }}
  </span>
</template>

<style scoped>
.status {
  background: color-mix(in srgb, var(--status) 18%, transparent);
  border: 1px solid color-mix(in srgb, var(--status) 55%, transparent);
  border-radius: 3px;
  color: color-mix(in srgb, var(--status) 80%, white);
  display: inline-block;
  flex: none;
  font-size: 0.72em;
  font-weight: 700;
  letter-spacing: 0.08em;
  line-height: 1;
  padding: 4px 6px;
  text-transform: uppercase;
  white-space: nowrap;
}

.status.small {
  font-size: 0.64em;
  padding: 3px 5px;
}
</style>

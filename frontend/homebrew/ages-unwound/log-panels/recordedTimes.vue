<script lang="ts" setup>
/* The campaign-log entries Ages Unwound records "with a time".
 *
 * Each entry is a `RecordedTime` object -- `{ event, agenda, doom }`, see the
 * campaign's Haskell helpers -- so the generic recorded set list had nothing to
 * print it with and showed "[object Object]". The time is the guide's own
 * notation: the agenda number and the doom on it, in brackets, as in
 * "the boundary is broken (1,5)".
 */
import { computed } from 'vue'
import { logKeyTitle } from '@/arkham/types/Log'
import type { Game } from '@/arkham/types/Game'
import { useI18n } from 'vue-i18n'

const props = defineProps<{ entries: any[]; game: Game }>()

const { t } = useI18n()

/* The event is stored as the campaign-log key's own constructor name
 * (`recordTheTimeFor` writes `tshow`), which is the campaign's locale path once
 * its first letter is lowered. `logKeyTitle` turns an unknown one into words
 * rather than leaving the path on screen. */
const eventTitle = (event: string): string => {
  const key = event.charAt(0).toLowerCase() + event.slice(1)
  // The log's entries are written as sentences and end in a full stop, which
  // reads wrong with the time after it.
  return logKeyTitle(`agesUnwound.key.${key}`, t).replace(/\.$/, '')
}

const times = computed(() =>
  (props.entries ?? []).flatMap((entry, idx) => {
    const time = entry?.contents ?? entry?.recordVal?.contents
    if (!time || typeof time !== 'object') return []
    const event = String(time.event ?? '')
    return [
      {
        key: `${event}:${time.agenda}:${time.doom}:${idx}`,
        title: eventTitle(event),
        agenda: Number(time.agenda),
        doom: Number(time.doom),
        crossedOut: entry?.tag === 'CrossedOut',
      },
    ]
  }),
)
</script>

<template>
  <ul class="recorded-times">
    <li v-for="time in times" :key="time.key" :class="{ 'crossed-out': time.crossedOut }">
      <span class="event">{{ time.title }}</span>
      <span
        class="time"
        :title="t('agesUnwound.recordedTimes.tooltip', { agenda: time.agenda, doom: time.doom })"
      >({{ time.agenda }},{{ time.doom }})</span>
    </li>
  </ul>
</template>

<style scoped>
.recorded-times {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.recorded-times li {
  display: flex;
  align-items: baseline;
  gap: 8px;
  margin: 0;
  padding: 7px 10px;
  border-radius: 5px;
  background: rgba(255, 255, 255, 0.04);
  color: var(--title);
  font-size: 0.92rem;
  line-height: 1.4;
}

.event {
  flex: 1;
  overflow-wrap: break-word;
  min-width: 0;
}

/* The bracketed pair, in the gold tally tone the log uses for a running count. */
.time {
  flex-shrink: 0;
  padding: 1px 7px;
  border-radius: 4px;
  background: rgba(214, 178, 92, 0.15);
  color: #d6b25c;
  border: 1px solid rgba(214, 178, 92, 0.3);
  font-size: 0.82rem;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
  letter-spacing: 0.02em;
  cursor: help;
}

.crossed-out {
  text-decoration: line-through;
}
</style>

<script lang="ts" setup>
/* The campaign-log entries Ages Unwound records "with a time".
 *
 * Each entry is a `RecordedTime` object -- `{ event, agenda, doom }`, see the
 * campaign's Haskell helpers -- so the generic recorded set list had nothing to
 * print it with and showed "[object Object]". The time is the guide's own
 * notation: the agenda number and the doom on it, in brackets, as in
 * "the boundary is broken (1,5)".
 */
import { computed, ref } from 'vue'
import { useShiftHoverDebug } from '@/composable/shiftHoverDebug'
import DebugToggle from '@/arkham/components/debug/DebugToggle.vue'
import { useDebug } from '@/arkham/debug'
import { logKeyTitle } from '@/arkham/types/Log'
import type { Game } from '@/arkham/types/Game'
import { useI18n } from 'vue-i18n'

const props = defineProps<{ entries: any[]; game: Game }>()

const { t } = useI18n()
const debug = useDebug()
const { active, showToggle, onEnter, onLeave, toggle } = useShiftHoverDebug()
const editing = ref<string | null>(null)
const agenda = ref<number | string>(1)
const doom = ref<number | string>(0)
const saving = ref(false)
const saveFailed = ref(false)
function toggleDebug() {
  toggle()
  editing.value = null
}
const validTime = computed(
  () =>
    typeof agenda.value === 'number' &&
    Number.isSafeInteger(agenda.value) &&
    agenda.value >= 1 &&
    typeof doom.value === 'number' &&
    Number.isSafeInteger(doom.value) &&
    doom.value >= 0,
)

function startEdit(time: (typeof times.value)[number]) {
  editing.value = time.key
  agenda.value = time.agenda
  doom.value = time.doom
  saveFailed.value = false
}

async function save(time: (typeof times.value)[number]) {
  if (!validTime.value || saving.value) return
  // Log decoding flattens SomeRecorded; restore its wire envelope for the message.
  const { recordType = 'RecordableGeneric', recordVal, ...flattened } = time.entry
  const original = recordVal ?? flattened
  saving.value = true
  saveFailed.value = false
  try {
    await debug.send(props.game.id, {
      tag: 'RecordSetReplace',
      contents: [
        { tag: 'HomebrewCampaignLogKey', contents: 'agesUnwound.RecordedTimes' },
        { recordType, recordVal: original },
        {
          recordType,
          recordVal: {
            ...original,
            contents: { ...original.contents, agenda: agenda.value, doom: doom.value },
          },
        },
      ],
    })
    editing.value = null
  } catch {
    saveFailed.value = true
  } finally {
    saving.value = false
  }
}

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
        entry,
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
  <div class="recorded-times-panel" @mouseenter="onEnter" @mouseleave="onLeave">
    <DebugToggle
      v-if="showToggle"
      :active="active"
      :disabled="saving"
      @click="toggleDebug"
    />
    <ul class="recorded-times">
      <li v-for="time in times" :key="time.key" :class="{ 'crossed-out': time.crossedOut }">
        <span class="event">{{ time.title }}</span>
        <form
          v-if="active && editing === time.key"
          class="time-editor"
          @submit.prevent="save(time)"
        >
          <label
            >{{ t('agesUnwound.recordedTimes.agenda') }}
            <input
              v-model.number="agenda"
              type="number"
              min="1"
              step="1"
              required
              :disabled="saving"
            />
          </label>
          <label
            >{{ t('agesUnwound.recordedTimes.doom') }}
            <input
              v-model.number="doom"
              type="number"
              min="0"
              step="1"
              required
              :disabled="saving"
            />
          </label>
          <button type="submit" :disabled="!validTime || saving">
            {{ t('debug.campaign.save') }}
          </button>
          <button type="button" :disabled="saving" @click="editing = null">
            {{ t('debug.campaign.cancel') }}
          </button>
          <span v-if="saveFailed" role="alert">{{
            t('agesUnwound.recordedTimes.saveFailed')
          }}</span>
        </form>
        <span
          v-else
          class="time"
          :title="t('agesUnwound.recordedTimes.tooltip', { agenda: time.agenda, doom: time.doom })"
          >({{ time.agenda }},{{ time.doom }})</span
        >
        <button
          v-if="active && editing !== time.key"
          type="button"
          :disabled="saving"
          @click="startEdit(time)"
        >
          {{ t('debug.campaign.edit') }}
        </button>
      </li>
    </ul>
  </div>
</template>

<style scoped>
.recorded-times-panel {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.debug-toggle {
  align-self: flex-end;
}

.time-editor {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 8px;
  text-decoration: none;
}

.time-editor label {
  display: flex;
  align-items: center;
  gap: 4px;
}

.time-editor input {
  width: 5em;
}

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

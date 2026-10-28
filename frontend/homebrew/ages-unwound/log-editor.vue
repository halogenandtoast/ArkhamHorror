<script setup lang="ts">
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDebug } from '@/arkham/debug'
import type { Game } from '@/arkham/types/Game'
import { logKeyTitle, type LogContents, type LogKey } from '@/arkham/types/Log'
import CardCodeField from '@/arkham/components/debug/CardCodeField.vue'
import { useCardStore } from '@/stores/cards'
import {
  recordRows,
  countKeys,
  investigatorKeys,
  timedKeys,
  recordKey,
  recordChoiceMessages,
  validateRecordKeys,
  activeRecordKeys,
} from './logRecords'

const props = defineProps<{ game: Game; log: LogContents }>()
const emit = defineEmits<{ refresh: [] }>()
const { t } = useI18n()
const debug = useDebug()
const cards = useCardStore()
const saving = ref(false)
const failed = ref(false)
const saved = ref(false)
const outcomeDrafts = ref<Record<string, string>>({})
const saveBlocked = ref(false)
const hasOutcomeChanges = computed(() => Object.keys(outcomeDrafts.value).length > 0)
const candidateKeys = computed(() => {
  const keys = new Set(activeRecordKeys(props.log))
  for (const [key, status] of Object.entries(outcomeDrafts.value)) {
    if (status === 'recorded') keys.add(key)
    else keys.delete(key)
  }
  return keys
})
const validationIssues = computed(() => validate())
function validate() {
  return validateRecordKeys(candidateKeys.value)
}
defineExpose({ validate })
const rowIssues = (keys: string[]) =>
  validationIssues.value.filter((issue) => issue.keys.some((key) => keys.includes(key)))
const activeTab = ref('outcomes')
const tabs = ['outcomes', 'counts', 'investigators', 'enemies']
const search = ref('')
function showInvalidOutcomes() {
  activeTab.value = 'outcomes'
  search.value = ''
}
const filteredRows = computed(() =>
  recordRows.filter((row) =>
    `${row.label ?? ''} ${row.keys.map(title).join(' ')}`
      .toLocaleLowerCase()
      .includes(search.value.toLocaleLowerCase().trim()),
  ),
)
const enemy = ref('')
const drafts = ref<Record<string, { agenda: number | string; doom: number | string }>>({})
const text = (key: string) => t(`agesUnwound.logEditor.${key}`)
const keyJson = recordKey
const matches = (entry: LogKey, key: string) => {
  const raw = entry.raw as { tag?: string; contents?: string } | undefined
  return (
    raw?.tag === 'HomebrewCampaignLogKey' &&
    [key, `agesUnwound.${key}`].includes(raw.contents ?? '')
  )
}
const title = (key: string) =>
  logKeyTitle(`agesUnwound.key.${key[0].toLowerCase()}${key.slice(1)}`, t)
const state = (key: string, log = props.log): string =>
  log === props.log && outcomeDrafts.value[key] !== undefined
    ? outcomeDrafts.value[key]
    : log.recorded.some((k) => matches(k, key))
      ? 'recorded'
      : log.crossedOut.some((k) => matches(k, key))
        ? 'crossedOut'
        : 'none'
const selected = (keys: string[]) =>
  keys.find((key) => state(key) === 'recorded') ??
  keys.find((key) => state(key) === 'crossedOut') ??
  ''
const visibleTimes = (keys: string[]) =>
  keys.filter((k) => timedKeys.includes(k) && (state(k) !== 'none' || timeEntries(k).length > 0))
// Log decoding flattens SomeRecorded. Restore its wire envelope on writes.
type Entry = { tag: string; contents: any; recordType?: string; recordVal?: any }
const entries = (key: string): Entry[] =>
  Object.entries(props.log.recordedSets)
    .filter(([path]) => path.split('.').pop() === `${key[0].toLowerCase()}${key.slice(1)}`)
    .flatMap(([, values]) => values as Entry[])
const wire = (entry: Entry) => {
  const { recordType = 'RecordableGeneric', recordVal, ...value } = entry
  return { recordType, recordVal: recordVal ?? value }
}
const timeEntries = (key: string) =>
  entries('RecordedTimes').filter((entry) => (entry.recordVal ?? entry).contents?.event === key)
const forget = (key: string) => {
  const originals = [
    ...props.log.recorded,
    ...props.log.crossedOut,
    ...props.log.recordedCounts.map(([entry]) => entry),
  ].filter((entry) => matches(entry, key))
  return [
    ...new Map(
      [keyJson(key), ...originals.map((entry) => entry.raw)].map((raw) => [
        JSON.stringify(raw),
        raw,
      ]),
    ).values(),
  ].map((raw) => ({ tag: 'ForgetRecord', contents: raw }))
}
const setKey = (key: string) => {
  const path = Object.keys(props.log.recordedSets).find(
    (path) => path.split('.').pop() === `${key[0].toLowerCase()}${key.slice(1)}`,
  )
  return path && !path.startsWith('agesUnwound.')
    ? { tag: 'HomebrewCampaignLogKey', contents: key }
    : keyJson(key)
}
const removeTimes = (key: string) =>
  timeEntries(key).length
    ? [
        {
          tag: 'RemoveRecordSetEntries',
          contents: [setKey('RecordedTimes'), timeEntries(key).map(wire)],
        },
      ]
    : []
async function send(messages: unknown[], savingOutcomes = false) {
  if (saving.value || !messages.length) return false
  // Validate the complete proposed history, never just the edited row.
  if (validate().length || (!savingOutcomes && hasOutcomeChanges.value)) {
    saveBlocked.value = true
    saved.value = false
    return false
  }
  saveBlocked.value = false
  saving.value = true
  failed.value = false
  saved.value = false
  try {
    await debug.send(props.game.id, { tag: 'Run', contents: messages })
    saved.value = true
    emit('refresh')
    return true
  } catch {
    failed.value = true
    return false
  } finally {
    saving.value = false
  }
}
function choose(keys: string[], key: string, status = 'recorded') {
  for (const alternative of keys)
    outcomeDrafts.value[alternative] = alternative === key ? status : 'none'
  saved.value = false
  saveBlocked.value = false
}
async function saveOutcomes() {
  if (!hasOutcomeChanges.value) return
  const messages = recordRows
    .filter((row) => row.keys.some((key) => key in outcomeDrafts.value))
    .flatMap((row) => {
      const key = selected(row.keys)
      return recordChoiceMessages(row.keys, key, key ? state(key) : 'none', forget, removeTimes)
    })
  if (await send(messages, true)) outcomeDrafts.value = {}
}
function resetOutcomes() {
  outcomeDrafts.value = {}
  saveBlocked.value = false
}
const valueOf = (event: Event) => (event.target as HTMLInputElement).value
const count = (key: string) =>
  props.log.recordedCounts.find(([entry]) => matches(entry, key))?.[1] ?? 0
function setCount(key: string, event: Event) {
  const value = Number(valueOf(event))
  if (Number.isSafeInteger(value) && value >= 0)
    send([...forget(key), { tag: 'RecordCount', contents: [keyJson(key), value] }])
}
const time = (key: string) => (timeEntries(key)[0]?.recordVal ?? timeEntries(key)[0])?.contents
const draft = (key: string) => drafts.value[key] ?? time(key) ?? { agenda: '', doom: '' }
function setDraft(key: string, field: 'agenda' | 'doom', event: Event) {
  drafts.value[key] = { agenda: draft(key).agenda, doom: draft(key).doom, [field]: valueOf(event) }
}
const validTime = (key: string) => {
  const { agenda, doom } = draft(key)
  return (
    agenda !== '' &&
    doom !== '' &&
    Number.isSafeInteger(Number(agenda)) &&
    Number(agenda) >= 1 &&
    Number.isSafeInteger(Number(doom)) &&
    Number(doom) >= 0
  )
}
async function saveTime(key: string) {
  if (!validTime(key)) return
  await send([
    ...removeTimes(key),
    {
      tag: 'RecordSetInsert',
      contents: [
        setKey('RecordedTimes'),
        [
          {
            recordType: 'RecordableGeneric',
            recordVal: {
              tag: 'Recorded',
              contents: {
                event: key,
                agenda: Number(draft(key).agenda),
                doom: Number(draft(key).doom),
              },
            },
          },
        ],
      ],
    },
  ])
}
const investigators = computed(() => Object.values(props.game.investigators))
function setInvestigator(iid: string, key: string, event: Event) {
  const log = props.game.investigators[iid].log
  const existing = [...log.recorded, ...log.crossedOut].filter((entry) => matches(entry, key))
  send(
    (event.target as HTMLInputElement).checked
      ? [{ tag: 'RecordForInvestigator', contents: [iid, keyJson(key)] }]
      : existing.map((entry) => ({
          tag: 'ForgetRecordForInvestigator',
          contents: [iid, entry.raw],
        })),
  )
}
const disappeared = computed(() => entries('DisappearedUnexpectedly'))
const enemyTitle = (code: string) =>
  cards.cards.find((card) => card.cardCode === code)?.name.title ?? code
async function addEnemy() {
  if (!enemy.value.trim()) return
  await send([
    {
      tag: 'RecordSetInsert',
      contents: [
        setKey('DisappearedUnexpectedly'),
        [
          {
            recordType: 'RecordableCardCode',
            recordVal: { tag: 'Recorded', contents: enemy.value.trim() },
          },
        ],
      ],
    },
  ])
  if (!failed.value && !saveBlocked.value) enemy.value = ''
}
</script>

<template>
  <section class="campaign-record-editor">
    <header class="editor-heading">
      <div>
        <h3>{{ text('title') }}</h3>
        <p>{{ text('hint') }}</p>
      </div>
      <span class="save-status" role="status">{{
        saving ? text('saving') : saved ? text('saved') : ''
      }}</span>
    </header>
    <div v-if="validationIssues.length" class="validation-summary" role="alert">
      <strong>{{ text('invalidTitle') }}</strong>
      <p>{{ text('invalidHint') }}</p>
      <ul>
        <li v-for="issue in validationIssues" :key="issue.id">
          <span>{{ issue.keys.map(title).join(' + ') }}</span>
          <p>{{ text(`validation.${issue.reason}`) }}</p>
        </li>
      </ul>
      <button type="button" @click="showInvalidOutcomes">
        {{ text('showInvalid') }}
      </button>
    </div>
    <p v-if="saveBlocked && !validationIssues.length" class="validation-summary" role="alert">
      {{ text('saveOutcomesFirst') }}
    </p>
    <nav class="editor-tabs" :aria-label="text('title')">
      <button
        v-for="tab in tabs"
        :key="tab"
        type="button"
        :aria-pressed="activeTab === tab"
        @click="activeTab = tab"
      >
        {{ text(`${tab}Tab`) }}
      </button>
    </nav>
    <p v-if="failed" role="alert">{{ text('saveFailed') }}</p>
    <fieldset :disabled="saving">
      <template v-if="activeTab === 'outcomes'">
        <div class="outcome-save-actions">
          <span>{{ hasOutcomeChanges ? text('unsavedOutcomes') : text('outcomesSaved') }}</span>
          <button
            type="button"
            :disabled="!hasOutcomeChanges || validationIssues.length > 0"
            @click="saveOutcomes"
          >
            {{ text('saveOutcomes') }}
          </button>
          <button type="button" :disabled="!hasOutcomeChanges" @click="resetOutcomes">
            {{ text('resetOutcomes') }}
          </button>
        </div>
        <div class="editor-filter">
          <input
            v-model="search"
            type="search"
            :placeholder="text('search')"
            :aria-label="text('search')"
          /><span>{{ filteredRows.length }} / {{ recordRows.length }}</span>
        </div>
        <div class="table-scroll">
          <table class="outcomes-table">
            <thead>
              <tr>
                <th>{{ text('entry') }}</th>
                <th>{{ text('state') }}</th>
                <th>{{ text('time') }}</th>
              </tr>
            </thead>
            <tbody>
              <tr
                v-for="(row, index) in filteredRows"
                :key="row.keys[0]"
                :data-state="state(selected(row.keys) || row.keys[0])"
                :class="{ 'invalid-row': rowIssues(row.keys).length > 0 }"
              >
                <td>
                  <template v-if="row.keys.length === 1">{{ title(row.keys[0]) }}</template>
                  <label v-else class="outcome-label" :for="`record-outcome-${index}`">{{
                    row.label
                  }}</label>
                  <select
                    v-if="row.keys.length > 1"
                    :id="`record-outcome-${index}`"
                    :value="selected(row.keys)"
                    :aria-invalid="rowIssues(row.keys).length > 0"
                    @change="choose(row.keys, valueOf($event))"
                  >
                    <option value="">{{ text('none') }}</option>
                    <option v-for="key in row.keys" :key="key" :value="key">
                      {{ title(key) }}
                    </option>
                  </select>
                  <span v-for="issue in rowIssues(row.keys)" :key="issue.id" class="row-validation">
                    {{ text(`validation.${issue.reason}`) }}
                  </span>
                </td>
                <td>
                  <select
                    :disabled="row.keys.length > 1 && !selected(row.keys)"
                    :aria-label="`${row.label ?? title(row.keys[0])}: ${text('state')}`"
                    :value="state(selected(row.keys) || row.keys[0])"
                    :aria-invalid="rowIssues(row.keys).length > 0"
                    @change="choose(row.keys, selected(row.keys) || row.keys[0], valueOf($event))"
                  >
                    <option value="none">{{ text('none') }}</option>
                    <option value="recorded">{{ text('recorded') }}</option>
                    <option value="crossedOut">{{ text('crossedOut') }}</option>
                  </select>
                </td>
                <td>
                  <template v-for="key in visibleTimes(row.keys)" :key="key">
                    <div class="time-fields">
                      <label
                        >{{ text('agenda')
                        }}<input
                          type="number"
                          min="1"
                          step="1"
                          :value="draft(key).agenda"
                          @input="setDraft(key, 'agenda', $event)"
                      /></label>
                      <label
                        >{{ text('doom')
                        }}<input
                          type="number"
                          min="0"
                          step="1"
                          :value="draft(key).doom"
                          @input="setDraft(key, 'doom', $event)"
                      /></label>
                      <button type="button" :disabled="!validTime(key)" @click="saveTime(key)">
                        {{ text('saveTime') }}
                      </button>
                      <button
                        v-if="timeEntries(key).length"
                        type="button"
                        @click="send(removeTimes(key))"
                      >
                        {{ text('clearTime') }}
                      </button>
                    </div>
                  </template>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
        <p v-if="!filteredRows.length" class="empty-editor">{{ text('noMatches') }}</p>
      </template>
      <template v-if="activeTab === 'counts'">
        <h4>{{ text('counts') }}</h4>
        <table class="tallies-table">
          <tbody>
            <tr v-for="key in countKeys" :key="key">
              <th>
                <label :for="`count-${key}`">{{ title(key) }}</label>
              </th>
              <td>
                <input
                  :id="`count-${key}`"
                  type="number"
                  min="0"
                  step="1"
                  :value="count(key)"
                  @change="setCount(key, $event)"
                />
              </td>
              <td>
                <button type="button" @click="send(forget(key))">{{ text('remove') }}</button>
              </td>
            </tr>
          </tbody>
        </table>
      </template>
      <template v-if="activeTab === 'investigators'">
        <h4>{{ text('investigators') }}</h4>
        <div class="table-scroll">
          <table>
            <thead>
              <tr>
                <th>{{ text('entry') }}</th>
                <th v-for="investigator in investigators" :key="investigator.id">
                  {{ investigator.name.title }}
                </th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="key in investigatorKeys" :key="key">
                <th>{{ title(key) }}</th>
                <td v-for="investigator in investigators" :key="investigator.id">
                  <input
                    type="checkbox"
                    :aria-label="`${investigator.name.title}: ${title(key)}`"
                    :checked="state(key, investigator.log) === 'recorded'"
                    @change="setInvestigator(investigator.id, key, $event)"
                  />
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </template>
      <template v-if="activeTab === 'enemies'">
        <h4>{{ text('enemies') }}</h4>
        <p v-if="!disappeared.length" class="empty-editor">{{ text('noEnemies') }}</p>
        <table>
          <tbody>
            <tr v-for="(entry, index) in disappeared" :key="index">
              <td>{{ enemyTitle((entry.recordVal ?? entry).contents) }}</td>
              <td>
                <button
                  type="button"
                  @click="
                    send([
                      {
                        tag: 'RemoveRecordSetEntries',
                        contents: [setKey('DisappearedUnexpectedly'), [wire(entry)]],
                      },
                    ])
                  "
                >
                  {{ text('remove') }}
                </button>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="enemy-picker">
          <CardCodeField v-model="enemy" :kinds="['Enemy']" /><button
            type="button"
            :disabled="!enemy.trim()"
            @click="addEnemy"
          >
            {{ text('addEnemy') }}
          </button>
        </div>
      </template>
    </fieldset>
  </section>
</template>

<style scoped>
.campaign-record-editor {
  --palette-sage: oklch(80% 0.08 155);
  --palette-amber: oklch(83% 0.1 85);
  --palette-slate: oklch(77% 0.025 250);
  --palette-blue: oklch(82% 0.065 245);
  --palette-rose: oklch(80% 0.09 25);
  --recorded-accent: var(--palette-sage);
  --crossed-accent: var(--palette-amber);
  --empty-accent: var(--palette-slate);
  --time-accent: var(--palette-blue);
  --editor-accent: var(--palette-amber);
  --error-accent: var(--palette-rose);
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 8px;
  background: var(--box-background);
  color: var(--title);
  overflow: hidden;
  font-size: 0.9rem;
  line-height: 1.5;
}
.editor-heading {
  display: flex;
  align-items: start;
  justify-content: space-between;
  gap: 24px;
  padding: 22px 24px 16px;
}
h3,
h4 {
  margin: 0;
  font-weight: normal;
}
h3 {
  font-family: teutonic, sans-serif;
  font-size: 1.35rem;
  letter-spacing: 0.06em;
}
h4 {
  font-size: 1rem;
  margin: 4px 0 20px;
}
.editor-heading p {
  margin: 6px 0 0;
  max-width: 65ch;
  color: var(--text);
  font-size: 0.85rem;
}
.save-status {
  flex-shrink: 0;
  font-size: 0.8rem;
  padding-top: 4px;
  color: var(--text);
}
.editor-tabs {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  padding: 0 24px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.1);
}
.editor-tabs button {
  border: 0;
  border-radius: 0;
  border-bottom: 2px solid transparent;
  background: transparent;
  padding: 12px 4px;
  margin-right: 16px;
  color: var(--text);
}
.editor-tabs button[aria-pressed='true'] {
  color: var(--title);
  border-bottom-color: currentColor;
}
fieldset {
  border: 0;
  padding: 20px 24px 24px;
  margin: 0;
  min-width: 0;
}
.editor-filter {
  display: flex;
  gap: 16px;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 20px;
}
.editor-filter input {
  width: min(100%, 25rem);
}
.editor-filter span {
  color: var(--text);
  font-size: 0.8rem;
  font-variant-numeric: tabular-nums;
  white-space: nowrap;
}
.table-scroll {
  overflow-x: auto;
}
table {
  width: 100%;
  border-collapse: collapse;
  text-align: left;
}
th,
td {
  padding: 16px 14px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.07);
  vertical-align: middle;
  font-weight: normal;
}
th:first-child,
td:first-child {
  padding-left: 0;
}
th:last-child,
td:last-child {
  padding-right: 0;
}
thead th {
  padding-top: 0;
  padding-bottom: 12px;
  color: var(--text);
  font-size: 0.76rem;
  font-weight: 600;
  letter-spacing: 0.04em;
}
tbody tr:last-child > * {
  border-bottom: 0;
}
.outcomes-table {
  table-layout: fixed;
}
.outcomes-table th:first-child {
  width: 47%;
}
.outcomes-table th:nth-child(2) {
  width: 17%;
}
.outcome-label {
  display: block;
  margin-bottom: 6px;
  font-size: 0.78rem;
  color: var(--text);
}
button,
select,
input {
  font: inherit;
}
select,
input:not([type='checkbox']) {
  border: 1px solid rgba(255, 255, 255, 0.16);
  border-radius: 5px;
  background: var(--background);
  color: var(--title);
  min-height: 36px;
  padding: 7px 10px;
}
select {
  width: 100%;
  display: block;
  text-overflow: ellipsis;
  cursor: pointer;
}
option {
  background: var(--background);
  color: var(--title);
}
input[type='number'] {
  width: 4.4rem;
  font-variant-numeric: tabular-nums;
}
input[type='checkbox'] {
  width: 18px;
  height: 18px;
  cursor: pointer;
}
button {
  border: 1px solid rgba(255, 255, 255, 0.15);
  border-radius: 5px;
  background: rgba(255, 255, 255, 0.05);
  color: var(--title);
  padding: 7px 12px;
  min-height: 36px;
  cursor: pointer;
  transition:
    background 0.16s,
    border-color 0.16s;
}
button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.1);
  border-color: rgba(255, 255, 255, 0.3);
}
button:active:not(:disabled) {
  transform: translateY(1px);
}
button:disabled,
fieldset:disabled {
  opacity: 0.55;
}
:where(button, select, input):focus-visible {
  outline: 2px solid var(--title);
  outline-offset: 3px;
}
.time-fields {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  align-items: end;
}
.time-fields label {
  display: flex;
  flex-direction: column;
  gap: 4px;
  color: var(--text);
  font-size: 0.75rem;
}
.time-fields input {
  font-size: 0.85rem;
  color: var(--title);
}
.time-fields button {
  font-size: 0.76rem;
  padding-inline: 9px;
}
.enemy-picker {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  align-items: flex-start;
  margin-top: 20px;
}
/* Keep this picker's results in the editor's flow so the panel grows with
   them instead of clipping an absolutely positioned menu at its lower edge. */
.enemy-picker :deep(.field-row) {
  flex: 1 1 24rem;
}
.enemy-picker :deep(.binding-menu) {
  position: static;
  max-height: min(15rem, 40dvh);
  width: 100%;
  box-sizing: border-box;
}
.empty-editor {
  margin: 16px 0;
  color: var(--text);
}
[role='alert'] {
  display: block;
  padding: 10px 12px;
  margin: 8px 0;
  border-left: 2px solid currentColor;
  background: rgba(255, 255, 255, 0.05);
}
@media (max-width: 760px) {
  .editor-heading {
    padding: 18px 16px 12px;
  }
  .editor-tabs {
    padding: 0 16px;
    gap: 4px;
  }
  .editor-tabs button {
    margin-right: 8px;
    font-size: 0.8rem;
  }
  fieldset {
    padding: 16px;
  }
  .outcomes-table,
  .outcomes-table tbody {
    display: block;
  }
  .outcomes-table thead {
    display: none;
  }
  .outcomes-table tr {
    display: grid;
    grid-template-columns: minmax(0, 1fr);
    padding: 16px 0;
    gap: 12px;
    border-bottom: 1px solid rgba(255, 255, 255, 0.1);
  }
  .outcomes-table td {
    border: 0;
    padding: 0;
    min-width: 0;
  }
  .outcomes-table td:nth-child(2) select {
    max-width: 12rem;
  }
  .outcomes-table td:last-child {
    grid-column: 1 / -1;
  }
  .outcomes-table td:last-child:empty {
    display: none;
  }
}
/* Status stays explicit in the select; color adds a second reading cue. */
.editor-heading {
  background: color-mix(in oklab, var(--editor-accent) 4%, var(--box-background));
  border-top: 2px solid color-mix(in oklab, var(--editor-accent) 65%, transparent);
}
h3 {
  color: var(--editor-accent);
}
.editor-tabs button[aria-pressed='true'] {
  color: var(--editor-accent);
}
.save-status {
  color: var(--recorded-accent);
}
.outcomes-table tr {
  --row-accent: var(--empty-accent);
}
.outcomes-table tr[data-state='recorded'] {
  --row-accent: var(--recorded-accent);
}
.outcomes-table tr[data-state='crossedOut'] {
  --row-accent: var(--crossed-accent);
}
.outcomes-table tbody tr {
  background: color-mix(in oklab, var(--row-accent) 4%, transparent);
}
.outcomes-table tbody tr:nth-child(even) {
  background: color-mix(in oklab, var(--row-accent) 7%, transparent);
}
.outcomes-table tbody td:first-child {
  border-left: 2px solid color-mix(in oklab, var(--row-accent) 50%, transparent);
  padding-left: 14px;
}
.outcomes-table td:nth-child(2) select {
  color: var(--row-accent);
  border-color: color-mix(in oklab, var(--row-accent) 35%, transparent);
  background: color-mix(in oklab, var(--row-accent) 8%, var(--background));
}
.time-fields label,
.time-fields input {
  color: var(--time-accent);
}
.time-fields input {
  background: color-mix(in oklab, var(--time-accent) 6%, var(--background));
  border-color: color-mix(in oklab, var(--time-accent) 25%, transparent);
}
.tallies-table input {
  color: var(--editor-accent);
  border-color: color-mix(in oklab, var(--editor-accent) 35%, transparent);
}
input[type='checkbox'] {
  accent-color: var(--recorded-accent);
}
[role='alert'] {
  color: var(--error-accent);
}
@media (max-width: 760px) {
  .outcomes-table tbody td:first-child {
    border-left: 0;
    padding-left: 0;
  }
  .outcomes-table tbody tr {
    padding: 16px 12px;
    border-left: 2px solid var(--row-accent);
  }
}

.validation-summary {
  margin: 16px 24px;
  padding: 16px;
  border: 1px solid color-mix(in oklab, var(--error-accent) 40%, transparent);
  border-radius: 6px;
  background: color-mix(in oklab, var(--error-accent) 6%, var(--background));
  color: var(--error-accent);
}
.validation-summary p {
  margin: 4px 0 12px;
}
.validation-summary ul {
  margin: 12px 0;
  padding-left: 20px;
}
.validation-summary li + li {
  margin-top: 12px;
}
.validation-summary li span {
  font-weight: 600;
}
.validation-summary li p {
  color: var(--text);
  font-size: 0.85rem;
}
.outcome-save-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  align-items: center;
  margin-bottom: 20px;
}
.outcome-save-actions span {
  flex: 1;
  color: var(--text);
  font-size: 0.8rem;
}
.outcomes-table tr.invalid-row {
  --row-accent: var(--error-accent);
}
.row-validation {
  display: block;
  color: var(--error-accent);
  font-size: 0.8rem;
  margin-top: 8px;
}
[aria-invalid='true'] {
  border-color: var(--error-accent);
}
</style>

<script lang="ts" setup>
/* Add, edit or delete the campaign log's free-text entries. Official entries
 * and a homebrew campaign's own keys are left alone: this edits players' notes,
 * not the campaign. An edit is delete-then-record in one `Run`, so it undoes
 * as a single step. */
import { computed, ref } from 'vue'
import { useDebug } from '@/arkham/debug'
import type { Game } from '@/arkham/types/Game'
import type { LogContents, LogKey } from '@/arkham/types/Log'
import {
  customLogKey,
  editableLogText,
  isEditableLogKey,
  recordCountMessage,
  recordMessage,
  forgetRecordMessage,
  renameCountMessage,
  renameNoteMessage,
  type CampaignLogKeyJson,
} from '@/arkham/debugPlayer'

const props = defineProps<{
  game: Game
  log: LogContents
  homebrewScope?: string
}>()

const debug = useDebug()
const send = (message: unknown) => debug.send(props.game.id, message)

const rawKey = (key: LogKey) => key.raw as CampaignLogKeyJson
const keyId = (key: LogKey) => JSON.stringify(key.raw)

const notes = computed(() =>
  props.log.recorded
    .filter((key) => isEditableLogKey(key.raw, props.homebrewScope))
    .map((key) => ({ key, id: keyId(key), text: editableLogText(key.raw) }))
)

const counts = computed(() =>
  props.log.recordedCounts
    .filter(([key]) => isEditableLogKey(key.raw, props.homebrewScope))
    .map(([key, value]) => ({ key, id: keyId(key), text: editableLogText(key.raw), value }))
)

/* One entry is edited at a time; `editing` is its id, `draft` its new text. */
const editing = ref<string | null>(null)
const draft = ref('')

const startEdit = (id: string, text: string) => {
  editing.value = id
  draft.value = text
}
const cancelEdit = () => { editing.value = null }

const saveNote = (key: LogKey) => {
  const text = draft.value.trim()
  if (text && text !== editableLogText(key.raw)) send(renameNoteMessage(rawKey(key), text))
  editing.value = null
}

const saveCountName = (key: LogKey, value: number) => {
  const text = draft.value.trim()
  if (text && text !== editableLogText(key.raw)) send(renameCountMessage(rawKey(key), text, value))
  editing.value = null
}

const setCount = (key: LogKey, event: Event) => {
  const value = Number((event.target as HTMLInputElement).value)
  if (Number.isFinite(value)) send(recordCountMessage(rawKey(key), value))
}

const remove = (key: LogKey) => send(forgetRecordMessage(rawKey(key)))

const newNote = ref('')
const addNote = () => {
  const text = newNote.value.trim()
  if (!text) return
  send(recordMessage(customLogKey(text)))
  newNote.value = ''
}

const newCountName = ref('')
const newCountValue = ref(1)
const addCount = () => {
  const text = newCountName.value.trim()
  if (!text) return
  send(recordCountMessage(customLogKey(text), newCountValue.value))
  newCountName.value = ''
  newCountValue.value = 1
}
</script>

<template>
  <section class="log-debug">
    <h3 class="log-debug__title">{{ $t('debug.campaign.customEntries') }}</h3>
    <p class="log-debug__hint">{{ $t('debug.campaign.customEntriesHint') }}</p>

    <ul v-if="notes.length > 0" class="log-debug__list">
      <li v-for="note in notes" :key="note.id" class="log-debug__item">
        <template v-if="editing === note.id">
          <input v-model="draft" type="text" class="log-debug__text" @keydown.enter="saveNote(note.key)" @keydown.esc.stop="cancelEdit" />
          <button type="button" @click="saveNote(note.key)">{{ $t('debug.campaign.save') }}</button>
          <button type="button" @click="cancelEdit">{{ $t('debug.campaign.cancel') }}</button>
        </template>
        <template v-else>
          <span class="log-debug__label">{{ note.text }}</span>
          <button type="button" @click="startEdit(note.id, note.text)">{{ $t('debug.campaign.edit') }}</button>
          <button type="button" class="log-debug__danger" @click="remove(note.key)">{{ $t('debug.campaign.delete') }}</button>
        </template>
      </li>
    </ul>

    <ul v-if="counts.length > 0" class="log-debug__list">
      <li v-for="count in counts" :key="count.id" class="log-debug__item">
        <template v-if="editing === count.id">
          <input v-model="draft" type="text" class="log-debug__text" @keydown.enter="saveCountName(count.key, count.value)" @keydown.esc.stop="cancelEdit" />
          <button type="button" @click="saveCountName(count.key, count.value)">{{ $t('debug.campaign.save') }}</button>
          <button type="button" @click="cancelEdit">{{ $t('debug.campaign.cancel') }}</button>
        </template>
        <template v-else>
          <span class="log-debug__label">{{ count.text }}</span>
          <input
            type="number"
            class="log-debug__number"
            :value="count.value"
            v-tooltip="$t('debug.campaign.countHint')"
            @change="setCount(count.key, $event)"
          />
          <button type="button" @click="startEdit(count.id, count.text)">{{ $t('debug.campaign.rename') }}</button>
          <button type="button" class="log-debug__danger" @click="remove(count.key)">{{ $t('debug.campaign.delete') }}</button>
        </template>
      </li>
    </ul>

    <div class="log-debug__add">
      <input v-model="newNote" type="text" class="log-debug__text" :placeholder="$t('debug.campaign.notePlaceholder')" @keydown.enter="addNote" />
      <button type="button" :disabled="!newNote.trim()" @click="addNote">{{ $t('debug.campaign.addNote') }}</button>
    </div>
    <div class="log-debug__add">
      <input v-model="newCountName" type="text" class="log-debug__text" :placeholder="$t('debug.campaign.countPlaceholder')" @keydown.enter="addCount" />
      <input v-model.number="newCountValue" type="number" class="log-debug__number" />
      <button type="button" :disabled="!newCountName.trim()" @click="addCount">{{ $t('debug.campaign.addCount') }}</button>
    </div>
  </section>
</template>

<style scoped>
.log-debug {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 14px 16px;
  border: 1px dashed rgba(255, 255, 255, 0.25);
  border-radius: 8px;
  background: var(--box-background);
  color: var(--text);
  font-size: 0.85rem;
}

.log-debug__title {
  margin: 0;
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.75);
}

.log-debug__hint {
  margin: 0;
  color: rgba(255, 255, 255, 0.55);
}

.log-debug__list {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.log-debug__item,
.log-debug__add {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 6px;
}

.log-debug__item {
  padding: 6px 10px;
  border-radius: 5px;
  background: rgba(255, 255, 255, 0.04);
}

.log-debug__label {
  flex: 1;
  min-width: 8em;
  color: var(--title);
}

.log-debug input {
  padding: 4px 6px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
  font: inherit;
}

.log-debug__text {
  flex: 1;
  min-width: 10em;
}

.log-debug__number {
  width: 5em;
}

.log-debug button {
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
  font-size: 0.8rem;
  padding: 4px 10px;
  cursor: pointer;
}

.log-debug button:hover:not(:disabled) {
  background: var(--button);
  border-color: var(--select);
}

.log-debug button.log-debug__danger:hover:not(:disabled) {
  background: #7a2020;
  border-color: #c05050;
}

.log-debug button:disabled {
  opacity: 0.45;
  cursor: not-allowed;
}
</style>

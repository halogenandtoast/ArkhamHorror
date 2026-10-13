<script setup lang="ts">
/* The home-page banner: writing a notification here is publishing it.
 *
 * There is no draft state to save into -- the public read has no filter, so a
 * row exists and is live, or it does not exist. Two things follow, and the page
 * says both rather than letting them be discovered: taking one down is a
 * delete, and editing one does not bring it back for anybody who dismissed it,
 * because dismissal is per-browser and keyed by the row's id. Re-announcing
 * something means a new notification.
 *
 * The body is rendered as HTML on the home page, so it is rendered as HTML here
 * too, in the same chrome players see it in. An admin writing markup needs to
 * see the markup land, not an escaped copy of it.
 */
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'

const { t } = useI18n()
const K = 'adminNotifications.'

const rows = ref<Api.AppNotification[]>([])
const loading = ref(true)
const error = ref<string | null>(null)
const status = ref<string | null>(null)

/* The composer. `draftDate` empty means "now", which is what publishing almost
 * always means; it is here because the list is ordered by it. */
const draftBody = ref('')
const draftDate = ref('')
const creating = ref(false)

/* Which row is being edited, and its pending values. One at a time: editing a
 * live banner is a deliberate act, not something to have several of open. */
const editingId = ref<number | null>(null)
const editBody = ref('')
const editDate = ref('')

const busy = ref<number | null>(null)
const confirmingDelete = ref<number | null>(null)

const draftValid = computed(() => draftBody.value.trim().length > 0)
const editValid = computed(() => editBody.value.trim().length > 0)

async function load() {
  loading.value = true
  error.value = null
  try {
    rows.value = await Api.fetchAdminNotifications()
  } catch (e) {
    console.error(e)
    error.value = t(`${K}loadFailed`)
  } finally {
    loading.value = false
  }
}

onMounted(load)

/* `datetime-local` speaks local wall-clock with no zone; the API speaks UTC.
 * These two are the only places that conversion happens. */
function toLocalInput(iso: string): string {
  const d = new Date(iso)
  const pad = (n: number) => String(n).padStart(2, '0')
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}T${pad(d.getHours())}:${pad(d.getMinutes())}`
}

function fromLocalInput(value: string): string | undefined {
  if (!value) return undefined
  const d = new Date(value)
  return Number.isNaN(d.getTime()) ? undefined : d.toISOString()
}

/* Newest first, the order the home page shows them in, so the top row here is
 * the top banner there. */
const inOrder = (list: Api.AppNotification[]) =>
  [...list].sort((a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime())

async function create() {
  if (!draftValid.value) return
  creating.value = true
  error.value = null
  status.value = null
  try {
    const created = await Api.createNotification(draftBody.value.trim(), fromLocalInput(draftDate.value))
    rows.value = inOrder([created, ...rows.value])
    draftBody.value = ''
    draftDate.value = ''
    status.value = t(`${K}published`)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}createFailed`)
  } finally {
    creating.value = false
  }
}

function startEdit(row: Api.AppNotification) {
  editingId.value = row.id
  editBody.value = row.body
  editDate.value = toLocalInput(row.createdAt)
  confirmingDelete.value = null
  status.value = null
  error.value = null
}

function cancelEdit() {
  editingId.value = null
  editBody.value = ''
  editDate.value = ''
}

async function save(row: Api.AppNotification) {
  if (!editValid.value) return
  busy.value = row.id
  error.value = null
  status.value = null
  try {
    /* Only what actually changed is sent, since the handler writes exactly the
     * fields it is given. The date is compared as the input spells it, not as
     * an instant: the input has minute resolution, so a stored 07:59:18 reads
     * back as 07:59 and would look changed on every save, silently rounding the
     * seconds off a row whose body was the only thing touched. */
    const changes: { body?: string; createdAt?: string } = {}
    const body = editBody.value.trim()
    if (body !== row.body) changes.body = body
    if (editDate.value && editDate.value !== toLocalInput(row.createdAt)) {
      const date = fromLocalInput(editDate.value)
      if (date) changes.createdAt = date
    }

    if (!Object.keys(changes).length) {
      cancelEdit()
      return
    }

    const updated = await Api.updateNotification(row.id, changes)
    rows.value = inOrder(rows.value.map((r) => (r.id === updated.id ? updated : r)))
    cancelEdit()
    status.value = t(`${K}saved`)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}saveFailed`)
  } finally {
    busy.value = null
  }
}

async function remove(row: Api.AppNotification) {
  busy.value = row.id
  error.value = null
  status.value = null
  try {
    await Api.deleteNotification(row.id)
    rows.value = rows.value.filter((r) => r.id !== row.id)
    confirmingDelete.value = null
    if (editingId.value === row.id) cancelEdit()
    status.value = t(`${K}deleted`)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}deleteFailed`)
  } finally {
    busy.value = null
  }
}

const when = (iso: string) => new Date(iso).toLocaleString()

// A notification dated ahead of now is already live: the read has no filter.
const isFuture = (iso: string) => new Date(iso).getTime() > Date.now()
</script>

<template>
  <section class="admin-block">
    <header class="section-header">
      <h2>{{ t(`${K}title`) }}</h2>
      <span class="count-badge">{{ rows.length }}</span>
    </header>

    <p class="section-note">{{ t(`${K}liveWarning`) }}</p>

    <p v-if="status" class="status">{{ status }}</p>
    <p v-if="error" class="error">{{ error }}</p>

    <form class="composer" @submit.prevent="create">
      <label for="new-notification">{{ t(`${K}newLabel`) }}</label>
      <textarea
        id="new-notification"
        v-model="draftBody"
        rows="3"
        :placeholder="t(`${K}bodyPlaceholder`)"
        @keydown.stop
      ></textarea>
      <p class="hint">{{ t(`${K}htmlHint`) }}</p>

      <!-- What the home page will show. The same markup and the same chrome,
           because the point of a preview is that nothing else differs. -->
      <div v-if="draftValid" class="preview">
        <p class="preview-label">{{ t(`${K}preview`) }}</p>
        <div class="notification">
          <p v-html="draftBody"></p>
          <a href="#" @click.prevent>{{ t('home.dismiss') }}</a>
        </div>
      </div>

      <div class="composer-foot">
        <label class="date-field">
          <span>{{ t(`${K}dateLabel`) }}</span>
          <input type="datetime-local" v-model="draftDate" @keydown.stop />
        </label>
        <button type="submit" class="publish" :disabled="!draftValid || creating">
          {{ creating ? t(`${K}publishing`) : t(`${K}publish`) }}
        </button>
      </div>
      <p class="hint">{{ t(`${K}dateHint`) }}</p>
    </form>

    <p v-if="loading" class="empty box">{{ t(`${K}loading`) }}</p>
    <p v-else-if="!rows.length" class="empty box">{{ t(`${K}empty`) }}</p>

    <ul v-else class="list">
      <li v-for="row in rows" :key="row.id" class="row">
        <div class="head">
          <p class="meta">
            <span class="id">#{{ row.id }}</span>
            ·
            {{ when(row.createdAt) }}
            <span v-if="isFuture(row.createdAt)" class="tag future">
              {{ t(`${K}futureDated`) }}
            </span>
          </p>
          <div class="actions">
            <button
              v-if="editingId !== row.id"
              type="button"
              class="ghost"
              @click="startEdit(row)"
            >
              {{ t(`${K}edit`) }}
            </button>
            <button
              v-if="confirmingDelete !== row.id"
              type="button"
              class="danger"
              :disabled="busy === row.id"
              @click="confirmingDelete = row.id; editingId = null"
            >
              {{ t(`${K}takeDown`) }}
            </button>
          </div>
        </div>

        <div v-if="editingId === row.id" class="editor">
          <textarea v-model="editBody" rows="3" @keydown.stop @keydown.esc="cancelEdit"></textarea>
          <div class="preview">
            <p class="preview-label">{{ t(`${K}preview`) }}</p>
            <div class="notification">
              <p v-html="editBody"></p>
              <a href="#" @click.prevent>{{ t('home.dismiss') }}</a>
            </div>
          </div>
          <p class="hint">{{ t(`${K}editHint`) }}</p>
          <div class="editor-foot">
            <label class="date-field">
              <span>{{ t(`${K}dateLabel`) }}</span>
              <input type="datetime-local" v-model="editDate" @keydown.stop />
            </label>
            <button type="button" class="publish" :disabled="!editValid || busy === row.id" @click="save(row)">
              {{ t(`${K}save`) }}
            </button>
            <button type="button" class="ghost" @click="cancelEdit">{{ t(`${K}cancel`) }}</button>
          </div>
        </div>

        <!-- Not editing: the notification exactly as players meet it. -->
        <div v-else class="notification">
          <p v-html="row.body"></p>
          <a href="#" @click.prevent>{{ t('home.dismiss') }}</a>
        </div>

        <div v-if="confirmingDelete === row.id" class="confirm">
          <p>{{ t(`${K}confirmTakeDown`) }}</p>
          <div class="confirm-actions">
            <button type="button" class="danger" :disabled="busy === row.id" @click="remove(row)">
              {{ t(`${K}confirmTakeDownYes`) }}
            </button>
            <button type="button" class="ghost" @click="confirmingDelete = null">
              {{ t(`${K}cancel`) }}
            </button>
          </div>
        </div>
      </li>
    </ul>
  </section>
</template>

<style scoped>
/* The admin shell's styles are scoped, so the panel chrome is restated here
   rather than inherited. */
.admin-block {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
  display: flex;
  flex-direction: column;
  gap: 12px;
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
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.6rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.count-badge {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--spooky-green);
  border-left-width: 4px;
  border-radius: 3px;
  color: color-mix(in srgb, var(--spooky-green) 78%, white);
  display: inline-flex;
  font-size: 0.78rem;
  font-weight: 800;
  justify-content: center;
  line-height: 1;
  min-width: 2.1em;
  padding: 5px 7px;
}

.section-note {
  color: color-mix(in srgb, var(--title) 62%, transparent);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  margin: 0;
  text-transform: uppercase;
}

.status,
.error {
  border-radius: 4px;
  font-size: 0.9rem;
  margin: 0;
  padding: 8px 10px;
}

.status {
  background: color-mix(in srgb, var(--spooky-green-dark) 60%, transparent);
  color: white;
}

.error {
  background: color-mix(in srgb, #7a2020 65%, transparent);
  color: #ffdede;
}

.empty {
  color: var(--title);
  margin: 0;
  opacity: 0.75;
  padding: 10px;
}

/* ---------------------------------------------------------------- composer --- */

.composer {
  border: 1px solid var(--box-border);
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 12px;
}

.composer > label,
.date-field > span {
  color: color-mix(in srgb, var(--title) 72%, transparent);
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

textarea {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  box-sizing: border-box;
  color: var(--title);
  font-family: "Noto Sans", Avenir, Helvetica, Arial, sans-serif;
  font-size: 0.95rem;
  padding: 8px;
  resize: vertical;
  width: 100%;
}

textarea:focus {
  border-color: var(--spooky-green);
  outline: none;
}

.hint {
  color: color-mix(in srgb, var(--title) 55%, transparent);
  font-size: 0.8rem;
  margin: 0;
}

.composer-foot,
.editor-foot {
  align-items: end;
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}

.date-field {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

input[type="datetime-local"] {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  /* The global button/input padding is on border-box, so it is set rather than
     inherited: the picker widget needs the room. */
  padding: 7px 8px;
}

input[type="datetime-local"]:focus {
  border-color: var(--spooky-green);
  outline: none;
}

button {
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.82rem;
  font-weight: 800;
  letter-spacing: 0.06em;
  padding: 8px 12px;
  text-transform: uppercase;
}

button:disabled {
  cursor: not-allowed;
  opacity: 0.45;
}

.publish {
  background: var(--spooky-green-dark);
  border: 1px solid var(--spooky-green);
  color: white;
}

.publish:hover:not(:disabled) {
  background: var(--spooky-green);
}

.ghost {
  background: transparent;
  border: 1px solid var(--box-border);
  color: color-mix(in srgb, var(--title) 80%, transparent);
}

.ghost:hover {
  border-color: var(--spooky-green);
  color: white;
}

.danger {
  background: transparent;
  border: 1px solid #8d3a3a;
  color: #e79b9b;
}

.danger:hover:not(:disabled) {
  background: #8d3a3a;
  color: white;
}

/* ------------------------------------------------------------------- list --- */

.list {
  display: flex;
  flex-direction: column;
  gap: 12px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.row {
  border: 1px solid var(--box-border);
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 12px;
}

.head {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}

.meta {
  color: color-mix(in srgb, var(--title) 62%, transparent);
  flex: 1;
  font-size: 0.82rem;
  margin: 0;
}

.id {
  color: color-mix(in srgb, var(--spooky-green) 80%, white);
  font-weight: 800;
}

.tag.future {
  background: #6b4a12;
  border-radius: 3px;
  color: #ffe6ad;
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.06em;
  margin-left: 6px;
  padding: 3px 6px;
  text-transform: uppercase;
}

.actions {
  display: flex;
  gap: 8px;
}

.editor {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.preview {
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.preview-label {
  color: color-mix(in srgb, var(--title) 55%, transparent);
  font-size: 0.72rem;
  font-weight: 700;
  letter-spacing: 0.1em;
  margin: 0;
  text-transform: uppercase;
}

/* Lifted from Home.vue unchanged. A preview that is styled even slightly
   differently from the real banner is not a preview of it. */
.notification {
  --text: #816F3A;
  --border: var(--text);
  --background: #FFF8E6;
  background-color: var(--background);
  border: 2px solid var(--border);
  border-radius: 5px;
  box-sizing: border-box;
  color: var(--text);
  display: flex;
  flex-direction: row;
  font-size: 1.2em;
  gap: 5px;
  padding: 10px;
}

.notification > p {
  flex: 1;
  margin: 0;
}

.notification a {
  color: var(--seeker-dark);
  text-decoration: underline;
}

.notification :deep(a) {
  color: var(--seeker-dark);
  text-decoration: underline;
}

.confirm {
  align-items: center;
  background: color-mix(in srgb, #7a2020 32%, transparent);
  border-radius: 4px;
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  padding: 8px 10px;
}

.confirm p {
  color: #ffdede;
  flex: 1;
  font-size: 0.88rem;
  margin: 0;
}

.confirm-actions {
  display: flex;
  gap: 8px;
}

@media (max-width: 620px) {
  .head {
    align-items: flex-start;
    flex-direction: column;
  }

  .composer-foot,
  .editor-foot {
    align-items: stretch;
    flex-direction: column;
  }
}
</style>

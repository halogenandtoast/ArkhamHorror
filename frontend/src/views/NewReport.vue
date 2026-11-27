<script setup lang="ts">
/* Filing a report from outside a game.
 *
 * The in-game button is the better path for a bug -- it knows which game you
 * are in and the server takes the export from it -- so this page offers the
 * same attachment as a choice among your own games, and says why it matters.
 */
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import ReportRow from '@/components/ReportRow.vue'
import type { GameDetailsEntry } from '@/arkham/types/Game'

const router = useRouter()
const { t } = useI18n()

const kind = ref<Api.ReportKind>('bug')
const title = ref('')
const body = ref('')
const blocking = ref(false)
const gameId = ref('')
const games = ref<GameDetailsEntry[]>([])
const sending = ref(false)
const failed = ref<string | null>(null)

onMounted(async () => {
  try {
    games.value = await Api.fetchGames()
  } catch {
    games.value = []
  }
})

const playable = computed(() =>
  games.value.flatMap((entry) => (entry.tag === 'game' ? [entry] : [])),
)

/* "Has this been said already?" -- asked while it can still change what they
 * do, rather than answered with a duplicate marker three days later.
 *
 * Fed by the title and the description together, because the distinctive word
 * is as often in one as the other, and left to the server to rank: it is the
 * only side that can see the reports this account has never opened.
 *
 * Only ever a suggestion. Nothing here disables the form or asks for a
 * confirmation -- plenty of reports that look alike are not the same bug, and
 * the person filing is better placed to tell than a word count is.
 */
const MIN_TYPED = 8
const DEBOUNCE_MS = 400

const similar = ref<Api.Report[]>([])
const dismissed = ref(false)

/* Dropped responses rather than cancelled requests: a search that comes back
 * after the one for what is now in the box would otherwise overwrite it. */
let newest = 0
let pending: ReturnType<typeof setTimeout> | null = null

const lookForSimilar = async () => {
  const typed = `${title.value} ${body.value}`.trim()
  if (typed.length < MIN_TYPED) {
    similar.value = []
    return
  }
  const mine = ++newest
  try {
    const found = await Api.fetchSimilarReports(typed)
    if (mine === newest) similar.value = found
  } catch {
    /* The suggestion is a courtesy; a filing form that reported its own
     * failures to search would be worse than one that quietly does not. */
    if (mine === newest) similar.value = []
  }
}

watch([title, body], () => {
  if (pending) clearTimeout(pending)
  pending = setTimeout(lookForSimilar, DEBOUNCE_MS)
})

onBeforeUnmount(() => {
  if (pending) clearTimeout(pending)
})

/* Dismissing is for the whole draft. Somebody who has read these and decided
 * theirs is a different bug should not have to read them again at every
 * keystroke. */
const suggestions = computed(() => (dismissed.value ? [] : similar.value))

const submit = async () => {
  sending.value = true
  failed.value = null
  try {
    const detail = await Api.createReport({
      kind: kind.value,
      title: title.value,
      body: body.value,
      blocking: kind.value === 'bug' ? blocking.value : false,
      gameId: gameId.value || undefined,
    })
    router.push({ name: 'Report', params: { number: detail.report.number } })
  } catch (error) {
    failed.value = error instanceof Error ? error.message : t('reports.new.failed')
    sending.value = false
  }
}
</script>

<template>
  <div class="page-container">
    <div class="new-report page-content">
      <RouterLink class="back" :to="{ name: 'Reports' }">← {{ $t('reports.title') }}</RouterLink>
      <h1>{{ $t('reports.new.title') }}</h1>

      <form class="box" @submit.prevent="submit">
        <fieldset class="kinds">
          <legend>{{ $t('reports.new.kind') }}</legend>
          <label :class="{ on: kind === 'bug' }">
            <input v-model="kind" type="radio" value="bug" />
            <font-awesome-icon icon="bug" />
            <span>
              <strong>{{ $t('reports.kind.bug') }}</strong>
              <em>{{ $t('reports.new.bugHint') }}</em>
            </span>
          </label>
          <label :class="{ on: kind === 'feature' }">
            <input v-model="kind" type="radio" value="feature" />
            <font-awesome-icon icon="lightbulb" />
            <span>
              <strong>{{ $t('reports.kind.feature') }}</strong>
              <em>{{ $t('reports.new.featureHint') }}</em>
            </span>
          </label>
        </fieldset>

        <label class="field">
          <span>{{ $t('reports.new.summary') }}</span>
          <input v-model="title" required maxlength="200" :placeholder="$t('reports.new.summaryPlaceholder')" />
        </label>

        <label class="field">
          <span>{{ kind === 'bug' ? $t('reports.new.whatHappened') : $t('reports.new.whatYouWant') }}</span>
          <textarea
            v-model="body"
            required
            rows="8"
            :placeholder="kind === 'bug' ? $t('reports.new.bugPlaceholder') : $t('reports.new.featurePlaceholder')"
          ></textarea>
        </label>

        <!-- Opened in a new tab: this form is full of their typing, and a
             suggestion that threw it away to show them somebody else's report
             would be a worse outcome than the duplicate it prevented. -->
        <section v-if="suggestions.length > 0" class="similar">
          <header>
            <h2>{{ $t('reports.new.similar') }}</h2>
            <button type="button" class="dismiss" :title="$t('reports.new.similarDismiss')" @click="dismissed = true">
              <font-awesome-icon icon="times" />
            </button>
          </header>
          <p>{{ $t('reports.new.similarHint') }}</p>
          <ul>
            <li v-for="report in suggestions" :key="report.id">
              <ReportRow :report="report" target="_blank" />
            </li>
          </ul>
        </section>

        <label v-if="playable.length > 0" class="field">
          <span>{{ $t('reports.new.game') }}</span>
          <select v-model="gameId">
            <option value="">{{ $t('reports.new.noGame') }}</option>
            <option v-for="entry in playable" :key="entry.id" :value="entry.id">
              {{ entry.name }}
            </option>
          </select>
          <em>{{ $t('reports.new.gameHint') }}</em>
        </label>

        <label v-if="kind === 'bug'" class="check">
          <input v-model="blocking" type="checkbox" />
          {{ $t('reports.new.blocking') }}
        </label>

        <p v-if="failed" class="error">{{ failed }}</p>

        <div class="buttons">
          <button type="submit" :disabled="sending || !title.trim() || !body.trim()">
            {{ sending ? $t('reports.new.filing') : $t('reports.new.submit') }}
          </button>
        </div>
      </form>
    </div>
  </div>
</template>

<style scoped>
.new-report {
  align-content: start;
  color: var(--text);
  width: min(720px, 94vw);
  display: grid;
  gap: 12px;
}

.back {
  color: var(--title);
  text-decoration: none;
}

h1 {
  font-family: teutonic, sans-serif;
  font-size: 1.8em;
  margin: 0;
}

.box {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  display: grid;
  gap: 14px;
  padding: 16px;
}

.kinds {
  border: none;
  display: grid;
  gap: 8px;
  grid-template-columns: 1fr 1fr;
  margin: 0;
  padding: 0;
}

.kinds legend,
.field,
.check {
  color: var(--title);
}

.kinds legend {
  font-family: teutonic, sans-serif;
  letter-spacing: 0.04em;
  margin-bottom: 6px;
  padding: 0;
  text-transform: uppercase;
}

.kinds label {
  align-items: start;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  cursor: pointer;
  display: flex;
  gap: 8px;
  padding: 10px 12px;
}

.kinds label.on {
  border-color: var(--button-1);
}

.kinds input {
  display: none;
}

.kinds svg {
  margin-top: 3px;
  opacity: 0.8;
}

.kinds strong {
  color: var(--text);
  display: block;
}

em {
  color: var(--title);
  font-size: 0.82em;
  font-style: normal;
  opacity: 0.85;
}

.similar {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  display: grid;
  gap: 8px;
  padding: 12px 14px;
}

.similar > header {
  align-items: baseline;
  display: flex;
  gap: 12px;
  justify-content: space-between;
}

.similar h2 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1em;
  letter-spacing: 0.08em;
  margin: 0;
  text-transform: uppercase;
}

.similar p {
  color: var(--title);
  font-size: 0.85em;
  margin: 0;
}

.similar ul {
  display: grid;
  gap: 6px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.dismiss {
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  padding: 0 2px;
}

.dismiss:hover {
  color: var(--text);
}

.field {
  display: grid;
  gap: 5px;
}

.field > span {
  font-family: teutonic, sans-serif;
  letter-spacing: 0.04em;
  text-transform: uppercase;
}

.field input,
.field select,
.field textarea {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--text);
  font-size: 1rem;
  padding: 8px 10px;
}

.field textarea {
  resize: vertical;
}

.check {
  align-items: center;
  display: flex;
  gap: 8px;
}

.buttons {
  display: flex;
  justify-content: flex-end;
}

button {
  background: var(--button-1);
  border: none;
  border-radius: 4px;
  color: var(--text);
  cursor: pointer;
  font-weight: 600;
  padding: 9px 18px;
}

button:disabled {
  cursor: default;
  opacity: 0.5;
}

.error {
  color: var(--survivor);
  margin: 0;
}

@media (max-width: 600px) {
  .kinds {
    grid-template-columns: 1fr;
  }
}
</style>

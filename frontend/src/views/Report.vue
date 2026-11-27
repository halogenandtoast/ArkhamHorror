<script setup lang="ts">
/* One report: what was filed, the game it came from, and the whole
 * conversation about it.
 *
 * The log is replies and column moves in one stream, because that is how the
 * thing actually reads -- "moved to In Progress" between two replies is the
 * answer to the reply above it.
 *
 * Opening the page clears the unread mark, but only for somebody the report is
 * addressed to; anyone else reading it leaves no trace.
 */
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import * as Api from '@/arkham/api'
import { useUserStore } from '@/stores/user'
import { useReportsStore } from '@/stores/reports'
import ReportStatusTag from '@/components/ReportStatusTag.vue'
import ReportText from '@/components/ReportText.vue'

const route = useRoute()
const router = useRouter()
const { t } = useI18n()
const user = useUserStore()
const reports = useReportsStore()

const number = computed(() => Number(route.params.number))
const detail = ref<Api.ReportDetail | null>(null)
const failed = ref(false)
const reply = ref('')
const sending = ref(false)
const replyFailed = ref<string | null>(null)
const working = ref(false)

const isAdmin = computed(() => user.isAdmin)

const load = async () => {
  try {
    const result = await Api.fetchReport(number.value)
    const hadNews = result.report.unread
    detail.value = result
    failed.value = false
    if (hadNews) {
      await Api.markReportSeen(number.value).catch(() => null)
      void reports.refresh()
    }
  } catch {
    failed.value = true
  }
}

watch(
  number,
  () => {
    detail.value = null
    void load()
  },
  { immediate: true },
)

/* Open pages pick up replies and moves without a reload. The same minute the
 * store polls on -- see stores/reports.ts for why it is a poll at all. */
const timer = setInterval(() => void load(), 60_000)
onBeforeUnmount(() => clearInterval(timer))

const send = async () => {
  if (!reply.value.trim()) return
  sending.value = true
  replyFailed.value = null
  try {
    detail.value = await Api.commentOnReport(number.value, reply.value)
    reply.value = ''
    void reports.refresh()
  } catch (error) {
    replyFailed.value = error instanceof Error ? error.message : t('reports.replyFailed')
  } finally {
    sending.value = false
  }
}

const change = async (changes: Parameters<typeof Api.updateReport>[1]) => {
  if (!detail.value) return
  working.value = true
  try {
    await Api.updateReport(detail.value.report.id, changes)
    await load()
  } finally {
    working.value = false
  }
}

/* For spam, and reachable from the report rather than only from the board: the
 * thing worth deleting is usually recognised while reading it. Declining is the
 * answer for anything that was read and turned down, which is why this asks
 * first and says so. The page cannot survive it, so it leaves for the list. */
const remove = async () => {
  if (!detail.value) return
  if (!window.confirm(t('reports.admin.confirmDelete', { number: detail.value.report.number })))
    return
  working.value = true
  try {
    await Api.deleteReport(detail.value.report.id)
    void reports.refresh()
    await router.push({ name: 'Reports' })
  } finally {
    working.value = false
  }
}

const takeDown = async (entry: Api.ReportLogEntry) => {
  if (!window.confirm(t('reports.confirmDeleteReply'))) return
  await Api.deleteReportComment(entry.id)
  await load()
}

const duplicateOf = ref('')

const markDuplicate = async () => {
  const value = Number(duplicateOf.value)
  if (!Number.isFinite(value) || value <= 0) return
  await change({ status: 'duplicate', duplicateOf: value })
  duplicateOf.value = ''
}

const when = (at: string) => new Date(at).toLocaleString()

/* How far the reporter played past what they reported. The export is a
 * snapshot, so a game that has moved on is a fix aimed at a state nobody is in
 * any more -- worth saying out loud rather than leaving to be discovered. */
const stepsSince = computed(() => {
  const game = detail.value?.report.game
  if (!game || game.step === null || game.currentStep === null) return null
  return Math.max(0, game.currentStep - game.step)
})
</script>

<template>
  <div class="page-container">
    <div class="report page-content">
      <RouterLink class="back" :to="{ name: 'Reports' }">← {{ $t('reports.title') }}</RouterLink>

      <p v-if="failed" class="empty">{{ $t('reports.notFound') }}</p>

      <template v-if="detail">
        <header class="masthead">
          <font-awesome-icon
            class="kind"
            :class="detail.report.kind"
            :icon="detail.report.kind === 'feature' ? 'lightbulb' : 'bug'"
            :title="$t(`reports.kind.${detail.report.kind}`)"
          />
          <h1>{{ detail.report.title }}</h1>
          <ReportStatusTag :status="detail.report.status" />
        </header>

        <p class="meta">
          <span class="number">#{{ detail.report.number }}</span>
          <span v-if="!detail.report.author">{{ $t('reports.byNobody') }}</span>
          <span v-else-if="detail.report.authorAccount">{{ $t('reports.by', { who: detail.report.author }) }}</span>
          <span v-else>{{ $t('reports.byGithub', { who: detail.report.author }) }}</span>
          <span>{{ when(detail.report.createdAt) }}</span>
          <span v-if="detail.report.blocking" class="blocking">
            <font-awesome-icon icon="ban" /> {{ $t('reports.blockingLong') }}
          </span>
          <span v-if="detail.report.hidden" class="hidden-flag">{{ $t('reports.hiddenFlag') }}</span>
          <RouterLink
            v-if="detail.report.duplicateOf"
            :to="{ name: 'Report', params: { number: detail.report.duplicateOf } }"
          >{{ $t('reports.duplicateOf', { number: detail.report.duplicateOf }) }}</RouterLink>
        </p>

        <div class="main">
          <article class="body box">
            <ReportText :text="detail.body" />
          </article>

          <!-- No heading over an empty thread: "Conversation / nothing yet" is a
               sentence about nothing, and the reply box below is the invitation. -->
          <section v-if="detail.log.length > 0" class="log">
            <h2>{{ $t('reports.log.title') }}</h2>
            <ol>
              <li v-for="entry in detail.log" :key="entry.id" :class="entry.kind">
                <template v-if="entry.kind === 'status'">
                  <font-awesome-icon icon="arrow-right-arrow-left" />
                  <span class="moved">
                    <i18n-t keypath="reports.log.moved" tag="span" scope="global">
                      <template #who>{{ entry.author ?? $t('reports.log.somebody') }}</template>
                      <template #column>
                        <ReportStatusTag v-if="entry.status" :status="entry.status" small />
                      </template>
                    </i18n-t>
                  </span>
                  <time :datetime="entry.createdAt">{{ when(entry.createdAt) }}</time>
                </template>
                <template v-else>
                  <header>
                    <strong :class="{ dev: entry.admin }">
                      {{ entry.author ?? $t('reports.log.somebody') }}
                      <span v-if="entry.admin" class="badge">{{ $t('reports.log.dev') }}</span>
                      <span v-else-if="entry.author && !entry.authorAccount" class="badge from">
                        {{ $t('reports.log.fromGithub') }}
                      </span>
                    </strong>
                    <time :datetime="entry.createdAt">{{ when(entry.createdAt) }}</time>
                    <button v-if="isAdmin" class="take-down" @click="takeDown(entry)">
                      <font-awesome-icon icon="trash" />
                    </button>
                  </header>
                  <ReportText :text="entry.body ?? ''" />
                </template>
              </li>
            </ol>
          </section>

          <form class="reply box" @submit.prevent="send">
            <label for="reply">{{ $t('reports.reply') }}</label>
            <textarea id="reply" v-model="reply" rows="4" :placeholder="$t('reports.replyPlaceholder')"></textarea>
            <p v-if="replyFailed" class="error">{{ replyFailed }}</p>
            <div class="buttons">
              <button type="submit" :disabled="sending || !reply.trim()">
                {{ sending ? $t('reports.sending') : $t('reports.send') }}
              </button>
            </div>
          </form>
        </div>

        <!-- Everything about the report that is not the conversation: where it
             came from, and what a maintainer does with it. Beside the thread,
             because on a long one they were scrolled past and lost. -->
        <aside class="side">
          <section v-if="detail.report.game" class="game box">
            <p v-if="detail.report.game.name" class="game-name">{{ detail.report.game.name }}</p>
            <p class="game-facts">
              <span v-if="detail.report.game.step !== null">
                {{ $t('reports.game.step') }} {{ detail.report.game.step }}
              </span>
              <span v-if="stepsSince !== null && stepsSince > 0" class="since">
                {{ $t('reports.game.playedOn', { steps: stepsSince }) }}
              </span>
              <span v-if="!detail.report.game.id" class="gone">{{ $t('reports.game.goneBody') }}</span>
            </p>
            <div v-if="detail.report.game.id || detail.report.game.exportUrl" class="links">
              <a
                v-if="detail.report.game.id"
                class="chip"
                :href="`#/games/${detail.report.game.id}`"
              >{{ $t('reports.game.open') }}</a>
              <a
                v-if="detail.report.game.exportUrl"
                class="chip"
                :href="detail.report.game.exportUrl"
                rel="noreferrer"
                target="_blank"
              >{{ $t('reports.game.export') }}</a>
            </div>
          </section>

          <section v-if="isAdmin" class="admin box">
            <h2>{{ $t('reports.admin.title') }}</h2>
            <div class="controls">
              <!-- Customizable select: the options carry the same pill the rest of
                   the tracker labels a status with, rather than its name in plain
                   text. Where `appearance: base-select` is not supported the
                   browser draws its own menu from the options' text, which is
                   that same name -- so the fallback is what this was before. -->
              <label>
                {{ $t('reports.admin.status') }}
                <select
                  class="rich-select"
                  :value="detail.report.status"
                  :disabled="working"
                  @change="change({ status: ($event.target as HTMLSelectElement).value as Api.ReportStatus })"
                >
                  <button type="button"><selectedcontent></selectedcontent></button>
                  <option v-for="status in Api.OPEN_REPORT_STATUSES.concat(['done', 'declined', 'duplicate'])" :key="status" :value="status">
                    <ReportStatusTag :status="status" />
                  </option>
                </select>
              </label>
              <label>
                {{ $t('reports.admin.kind') }}
                <select
                  class="rich-select"
                  :value="detail.report.kind"
                  :disabled="working"
                  @change="change({ kind: ($event.target as HTMLSelectElement).value as Api.ReportKind })"
                >
                  <button type="button"><selectedcontent></selectedcontent></button>
                  <option value="bug">
                    <font-awesome-icon class="kind bug" icon="bug" /> {{ $t('reports.kind.bug') }}
                  </option>
                  <option value="feature">
                    <font-awesome-icon class="kind feature" icon="lightbulb" /> {{ $t('reports.kind.feature') }}
                  </option>
                </select>
              </label>
              <label class="duplicate">
                {{ $t('reports.admin.duplicate') }}
                <span>
                  <input v-model="duplicateOf" type="number" min="1" placeholder="#" />
                  <button :disabled="working || !duplicateOf" @click="markDuplicate">
                    {{ $t('reports.admin.markDuplicate') }}
                  </button>
                </span>
              </label>
              <button :disabled="working" @click="change({ hidden: !detail.report.hidden })">
                {{ detail.report.hidden ? $t('reports.admin.unhide') : $t('reports.admin.hide') }}
              </button>
              <button class="danger" :disabled="working" @click="remove">
                <font-awesome-icon icon="trash" /> {{ $t('reports.admin.delete') }}
              </button>
            </div>
          </section>
        </aside>
      </template>
    </div>
  </div>
</template>

<style scoped>
/* Two columns: the thread on the left, everything about the report on the
   right. Nothing stretches to the height of its row -- a one-line description
   beside a full triage panel was a box of empty space. */
.report {
  align-content: start;
  align-items: start;
  /* The app body is near-black text on a dark page, so every page states its
     own: without this the title renders black on blue. */
  color: var(--text);
  width: min(1100px, 94vw);
  display: grid;
  gap: 12px 18px;
  grid-template-columns: minmax(0, 1fr) 300px;
}

.back,
.masthead,
.meta,
.report > .empty {
  grid-column: 1 / -1;
}

.main {
  align-content: start;
  display: grid;
  gap: 12px;
  grid-column: 1;
  min-width: 0;
}

.side {
  align-content: start;
  display: grid;
  gap: 12px;
  grid-column: 2;
  min-width: 0;
  position: sticky;
  top: 12px;
}

.back {
  color: var(--title);
  text-decoration: none;
}

.masthead {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}

h1 {
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.7em;
  margin: 0;
  min-width: 200px;
}

h2 {
  color: var(--title);
  font-family: teutonic, sans-serif;
  font-size: 1.05em;
  letter-spacing: 0.04em;
  margin: 0 0 8px;
  text-transform: uppercase;
}

.kind.bug {
  color: var(--survivor);
}

.kind.feature {
  color: var(--rogue);
}

.meta {
  color: var(--title);
  display: flex;
  flex-wrap: wrap;
  font-size: 0.85em;
  gap: 4px 14px;
  margin: 0;
}

.meta a {
  color: var(--guardian);
}

.number {
  font-variant-numeric: tabular-nums;
}

.blocking {
  color: var(--survivor);
}

.hidden-flag {
  color: var(--seeker);
}

.box {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  padding: 12px 14px;
}

/* The game this came from. A label column and two underlined links read as a
   form; what it actually is, is a name and somewhere to go. */
.game {
  display: grid;
  gap: 6px;
}

.game-name {
  font-family: teutonic, sans-serif;
  font-size: 1.15em;
  margin: 0;
}

.game-facts {
  color: var(--title);
  display: flex;
  flex-wrap: wrap;
  font-size: 0.8em;
  gap: 2px 10px;
  margin: 0;
}

.game-facts:empty {
  display: none;
}

.since {
  color: var(--seeker);
}

.gone {
  color: var(--survivor);
}

/* One per row, both the full width of the card. Sized to their own labels they
   were two different widths that wrapped at two different points, which is the
   whole of why the card looked unfinished. */
.links {
  display: grid;
  gap: 6px;
  margin-top: 4px;
}

.chip {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  font-size: 0.85em;
  padding: 7px 10px;
  text-align: center;
  text-decoration: none;
}

/* Opening the game is what somebody came here for; the export is for whoever
   is fixing it. */
.chip:first-child {
  border-color: var(--button-1-muted);
  color: var(--text);
}

.chip:hover {
  background: var(--box-background);
  border-color: var(--button-1);
  color: var(--text);
}

.controls {
  display: grid;
  gap: 10px;
}

.controls label {
  color: var(--title);
  display: grid;
  font-size: 0.8em;
  gap: 4px;
}

/* The two kinds, in the colours the tracker gives them everywhere else. The
   rest of this dropdown's chrome is shared -- see styles/select.css. */
.rich-select .kind.bug {
  color: var(--survivor);
}

.rich-select .kind.feature {
  color: var(--rogue);
}

/* One size for everything in this column: the chips above are 0.85em of the
   panel, and a control at the browser's own 1rem next to them read as a
   different piece of furniture. In rem rather than em because the label these
   sit inside is already 0.8em, and a relative size would compound with it. */
.controls select,
.controls input,
.controls button {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--text);
  font-size: 0.85rem;
  padding: 6px 8px;
}

.controls button {
  cursor: pointer;
}

/* The one red control in the panel: everything else here can be undone by
   setting it back, and this cannot. Tinted rather than filled -- it still sits
   among the triage controls, and a solid red button in a 300px column reads as
   the thing the panel is for. */
.controls .danger {
  border-color: color-mix(in srgb, var(--survivor) 40%, var(--box-border));
  color: var(--survivor);
}

.controls .danger:hover:not(:disabled) {
  background: color-mix(in srgb, var(--survivor) 18%, var(--background-dark));
}

.duplicate span {
  display: flex;
  gap: 4px;
}

.duplicate input {
  width: 6em;
}

.log ol {
  display: grid;
  gap: 8px;
  list-style: none;
  margin: 0;
  padding: 0;
}

.log li.comment {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  padding: 10px 12px;
}

.log li.comment header {
  align-items: baseline;
  display: flex;
  gap: 10px;
  margin-bottom: 6px;
}

.log li.comment strong {
  color: var(--title);
}

.log li.comment strong.dev {
  color: var(--text);
}

.badge.from {
  background: none;
  border: 1px solid var(--box-border);
  color: var(--title);
}

.badge {
  background: var(--button-1-muted);
  border-radius: 3px;
  font-size: 0.7em;
  letter-spacing: 0.08em;
  margin-left: 4px;
  padding: 2px 5px;
  text-transform: uppercase;
  vertical-align: middle;
}

.log li.comment time,
.log li.status time {
  color: var(--title);
  font-size: 0.75em;
  opacity: 0.8;
}

.log li.comment header time {
  flex: 1;
}

.take-down {
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  opacity: 0.6;
  padding: 0;
}

.take-down:hover {
  color: var(--survivor);
  opacity: 1;
}

/* A move is a line in the margin, not a card: it is punctuation between the
   replies that mean something. */
.log li.status {
  align-items: center;
  color: var(--title);
  display: flex;
  flex-wrap: wrap;
  font-size: 0.9em;
  gap: 8px;
  padding: 2px 12px;
}

.log li.status svg {
  opacity: 0.5;
}

.moved {
  align-items: center;
  display: inline-flex;
  gap: 6px;
}

.reply {
  display: grid;
  gap: 6px;
}

.reply label {
  color: var(--title);
  font-family: teutonic, sans-serif;
  letter-spacing: 0.04em;
  text-transform: uppercase;
}

.reply textarea {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--text);
  font-family: inherit;
  font-size: 1rem;
  padding: 8px 10px;
  resize: vertical;
}

.reply .buttons {
  display: flex;
  justify-content: flex-end;
}

.reply button {
  background: var(--button-1);
  border: none;
  border-radius: 4px;
  color: var(--text);
  cursor: pointer;
  font-weight: 600;
  padding: 8px 16px;
}

.reply button:disabled {
  cursor: default;
  opacity: 0.5;
}

.error {
  color: var(--survivor);
  margin: 0;
}

.empty {
  color: var(--title);
  margin: 0;
}

@media (max-width: 900px) {
  .report {
    grid-template-columns: minmax(0, 1fr);
  }

  /* Stacked, the side goes above the thread rather than under the whole of it:
     it is where the report came from, and the link somebody came to follow.
     Only the thread is reordered -- ordering the side instead would lift it
     over the title and the back link too. */
  .main {
    order: 1;
  }

  .side {
    grid-column: 1;
    position: static;
  }
}
</style>

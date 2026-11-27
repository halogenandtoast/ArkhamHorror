<script setup lang="ts">
/* The board that replaced the GitHub project.
 *
 * Two boards, not one: a bug and a feature request are different work and the
 * same column means a different thing on each, so they are never mixed. The
 * kind is in the URL, so a board is a link.
 *
 * Columns are the server's `reportStatuses`, in its order, so adding one is a
 * backend change and this page follows.
 *
 * A card moves two ways and both end in the same call: dragged, and through the
 * card's own menu. The menu is not a fallback bolted on afterwards -- it is the
 * only one of the two that works on a phone and from a keyboard.
 *
 * A drop sends the destination column in its new order and the server rewrites
 * the positions from 1, so the same drop twice is the same column.
 */
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { OnClickOutside } from '@vueuse/components'
import * as Api from '@/arkham/api'
import ReportStatusTag from '@/components/ReportStatusTag.vue'

const { t } = useI18n()
const route = useRoute()

const board = ref<Api.ReportBoard | null>(null)
const loading = ref(true)
const failed = ref(false)
const working = ref(false)
const showClosed = ref(false)
const menuFor = ref<string | null>(null)

const kind = computed<Api.ReportKind>(() => (route.params.kind === 'feature' ? 'feature' : 'bug'))

/* The column that "deployed" empties. Named rather than written into the
 * template twice, since the button and what it acts on must agree. */
const DEPLOYING = 'pending-deploy'

const load = async () => {
  try {
    board.value = await Api.fetchReportBoard()
    failed.value = false
  } catch {
    failed.value = true
  } finally {
    loading.value = false
  }
}

onMounted(async () => {
  await load()
  await nextTick()
  measure()
})
watch(kind, () => (menuFor.value = null))

const ofKind = (which: Api.ReportKind) =>
  (board.value?.reports ?? []).filter((report) => report.kind === which)

const counts = computed(() => ({
  bug: ofKind('bug').length,
  feature: ofKind('feature').length,
}))

const statuses = computed(() =>
  showClosed.value ? (board.value?.statuses ?? []) : (board.value?.openStatuses ?? []),
)

// --- the board is wider than the window ------------------------------------

/* Six columns do not fit in a narrow window, and macOS hides a scrollbar until
 * something moves it: without an edge that says so, a board whose first column
 * fills the screen reads as a list with no columns at all. */
const columnsEl = ref<HTMLElement | null>(null)
const canLeft = ref(false)
const canRight = ref(false)

const measure = () => {
  const el = columnsEl.value
  canLeft.value = !!el && el.scrollLeft > 4
  canRight.value = !!el && el.scrollLeft + el.clientWidth < el.scrollWidth - 4
}

const nudge = (direction: 1 | -1) => {
  const el = columnsEl.value
  if (!el) return
  const step = el.querySelector<HTMLElement>('.column')?.offsetWidth ?? 260
  el.scrollBy({ left: direction * (step + 10), behavior: 'smooth' })
}

/* A card cannot be dropped on a column that is off the edge, so holding one
 * over the edge pulls the board along. */
const dragNear = (event: DragEvent) => {
  const el = columnsEl.value
  if (!dragging.value || !el) return
  const box = el.getBoundingClientRect()
  if (event.clientX < box.left + 70) el.scrollBy({ left: -18 })
  else if (event.clientX > box.right - 70) el.scrollBy({ left: 18 })
}

watch([statuses, kind], async () => {
  await nextTick()
  measure()
})

window.addEventListener('resize', measure)
onBeforeUnmount(() => {
  window.removeEventListener('resize', measure)
  endDrag()
})

/* The cards of one column of the board being shown, the one in hand included:
 * it is hidden while dragged, not taken out of the page. A drag whose source
 * node leaves the document is a drag Chrome throws away. */
const column = (status: string) =>
  ofKind(kind.value).filter((report) => report.status === status)

// --- dragging -------------------------------------------------------------

const dragging = ref<Api.Report | null>(null)

/* Where the card would land, named by the card it would land above -- `before:
 * null` is the end of the column. An index would be counted against a list
 * that still holds the hidden card in hand, and so would be off by one from
 * the side the card came from. */
const dropAt = ref<{ status: string; before: string | null } | null>(null)

type Slot = { gap: true } | { gap: false; report: Api.Report }

/* What a column draws: its cards, with a gap opened where the card in hand
 * would land. */
const slots = (status: string): Slot[] => {
  const cards: Slot[] = []
  const at = dragging.value ? dropAt.value : null
  for (const report of column(status)) {
    if (at?.status === status && at.before === report.id) cards.push({ gap: true })
    cards.push({ gap: false, report })
  }
  if (at?.status === status && at.before === null) cards.push({ gap: true })
  return cards
}

const startDrag = (event: DragEvent, report: Api.Report) => {
  menuFor.value = null
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'move'
    // Firefox will not start a drag without data on the transfer.
    event.dataTransfer.setData('text/plain', report.id)
  }
  /* Only once the browser owns the drag. Hiding the card while `dragstart` is
   * still running cancels the drag, which is what a card that shivers under
   * the pointer and stays where it was looks like. */
  takingHold = setTimeout(() => {
    takingHold = null
    dragging.value = report
  }, 0)
}

/* A drag that is over before that timer runs would otherwise leave the card
   hidden with nothing holding it. */
let takingHold: ReturnType<typeof setTimeout> | null = null

const endDrag = () => {
  if (takingHold) clearTimeout(takingHold)
  takingHold = null
  dragging.value = null
  dropAt.value = null
}

/* Which card the pointer is above, measured against each card's middle. The
 * card in hand is hidden but still in the list, and is not one of the answers. */
const overColumn = (event: DragEvent, status: string) => {
  if (!dragging.value) return
  event.preventDefault()
  const list = event.currentTarget as HTMLElement
  const cards = [...list.querySelectorAll<HTMLElement>('[data-card]')].filter(
    (el) => el.dataset.card !== dragging.value?.id,
  )
  let before: string | null = null
  for (const el of cards) {
    const box = el.getBoundingClientRect()
    if (event.clientY < box.top + box.height / 2) {
      before = el.dataset.card ?? null
      break
    }
  }
  dropAt.value = { status, before }
}

/* Leaving one of the column's own cards is not leaving the column: `dragleave`
 * fires on the list every time the pointer crosses into a card inside it, and
 * dropping the gap on each of them is the flicker. */
const leaveColumn = (event: DragEvent, status: string) => {
  const list = event.currentTarget as HTMLElement
  const to = event.relatedTarget as Node | null
  if (to && list.contains(to)) return
  if (dropAt.value?.status === status) dropAt.value = null
}

const drop = async (status: string) => {
  const report = dragging.value
  const target = dropAt.value
  if (!report || !target) {
    endDrag()
    return
  }

  const ids = column(status)
    .filter((other) => other.id !== report.id)
    .map((other) => other.id)
  const at = target.before ? ids.indexOf(target.before) : -1
  ids.splice(at < 0 ? ids.length : at, 0, report.id)
  endDrag()

  /* Moved on screen before the server hears about it: a card that springs back
   * to where it was for half a second reads as a drop that failed. */
  if (board.value) {
    const moved = { ...report, status: status as Api.ReportStatus }
    const rest = board.value.reports.filter((other) => other.id !== report.id)
    const nextId = ids[ids.indexOf(report.id) + 1]
    const at = nextId ? rest.findIndex((other) => other.id === nextId) : -1
    if (at >= 0) rest.splice(at, 0, moved)
    else rest.push(moved)
    board.value = { ...board.value, reports: rest }
  }

  working.value = true
  try {
    await Api.orderReports(status as Api.ReportStatus, ids)
  } finally {
    working.value = false
    await load()
  }
}

// --- the card menu, which is how any of this works on a phone --------------

const move = async (report: Api.Report, status: Api.ReportStatus) => {
  menuFor.value = null
  if (report.status === status) return
  working.value = true
  try {
    await Api.updateReport(report.id, { status })
    await load()
  } finally {
    working.value = false
  }
}

/* A deploy finishes a whole column at once: what went out went out together,
 * and clearing Deploying card by card is the same fact confirmed six times.
 *
 * One call, because the reorder endpoint already moves every id it is given
 * into the status it is given -- so each card writes its status event and the
 * reporters all hear about it, exactly as if each had been dragged across.
 * Only the board on screen: bugs and requests deploy together, but the other
 * board's cards are not in front of you to be counted.
 */
const markAllDeployed = async () => {
  const ids = column(DEPLOYING).map((report) => report.id)
  if (ids.length === 0) return
  if (!window.confirm(t('reports.board.confirmDeployed', ids.length))) return
  working.value = true
  try {
    await Api.orderReports('done', ids)
    await load()
  } finally {
    working.value = false
  }
}

const remove = async (report: Api.Report) => {
  menuFor.value = null
  if (!window.confirm(t('reports.admin.confirmDelete', { number: report.number }))) return
  working.value = true
  try {
    await Api.deleteReport(report.id)
    await load()
  } finally {
    working.value = false
  }
}
</script>

<template>
  <div class="board-page">
    <header class="masthead">
      <nav class="boards">
        <RouterLink
          v-for="tab in (['bug', 'feature'] as Api.ReportKind[])"
          :key="tab"
          class="tab"
          :class="{ on: kind === tab }"
          :to="{ name: 'AdminBoard', params: { kind: tab } }"
        >
          <font-awesome-icon :icon="tab === 'feature' ? 'lightbulb' : 'bug'" />
          {{ $t(`reports.board.${tab}s`) }}
          <span class="tally">{{ counts[tab] }}</span>
        </RouterLink>
      </nav>

      <label class="toggle">
        <input v-model="showClosed" type="checkbox" />
        {{ $t('reports.board.showClosed') }}
      </label>
    </header>

    <p v-if="failed" class="note">{{ $t('reports.loadFailed') }}</p>
    <p v-else-if="loading" class="note">{{ $t('reports.loading') }}</p>

    <div v-if="board" class="tray" :class="{ 'more-left': canLeft, 'more-right': canRight }">
      <button v-if="canLeft" class="edge left" :aria-label="$t('reports.board.scrollLeft')" @click="nudge(-1)">
        <font-awesome-icon icon="chevron-left" />
      </button>
      <button v-if="canRight" class="edge right" :aria-label="$t('reports.board.scrollRight')" @click="nudge(1)">
        <font-awesome-icon icon="chevron-right" />
      </button>

      <div ref="columnsEl" class="columns" :class="{ working }" @scroll="measure" @dragover="dragNear">
        <section
          v-for="status in statuses"
          :key="status"
          class="column"
          :class="{ over: dropAt?.status === status }"
        >
          <header>
            <ReportStatusTag :status="status as Api.ReportStatus" />
            <span class="count">{{ column(status).length }}</span>
            <button
              v-if="status === DEPLOYING && column(status).length > 0"
              class="deployed"
              :disabled="working"
              :title="$t('reports.board.markDeployedLong')"
              @click="markAllDeployed"
            >
              <font-awesome-icon icon="check-double" />
              {{ $t('reports.board.markDeployed') }}
            </button>
          </header>
  
          <ul
            class="cards"
            @dragover="overColumn($event, status)"
            @dragleave="leaveColumn($event, status)"
            @drop.prevent="drop(status)"
          >
            <li
              v-for="slot in slots(status)"
              :key="slot.gap ? 'gap' : slot.report.id"
              :class="{ inhand: !slot.gap && slot.report.id === dragging?.id }"
            >
              <div v-if="slot.gap" class="placeholder"></div>
              <article
                v-else
                :data-card="slot.report.id"
                class="card"
                draggable="true"
                @dragstart="startDrag($event, slot.report)"
                @dragend="endDrag"
              >
                <RouterLink
                  class="face"
                  :to="{ name: 'Report', params: { number: slot.report.number }, query: { board: kind } }"
                >
                  <span class="number">#{{ slot.report.number }}</span>
                  <span class="title">{{ slot.report.title }}</span>
                </RouterLink>
  
                <div class="card-meta">
                  <span v-if="slot.report.author" class="who">{{ slot.report.author }}</span>
                  <span v-if="slot.report.blocking" class="blocking" :title="$t('reports.blockingLong')">
                    <font-awesome-icon icon="ban" />
                  </span>
                  <span v-if="slot.report.game" class="has-game" :title="$t('reports.game.title')">
                    <font-awesome-icon icon="dice-d20" />
                  </span>
                  <span v-if="slot.report.comments > 0" class="replies">
                    <font-awesome-icon icon="comment" /> {{ slot.report.comments }}
                  </span>
                  <OnClickOutside class="menu-wrap" @trigger="menuFor = menuFor === slot.report.id ? null : menuFor">
                    <button
                      class="more"
                      :aria-label="$t('reports.board.cardMenu')"
                      :disabled="working"
                      @click.stop="menuFor = menuFor === slot.report.id ? null : slot.report.id"
                    >
                      <font-awesome-icon icon="ellipsis" />
                    </button>
                    <div v-if="menuFor === slot.report.id" class="menu">
                      <p>{{ $t('reports.board.moveTo') }}</p>
                      <button
                        v-for="other in board.statuses"
                        :key="other"
                        :disabled="other === slot.report.status"
                        @click="move(slot.report, other as Api.ReportStatus)"
                      >
                        <ReportStatusTag :status="other as Api.ReportStatus" small />
                      </button>
                      <button class="danger" @click="remove(slot.report)">
                        <font-awesome-icon icon="trash" /> {{ $t('reports.admin.delete') }}
                      </button>
                    </div>
                  </OnClickOutside>
                </div>
              </article>
            </li>
  
            <li v-if="column(status).length === 0 && dropAt?.status !== status" class="column-empty">
              {{ $t('reports.board.emptyColumn') }}
            </li>
          </ul>
        </section>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* The page is the board: it fills the admin shell's content area exactly, and
   nothing outside a column scrolls. Three nested scrollers -- the page, the row
   of columns and the column -- is what made the wheel land on whichever one
   happened to be under the pointer. */
.board-page {
  display: flex;
  flex: 1;
  flex-direction: column;
  gap: 10px;
  min-height: 0;
}

.masthead {
  align-items: center;
  display: flex;
  flex: none;
  flex-wrap: wrap;
  gap: 10px 16px;
}

.boards {
  display: flex;
  gap: 4px;
  margin-right: auto;
}

.tab {
  align-items: center;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  display: flex;
  gap: 7px;
  padding: 7px 12px;
  text-decoration: none;
}

.tab.on {
  background: var(--button-1-muted);
  border-color: var(--button-1);
  color: var(--text);
}

.tally {
  background: var(--background-dark);
  border-radius: 999px;
  font-size: 0.75em;
  font-variant-numeric: tabular-nums;
  min-width: 1.7em;
  padding: 2px 5px;
  text-align: center;
}

.toggle {
  align-items: center;
  color: var(--title);
  cursor: pointer;
  display: flex;
  font-size: 0.9em;
  gap: 6px;
}

.note {
  color: var(--title);
  flex: none;
  margin: 0;
}

/* Holds the one thing that scrolls sideways, and the edges that say so.
   A container, not a media query: what decides how wide a column should be is
   the room the board has, and the admin rail takes a third of the window. */
.tray {
  container-type: inline-size;
  display: flex;
  flex: 1;
  min-height: 0;
  position: relative;
}

/* A fade at whichever end has more board behind it. It must not take the
   pointer: it lies over the cards of the column under it. */
.tray::before,
.tray::after {
  bottom: 12px;
  content: '';
  opacity: 0;
  pointer-events: none;
  position: absolute;
  top: 0;
  transition: opacity 0.15s;
  width: 40px;
  z-index: 2;
}

.tray::before {
  background: linear-gradient(to right, var(--background), transparent);
  left: 0;
}

.tray::after {
  background: linear-gradient(to left, var(--background), transparent);
  right: 0;
}

.tray.more-left::before,
.tray.more-right::after {
  opacity: 1;
}

/* A mouse has no sideways wheel, so the fade is also a button. */
.edge {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 50%;
  color: var(--text);
  cursor: pointer;
  display: flex;
  height: 30px;
  justify-content: center;
  padding: 0;
  position: absolute;
  top: 50%;
  translate: 0 -50%;
  width: 30px;
  z-index: 3;
}

.edge:hover {
  border-color: var(--button-1);
}

.edge.left {
  left: 2px;
}

.edge.right {
  right: 2px;
}

.columns {
  display: flex;
  flex: 1;
  gap: 10px;
  min-height: 0;
  overflow-x: auto;
  overflow-y: hidden;
  padding-bottom: 6px;
  scrollbar-color: var(--background-mid) transparent;
  scrollbar-width: thin;
}

/* Left to itself macOS hides this until the board is already moving, which is
   exactly when nobody needs telling. */
.columns::-webkit-scrollbar {
  height: 9px;
}

.columns::-webkit-scrollbar-thumb {
  background: var(--background-mid);
  border-radius: 999px;
}

.columns::-webkit-scrollbar-track {
  background: transparent;
}

.columns.working {
  opacity: 0.65;
}

.column {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  display: flex;
  flex: 0 0 290px;
  flex-direction: column;
  min-height: 0;
  padding: 10px;
}

.column.over {
  border-color: var(--button-1);
}

.column > header {
  align-items: center;
  display: flex;
  flex: none;
  gap: 8px;
  padding-bottom: 8px;
}

.count {
  color: var(--title);
  font-size: 0.8em;
  font-variant-numeric: tabular-nums;
  margin-left: auto;
}

/* Quiet until wanted: a column header is a label and a tally, and a third thing
   shouting in it would be read before either. */
.deployed {
  background: none;
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  flex: none;
  font-size: 0.75em;
  padding: 3px 6px;
}

.deployed:hover:not(:disabled) {
  border-color: var(--button-1);
  color: var(--text);
}

/* The only vertical scroller on the page. */
.cards {
  display: flex;
  flex: 1;
  flex-direction: column;
  gap: 8px;
  list-style: none;
  margin: 0;
  min-height: 0;
  overflow-y: auto;
  padding: 0;
  scrollbar-width: thin;
}

/* The card in hand keeps its place in the page so the drag survives, but not
   its place in the column: the gap stands in for it. */
.cards li.inhand {
  display: none;
}

.placeholder {
  background: color-mix(in srgb, var(--button-1) 18%, transparent);
  border: 1px dashed var(--button-1);
  border-radius: 5px;
  height: 56px;
}

.card {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  cursor: grab;
  display: grid;
  gap: 6px;
  padding: 9px 10px;
}

.card:active {
  cursor: grabbing;
}

.card:hover {
  border-color: var(--background-mid);
}

.face {
  color: var(--text);
  display: grid;
  gap: 3px;
  text-decoration: none;
}

.number {
  color: var(--title);
  font-size: 0.75em;
  font-variant-numeric: tabular-nums;
}

.title {
  font-size: 0.92em;
  line-height: 1.35;
  overflow-wrap: anywhere;
}

.face:hover .title {
  text-decoration: underline;
}

.card-meta {
  align-items: center;
  color: var(--title);
  display: flex;
  font-size: 0.75em;
  gap: 8px;
}

.who {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.blocking {
  color: var(--survivor);
}

.has-game {
  opacity: 0.7;
}

.replies {
  flex: none;
}

.menu-wrap {
  margin-left: auto;
  position: relative;
}

.more {
  background: none;
  border: none;
  color: var(--title);
  cursor: pointer;
  line-height: 1;
  opacity: 0.6;
  padding: 2px 4px;
}

.more:hover {
  opacity: 1;
}

.menu {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.45);
  display: grid;
  gap: 2px;
  padding: 6px;
  position: absolute;
  right: 0;
  top: 100%;
  width: max-content;
  z-index: 2;
}

.menu p {
  color: var(--title);
  font-size: 0.9em;
  margin: 0 0 2px;
  opacity: 0.7;
}

.menu button {
  background: none;
  border: none;
  border-radius: 3px;
  color: var(--text);
  cursor: pointer;
  display: flex;
  gap: 6px;
  padding: 4px 6px;
  text-align: left;
}

.menu button:hover:not(:disabled) {
  background: var(--box-background);
}

.menu button:disabled {
  cursor: default;
  opacity: 0.4;
}

.menu .danger {
  border-top: 1px solid var(--box-border);
  color: var(--survivor);
  margin-top: 4px;
  padding-top: 6px;
}

.column-empty {
  color: var(--title);
  font-size: 0.8em;
  opacity: 0.55;
}

/* Never one column wide: a column that fills the board is a list, and a board
   you cannot see the next column of does not read as a board at all. */
@container (max-width: 1300px) {
  .column {
    flex-basis: 240px;
  }
}

@container (max-width: 600px) {
  .column {
    flex-basis: 210px;
    padding: 8px;
  }

  .card {
    padding: 7px 8px;
  }
}
</style>

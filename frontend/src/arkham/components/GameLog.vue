<script lang="ts" setup>
/* The game log panel.
 *
 * Owns the open/closed state for the whole list, so the "live tail" rule has
 * one implementation:
 *
 *   The newest top-level entry has its entire spine open; it closes when the
 *   next entry arrives. A click pins an entry against that rule and survives
 *   new arrivals, so you can study an older test while play continues.
 *
 * The rule is applied to the batch, not per entry: entries arrive one frame
 * per action, so the panel lands with the last event of that action open.
 * See docs/game-log/ for why this beats collapsing everything.
 */
import { computed, nextTick, ref, watch } from 'vue'
import GameLogEntry from '@/arkham/components/GameLogEntry.vue'
import LogGroupBlock from '@/arkham/components/LogGroupBlock.vue'
import { groupLogEntries, type LogEntry } from '@/arkham/types/GameLog'

const props = withDefaults(
  defineProps<{
    entries: readonly LogEntry[]
    /* Whether entries offer "undo back to here". Off by default so a viewer
       that cannot rewind -- the replay viewer -- has to say nothing. */
    canUndo?: boolean
    /* Whether the compose box is shown. Same default and same reason. */
    canChat?: boolean
    /* This client's seat, for entries addressed to one player.

       Worth being precise about what that is: every investigator's hand is
       already in the payload this client decodes, so an OnlyPlayer entry is a
       presentation choice -- "this line is about your hand, not the table's" --
       and not a confidentiality boundary. Treat it as such. */
    playerId?: string | null
  }>(),
  { canUndo: false, canChat: false, playerId: null },
)

const emit = defineEmits<{
  undo: [step: number, label: string]
  say: [text: string]
  loadOlder: [beforeSeq: number]
}>()

/* The compose box. Deliberately plain: a line of text, Enter to send. Anything
   richer belongs in the log entry, not here. */
const draft = ref('')

function send() {
  const text = draft.value.trim()
  if (!text) return
  draft.value = ''
  emit('say', text)
}

/* An entry addressed to one seat is hidden from the others. With no seat at all
   (a spectator) only the entries meant for everyone show. */
const forThisSeat = computed(() =>
  props.entries.filter(
    (e) => e.audience.tag === 'Everyone' || e.audience.contents === props.playerId,
  ),
)

/* Everything loaded, not a fixed window. The server already bounds the opening
   payload to its tail; once a reader has deliberately paged further back, the
   point is to show what they asked for. */
const visible = computed(() => forThisSeat.value)

/* Consecutive entries sharing a group id are drawn as one block. The grouping
   is done here, not on the server: the stored log stays a flat append-only
   list, which is what lets undo delete by step and leave a partially-undone
   block looking right. */
const items = computed(() => groupLogEntries(visible.value))

/* The newest top-level entry that actually has detail to show. An entry with
   no children cannot be the tail -- but it still ends the previous one's turn
   as tail, which is what makes "or nested" in the rule true.

   Indexed into `items`, not `entries`: the list renders items, and a path is
   that item's position. */
const tailPath = computed(() => {
  for (let i = items.value.length - 1; i >= 0; i -= 1) {
    const item = items.value[i]
    if (item.kind === 'entry' && item.entry.children.length > 0) return String(i)
  }
  return null
})

/* Overrides, keyed by the same index path the entries render with. Recorded
   only where the choice differs from the rule, so an entry falls back to the
   rule the moment they agree again -- that is what stops a pin from going
   stale once the entry it pinned becomes the tail anyway. */
const pinnedOpen = ref(new Set<string>())
const pinnedShut = ref(new Set<string>())

function autoOpen(path: string): boolean {
  const top = path.split('.')[0]
  const item = items.value[Number(top)]
  /* A block stays open until something outside it follows -- and chat does not
     count, because someone typing during a test should not fold the test away
     under them.

     NOT "collapse when the summary arrives": entries keep arriving after the
     result -- the clue the investigation discovered lands after the test is
     resolved -- so closing on the summary shut the block while it was still
     being written to, and the result flashed past. */
  if (item && item.kind === 'group') {
    return !items.value
      .slice(Number(top) + 1)
      .some((next) => next.kind === 'group' || next.entry.kind !== 'Chat')
  }
  return tailPath.value !== null && top === tailPath.value
}

function isOpen(path: string): boolean {
  if (pinnedShut.value.has(path)) return false
  if (pinnedOpen.value.has(path)) return true
  return autoOpen(path)
}

function isTail(path: string): boolean {
  return path === tailPath.value
}

function isPinned(path: string): boolean {
  return pinnedOpen.value.has(path) || pinnedShut.value.has(path)
}

function onToggle(path: string, open: boolean) {
  pinnedOpen.value.delete(path)
  pinnedShut.value.delete(path)
  if (open !== autoOpen(path)) {
    ;(open ? pinnedOpen : pinnedShut).value.add(path)
  }
  /* Sets are not deeply reactive about membership through a ref, so nudge them. */
  pinnedOpen.value = new Set(pinnedOpen.value)
  pinnedShut.value = new Set(pinnedShut.value)
}

const scroller = ref<HTMLElement | null>(null)
const pinnedToBottom = ref(true)

/* Only follow the log when the reader is already at the bottom. Yanking them
   back down while they are reading scrollback is the classic chat-log bug. */
/* The oldest structured entry we hold, which is the cursor for the next page
   back. Legacy rows carry no seq and sort before every structured one, so they
   are not reachable this way -- see getGameLogBefore. */
const oldestSeq = computed(() => {
  for (const entry of props.entries) {
    if (entry.seq > 0) return entry.seq
  }
  return null
})

const loadingOlder = ref(false)

function onScroll() {
  const el = scroller.value
  if (!el) return
  pinnedToBottom.value = el.scrollHeight - el.scrollTop - el.clientHeight < 40

  /* Near the top: ask for the page before what we hold. Guarded by a flag
     rather than by scroll position alone, because prepending keeps the reader
     near the top and would otherwise fire again immediately. */
  if (el.scrollTop < 80 && !loadingOlder.value && oldestSeq.value !== null) {
    loadingOlder.value = true
    emit('loadOlder', oldestSeq.value)
  }
}

/* Clear the guard once the prepend has landed, and hold the reader's place:
   without this the view jumps, because the content above them just grew. */
watch(
  () => props.entries.length,
  async (now, before) => {
    if (!loadingOlder.value) return
    const el = scroller.value
    const heightBefore = el?.scrollHeight ?? 0
    await nextTick()
    if (el && now > before) el.scrollTop = el.scrollHeight - heightBefore
    loadingOlder.value = false
  },
)

watch(
  /* Length plus the newest seq, not a deep watch: entries are append-only, so
     these are the only two things that should move the scroll. A legacy-only
     log has no seq to change, which is why length is in here too. */
  () => [props.entries.length, props.entries[props.entries.length - 1]?.seq ?? 0] as const,
  async () => {
    if (!pinnedToBottom.value) return
    await nextTick()
    const el = scroller.value
    if (el) el.scrollTop = el.scrollHeight
  },
  { immediate: true, flush: 'post' },
)
</script>

<template>
  <div class="game-log">
    <div ref="scroller" class="game-log__scroll" @scroll.passive="onScroll">
      <ul class="game-log__list">
        <template v-for="(item, i) in items" :key="item.kind === 'group' ? `g${item.id}` : `e${i}`">
          <GameLogEntry
            v-if="item.kind === 'entry'"
            :entry="item.entry"
            :path="String(i)"
            :depth="0"
            :is-open="isOpen"
            :is-tail="isTail"
            :is-pinned="isPinned"
            :can-undo="canUndo"
            @toggle="onToggle"
            @undo="(step, label) => emit('undo', step, label)"
          />
          <LogGroupBlock
            v-else
            :group="item"
            :path="String(i)"
            :is-open="isOpen"
            :is-tail="isTail"
            :is-pinned="isPinned"
            :can-undo="canUndo"
            @toggle="onToggle"
            @undo="(step, label) => emit('undo', step, label)"
          />
        </template>
      </ul>
    </div>

    <form v-if="canChat" class="game-log__compose" @submit.prevent="send">
      <input
        v-model="draft"
        class="game-log__input"
        type="text"
        maxlength="500"
        :placeholder="$t('log.sayPlaceholder')"
        :aria-label="$t('log.sayPlaceholder')"
      />
      <button class="game-log__send" type="submit" :disabled="draft.trim().length === 0">
        {{ $t('log.say') }}
      </button>
    </form>
  </div>
</template>

<style scoped>
/* No box inside a box. The sidebar is already a panel, and the log used to sit
   in a second rounded, inset card inside it, with a third around every entry.
   The log IS the panel now: it keeps the dark surface (the entries are light
   text, and the sidebar itself is pale in a light theme) but fills its parent
   with no margin, no radius and no second edge. */
.game-log {
  background: var(--neutral-dark);
  width: 100%;
  height: 100%;
  min-height: 0;
  flex: 1 1 auto;
  overflow: hidden;
  display: flex;
  flex-direction: column;
}

.game-log__scroll {
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  overflow-x: hidden;
  /* The padding lives here, not on a wrapper, so a phase banner can cancel it
     and run edge to edge. */
  padding: 10px 12px 14px;
}

.game-log__list {
  list-style: none;
  margin: 0;
  padding: 0;
}

.game-log__compose {
  flex: none;
  display: flex;
  gap: 6px;
  padding: 8px 10px;
  border-top: 1px solid var(--box-border);
  background: rgba(0, 0, 0, 0.25);
}

.game-log__input {
  flex: 1 1 auto;
  min-width: 0;
  padding: 7px 9px;
  font: inherit;
  font-size: 0.8em;
  color: #fff;
  background: rgba(0, 0, 0, 0.35);
  border: 1px solid var(--box-border);
  border-radius: 4px;
}

.game-log__input::placeholder {
  color: #7f8798;
}

.game-log__input:focus-visible {
  outline: 2px solid var(--important);
  outline-offset: -1px;
}

/* Its own styles, because Question.vue puts blanket rules on bare `button`.
   Muted on purpose: it sits under the log all game and should not compete with
   the entries for attention. */
.game-log__send {
  flex: none;
  padding: 7px 12px;
  font: inherit;
  font-size: 0.74em;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: #cfd6e2;
  background: rgba(255, 255, 255, 0.07);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  cursor: pointer;
  transition: background 120ms ease, color 120ms ease;
}

.game-log__send:hover:not(:disabled) {
  color: #fff;
  background: rgba(255, 255, 255, 0.13);
}

.game-log__send:disabled {
  opacity: 0.4;
  cursor: default;
}

@media (prefers-reduced-motion: reduce) {
  .game-log__send {
    transition: none;
  }
}
</style>

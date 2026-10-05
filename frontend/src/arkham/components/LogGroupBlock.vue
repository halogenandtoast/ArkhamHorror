<script lang="ts" setup>
/* A run of log entries that share a group id, drawn as one block.
 *
 * The server sends a flat, append-only list; grouping happens here. A block is
 * three roles: a header that opens and names it, members that accumulate while
 * it runs, and a summary that closes it with the outcome. Collapsed, the block
 * is its summary (or its header, while it is still running) -- which is what
 * makes a finished skill test one scannable band.
 *
 * Any of the three may be missing. An undo deletes entries by step, so a block
 * can quite legitimately lose its summary and keep its header and half its
 * members; that has to render, not crash.
 */
import { computed } from 'vue'
import { ArrowUturnLeftIcon } from '@heroicons/vue/20/solid'
import LogPart from '@/arkham/components/LogPart.vue'
import GameLogEntry from '@/arkham/components/GameLogEntry.vue'
import type { LogEntry, LogItem } from '@/arkham/types/GameLog'

const props = defineProps<{
  group: Extract<LogItem, { kind: 'group' }>
  path: string
  isOpen: (path: string) => boolean
  isTail: (path: string) => boolean
  isPinned: (path: string) => boolean
  canUndo?: boolean
}>()

const emit = defineEmits<{
  toggle: [path: string, open: boolean]
  undo: [step: number, label: string]
}>()

/* The header sits at the top of the block for its whole life, and the summary
   closes it at the foot. While the test is still running there is no summary
   and the header is the only bar; once the result lands they bracket the
   detail between them. */
const header = computed<LogEntry | null>(() => props.group.header)
const band = computed<LogEntry | null>(() => props.group.summary)

const tone = computed(() => band.value?.tone?.toLowerCase() ?? 'neutral')

/* A Test body is the sentence followed by at most one stats part
   (log.testArithmetic). The band draws them as separate stripes so the numbers
   sit apart from the sentence rather than running on from it. */
const bandSentence = computed(() =>
  band.value && band.value.body.length > 1 ? band.value.body.slice(0, -1) : (band.value?.body ?? []),
)

const bandStats = computed(() =>
  band.value && band.value.body.length > 1 ? band.value.body[band.value.body.length - 1] : null,
)

const hasDetail = computed(() => props.group.members.length > 0)
const open = computed(() => hasDetail.value && props.isOpen(props.path))

const undoStep = computed(() =>
  props.canUndo && band.value?.step != null ? band.value.step : null,
)

function toggle() {
  if (hasDetail.value) emit('toggle', props.path, !open.value)
}

function bandLabel(): string {
  return bandSentence.value
    .map((part) => (part.tag === 'LogText' ? part.contents : part.tag === 'LogRefPart' ? part.contents.name : ''))
    .join('')
    .replace(/\s+/g, ' ')
    .trim()
}

function requestUndo() {
  if (undoStep.value !== null) emit('undo', undoStep.value, bandLabel())
}
</script>

<template>
  <li class="log-group" :class="[`log-group--${tone}`, { 'log-entry--tail': isTail(path) }]">
    <component
      :is="hasDetail ? 'button' : 'div'"
      v-if="header"
      class="log-group__header"
      :type="hasDetail ? 'button' : undefined"
      :aria-expanded="hasDetail ? open : undefined"
      @click="toggle"
    >
      <span v-if="hasDetail" class="log-caret" :class="{ 'log-caret--open': open }">&#9654;</span>
      <span v-else class="log-caret-spacer" />
      <span class="log-group__body">
        <LogPart v-for="(part, i) in header.body" :key="i" :part="part" />
      </span>
    </component>

    <ul v-if="open" class="log-group__detail">
      <GameLogEntry
        v-for="(member, i) in group.members"
        :key="i"
        :entry="member"
        :path="`${path}.${i}`"
        :depth="1"
        :is-open="isOpen"
        :is-tail="isTail"
        :is-pinned="isPinned"
        :can-undo="false"
        @toggle="(p, o) => emit('toggle', p, o)"
      />
    </ul>

    <component
      :is="hasDetail ? 'button' : 'div'"
      v-if="band"
      class="log-group__band"
      :type="hasDetail ? 'button' : undefined"
      @click="toggle"
    >
      <span class="log-group__line">
        <span class="log-caret-spacer" />
        <span class="log-group__body">
          <LogPart v-for="(part, i) in bandSentence" :key="i" :part="part" />
        </span>
      </span>
      <!-- A stripe across the foot of the block, not a chip beside the
           sentence: the arithmetic is the thing you check after reading the
           result, so it reads better as its own row. -->
      <span v-if="bandStats" class="log-group__stats"><LogPart :part="bandStats" /></span>
    </component>

    <button
      v-if="undoStep !== null"
      class="log-undo"
      type="button"
      :title="$t('log.undoToHere')"
      :aria-label="$t('log.undoToHere')"
      @click.stop="requestUndo"
    >
      <ArrowUturnLeftIcon aria-hidden="true" />
    </button>
  </li>
</template>

<style scoped>
.log-group {
  --tone: var(--box-border);
  position: relative;
  list-style: none;
  margin: 10px 0;
  padding: 0;
  border: 1px solid rgba(255, 255, 255, 0.09);
  border-left: 3px solid var(--tone);
  border-radius: 3px 6px 6px 3px;
  background: rgba(255, 255, 255, 0.03);
  overflow: hidden;
  color: white;
  font-size: 0.8em;
  line-height: 1.5;
}

.log-group--good { --tone: var(--spooky-green); }
.log-group--bad { --tone: #9f2929; }

/* The block's title bar: full width at the top, there for the whole life of the
   group. It is the only bar while the test is running; once the summary lands
   the two bracket the detail. */
.log-group__header {
  display: flex;
  gap: 7px;
  align-items: baseline;
  width: 100%;
  padding: 7px 10px 7px 9px;
  font: inherit;
  color: inherit;
  text-align: left;
  border: 0;
  border-bottom: 1px solid rgba(255, 255, 255, 0.09);
  border-radius: 0;
  background: rgba(255, 255, 255, 0.05);
}

button.log-group__header {
  cursor: pointer;
}

button.log-group__header:hover {
  background: rgba(255, 255, 255, 0.09);
}

button.log-group__header:focus-visible {
  outline: 2px solid var(--important);
  outline-offset: -2px;
}

.log-group__detail {
  margin: 0;
  padding: 7px 10px 6px 12px;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.log-group__band {
  display: block;
  width: 100%;
  padding: 0;
  border-top: 1px solid rgba(255, 255, 255, 0.09);
  font: inherit;
  color: inherit;
  text-align: left;
  border: 0;
  border-radius: 0;
  background: color-mix(in srgb, var(--tone) 28%, transparent);
}

.log-group__line {
  display: flex;
  gap: 7px;
  align-items: baseline;
  padding: 7px 10px 7px 9px;
}

button.log-group__band {
  cursor: pointer;
}

button.log-group__band:hover {
  background: color-mix(in srgb, var(--tone) 40%, transparent);
}

button.log-group__band:focus-visible {
  outline: 2px solid var(--important);
  outline-offset: -2px;
}

.log-group__body {
  flex: 1 1 auto;
  min-width: 0;
}

/* The foot of the block: full width, its own rule, darker than the band above
   it so the two read as separate stripes rather than one wrapped sentence. */
.log-group__stats {
  display: block;
  padding: 5px 10px 5px 25px;
  border-top: 1px solid rgba(255, 255, 255, 0.1);
  background: rgba(0, 0, 0, 0.24);
  font-size: 0.88em;
  font-variant-numeric: tabular-nums;
  letter-spacing: 0.02em;
  color: #cfd6e2;
}

.log-caret {
  flex: none;
  width: 9px;
  font-size: 0.6em;
  color: var(--spooky-green);
  transition: transform 140ms ease;
}

.log-caret--open {
  transform: rotate(90deg);
}

.log-caret-spacer {
  flex: none;
  width: 9px;
}

.log-undo {
  position: absolute;
  top: 3px;
  right: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  /* padding: 0 explicitly -- base.css gives every button 11px of side padding
     over border-box, which squeezes an icon-only button to nothing. */
  width: 21px;
  height: 21px;
  padding: 0;
  color: #cfd6e2;
  background: rgba(0, 0, 0, 0.55);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  cursor: pointer;
  opacity: 0;
  transition: opacity 120ms ease;
}

.log-undo svg {
  width: 12px;
  height: 12px;
}

.log-group:hover > .log-undo,
.log-undo:focus-visible {
  opacity: 1;
}

.log-undo:hover {
  color: #fff;
  border-color: var(--important);
}

@media (prefers-reduced-motion: reduce) {
  .log-caret,
  .log-undo {
    transition: none;
  }
}
</style>

<script lang="ts" setup>
/* One log entry, with its detail folded underneath.
 *
 * Recursive: an entry holding children renders them as the same component one
 * level down, keyed by index path ("7.0.1"). The path is also the key for the
 * open/closed state, which lives in GameLog.vue -- this component is told
 * whether it is open and reports a click, so the tail rule has exactly one
 * implementation.
 */
import { computed } from 'vue'
import { ArrowUturnLeftIcon } from '@heroicons/vue/20/solid'
import LogPart from '@/arkham/components/LogPart.vue'
import { cardArt } from '@/arkham/cardImages'
import { investigatorClass } from '@/arkham/helpers'
import { logEntrySize, type LogEntry } from '@/arkham/types/GameLog'

const props = defineProps<{
  entry: LogEntry
  path: string
  depth: number
  isOpen: (path: string) => boolean
  isTail: (path: string) => boolean
  isPinned: (path: string) => boolean
  /* Whether this log can rewind the game. False in the replay viewer and for
     a spectator, where the control would be a lie. */
  canUndo?: boolean
}>()

const emit = defineEmits<{
  toggle: [path: string, open: boolean]
  undo: [step: number, label: string]
}>()

/* Phase banners reuse the tints from the phase rail in Scenario.vue, so the log
 * and the board agree on what "mythos" looks like. The phase is read off the
 * entry's i18n key rather than carried as a separate field — "log.phase.mythos"
 * already says it. */
const structureTone = computed(() => {
  const first = props.entry.body[0]
  if (first?.tag !== 'LogI18n') return null
  const key = first.contents[0]
  if (key.startsWith('log.phase.')) return key.slice('log.phase.'.length)
  if (key === 'log.scenarioBegins') return 'scenario'
  if (key === 'log.round') return 'round'
  if (key === 'log.turn') return 'turn'
  return null
})

/* The test band carries no separate PASSED/FAILED chip: the sentence already
   says "passes" or "fails", so one would be both redundant and a second piece
   of English to keep in step with the locale. The tone only colours the band.
   (Kept here and not in the template -- an HTML comment there is emitted into
   the DOM.) */

/* Chat draws itself: a speaker line and the words under it, rather than the
   inline "Name: text" every other entry shape would give. The server still
   builds the body as one sentence so the flat rendering (traces, legacy
   clients) reads correctly; this just takes it apart again. */
/* The speaker's name takes their investigator's class colour. The name itself
   is an account name, so it carries no class -- the investigator rides along as
   the entry's source for exactly this. */
const chatClass = computed(() => {
  const code = props.entry.source?.cardCode
  if (!code) return {}
  return investigatorClass(cardArt(code))
})

const chatSpeaker = computed(() => {
  const first = props.entry.body[0]
  /* Either shape: the speaker is the account name when the API resolved one (a
     LogText) and the investigator chip when it did not. */
  if (props.entry.body.length < 2) return null
  return first?.tag === 'LogRefPart' || first?.tag === 'LogText' ? first : null
})

const chatWords = computed(() => {
  const rest = props.entry.body.slice(chatSpeaker.value ? 1 : 0)
  const [first] = rest
  return first?.tag === 'LogText' && first.contents === ': ' ? rest.slice(1) : rest
})

/* A Test entry's body is the result sentence followed by at most one stats
   part (log.testArithmetic). The band draws them as separate stripes, so the
   numbers sit apart from the sentence instead of running straight on from it.
   The narrator builds it that way on purpose; see renderSkillTestResult. */
const testSentence = computed(() =>
  props.entry.body.length > 1 ? props.entry.body.slice(0, -1) : props.entry.body,
)

const testStats = computed(() =>
  props.entry.body.length > 1 ? props.entry.body[props.entry.body.length - 1] : null,
)

/* An annotation is detail that belongs to the line rather than depth under it:
   a cost surcharge, a note on why a number came out the way it did. One leaf
   notice is not worth a caret, and it must not disappear once the entry stops
   being the tail -- "Daisy Walker moves to Attic / +1 action from Frozen in
   Fear" is meant to read as one log entry, which is the whole point of
   attaching it instead of sending it on its own. */
const annotated = computed(
  () =>
    props.entry.children.length > 0 &&
    props.entry.children.every((c) => c.kind === 'Notice' && c.children.length === 0),
)

const hasKids = computed(() => !annotated.value && props.entry.children.length > 0)
const open = computed(() => (annotated.value ? true : hasKids.value && props.isOpen(props.path)))
const hiddenRows = computed(() =>
  props.entry.children.reduce((acc, c) => acc + logEntrySize(c), 0),
)

function toggle() {
  if (hasKids.value) emit('toggle', props.path, !open.value)
}

/* Only a top-level entry offers an undo. A child is part of the same action as
   its parent and shares its step, so a second control beside it would promise
   a finer rewind than the engine can do.

   Chat never offers one. A typed line survives an undo, which also means the
   server rewrites its step to sit at whatever point was rolled back to -- so
   the step on a chat entry is not a rewind target, it is just where the line
   now sits. */
const undoStep = computed(() =>
  props.canUndo && props.depth === 0 && props.entry.step !== null && props.entry.kind !== 'Chat'
    ? props.entry.step
    : null,
)

/* Flat text for the confirmation, which names the line being undone to. The
   parts are already rendered next to it, so this only needs to be recognisable
   -- i18n parts fall back to their key rather than being resolved twice. */
function entryLabel(entry: LogEntry): string {
  return entry.body
    .map((part) => {
      switch (part.tag) {
        case 'LogText':
          return part.contents
        case 'LogNumber':
          return String(part.contents)
        case 'LogDelta':
          return part.contents >= 0 ? `+${part.contents}` : String(part.contents)
        case 'LogRefPart':
          return part.contents.name
        case 'LogI18n':
          return Object.values(part.contents[1]).map((v) => entryLabel({ ...entry, body: [v] })).join(' ')
        case 'LogList':
          return part.contents.map((v) => entryLabel({ ...entry, body: [v] })).join(', ')
        default:
          return ''
      }
    })
    .join('')
    .replace(/\s+/g, ' ')
    .trim()
}

function requestUndo() {
  if (undoStep.value !== null) emit('undo', undoStep.value, entryLabel(props.entry))
}
</script>

<template>
  <li
    v-if="entry.kind === 'Structure'"
    class="log-structure"
    :class="structureTone ? `log-structure--${structureTone}` : null"
  >
    <LogPart v-for="(part, i) in entry.body" :key="i" :part="part" />
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

  <!-- A skill test is a block, not a line: the detail folds away and what is
       left is the result band, which is the one thing a reader scanning the log
       wants from a test. -->
  <li
    v-else-if="entry.kind === 'Test' && depth === 0"
    class="log-entry log-entry--test"
    :class="[`log-test--${entry.tone?.toLowerCase() ?? 'neutral'}`, { 'log-entry--tail': isTail(path) }]"
  >
    <ul v-if="open" class="log-test__detail">
      <GameLogEntry
        v-for="(child, i) in entry.children"
        :key="i"
        :entry="child"
        :path="`${path}.${i}`"
        :depth="depth + 1"
        :is-open="isOpen"
        :is-tail="isTail"
        :is-pinned="isPinned"
        :can-undo="canUndo"
        @toggle="(p, o) => emit('toggle', p, o)"
        @undo="(st, l) => emit('undo', st, l)"
      />
    </ul>

    <component
      :is="hasKids ? 'button' : 'div'"
      class="log-test__band"
      :type="hasKids ? 'button' : undefined"
      :aria-expanded="hasKids ? open : undefined"
      @click="toggle"
    >
      <span v-if="hasKids" class="log-caret" :class="{ 'log-caret--open': open }">&#9654;</span>
      <span class="log-test__body">
        <LogPart v-for="(part, i) in testSentence" :key="i" :part="part" />
      </span>
      <span v-if="testStats" class="log-test__stats"><LogPart :part="testStats" /></span>
      <span v-if="hasKids && isPinned(path)" class="log-pin" :title="$t('log.pinned')" />
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

  <li v-else-if="entry.kind === 'Chat'" class="log-entry log-entry--chat">
    <div class="log-chat">
      <LogPart v-if="chatSpeaker" :part="chatSpeaker" class="log-chat__who" :class="chatClass" />
      <p class="log-chat__words">
        <LogPart v-for="(part, i) in chatWords" :key="i" :part="part" />
      </p>
    </div>
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

  <li
    v-else
    class="log-entry"
    :class="[
      `log-entry--${entry.kind.toLowerCase()}`,
      { 'log-entry--nested': depth > 0, 'log-entry--tail': isTail(path) },
    ]"
  >
    <component
      :is="hasKids ? 'button' : 'div'"
      class="log-headline"
      :type="hasKids ? 'button' : undefined"
      :aria-expanded="hasKids ? open : undefined"
      @click="toggle"
    >
      <span v-if="hasKids" class="log-caret" :class="{ 'log-caret--open': open }">&#9654;</span>
      <span v-else class="log-caret-spacer" />
      <span class="log-body">
        <LogPart v-for="(part, i) in entry.body" :key="i" :part="part" />
      </span>
      <!-- Says how much is folded, so collapsed never means hidden. -->
      <span v-if="hasKids && !open" class="log-count">+{{ hiddenRows }}</span>
      <span
        v-if="hasKids && isPinned(path)"
        class="log-pin"
        :title="$t('log.pinned')"
      />
    </component>

    <!-- A sibling of the headline, not a child: the headline is itself a
         <button> once the entry has detail, and a button inside a button is
         invalid and unclickable. -->
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

    <ul v-if="open" class="log-children" :class="{ 'log-children--annotation': annotated }">
      <GameLogEntry
        v-for="(child, i) in entry.children"
        :key="i"
        :entry="child"
        :path="`${path}.${i}`"
        :depth="depth + 1"
        :is-open="isOpen"
        :is-tail="isTail"
        :is-pinned="isPinned"
        :can-undo="canUndo"
        @toggle="(p, o) => emit('toggle', p, o)"
        @undo="(st, l) => emit('undo', st, l)"
      />
    </ul>
  </li>
</template>

<style scoped>
/* A banner, not a line: a solid band across the full width of the column, so a
   phase change reads as a break rather than as another entry. Flat on purpose
   -- a gradient fade made it read as a half-drawn row. */
.log-structure {
  --tone: var(--spooky-green);
  position: relative;
  list-style: none;
  /* The negative margins cancel the scroller's padding so the band runs edge to
     edge; the left padding puts the text back in the column. */
  margin: 16px -12px 6px;
  padding: 7px 12px 7px 13px;
  border-left: 3px solid var(--tone);
  background: color-mix(in srgb, var(--tone) 45%, transparent);
  font-size: 0.72em;
  font-weight: 700;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: #fff;
}

.log-structure:first-child {
  margin-top: 0;
}

/* The phase rail's own palette (Scenario.vue). */
.log-structure--mythos { --tone: #7b4b91; }
.log-structure--investigation { --tone: #a87532; }
.log-structure--enemy { --tone: #9f2929; }
.log-structure--upkeep { --tone: #315b70; }
.log-structure--resolution { --tone: var(--spooky-green-dark); }
.log-structure--campaign { --tone: var(--mythos-dark); }

/* The scenario title opens the whole log: the one banner that gets to be big,
   and the only structure line that keeps its own capitalisation. */
.log-structure--scenario {
  --tone: var(--mythos-dark);
  background: color-mix(in srgb, var(--tone) 85%, transparent);
  margin-top: 26px;
  padding: 14px 12px 14px 13px;
  font-size: 1.25em;
  font-weight: 800;
  letter-spacing: 0.06em;
  text-transform: none;
  text-align: center;
}

/* A round opens the biggest break; a turn is a lighter one inside it. */
.log-structure--round {
  --tone: var(--multiclass-dark);
  margin-top: 26px;
  font-size: 0.78em;
}

.log-structure--turn {
  --tone: var(--box-border);
  letter-spacing: 0.1em;
  text-transform: none;
  font-size: 0.76em;
}

/* Flat: no card per line. A hairline between entries carries the separation a
   box used to, without the third level of container. */
.log-entry {
  position: relative;
  list-style: none;
  margin: 0;
  color: white;
  font-size: 0.8em;
  line-height: 1.5;
  border-bottom: 1px solid rgba(255, 255, 255, 0.055);
}

.log-entry:last-child {
  border-bottom: 0;
}

.log-entry--nested {
  margin: 4px 0;
  font-size: 1em;
  font-weight: 400;
  color: #cfd6e2;
  border-bottom: 0;
  border-left: 1px solid var(--box-border);
  padding-left: 9px;
}

.log-entry--tail {
  box-shadow: inset 2px 0 0 var(--spooky-green);
}

/* The test block. The band is the bottom edge of it, and the only thing left
   when the detail is folded away. */
.log-entry--test {
  --tone: var(--box-border);
  margin: 10px 0;
  padding: 0;
  border: 1px solid rgba(255, 255, 255, 0.09);
  border-left: 3px solid var(--tone);
  border-radius: 3px 6px 6px 3px;
  background: rgba(255, 255, 255, 0.03);
  overflow: hidden;
}

.log-test--good { --tone: var(--spooky-green); }
.log-test--bad { --tone: #9f2929; }

.log-test__detail {
  margin: 0;
  padding: 7px 10px 6px 12px;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.log-test__band {
  display: flex;
  gap: 7px;
  align-items: baseline;
  width: 100%;
  padding: 7px 10px 7px 9px;
  font: inherit;
  color: inherit;
  text-align: left;
  border: 0;
  border-radius: 0;
  background: color-mix(in srgb, var(--tone) 28%, transparent);
}

button.log-test__band {
  cursor: pointer;
}

button.log-test__band:hover {
  background: color-mix(in srgb, var(--tone) 40%, transparent);
}

button.log-test__band:focus-visible {
  outline: 2px solid var(--important);
  outline-offset: -2px;
}

.log-test__body {
  flex: 1 1 auto;
  min-width: 0;
}

/* Its own stripe at the end of the band: the arithmetic is what you check, not
   what you read, so it sits apart from the sentence. */
.log-test__stats {
  flex: none;
  align-self: center;
  margin-left: 8px;
  padding: 2px 7px;
  font-size: 0.82em;
  font-variant-numeric: tabular-nums;
  color: #e8ebf1;
  background: rgba(0, 0, 0, 0.3);
  border-radius: 9px;
  white-space: nowrap;
}

/* Not a line of play at all, so it does not look like one: a card of its own,
   lifted off the column, with the speaker above the words. */
.log-entry--chat {
  margin: 12px 0;
  padding: 0;
  border: 0;
}

.log-chat {
  position: relative;
  padding: 8px 11px 9px;
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid rgba(255, 255, 255, 0.11);
  border-left: 3px solid var(--spooky-green);
  border-radius: 4px 7px 7px 4px;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.35);
}

/* The speaker reads as a label, not as the first words of the sentence: a
   player talking should be unmistakable at a glance among the engine's lines. */
.log-chat__who {
  display: inline-block;
  margin-bottom: 4px;
  padding: 1px 7px;
  font-size: 0.74em;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  /* Dark text on a solid class-coloured fill rather than tinted text on a wash:
     the tinted version was too low-contrast to read at this size. Neutral until
     the class is known. */
  color: #12151b;
  background: var(--neutral);
  border-radius: 9px;
  white-space: nowrap;
}

.log-chat__who.guardian { background: var(--guardian); }
.log-chat__who.seeker { background: var(--seeker); }
.log-chat__who.rogue { background: var(--rogue); }
.log-chat__who.mystic { background: var(--mystic); }
.log-chat__who.survivor { background: var(--survivor); }
.log-chat__who.neutral { background: var(--neutral); }

.log-chat__words {
  margin: 0;
  color: #f1f3f7;
  font-size: 1.05em;
  line-height: 1.45;
  /* Keeps the breaks the writer typed, and stops one long word from widening
     the panel. */
  white-space: pre-wrap;
  overflow-wrap: anywhere;
}

.log-entry--narrative {
  font-style: italic;
  color: #d5cdb6;
}

.log-entry--notice {
  color: #9aa4b6;
}

/* A campaign-log write is the one kind of entry that outlives the scenario, so
   it reads as something written down rather than something that happened:
   parchment tone, gold rule, and the words in small caps. */
.log-entry--record {
  margin: 8px 0;
  border-bottom: 0;
  border-left: 3px solid var(--multiclass);
  background: color-mix(in srgb, var(--multiclass) 11%, transparent);
  border-radius: 0 4px 4px 0;
  color: #f0e4c0;
}

.log-entry--record > .log-headline {
  padding-left: 9px;
  font-variant-caps: all-small-caps;
  letter-spacing: 0.05em;
  font-size: 1.1em;
}

.log-entry--problem {
  color: #e39b94;
}

/* A <button> here, not a div with a click handler: it is focusable and
   operable from the keyboard for free. Question.vue's blanket `button` styles
   do not reach scoped styles in this component, but the reset below keeps it
   from inheriting anything global. */
.log-headline {
  display: flex;
  gap: 8px;
  align-items: baseline;
  width: 100%;
  padding: 7px 2px;
  font: inherit;
  color: inherit;
  text-align: left;
  background: none;
  border: 0;
  border-radius: 5px;
}

.log-entry--nested > .log-headline {
  padding: 0;
}

button.log-headline {
  cursor: pointer;
}

button.log-headline:hover {
  background: rgba(255, 255, 255, 0.045);
}

button.log-headline:focus-visible {
  outline: 2px solid var(--important);
  outline-offset: 1px;
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

.log-body {
  flex: 1 1 auto;
  min-width: 0;
}

.log-count {
  flex: none;
  font-size: 0.78em;
  font-weight: 400;
  color: #9aa4b6;
  background: rgba(255, 255, 255, 0.06);
  border-radius: 9px;
  padding: 1px 6px;
}

.log-pin {
  flex: none;
  align-self: center;
  width: 5px;
  height: 5px;
  border-radius: 50%;
  background: var(--important);
}

/* Hidden until the row is hovered or focused: every entry can rewind the game,
   so a column of always-visible undo buttons would be both noisy and easy to
   hit by accident. Kept in the layout (opacity, not display) so it does not
   reflow the row on hover, and always visible to a keyboard user who tabs to
   it. */
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

.log-entry:hover > .log-undo,
.log-structure:hover > .log-undo,
.log-entry--chat:hover > .log-undo,
.log-undo:focus-visible {
  opacity: 1;
}

.log-undo:hover {
  color: #fff;
  border-color: var(--important);
}

/* An annotation hangs off the line above rather than sitting at its own level:
   no gap, tight to the headline, so it reads as a continuation of the entry
   instead of a reply to it. */
.log-children--annotation {
  gap: 0;
  padding: 0 2px 4px 19px;
}

.log-children--annotation :deep(.log-caret-spacer) {
  display: none;
}

.log-children {
  margin: 0;
  padding: 0 2px 8px 19px;
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.log-entry--nested > .log-children {
  padding: 5px 0 0 14px;
}

@media (prefers-reduced-motion: reduce) {
  .log-caret,
  .log-undo {
    transition: none;
  }
}
</style>

<script lang="ts" setup>
/* Debug control over the chaos token draw for the current skill test.
 *
 * Two halves, because the engine decides them at different times:
 *  - the reveal rule (how many tokens come out, how many of them resolve) is a
 *    `ChangeRevealStrategy` modifier read once at `TriggerSkillTest`, so it can
 *    only be set before the reveal step;
 *  - the forced faces are a queue on the bag consumed one per draw, so a
 *    two-token reveal takes two entries.
 *
 * Collapsed by default: this is a tool, not part of the test, so it stays out of
 * the way until asked for. Chrome is deliberately neutral -- magenta is reserved
 * for choices the game is waiting on.
 */
import { computed, ref, watch } from 'vue'
import { useDebug } from '@/arkham/debug'
import { chaosTokenImage, compareTokenFaces, type TokenFace } from '@/arkham/types/ChaosToken'
import type { ChaosBag } from '@/arkham/types/ChaosBag'
import { describeRevealStrategy, type RevealStrategy, type SkillTest } from '@/arkham/types/SkillTest'

const props = defineProps<{
  gameId: string
  skillTest: SkillTest
  chaosBag: ChaosBag
}>()

const debug = useDebug()

/* The engine's live rule, folded from the same modifiers `TriggerSkillTest`
 * reads. The steppers track it until you nudge one, so the panel opens on what
 * the test is actually about to do rather than on a guess. */
const current = computed<RevealStrategy>(() => props.skillTest.revealStrategy ?? { tag: 'Reveal', contents: 1 })
const currentLabel = computed(() => describeRevealStrategy(current.value))

// 0 in `choose` means every revealed token resolves, i.e. a plain `Reveal n`.
const seed = computed(() => {
  const unwrap = (strategy: RevealStrategy): { reveal: number, choose: number } => {
    switch (strategy.tag) {
      case 'Reveal': return { reveal: strategy.contents, choose: 0 }
      case 'RevealAndChoose': return { reveal: strategy.contents[0], choose: strategy.contents[1] }
      // Not expressible in two steppers; the label still shows the whole rule.
      case 'MultiReveal': return unwrap(strategy.contents[0])
    }
  }
  return unwrap(current.value)
})

const edited = ref<{ reveal: number, choose: number } | null>(null)
watch(() => props.skillTest.id, () => { edited.value = null })

const reveal = computed(() => edited.value?.reveal ?? seed.value.reveal)
const choose = computed(() => edited.value?.choose ?? seed.value.choose)

const beforeReveal = computed(() =>
  ['DetermineSkillOfTestStep', 'SkillTestFastWindow1', 'CommitCardsFromHandToSkillTestStep', 'SkillTestFastWindow2']
    .includes(props.skillTest.step)
)

/* Only faces still in the bag -- the engine falls back to a random draw for a
 * face it cannot find, which would look like the queue being ignored. */
const bagFaces = computed(() =>
  [...new Set(props.chaosBag.chaosTokens.map((token) => token.face))].sort(compareTokenFaces)
)

const queue = computed<TokenFace[]>(() => props.chaosBag.forceDraw)

const setQueue = (faces: TokenFace[]) =>
  debug.send(props.gameId, {
    tag: 'ChaosBagMessage',
    contents: { tag: 'DebugSetForcedChaosTokenDraws_', contents: faces },
  })

const step = (which: 'reveal' | 'choose', by: 1 | -1) => {
  const next = { reveal: reveal.value, choose: choose.value }
  next[which] = Math.min(10, Math.max(which === 'reveal' ? 1 : 0, next[which] + by))
  edited.value = next
}

const pendingLabel = computed(() => {
  const n = reveal.value
  const m = choose.value
  return m > 0 && m < n ? `${n} → ${m}` : `${n}`
})

const dirty = computed(() => pendingLabel.value !== currentLabel.value)

const applyRevealRule = () => {
  const n = Math.max(1, Math.floor(reveal.value))
  const m = Math.max(0, Math.floor(choose.value))
  const strategy = m > 0 && m < n
    ? { tag: 'RevealAndChoose', contents: [n, m] }
    : { tag: 'Reveal', contents: n }

  debug.skillTestModifier(
    props.gameId,
    props.skillTest.id,
    { tag: 'SkillTestTarget', contents: props.skillTest.id },
    { tag: 'ChangeRevealStrategy', contents: strategy },
  )
}
</script>

<template>
  <details class="draw-debug">
    <summary class="draw-debug__summary">
      <span class="draw-debug__caret" aria-hidden="true" />
      <span class="draw-debug__title">{{ $t('debug.skillTestDraw.title') }}</span>
      <span class="count-pill draw-debug__badge" v-tooltip="$t('debug.skillTestDraw.currentHint')">
        {{ currentLabel }}
      </span>
      <span v-if="queue.length > 0" class="count-pill">
        {{ $t('debug.skillTestDraw.forcedCount', { count: queue.length }) }}
      </span>
    </summary>

    <div class="draw-debug__body">
      <section class="draw-debug__section">
        <header class="draw-debug__legend">
          <span>{{ $t('debug.skillTestDraw.revealRule') }}</span>
          <span class="draw-debug__legend-value">
            <span :class="{ 'draw-debug__superseded': dirty }">{{ currentLabel }}</span>
            <template v-if="dirty"> &rArr; <span class="draw-debug__pending">{{ pendingLabel }}</span></template>
          </span>
        </header>
        <div class="draw-debug__controls">
          <div class="stepper" v-tooltip="$t('debug.skillTestDraw.revealHint')">
            <span class="stepper__label">{{ $t('debug.skillTestDraw.reveal') }}</span>
            <button class="stepper__button" @click="step('reveal', -1)">&minus;</button>
            <span class="stepper__value">{{ reveal }}</span>
            <button class="stepper__button" @click="step('reveal', 1)">+</button>
          </div>
          <div class="stepper" v-tooltip="$t('debug.skillTestDraw.chooseHint')">
            <span class="stepper__label">{{ $t('debug.skillTestDraw.choose') }}</span>
            <button class="stepper__button" @click="step('choose', -1)">&minus;</button>
            <span class="stepper__value">{{ choose === 0 ? $t('debug.skillTestDraw.all') : choose }}</span>
            <button class="stepper__button" @click="step('choose', 1)">+</button>
          </div>
          <button class="draw-debug__apply" :disabled="!beforeReveal || !dirty" @click="applyRevealRule">
            {{ $t('debug.skillTestDraw.apply') }}
          </button>
        </div>
        <p v-if="!beforeReveal" class="draw-debug__note">{{ $t('debug.skillTestDraw.tooLate') }}</p>
      </section>

      <section class="draw-debug__section">
        <header class="draw-debug__legend">
          <span>{{ $t('debug.skillTestDraw.forced') }}</span>
          <button v-if="queue.length > 0" class="draw-debug__clear" @click="setQueue([])">
            {{ $t('debug.skillTestDraw.clear') }}
          </button>
        </header>

        <ol class="draw-debug__queue">
          <li
            v-for="(face, idx) in queue"
            :key="`${face}${idx}`"
            class="draw-debug__slot draw-debug__slot--filled"
            v-tooltip="$t('debug.skillTestDraw.removeToken')"
            @click="setQueue(queue.filter((_, i) => i !== idx))"
          >
            <img class="draw-debug__token" :src="chaosTokenImage(face)" />
            <span class="draw-debug__order">{{ idx + 1 }}</span>
          </li>
          <li class="draw-debug__slot draw-debug__slot--empty">
            <span>{{ $t('debug.skillTestDraw.empty') }}</span>
          </li>
        </ol>

        <div class="draw-debug__palette">
          <img
            v-for="face in bagFaces"
            :key="face"
            class="draw-debug__token draw-debug__token--pick"
            :src="chaosTokenImage(face)"
            v-tooltip="$t('debug.skillTestDraw.addToken')"
            @click="setQueue([...queue, face])"
          />
        </div>
      </section>
    </div>
  </details>
</template>

<style scoped>
.draw-debug {
  --draw-debug-line: rgba(255, 255, 255, 0.14);
  align-self: stretch;
  margin-top: 4px;
  border: 1px solid var(--draw-debug-line);
  border-radius: 8px;
  background: rgba(0, 0, 0, 0.3);
  font-family: system-ui, -apple-system, "Segoe UI", sans-serif;
  font-size: min(12px, 2vw);
  overflow: hidden;
}

.draw-debug__summary {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 5px 9px;
  cursor: pointer;
  list-style: none;
  user-select: none;
  color: rgba(255, 255, 255, 0.7);
}

/* Safari still paints its own disclosure triangle without this. */
.draw-debug__summary::-webkit-details-marker {
  display: none;
}

.draw-debug__summary:hover {
  color: rgba(255, 255, 255, 0.95);
  background: rgba(255, 255, 255, 0.05);
}

.draw-debug__caret {
  width: 0;
  height: 0;
  border-left: 4px solid currentColor;
  border-top: 3.5px solid transparent;
  border-bottom: 3.5px solid transparent;
  transition: transform 120ms ease;
}

.draw-debug[open] .draw-debug__caret {
  transform: rotate(90deg);
}

.draw-debug__title {
  text-transform: uppercase;
  letter-spacing: 0.07em;
  font-size: 0.85em;
  font-weight: 600;
}

/* Pushes the live-rule chip and the forced-draw count to the right edge. */
.draw-debug__badge {
  margin-left: auto;
}

.draw-debug__body {
  display: flex;
  flex-direction: column;
  border-top: 1px solid var(--draw-debug-line);
}

.draw-debug__section {
  padding: 8px 9px;
}

.draw-debug__section + .draw-debug__section {
  border-top: 1px solid var(--draw-debug-line);
}

.draw-debug__legend {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 6px;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  font-size: 0.75em;
  font-weight: 600;
  color: rgba(255, 255, 255, 0.45);
}

.draw-debug__legend-value {
  font-variant-numeric: tabular-nums;
  letter-spacing: 0;
  color: rgba(255, 255, 255, 0.8);
}

/* The live rule stays visible while an unapplied edit sits beside it. */
.draw-debug__superseded {
  text-decoration: line-through;
  opacity: 0.5;
}

.draw-debug__pending {
  color: var(--important);
}

.draw-debug__legend > :last-child:not(:first-child) {
  margin-left: auto;
}

.draw-debug__controls {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 6px;
}

/* A segmented -/value/+ control rather than a number input: the native spinners
   are tiny at this size and the arrow keys steal the game's key handling. */
.stepper {
  display: inline-flex;
  align-items: center;
  border: 1px solid var(--draw-debug-line);
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.35);
  overflow: hidden;
}

.stepper__label {
  padding: 0 7px;
  font-size: 0.8em;
  color: rgba(255, 255, 255, 0.5);
}

.stepper__value {
  min-width: 2.4em;
  text-align: center;
  font-variant-numeric: tabular-nums;
  font-weight: 600;
  color: rgba(255, 255, 255, 0.9);
}

.stepper__button {
  padding: 0 7px;
  border: 0;
  border-radius: 0;
  background: rgba(255, 255, 255, 0.06);
  line-height: 1.6;
}

.stepper__button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.16);
}

.draw-debug__apply {
  margin-left: auto;
}

.draw-debug__clear {
  padding: 0 7px;
  font-size: 0.9em;
}

.draw-debug__note {
  margin: 6px 0 0;
  font-size: 0.8em;
  color: rgba(255, 255, 255, 0.45);
}

.draw-debug__queue {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 5px;
  margin: 0 0 8px;
  padding: 0;
  list-style: none;
}

.draw-debug__slot {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  width: 30px;
  height: 30px;
  border-radius: 50%;
}

.draw-debug__slot--filled {
  cursor: pointer;
  box-shadow: 0 0 0 1px rgba(255, 255, 255, 0.35);
}

.draw-debug__slot--filled:hover {
  box-shadow: 0 0 0 1px var(--delete);
}

/* The trailing dashed circle is the "next one goes here" affordance, and doubles
   as the empty state once it carries the label. */
.draw-debug__slot--empty {
  width: auto;
  min-width: 30px;
  padding: 0 9px;
  border: 1px dashed var(--draw-debug-line);
  border-radius: 999px;
  font-size: 0.78em;
  color: rgba(255, 255, 255, 0.35);
}

.draw-debug__queue > .draw-debug__slot--filled ~ .draw-debug__slot--empty span {
  display: none;
}

.draw-debug__token {
  width: 30px;
  height: 30px;
}

.draw-debug__order {
  position: absolute;
  right: -2px;
  bottom: -2px;
  min-width: 13px;
  height: 13px;
  border-radius: 999px;
  background: rgba(10, 12, 16, 0.95);
  box-shadow: 0 0 0 1px rgba(255, 255, 255, 0.3);
  font-size: 9px;
  font-weight: 700;
  line-height: 13px;
  text-align: center;
  color: rgba(255, 255, 255, 0.85);
}

.draw-debug__palette {
  display: flex;
  flex-wrap: wrap;
  gap: 3px;
  padding-top: 7px;
  border-top: 1px dashed var(--draw-debug-line);
}

.draw-debug__token--pick {
  width: 26px;
  height: 26px;
  cursor: pointer;
  opacity: 0.65;
  transition: opacity 90ms ease, transform 90ms ease;
}

.draw-debug__token--pick:hover {
  opacity: 1;
  transform: translateY(-2px);
}
</style>

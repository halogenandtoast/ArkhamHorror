<script lang="ts" setup>
/* Two modes: "Reveal only" draws real tokens out of the bag without revealing
 * them to the game, so nothing triggers and the calculator beside them changes
 * nothing; "Skill test" starts a real test with no card behind it. Shown
 * read-only outside debug mode while tokens are out, so a token missing from
 * the bag is never a mystery. */
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDebug } from '@/arkham/debug'
import type { Game } from '@/arkham/types/Game'
import type { ChaosBag, DebugRevealedChaosToken } from '@/arkham/types/ChaosBag'
import { chaosTokenImage } from '@/arkham/types/ChaosToken'
import { asCardCode, cardId } from '@/arkham/types/Card'
import { cardName } from '@/arkham/cardDetails'
import { useCardStore } from '@/stores/cards'
import { beginSealDrag, endCardDrag } from '@/arkham/debugCardDrop'
import {
  beginSkillTestMessage,
  formProblem,
  returnMessage,
  revealMessage,
  sandboxResult,
  skillTypes,
  targetKindFor,
  withOverride,
  type DebugSkillTestForm,
  type DebugTestAction,
  type SkillType,
} from '@/arkham/debugSandbox'

const props = defineProps<{
  game: Game
  chaosBag: ChaosBag
  playerId: string
}>()

const debug = useDebug()
const { t } = useI18n()
const cards = useCardStore()

const mode = ref<'reveal' | 'test'>('reveal')

function nameOf(code: string): string {
  const found = cards.cards.find((card) => card.cardCode === code)
    ?? cards.cards.find((card) => card.cardCode === code.replace(/^c/, ''))
  return found ? cardName(found) : code
}

const investigators = computed(() =>
  Object.values(props.game.investigators)
    .filter((investigator) => !investigator.eliminated)
    .map((investigator) => ({ id: investigator.id, name: investigator.name.title }))
)

const ownInvestigator = () =>
  Object.values(props.game.investigators).find((investigator) => investigator.playerId === props.playerId)?.id
    ?? props.game.activeInvestigatorId

const investigatorId = ref(ownInvestigator())

/* ---- Reveal only ---------------------------------------------------------- */

const sandbox = computed<DebugRevealedChaosToken[]>(() => props.chaosBag.debugRevealedChaosTokens)
const revealCount = ref(1)

// A finished draw leaves its `Resolved` step behind until the bag resets; only
// a draw still being decided blocks the sandbox.
const drawInProgress = computed(() => props.chaosBag.choice !== null && props.chaosBag.choice.tag !== 'Resolved')
const bagEmpty = computed(() => props.chaosBag.chaosTokens.length === 0)
const revealBlocker = computed(() => {
  if (drawInProgress.value) return t('debug.sandbox.drawInProgress')
  if (bagEmpty.value) return t('debug.sandbox.emptyBag')
  return null
})

const reveal = (count: number) => debug.send(props.game.id, revealMessage(investigatorId.value, count))
const returnTokens = () => debug.send(props.game.id, returnMessage())

const stepCount = (by: 1 | -1) => {
  revealCount.value = Math.min(10, Math.max(1, revealCount.value + by))
}

/* Calculator: local only. Overrides are keyed by token id so a token keeps its
 * typed value while more are revealed beside it. */
const skill = ref(0)
const difficulty = ref(0)
const failTies = ref(false)
const overrides = ref<Record<string, number | undefined>>({})

watch(() => sandbox.value.length === 0, (empty) => {
  if (empty) overrides.value = {}
})

const setOverride = (tokenId: string, event: Event) => {
  const raw = (event.target as HTMLInputElement).value
  overrides.value = { ...overrides.value, [tokenId]: raw === '' ? undefined : Number(raw) }
}

const effectiveTokens = computed(() =>
  sandbox.value.map((entry) =>
    withOverride(
      { face: entry.token.face, value: entry.value, autoFail: entry.autoFail, autoSuccess: entry.autoSuccess },
      overrides.value[entry.token.id],
    )
  )
)

const result = computed(() => sandboxResult(effectiveTokens.value, skill.value, difficulty.value, failTies.value))

const resultLabel = computed(() => {
  const r = result.value
  switch (r.kind) {
    case 'autoFail': return t('debug.sandbox.autoFail')
    case 'autoSuccess': return t('debug.sandbox.autoSuccess')
    case 'unknown': return t('debug.sandbox.unknown')
    case 'pass': return t('debug.sandbox.pass', { by: r.by, total: r.total, difficulty: difficulty.value })
    case 'fail': return t('debug.sandbox.fail', { by: r.by, total: r.total, difficulty: difficulty.value })
  }
  return ''
})

function valueLabel(entry: DebugRevealedChaosToken) {
  if (entry.autoFail) return t('gameBar.chaosBagStats.autoFail')
  if (entry.autoSuccess) return t('gameBar.chaosBagStats.autoSuccess')
  if (entry.value === null) return '?'
  return entry.value > 0 ? `+${entry.value}` : `${entry.value}`
}

/* ---- Skill test ----------------------------------------------------------- */

// One skill picked is a plain test, more than one is combined; the fixed-value
// chip excludes the skills.
const testSkills = ref<SkillType[]>(['SkillWillpower'])
const testFixed = ref(false)
const fixedValue = ref(0)
const action = ref<DebugTestAction | ''>('')
const targetId = ref('')
const sourceCard = ref('')
const difficultyKind = ref<'fixed' | 'target'>('fixed')
const testDifficulty = ref(0)

const actions: DebugTestAction[] = ['Fight', 'Evade', 'Investigate', 'Parley']

const toggleSkill = (skillType: SkillType) => {
  testFixed.value = false
  testSkills.value = testSkills.value.includes(skillType)
    ? testSkills.value.filter((s) => s !== skillType)
    : skillTypes.filter((s) => s === skillType || testSkills.value.includes(s))
}

const toggleFixed = () => {
  testFixed.value = !testFixed.value
  if (testFixed.value) testSkills.value = []
}

const targetKind = computed(() => {
  if (action.value === 'Parley') return 'EnemyTarget'
  return targetKindFor(action.value || null)
})

const targets = computed(() => {
  if (targetKind.value === 'EnemyTarget') {
    return Object.values(props.game.enemies).map((enemy) => ({
      id: enemy.id,
      name: nameOf(enemy.cardCode),
      hasValue: action.value === 'Fight' ? enemy.fight !== null : action.value === 'Evade' ? enemy.evade !== null : false,
    }))
  }
  if (targetKind.value === 'LocationTarget') {
    return Object.values(props.game.locations).map((location) => ({
      id: location.id,
      name: nameOf(location.cardCode),
      hasValue: location.shroud !== null,
    }))
  }
  return []
})

const selectedTarget = computed(() => targets.value.find((target) => target.id === targetId.value) ?? null)
const canUseTargetValue = computed(() => selectedTarget.value?.hasValue ?? false)

watch(action, () => { targetId.value = '' })
watch(canUseTargetValue, (can) => { if (!can) difficultyKind.value = 'fixed' })

type SourceOption = { id: string, name: string }

const sourceGroups = computed(() => {
  const fromEntities = (entities: Record<string, { cardId: string, cardCode: string }>): SourceOption[] =>
    Object.values(entities).map((entity) => ({ id: entity.cardId, name: nameOf(entity.cardCode) }))
  const hand = props.game.investigators[investigatorId.value]?.hand ?? []
  return [
    { key: 'hand', options: hand.map((card) => ({ id: cardId(card), name: nameOf(asCardCode(card)) })) },
    { key: 'assets', options: fromEntities(props.game.assets) },
    { key: 'events', options: fromEntities(props.game.events) },
    { key: 'treacheries', options: fromEntities(props.game.treacheries) },
    { key: 'enemies', options: fromEntities(props.game.enemies) },
    { key: 'locations', options: fromEntities(props.game.locations) },
  ]
    .map((group) => ({ ...group, options: [...group.options].sort((a, b) => a.name.localeCompare(b.name)) }))
    .filter((group) => group.options.length > 0)
})

const form = computed<DebugSkillTestForm>(() => ({
  investigator: investigatorId.value,
  skill: testFixed.value
    ? { kind: 'fixed', value: fixedValue.value }
    : testSkills.value.length === 1
      ? { kind: 'one', skill: testSkills.value[0] }
      : { kind: 'combined', skills: testSkills.value },
  action: action.value || null,
  target: targetKind.value && targetId.value ? { tag: targetKind.value, contents: targetId.value } : null,
  sourceCard: sourceCard.value || null,
  difficulty: difficultyKind.value === 'target' ? { kind: 'target' } : { kind: 'fixed', value: testDifficulty.value },
}))

const testActive = computed(() => props.game.skillTest !== null)
const canStart = computed(() => debug.active && !testActive.value && formProblem(form.value) === null)

const startTest = () => {
  if (!canStart.value) return
  debug.send(props.game.id, beginSkillTestMessage(form.value))
}
</script>

<template>
  <details class="sandbox" :open="sandbox.length > 0 || undefined">
    <summary class="sandbox__summary" v-tooltip="$t('debug.sandbox.help')">
      <span class="sandbox__caret" aria-hidden="true" />
      <span class="sandbox__title">{{ $t('debug.sandbox.title') }}</span>
      <span v-if="sandbox.length > 0" class="count-pill sandbox__badge">
        {{ $t('debug.sandbox.out', { count: sandbox.length }) }}
      </span>
    </summary>

    <div class="sandbox__body">
      <div v-if="debug.active" class="sandbox__row">
        <div class="segmented" role="tablist">
          <button
            type="button"
            :class="{ 'segmented--on': mode === 'reveal' }"
            @click="mode = 'reveal'"
          >{{ $t('debug.sandbox.modeReveal') }}</button>
          <button
            type="button"
            :class="{ 'segmented--on': mode === 'test' }"
            @click="mode = 'test'"
          >{{ $t('debug.sandbox.modeTest') }}</button>
        </div>
        <label class="sandbox__field">
          <span>{{ $t('debug.sandbox.investigator') }}</span>
          <select v-model="investigatorId">
            <option v-for="investigator in investigators" :key="investigator.id" :value="investigator.id">
              {{ investigator.name }}
            </option>
          </select>
        </label>
      </div>

      <template v-if="mode === 'reveal' || !debug.active">
        <section v-if="debug.active" class="sandbox__section sandbox__row">
          <div class="stepper">
            <button class="stepper__button" @click="stepCount(-1)">&minus;</button>
            <span class="stepper__value">{{ revealCount }}</span>
            <button class="stepper__button" @click="stepCount(1)">+</button>
          </div>
          <span v-tooltip="revealBlocker ?? false">
            <button class="sandbox__action" :disabled="revealBlocker !== null" @click="reveal(revealCount)">
              {{ $t('debug.sandbox.reveal') }}
            </button>
          </span>
          <span v-if="sandbox.length > 0" v-tooltip="revealBlocker ?? false">
            <button class="sandbox__action" :disabled="revealBlocker !== null" @click="reveal(1)">
              {{ $t('debug.sandbox.revealAnother') }}
            </button>
          </span>
          <button v-if="sandbox.length > 0" class="sandbox__action" @click="returnTokens">
            {{ $t('debug.sandbox.return') }}
          </button>
        </section>

        <section class="sandbox__section">
          <p v-if="sandbox.length === 0" class="sandbox__note">{{ $t('debug.sandbox.noTokens') }}</p>
          <ul v-else class="sandbox__tokens">
            <li
              v-for="entry in sandbox"
              :key="entry.token.id"
              class="sandbox__token"
            >
              <img
                :src="chaosTokenImage(entry.token.face)"
                :draggable="debug.active"
                v-tooltip="debug.active ? $t('debug.sandbox.dragToSeal') : false"
                @dragstart="beginSealDrag(entry.token)"
                @dragend="endCardDrag"
              />
              <span
                class="count-pill"
                v-tooltip="entry.value === null && !entry.autoFail && !entry.autoSuccess ? $t('debug.sandbox.unknownValue') : false"
              >{{ valueLabel(entry) }}</span>
              <input
                v-if="debug.active"
                class="sandbox__override"
                type="number"
                :placeholder="$t('debug.sandbox.override')"
                :value="overrides[entry.token.id] ?? ''"
                v-tooltip="$t('debug.sandbox.overrideHint')"
                @input="setOverride(entry.token.id, $event)"
              />
            </li>
          </ul>
        </section>

        <section v-if="debug.active && sandbox.length > 0" class="sandbox__section">
          <header class="sandbox__legend">{{ $t('debug.sandbox.calculator') }}</header>
          <div class="sandbox__row">
            <label class="sandbox__field">
              <span>{{ $t('debug.sandbox.skill') }}</span>
              <input v-model.number="skill" type="number" min="0" />
            </label>
            <label class="sandbox__field">
              <span>{{ $t('debug.sandbox.difficulty') }}</span>
              <input v-model.number="difficulty" type="number" min="0" />
            </label>
            <label class="sandbox__field sandbox__field--inline">
              <input v-model="failTies" type="checkbox" />
              <span>{{ $t('debug.sandbox.failTies') }}</span>
            </label>
          </div>
          <p class="sandbox__result" :class="`sandbox__result--${result.kind}`">{{ resultLabel }}</p>
        </section>
      </template>

      <template v-else>
        <p class="sandbox__note">{{ $t('debug.sandbox.testHelp') }}</p>
        <section class="sandbox__section sandbox__grid">
          <!-- A row of its own, with the fixed value's box inline, so toggling
               the chips never moves the fields after it. -->
          <div class="sandbox__field sandbox__field--wide">
            <span>{{ $t('debug.sandbox.testOf') }}</span>
            <div class="sandbox__chips">
              <button
                v-for="skillType in skillTypes"
                :key="skillType"
                type="button"
                class="sandbox__chip"
                :class="{ 'sandbox__chip--on': testSkills.includes(skillType) }"
                @click="toggleSkill(skillType)"
              >{{ $t(`debug.sandbox.skills.${skillType}`) }}</button>
              <button
                type="button"
                class="sandbox__chip"
                :class="{ 'sandbox__chip--on': testFixed }"
                @click="toggleFixed"
              >{{ $t('debug.sandbox.fixed') }}</button>
              <input
                v-if="testFixed"
                v-model.number="fixedValue"
                type="number"
                min="0"
                :aria-label="$t('debug.sandbox.fixed')"
              />
            </div>
          </div>

          <label class="sandbox__field">
            <span>{{ $t('debug.sandbox.action') }}</span>
            <select v-model="action">
              <option value="">{{ $t('debug.sandbox.noAction') }}</option>
              <option v-for="a in actions" :key="a" :value="a">{{ $t(`debug.sandbox.actions.${a}`) }}</option>
            </select>
          </label>
          <label v-if="targetKind" class="sandbox__field">
            <span>{{ $t('debug.sandbox.target') }}</span>
            <select v-model="targetId">
              <option value="">{{ $t('debug.sandbox.noTarget') }}</option>
              <option v-for="target in targets" :key="target.id" :value="target.id">{{ target.name }}</option>
            </select>
          </label>

          <label class="sandbox__field" v-tooltip="$t('debug.sandbox.sourceHint')">
            <span>{{ $t('debug.sandbox.source') }}</span>
            <select v-model="sourceCard">
              <option value="">{{ $t('debug.sandbox.noSource') }}</option>
              <optgroup v-for="group in sourceGroups" :key="group.key" :label="$t(`debug.sandbox.sourceGroups.${group.key}`)">
                <option v-for="option in group.options" :key="option.id" :value="option.id">{{ option.name }}</option>
              </optgroup>
            </select>
          </label>

          <div class="sandbox__field">
            <span>{{ $t('debug.sandbox.difficulty') }}</span>
            <div class="sandbox__row">
              <input v-model.number="testDifficulty" type="number" min="0" :disabled="difficultyKind === 'target'" />
              <label
                v-if="targetKind"
                class="sandbox__field--inline"
                v-tooltip="selectedTarget && !canUseTargetValue ? $t('debug.sandbox.noTargetValue') : false"
              >
                <input
                  type="checkbox"
                  :checked="difficultyKind === 'target'"
                  :disabled="!canUseTargetValue"
                  @change="difficultyKind = ($event.target as HTMLInputElement).checked ? 'target' : 'fixed'"
                />
                <span>{{ $t('debug.sandbox.useTargetValue') }}</span>
              </label>
            </div>
          </div>
        </section>
        <section class="sandbox__section sandbox__row">
          <span v-tooltip="testActive ? $t('debug.sandbox.testActive') : false">
            <button class="sandbox__action sandbox__action--primary" :disabled="!canStart" @click="startTest">
              {{ $t('debug.sandbox.start') }}
            </button>
          </span>
        </section>
      </template>
    </div>
  </details>
</template>

<style scoped>
.sandbox {
  --sandbox-line: rgba(255, 255, 255, 0.14);
  margin-top: 8px;
  border: 1px solid var(--sandbox-line);
  border-radius: 8px;
  background: rgba(0, 0, 0, 0.3);
  font-family: system-ui, -apple-system, "Segoe UI", sans-serif;
  font-size: min(12px, 2.6vw);
  color: rgba(255, 255, 255, 0.85);
  overflow: hidden;
}

.sandbox__summary {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 5px 9px;
  cursor: pointer;
  list-style: none;
  user-select: none;
  color: rgba(255, 255, 255, 0.7);
}

.sandbox__summary::-webkit-details-marker {
  display: none;
}

.sandbox__summary:hover {
  color: rgba(255, 255, 255, 0.95);
  background: rgba(255, 255, 255, 0.05);
}

.sandbox__caret {
  width: 0;
  height: 0;
  border-left: 4px solid currentColor;
  border-top: 3.5px solid transparent;
  border-bottom: 3.5px solid transparent;
  transition: transform 120ms ease;
}

.sandbox[open] .sandbox__caret {
  transform: rotate(90deg);
}

.sandbox__title {
  text-transform: uppercase;
  letter-spacing: 0.07em;
  font-size: 0.85em;
  font-weight: 600;
}

.sandbox__badge {
  margin-left: auto;
}

.sandbox__body {
  display: flex;
  flex-direction: column;
  gap: 2px;
  padding: 8px 9px;
  border-top: 1px solid var(--sandbox-line);
}

.sandbox__section {
  padding: 6px 0;
}

.sandbox__section + .sandbox__section {
  border-top: 1px solid var(--sandbox-line);
}

.sandbox__row {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 6px;
}

.sandbox__grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
  gap: 8px;
}

.sandbox__legend {
  margin-bottom: 6px;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  font-size: 0.75em;
  font-weight: 600;
  color: rgba(255, 255, 255, 0.45);
}

.sandbox__note {
  margin: 0;
  color: rgba(255, 255, 255, 0.55);
  line-height: 1.4;
}

.sandbox__field {
  display: flex;
  flex-direction: column;
  gap: 2px;
  min-width: 0;
}

.sandbox__field > span:first-child {
  font-size: 0.8em;
  color: rgba(255, 255, 255, 0.5);
}

.sandbox__field--wide {
  grid-column: 1 / -1;
}

.sandbox__field--inline {
  flex-direction: row;
  align-items: center;
  gap: 4px;
}

.sandbox input[type="number"],
.sandbox select {
  min-width: 0;
  max-width: 100%;
  padding: 2px 4px;
  border: 1px solid var(--sandbox-line);
  border-radius: 4px;
  background: rgba(0, 0, 0, 0.4);
  color: inherit;
  font: inherit;
}

.sandbox input[type="number"] {
  width: 5em;
}

.sandbox__action {
  padding: 2px 10px;
  border: 1px solid var(--sandbox-line);
  border-radius: 999px;
  background: rgba(255, 255, 255, 0.08);
  color: inherit;
  font: inherit;
  cursor: pointer;
}

.sandbox__action:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.16);
}

.sandbox__action:disabled {
  opacity: 0.45;
  cursor: not-allowed;
}

.sandbox__action--primary {
  border-color: var(--important, #c9a227);
}

.segmented {
  display: inline-flex;
  border: 1px solid var(--sandbox-line);
  border-radius: 999px;
  overflow: hidden;
}

.segmented button {
  padding: 2px 10px;
  border: 0;
  background: transparent;
  color: rgba(255, 255, 255, 0.6);
  font: inherit;
  cursor: pointer;
}

.segmented button.segmented--on {
  background: rgba(255, 255, 255, 0.16);
  color: rgba(255, 255, 255, 0.95);
}

.stepper {
  display: inline-flex;
  align-items: center;
  border: 1px solid var(--sandbox-line);
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.35);
  overflow: hidden;
}

.stepper__value {
  min-width: 2em;
  text-align: center;
  font-variant-numeric: tabular-nums;
  font-weight: 600;
}

.stepper__button {
  padding: 0 7px;
  border: 0;
  background: rgba(255, 255, 255, 0.06);
  color: inherit;
  line-height: 1.6;
  cursor: pointer;
}

.stepper__button:hover {
  background: rgba(255, 255, 255, 0.16);
}

.sandbox__tokens {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.sandbox__token {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 3px;
}

.sandbox__token img {
  width: min(48px, 12vw);
  height: auto;
}

.sandbox input.sandbox__override {
  width: 4.5em;
  text-align: center;
}

.sandbox__chips {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 4px;
}

.sandbox__chip {
  padding: 1px 8px;
  border: 1px solid var(--sandbox-line);
  border-radius: 999px;
  background: transparent;
  color: rgba(255, 255, 255, 0.6);
  font: inherit;
  cursor: pointer;
}

.sandbox__chip--on {
  background: rgba(255, 255, 255, 0.16);
  color: rgba(255, 255, 255, 0.95);
}

.sandbox__result {
  margin: 6px 0 0;
  font-weight: 600;
}

.sandbox__result--pass,
.sandbox__result--autoSuccess {
  color: #8fd18f;
}

.sandbox__result--fail,
.sandbox__result--autoFail {
  color: #e48c8c;
}

.sandbox__result--unknown {
  color: rgba(255, 255, 255, 0.6);
}
</style>

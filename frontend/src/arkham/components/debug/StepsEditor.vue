<script lang="ts" setup>
/* A list of steps, and the steps inside them.
 *
 * Branches and choices carry their own steps, so this renders itself for those
 * — which is what lets an ability say "if it is ready, attack; otherwise ready
 * it" or "choose an event, then play it". */
import { computed, ref } from 'vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'

const props = defineProps<{ modelValue: any[]; queryKinds: Record<string, string> }>()
const emit = defineEmits<{ 'update:modelValue': [v: any[]] }>()

const steps = computed(() => props.modelValue ?? [])

type StepKind =
  | 'query'
  | 'push'
  | 'if'
  | 'case'
  | 'forEach'
  | 'choose'
  | 'chooseFrom'
  | 'playCard'
  | 'fight'
  | 'attack'
  | 'ready'

const KIND_LABELS: Record<StepKind, string> = {
  query: 'Query',
  push: 'Push',
  if: 'If',
  case: 'Case',
  forEach: 'For each',
  choose: 'Choose',
  chooseFrom: 'Choose from',
  playCard: 'Play a card',
  fight: 'Fight',
  attack: 'Attack',
  ready: 'Ready',
}

function kindOf(step: any): StepKind {
  const kinds = [
    'query',
    'push',
    'if',
    'case',
    'forEach',
    'choose',
    'chooseFrom',
    'playCard',
    'fight',
    'attack',
    'ready',
  ] as StepKind[]
  for (const kind of kinds) {
    if (kind in (step ?? {})) return kind
  }
  return 'push'
}

const blankStep = (kind: StepKind) =>
  ({
    query: { query: { kind: 'enemy', matcher: null }, bind: '', mode: 'all' },
    push: { push: null },
    if: { if: { kind: 'enemy', matcher: null }, then: [], else: [] },
    case: { case: [{ if: { kind: 'enemy', matcher: null }, steps: [] }], else: [] },
    forEach: { forEach: { query: { kind: 'enemy', matcher: null }, bind: 'each', steps: [] } },
    choose: { choose: { options: [{ label: '', steps: [] }] } },
    chooseFrom: { chooseFrom: { query: { kind: 'enemy', matcher: null }, bind: 'chosen', steps: [] } },
    playCard: { playCard: { optional: true, matcher: null } },
    fight: { fight: { matcher: null, modifiers: [] } },
    attack: { attack: {} },
    ready: { ready: {} },
  })[kind]

const set = (index: number, step: any) =>
  emit('update:modelValue', steps.value.map((s, i) => (i === index ? step : s)))

const add = (kind: StepKind) => emit('update:modelValue', [...steps.value, blankStep(kind)])
const remove = (index: number) => emit('update:modelValue', steps.value.filter((_, i) => i !== index))

const matcherType = (kind: string | undefined) => props.queryKinds[kind ?? 'enemy'] ?? 'EnemyMatcher'

// --- choose options ---

const addingStep = ref(false)

function addAndClose(kind: StepKind) {
  add(kind)
  addingStep.value = false
}

const optionsOf = (step: any): any[] => step.choose?.options ?? []

const setOption = (step: any, index: number, option: any, at: number) =>
  set(at, { ...step, choose: { ...step.choose, options: optionsOf(step).map((o, i) => (i === index ? option : o)) } })

const addOption = (step: any, at: number) =>
  set(at, { ...step, choose: { ...step.choose, options: [...optionsOf(step), { label: '', steps: [] }] } })

const removeOption = (step: any, index: number, at: number) =>
  set(at, { ...step, choose: { ...step.choose, options: optionsOf(step).filter((_, i) => i !== index) } })
</script>

<template>
  <div class="steps">
    <div v-for="(step, index) in steps" :key="index" class="step">
      <div class="step-head">
        <span>{{ KIND_LABELS[kindOf(step)] }}</span>
        <button type="button" @click="remove(index)">×</button>
      </div>

      <template v-if="kindOf(step) === 'query'">
        <div class="row">
          <label>
            Kind
            <select
              :value="step.query?.kind"
              @change="set(index, { ...step, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            Bind to
            <input
              :value="step.bind"
              placeholder="enemies"
              @input="set(index, { ...step, bind: ($event.target as HTMLInputElement).value })"
              @keydown.stop
            />
          </label>
          <label>
            Mode
            <select :value="step.mode ?? 'all'" @change="set(index, { ...step, mode: ($event.target as HTMLSelectElement).value })">
              <option value="all">all</option>
              <option value="first">first</option>
              <option value="count">count</option>
            </select>
          </label>
        </div>
        <ValueEditor
          :type="matcherType(step.query?.kind)"
          label="Matcher"
          :modelValue="step.query?.matcher"
          @update:modelValue="set(index, { ...step, query: { ...step.query, matcher: $event } })"
        />
      </template>

      <ValueEditor
        v-else-if="kindOf(step) === 'push'"
        type="Message"
        label="Message"
        :modelValue="step.push"
        @update:modelValue="set(index, { ...step, push: $event })"
      />

      <template v-else-if="kindOf(step) === 'if'">
        <p class="hint">Runs the matcher; takes the first branch when it finds anything.</p>
        <label>
          Kind
          <select
            :value="step.if?.kind"
            @change="set(index, { ...step, if: { kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
          >
            <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
          </select>
        </label>
        <ValueEditor
          :type="matcherType(step.if?.kind)"
          label="Matcher"
          :modelValue="step.if?.matcher"
          @update:modelValue="set(index, { ...step, if: { ...step.if, matcher: $event } })"
        />
        <span class="branch-label">Then</span>
        <StepsEditor
          :queryKinds="queryKinds"
          :modelValue="step.then ?? []"
          @update:modelValue="set(index, { ...step, then: $event })"
        />
        <span class="branch-label">Otherwise</span>
        <StepsEditor
          :queryKinds="queryKinds"
          :modelValue="step.else ?? []"
          @update:modelValue="set(index, { ...step, else: $event })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'case'">
        <p class="hint">The first branch whose condition holds.</p>
        <div v-for="(b, bi) in step.case ?? []" :key="bi" class="option">
          <div class="row">
            <label>
              Kind
              <select
                :value="b.if?.kind"
                @change="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, if: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } : x) })"
              >
                <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
              </select>
            </label>
            <button type="button" @click="set(index, { ...step, case: step.case.filter((_: any, i: number) => i !== bi) })">×</button>
          </div>
          <ValueEditor
            :type="matcherType(b.if?.kind)"
            label="Matcher"
            :modelValue="b.if?.matcher"
            @update:modelValue="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, if: { ...x.if, matcher: $event } } : x) })"
          />
          <StepsEditor
            :queryKinds="queryKinds"
            :modelValue="b.steps ?? []"
            @update:modelValue="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, steps: $event } : x) })"
          />
        </div>
        <button type="button" class="add" @click="set(index, { ...step, case: [...(step.case ?? []), { if: { kind: 'enemy', matcher: null }, steps: [] }] })">
          + Branch
        </button>
        <span class="branch-label">Otherwise</span>
        <StepsEditor
          :queryKinds="queryKinds"
          :modelValue="step.else ?? []"
          @update:modelValue="set(index, { ...step, else: $event })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'forEach'">
        <p class="hint">Runs the steps once per thing found, with it bound below.</p>
        <div class="row">
          <label>
            Kind
            <select
              :value="step.forEach?.query?.kind"
              @change="set(index, { ...step, forEach: { ...step.forEach, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            Bind to
            <input
              :value="step.forEach?.bind"
              @input="set(index, { ...step, forEach: { ...step.forEach, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <ValueEditor
          :type="matcherType(step.forEach?.query?.kind)"
          label="Matcher"
          :modelValue="step.forEach?.query?.matcher"
          @update:modelValue="set(index, { ...step, forEach: { ...step.forEach, query: { ...step.forEach.query, matcher: $event } } })"
        />
        <StepsEditor
          :queryKinds="queryKinds"
          :modelValue="step.forEach?.steps ?? []"
          @update:modelValue="set(index, { ...step, forEach: { ...step.forEach, steps: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'choose'">
        <div v-for="(option, oi) in optionsOf(step)" :key="oi" class="option">
          <div class="row">
            <label>
              Option label
              <input
                :value="option.label"
                @input="setOption(step, oi, { ...option, label: ($event.target as HTMLInputElement).value }, index)"
                @keydown.stop
              />
            </label>
            <button type="button" @click="removeOption(step, oi, index)">×</button>
          </div>
          <StepsEditor
            :queryKinds="queryKinds"
            :modelValue="option.steps ?? []"
            @update:modelValue="setOption(step, oi, { ...option, steps: $event }, index)"
          />
        </div>
        <button type="button" class="add" @click="addOption(step, index)">+ Option</button>
      </template>

      <template v-else-if="kindOf(step) === 'playCard'">
        <p class="hint">
          Offers the cards you could play, paying the cost, each shown as the card itself. A
          discount is worked out before the choice, since a card is only playable if you can
          afford it.
        </p>
        <div class="row">
          <label v-if="step.playCard?.optional">
            Decline label
            <input
              :value="step.playCard?.declineLabel"
              placeholder="Do not"
              @input="set(index, { ...step, playCard: { ...step.playCard, declineLabel: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <label>
            Discount
            <input
              type="number"
              :value="step.playCard?.discount ?? 0"
              @input="set(index, { ...step, playCard: { ...step.playCard, discount: Number(($event.target as HTMLInputElement).value) } })"
              @keydown.stop
            />
          </label>
          <label class="inline">
            <input
              type="checkbox"
              :checked="!!step.playCard?.optional"
              @change="set(index, { ...step, playCard: { ...step.playCard, optional: ($event.target as HTMLInputElement).checked } })"
            />
            may decline
          </label>
        </div>
        <ValueEditor
          type="CardMatcher"
          label="Which cards"
          :modelValue="step.playCard?.matcher"
          @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, matcher: $event } })"
        />
        <ValueEditor
          type="Criterion"
          label="Discount only when (optional)"
          :modelValue="step.playCard?.discountIf?.criteria"
          @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, discountIf: $event ? { criteria: $event } : undefined } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'fight'">
        <p class="hint">
          Fight an enemy. Whether the card itself is a fight action comes from its Actions, not
          from here — this is the attack it makes.
        </p>
        <label class="inline">
          <input
            type="checkbox"
            :checked="!!step.fight?.basic"
            @change="set(index, { ...step, fight: { ...step.fight, basic: ($event.target as HTMLInputElement).checked } })"
          />
          a basic fight action instead
        </label>
        <p v-if="step.fight?.basic" class="hint">
          The enemy's own attack ability, granted so it costs no action. No card can be a basic
          fight action, so modifiers "for this attack" have nowhere to go here.
        </p>
        <ValueEditor
          type="EnemyMatcher"
          label="Which enemies (optional)"
          :modelValue="step.fight?.matcher"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, matcher: $event } })"
        />
        <ValueEditor
          v-if="!step.fight?.basic"
          type="[ModifierType]"
          label="For this attack"
          :modelValue="step.fight?.modifiers"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, modifiers: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'attack'">
        <p class="summary">
          <template v-if="step.attack?.target">This card attacks the chosen target.</template>
          <template v-else>
            This card attacks whoever triggered the ability — "it makes an immediate attack
            against you".
          </template>
        </p>
        <ValueEditor
          type="Target"
          label="Attack someone else instead (optional)"
          :modelValue="step.attack?.target"
          @update:modelValue="set(index, { ...step, attack: { ...step.attack, target: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'ready'">
        <p class="summary">
          <template v-if="step.ready?.target">Readies the chosen card.</template>
          <template v-else>Readies this card.</template>
        </p>
        <ValueEditor
          type="Target"
          label="Ready something else instead (optional)"
          :modelValue="step.ready?.target"
          @update:modelValue="set(index, { ...step, ready: { ...step.ready, target: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'chooseFrom'">
        <p class="hint">One option per thing the matcher finds, with it bound for the steps below.</p>
        <div class="row">
          <label>
            Kind
            <select
              :value="step.chooseFrom?.query?.kind"
              @change="set(index, { ...step, chooseFrom: { ...step.chooseFrom, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            Bind to
            <input
              :value="step.chooseFrom?.bind"
              @input="set(index, { ...step, chooseFrom: { ...step.chooseFrom, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <label class="inline">
            <input
              type="checkbox"
              :checked="!!step.chooseFrom?.optional"
              @change="set(index, { ...step, chooseFrom: { ...step.chooseFrom, optional: ($event.target as HTMLInputElement).checked } })"
            />
            may decline
          </label>
        </div>
        <ValueEditor
          :type="matcherType(step.chooseFrom?.query?.kind)"
          label="Matcher"
          :modelValue="step.chooseFrom?.query?.matcher"
          @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, query: { ...step.chooseFrom.query, matcher: $event } } })"
        />
        <StepsEditor
          :queryKinds="queryKinds"
          :modelValue="step.chooseFrom?.steps ?? []"
          @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, steps: $event } })"
        />
      </template>
    </div>

    <div class="step-actions">
      <button v-if="!addingStep" type="button" @click="addingStep = true">+ Step</button>
      <template v-else>
        <button v-for="(label, kind) in KIND_LABELS" :key="kind" type="button" @click="addAndClose(kind as StepKind)">
          {{ label }}
        </button>
        <button type="button" class="cancel-add" @click="addingStep = false">×</button>
      </template>
    </div>
  </div>
</template>

<style scoped lang="scss">
.steps {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}

.step {
  background: rgba(255, 255, 255, 0.04);
  border-radius: 4px;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  padding: 0.5rem;
}

.step-head {
  align-items: center;
  display: flex;
  font-size: 0.75rem;
  justify-content: space-between;
  opacity: 0.8;

  button {
    background: none;
    border: none;
    color: #eee;
    cursor: pointer;
  }
}

.option {
  border-left: 2px solid #374151;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  padding-left: 0.5rem;
}

.branch-label {
  font-size: 0.75rem;
  opacity: 0.7;
}

.row {
  display: flex;
  gap: 0.4rem;
  flex-wrap: wrap;
  align-items: flex-end;

  > label {
    flex: 1 1 100px;
  }
}

label {
  display: flex;
  flex-direction: column;
  font-size: 0.75rem;
  gap: 0.2rem;
  opacity: 0.9;

  &.inline {
    align-items: center;
    flex: 0 0 auto;
    flex-direction: row;
    gap: 0.3rem;
  }
}

input,
select {
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  padding: 0.3rem;
  width: 100%;
}

input[type='checkbox'] {
  width: auto;
}

.step-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
}

.cancel-add {
  opacity: 0.7;
}

.step-actions button,
.add {
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.25rem 0.6rem;
}

.summary {
  font-size: 0.8rem;
  margin: 0;
}

.hint {
  font-size: 0.75rem;
  margin: 0;
  opacity: 0.7;
}
</style>

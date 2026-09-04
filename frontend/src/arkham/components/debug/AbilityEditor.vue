<script lang="ts" setup>
/* Builds the `_abilities` and `_handlers` blocks of a custom card's meta.
 *
 * Each ability is an AbilityType plus optional criteria/limit, and a list of
 * steps. A step either runs a query and binds the result, or pushes a message —
 * the two operations the runner supports. Anything a step binds is available to
 * later steps as $name, alongside $id, $source, $target and $iid. */
import { computed, onMounted } from 'vue'
import { loadSchema, schemaLoaded } from '@/arkham/schema'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'

const props = defineProps<{ abilities: any[]; handlers: any[]; modifiers: any[] }>()
const emit = defineEmits<{
  'update:abilities': [v: any[]]
  'update:handlers': [v: any[]]
  'update:modifiers': [v: any[]]
}>()

onMounted(loadSchema)

/* Modifiers only reach entities that can be a modifier target. */
const MODIFIER_KINDS: Record<string, string> = {
  enemy: 'EnemyMatcher',
  location: 'LocationMatcher',
  investigator: 'InvestigatorMatcher',
  asset: 'AssetMatcher',
  treachery: 'TreacheryMatcher',
  event: 'EventMatcher',
  skill: 'SkillMatcher',
  card: 'ExtendedCardMatcher',
}

const QUERY_KINDS: Record<string, string> = {
  enemy: 'EnemyMatcher',
  location: 'LocationMatcher',
  investigator: 'InvestigatorMatcher',
  asset: 'AssetMatcher',
  treachery: 'TreacheryMatcher',
  event: 'EventMatcher',
  skill: 'SkillMatcher',
  story: 'StoryMatcher',
  act: 'ActMatcher',
  agenda: 'AgendaMatcher',
  card: 'ExtendedCardMatcher',
}

const abilities = computed(() => props.abilities ?? [])
const handlers = computed(() => props.handlers ?? [])
const modifiers = computed(() => props.modifiers ?? [])

function patch(list: any[], index: number, changes: Record<string, any>) {
  return list.map((item, i) => (i === index ? { ...item, ...changes } : item))
}

// --- abilities ---

const setAbility = (i: number, changes: Record<string, any>) =>
  emit('update:abilities', patch(abilities.value, i, changes))

const addAbility = () =>
  emit('update:abilities', [...abilities.value, { type: null, steps: [] }])

const removeAbility = (i: number) =>
  emit('update:abilities', abilities.value.filter((_, j) => j !== i))

// --- handlers ---

const setHandler = (i: number, changes: Record<string, any>) =>
  emit('update:handlers', patch(handlers.value, i, changes))

const addHandler = () => emit('update:handlers', [...handlers.value, { on: '', steps: [] }])

const removeHandler = (i: number) =>
  emit('update:handlers', handlers.value.filter((_, j) => j !== i))

// --- modifiers ---

const setModifier = (i: number, changes: Record<string, any>) =>
  emit('update:modifiers', patch(modifiers.value, i, changes))

const addModifier = () =>
  emit('update:modifiers', [...modifiers.value, { kind: 'enemy', matcher: null, modifiers: [] }])

const removeModifier = (i: number) =>
  emit('update:modifiers', modifiers.value.filter((_, j) => j !== i))

// --- steps, shared by both ---

function setSteps(owner: 'ability' | 'handler', index: number, steps: any[]) {
  if (owner === 'ability') setAbility(index, { steps })
  else setHandler(index, { steps })
}

const stepsOf = (item: any): any[] => item.steps ?? []

const stepKind = (step: any) => ('query' in (step ?? {}) ? 'query' : 'push')

function addStep(owner: 'ability' | 'handler', index: number, item: any, kind: 'query' | 'push') {
  const step = kind === 'query' ? { query: { kind: 'enemy', matcher: null }, bind: '', mode: 'all' } : { push: null }
  setSteps(owner, index, [...stepsOf(item), step])
}

function setStep(owner: 'ability' | 'handler', index: number, item: any, stepIndex: number, step: any) {
  setSteps(owner, index, stepsOf(item).map((s, i) => (i === stepIndex ? step : s)))
}

function removeStep(owner: 'ability' | 'handler', index: number, item: any, stepIndex: number) {
  setSteps(owner, index, stepsOf(item).filter((_, i) => i !== stepIndex))
}
</script>

<template>
  <div class="ability-editor">
    <p v-if="!schemaLoaded" class="loading">Loading type schema…</p>

    <template v-else>
      <div v-for="(ability, index) in abilities" :key="index" class="block">
        <div class="block-head">
          <strong>Ability {{ index + 1 }}</strong>
          <button type="button" @click="removeAbility(index)">Remove</button>
        </div>

        <ValueEditor
          type="AbilityType"
          label="When / how it is used"
          :modelValue="ability.type"
          @update:modelValue="setAbility(index, { type: $event })"
        />
        <ValueEditor
          type="Criterion"
          label="Criteria (optional) — gates whether the ability is available"
          :modelValue="ability.criteria"
          @update:modelValue="setAbility(index, { criteria: $event })"
        />
        <ValueEditor
          type="AbilityLimit"
          label="Limit (optional)"
          :modelValue="ability.limit"
          @update:modelValue="setAbility(index, { limit: $event })"
        />

        <div class="steps">
          <div v-for="(step, si) in stepsOf(ability)" :key="si" class="step">
            <div class="step-head">
              <span>{{ stepKind(step) === 'query' ? 'Query' : 'Push' }}</span>
              <button type="button" @click="removeStep('ability', index, ability, si)">×</button>
            </div>

            <template v-if="stepKind(step) === 'query'">
              <div class="row">
                <label>
                  Kind
                  <select
                    :value="step.query?.kind"
                    @change="setStep('ability', index, ability, si, { ...step, query: { ...step.query, kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
                  >
                    <option v-for="(_, kind) in QUERY_KINDS" :key="kind" :value="kind">{{ kind }}</option>
                  </select>
                </label>
                <label>
                  Bind to
                  <input
                    :value="step.bind"
                    placeholder="enemies"
                    @input="setStep('ability', index, ability, si, { ...step, bind: ($event.target as HTMLInputElement).value })"
                    @keydown.stop
                  />
                </label>
                <label>
                  Mode
                  <select
                    :value="step.mode ?? 'all'"
                    @change="setStep('ability', index, ability, si, { ...step, mode: ($event.target as HTMLSelectElement).value })"
                  >
                    <option value="all">all</option>
                    <option value="first">first</option>
                    <option value="count">count</option>
                  </select>
                </label>
              </div>
              <ValueEditor
                :type="QUERY_KINDS[step.query?.kind] ?? 'EnemyMatcher'"
                label="Matcher"
                :modelValue="step.query?.matcher"
                @update:modelValue="setStep('ability', index, ability, si, { ...step, query: { ...step.query, matcher: $event } })"
              />
            </template>

            <ValueEditor
              v-else
              type="Message"
              label="Message"
              :modelValue="step.push"
              @update:modelValue="setStep('ability', index, ability, si, { ...step, push: $event })"
            />
          </div>

          <div class="step-actions">
            <button type="button" @click="addStep('ability', index, ability, 'query')">+ Query</button>
            <button type="button" @click="addStep('ability', index, ability, 'push')">+ Push</button>
          </div>
        </div>
      </div>

      <button type="button" class="add" @click="addAbility">+ Ability</button>

      <div v-for="(handler, index) in handlers" :key="`h${index}`" class="block">
        <div class="block-head">
          <strong>Listens for</strong>
          <button type="button" @click="removeHandler(index)">Remove</button>
        </div>
        <label>
          Message tag
          <input
            :value="handler.on"
            placeholder="EnemyDamaged"
            @input="setHandler(index, { on: ($event.target as HTMLInputElement).value })"
            @keydown.stop
          />
        </label>
        <p class="hint">
          Fires when a message with this tag mentions this card. Its fields are available as
          <code>$0</code>, <code>$1</code>, … and the whole message as <code>$message</code>.
        </p>

        <div class="steps">
          <div v-for="(step, si) in stepsOf(handler)" :key="si" class="step">
            <div class="step-head">
              <span>{{ stepKind(step) === 'query' ? 'Query' : 'Push' }}</span>
              <button type="button" @click="removeStep('handler', index, handler, si)">×</button>
            </div>
            <template v-if="stepKind(step) === 'query'">
              <div class="row">
                <label>
                  Kind
                  <select
                    :value="step.query?.kind"
                    @change="setStep('handler', index, handler, si, { ...step, query: { ...step.query, kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
                  >
                    <option v-for="(_, kind) in QUERY_KINDS" :key="kind" :value="kind">{{ kind }}</option>
                  </select>
                </label>
                <label>
                  Bind to
                  <input
                    :value="step.bind"
                    @input="setStep('handler', index, handler, si, { ...step, bind: ($event.target as HTMLInputElement).value })"
                    @keydown.stop
                  />
                </label>
              </div>
              <ValueEditor
                :type="QUERY_KINDS[step.query?.kind] ?? 'EnemyMatcher'"
                label="Matcher"
                :modelValue="step.query?.matcher"
                @update:modelValue="setStep('handler', index, handler, si, { ...step, query: { ...step.query, matcher: $event } })"
              />
            </template>
            <ValueEditor
              v-else
              type="Message"
              label="Message"
              :modelValue="step.push"
              @update:modelValue="setStep('handler', index, handler, si, { ...step, push: $event })"
            />
          </div>
          <div class="step-actions">
            <button type="button" @click="addStep('handler', index, handler, 'query')">+ Query</button>
            <button type="button" @click="addStep('handler', index, handler, 'push')">+ Push</button>
          </div>
        </div>
      </div>

      <button type="button" class="add" @click="addHandler">+ Listener</button>

      <div v-for="(modifier, index) in modifiers" :key="`m${index}`" class="block">
        <div class="block-head">
          <strong>Gives modifiers to</strong>
          <button type="button" @click="removeModifier(index)">Remove</button>
        </div>
        <label>
          What to match
          <select
            :value="modifier.kind"
            @change="setModifier(index, { kind: ($event.target as HTMLSelectElement).value, matcher: null })"
          >
            <option v-for="(_, kind) in MODIFIER_KINDS" :key="kind" :value="kind">{{ kind }}</option>
          </select>
        </label>
        <ValueEditor
          :type="MODIFIER_KINDS[modifier.kind] ?? 'EnemyMatcher'"
          label="Matcher"
          :modelValue="modifier.matcher"
          @update:modelValue="setModifier(index, { matcher: $event })"
        />
        <ValueEditor
          type="[ModifierType]"
          label="Modifiers"
          :modelValue="modifier.modifiers"
          @update:modelValue="setModifier(index, { modifiers: $event })"
        />
        <p class="hint">
          Applies while this card is in play, to everything the matcher selects. Match
          <strong>card</strong> rather than an entity to reach a card before it is in play — that is
          what a keyword needs when the engine reads it at draw or spawn time.
        </p>
      </div>

      <button type="button" class="add" @click="addModifier">+ Modifier</button>
    </template>
  </div>
</template>

<style scoped lang="scss">
.ability-editor {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

.block {
  border: 1px solid #374151;
  border-radius: 6px;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  padding: 0.6rem;
}

.block-head {
  align-items: center;
  display: flex;
  justify-content: space-between;

  button {
    background: none;
    border: none;
    color: #f88;
    cursor: pointer;
    font-size: 0.8rem;
  }
}

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

.step-actions,
.row {
  display: flex;
  gap: 0.4rem;
  flex-wrap: wrap;
}

.row > label {
  flex: 1 1 100px;
}

label {
  display: flex;
  flex-direction: column;
  font-size: 0.75rem;
  gap: 0.2rem;
  opacity: 0.9;
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

button.add,
.step-actions button {
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.25rem 0.6rem;
}

.hint {
  font-size: 0.75rem;
  margin: 0;
  opacity: 0.7;
}

.loading {
  opacity: 0.7;
}
</style>

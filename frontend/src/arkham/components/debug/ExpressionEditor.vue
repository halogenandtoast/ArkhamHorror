<script lang="ts" setup>
/* Building an expression from "Arkham.Custom.Expr" without writing its JSON.
 *
 * An expression is one source -- a value, a property, something about the test
 * -- with any number of transforms hung off it. Transforms are not a kind of
 * expression you choose between; they are what you can do to whatever you have,
 * which is why they are offered on every expression and filtered by the type at
 * that point in the chain. */
import { computed } from 'vue'
import BindingField from '@/arkham/components/debug/BindingField.vue'
import PropertyField from '@/arkham/components/debug/PropertyField.vue'
import type { Binding } from '@/arkham/customCardBindings'
import {
  CARD_PROPS,
  SKILL_TEST_PROPS,
  expressionType,
  stageKey,
  stageResult,
  stagesFor,
  typeFits,
  unwindPipeline,
  windPipeline,
} from '@/arkham/customCardExpressions'

const props = withDefaults(
  defineProps<{
    modelValue: any
    /* The type this position wants. Undefined when anything will do, which is
     * the case for a `let`: it binds whatever it is given. */
    expect?: string
    bindings?: Binding[]
    label?: string
  }>(),
  { expect: undefined },
)
const emit = defineEmits<{ 'update:modelValue': [v: any] }>()

/* Entity kinds name a Field on that entity, whose type the schema knows but
 * this editor does not. `card` is the one we can type. */
const KINDS = ['card', 'enemy', 'location', 'investigator', 'asset', 'act']

/* What an expression can start as. Anything that takes exactly one operand is a
 * transform instead, so it is not repeated here. */
type Source = {
  key: string
  label: string
  shape: 'literal' | 'prop' | 'skillTest' | 'filter' | 'nary'
}

const SOURCES: Source[] = [
  { key: '', label: 'A value', shape: 'literal' },
  { key: 'get', label: 'A property of something', shape: 'prop' },
  { key: 'skillTest', label: 'Something about this skill test', shape: 'skillTest' },
  { key: 'filter', label: 'Only the ones that…', shape: 'filter' },
  { key: 'add', label: 'Several values added together', shape: 'nary' },
  { key: 'subtract', label: 'Several values subtracted', shape: 'nary' },
  { key: 'multiply', label: 'Several values multiplied', shape: 'nary' },
  { key: 'divide', label: 'Several values divided', shape: 'nary' },
]

const PREDICATES = [
  { key: 'eq', label: 'is' },
  { key: 'ne', label: 'is not' },
  { key: 'in', label: 'is one of' },
  { key: 'notIn', label: 'is not one of' },
  { key: 'gt', label: 'is more than' },
  { key: 'lt', label: 'is less than' },
  { key: 'gte', label: 'is at least' },
  { key: 'lte', label: 'is at most' },
]

/* The expression as a source and the transforms applied to it, which is how it
 * is edited and displayed even though it is stored nested. */
const pipeline = computed(() => unwindPipeline(props.modelValue))
const source = computed(() => pipeline.value.source)

const currentSource = computed<Source>(() => {
  const v = source.value
  if (v && typeof v === 'object' && !Array.isArray(v)) {
    const found = SOURCES.find((o) => o.key && o.key in v)
    if (found) return found
    if ('map' in v) return SOURCES.find((o) => o.key === 'get')!
  }
  return SOURCES[0]
})

const rebuild = (nextSource: any, stages: string[]) =>
  emit('update:modelValue', windPipeline(nextSource, stages))

const setSource = (v: any) => rebuild(v, pipeline.value.stages)
const patch = (changes: Record<string, any>) =>
  setSource({ ...(source.value ?? {}), ...changes })

function pickSource(key: string) {
  const stages = pipeline.value.stages
  const kept = source.value && typeof source.value === 'object' ? undefined : source.value
  const chosen = SOURCES.find((o) => o.key === key)!
  if (chosen.shape === 'literal') return rebuild(kept ?? null, stages)
  if (chosen.shape === 'prop') return rebuild({ get: 'id', kind: 'card', of: kept ?? null }, stages)
  if (chosen.shape === 'skillTest') return rebuild({ skillTest: 'difficulty' }, stages)
  if (chosen.shape === 'filter') return rebuild({ filter: { eq: null }, of: kept ?? null }, stages)
  return rebuild({ [key]: [kept ?? null, null] }, stages)
}

// --- the value a plain source holds ---

const literalText = computed(() => {
  const v = source.value
  if (v === null || v === undefined) return ''
  return typeof v === 'object' ? '' : String(v)
})

const isBindingText = computed(() => literalText.value.trim().startsWith('$'))

const applicableBindings = computed(() =>
  (props.bindings ?? []).filter(
    // A binding can stand here either as the whole answer or as something to
    // transform, so it is offered when either would fit.
    (b) => typeFits(b.type, props.expect) || stagesFor(b.type).length > 0,
  ),
)

function setLiteral(text: string) {
  const trimmed = (text ?? '').trim()
  if (trimmed === '') return setSource(null)
  if (/^-?\d+$/.test(trimmed)) return setSource(Number(trimmed))
  setSource(trimmed)
}

// --- the chain ---

/* The type at each point: what the source is, then after each transform. */
const pipelineTypes = computed(() => {
  let type = expressionType(source.value, props.bindings ?? [])
  const types = [type]
  for (const stage of pipeline.value.stages) {
    type = stageResult(stage, type)
    types.push(type)
  }
  return types
})

const stageAt = (at: number) => pipeline.value.stages[at]
const keyAt = (at: number) => stageKey(stageAt(at))

const stageOptions = (at: number) => {
  const all = stagesFor(pipelineTypes.value[at])
  const offered =
    at === pipeline.value.stages.length - 1
      ? all.filter((st) => typeFits(st.to, props.expect))
      : all
  const key = keyAt(at)
  if (!key || offered.some((st) => st.name === key)) return offered
  /* Whatever is already written stays listed even when it does not fit what it
   * is handed, because a dropdown with nothing selected reads as a step the
   * author left blank rather than one that needs looking at. */
  const known = stagesFor(undefined).find((st) => st.name === key)
  return [...offered, known ?? { name: key, label: key, to: undefined, template: stageAt(at) }]
}

const setStage = (at: number, name: string) => {
  const chosen =
    stagesFor(pipelineTypes.value[at]).find((st) => st.name === name) ??
    stagesFor(undefined).find((st) => st.name === name)
  if (!chosen) return
  rebuild(
    source.value,
    pipeline.value.stages.map((st, i) => (i === at ? { ...chosen.template } : st)),
  )
}

/* A stage that carries more than its name -- a predicate to test by -- is
 * edited in place. */
const patchStage = (at: number, changes: Record<string, any>) =>
  rebuild(
    source.value,
    pipeline.value.stages.map((st, i) => (i === at ? { ...st, ...changes } : st)),
  )

const stagePredicateKey = (at: number) => {
  const f = stageAt(at)?.filter
  if (!f || typeof f !== 'object') return 'eq'
  return PREDICATES.find((p) => p.key in f)?.key ?? 'eq'
}
const stagePredicateOperand = (at: number) => stageAt(at)?.filter?.[stagePredicateKey(at)] ?? null
const setStagePredicate = (at: number, key: string) =>
  patchStage(at, { filter: { [key]: stagePredicateOperand(at) } })
const setStagePredicateOperand = (at: number, v: any) =>
  patchStage(at, { filter: { [stagePredicateKey(at)]: v } })
const removeStage = (at: number) =>
  rebuild(source.value, pipeline.value.stages.filter((_, i) => i !== at))
const addStage = () => {
  const next = stagesFor(pipelineTypes.value[pipeline.value.stages.length])[0]
  if (next) rebuild(source.value, [...pipeline.value.stages, { ...next.template }])
}

const canAddStage = computed(
  () =>
    (source.value !== null || pipeline.value.stages.length > 0) &&
    stagesFor(pipelineTypes.value[pipeline.value.stages.length]).length > 0,
)


// --- shapes with operands of their own ---

const naryItems = computed<any[]>(() => {
  const v = source.value?.[currentSource.value.key]
  return Array.isArray(v) ? v : []
})
const setNary = (i: number, item: any) =>
  patch({ [currentSource.value.key]: naryItems.value.map((x, j) => (j === i ? item : x)) })
const addNary = () => patch({ [currentSource.value.key]: [...naryItems.value, null] })
const removeNary = (i: number) =>
  patch({ [currentSource.value.key]: naryItems.value.filter((_, j) => j !== i) })

const predicateKey = computed(() => {
  const f = source.value?.filter
  if (!f || typeof f !== 'object') return 'eq'
  return PREDICATES.find((p) => p.key in f)?.key ?? 'eq'
})
const predicateOperand = computed(() => source.value?.filter?.[predicateKey.value] ?? null)
const setPredicate = (key: string) => patch({ filter: { [key]: predicateOperand.value } })
const setPredicateOperand = (v: any) => patch({ filter: { [predicateKey.value]: v } })

const propsFor = computed(() => (source.value?.kind === 'card' ? CARD_PROPS : null))
</script>

<template>
  <div class="expr">
    <span v-if="label" class="expr-label">{{ label }}</span>

    <div class="row">
      <label>
        What
        <select :value="currentSource.key" @change="pickSource(($event.target as HTMLSelectElement).value)">
          <option v-for="o in SOURCES" :key="o.key" :value="o.key">{{ o.label }}</option>
        </select>
      </label>

      <template v-if="currentSource.shape === 'literal'">
        <label v-if="!isBindingText">
          Value
          <input
            :value="literalText"
            :placeholder="expect === 'Int' ? 'a number' : 'a value'"
            @input="setLiteral(($event.target as HTMLInputElement).value)"
            @keydown.stop
          />
        </label>
        <BindingField
          v-if="isBindingText || applicableBindings.length"
          class="grow"
          :modelValue="isBindingText ? literalText.trim() : null"
          :applicable="applicableBindings"
          :type="expect ?? 'anything'"
          @update:modelValue="setLiteral($event ?? '')"
        />
      </template>

      <template v-else-if="currentSource.shape === 'prop'">
        <label>
          Of what
          <select
            :value="source?.kind ?? 'card'"
            @change="patch({ kind: ($event.target as HTMLSelectElement).value })"
          >
            <option v-for="k in KINDS" :key="k" :value="k">{{ k }}</option>
          </select>
        </label>
        <PropertyField
          v-if="propsFor"
          class="grow"
          :modelValue="source?.get"
          :options="propsFor"
          of="a card"
          @update:modelValue="patch({ get: $event })"
        />
        <label v-else>
          Field
          <input
            :value="source?.get"
            placeholder="EnemyHealth"
            @input="patch({ get: ($event.target as HTMLInputElement).value })"
            @keydown.stop
          />
        </label>
      </template>

      <template v-else-if="currentSource.shape === 'skillTest'">
        <PropertyField
          class="grow"
          :modelValue="source?.skillTest"
          :options="SKILL_TEST_PROPS"
          of="the skill test"
          @update:modelValue="patch({ skillTest: $event })"
        />
      </template>

      <template v-else-if="currentSource.shape === 'filter'">
        <label>
          Which
          <select :value="predicateKey" @change="setPredicate(($event.target as HTMLSelectElement).value)">
            <option v-for="p in PREDICATES" :key="p.key" :value="p.key">{{ p.label }}</option>
          </select>
        </label>
      </template>
    </div>

    <div v-if="currentSource.shape === 'filter'" class="nested">
      <ExpressionEditor
        :modelValue="predicateOperand"
        :bindings="bindings"
        label="compared with"
        @update:modelValue="setPredicateOperand"
      />
      <ExpressionEditor
        :modelValue="source?.of"
        :bindings="bindings"
        expect="[any]"
        label="out of"
        @update:modelValue="patch({ of: $event })"
      />
    </div>

    <div v-else-if="currentSource.shape === 'prop'" class="nested">
      <ExpressionEditor
        :modelValue="source?.of"
        :bindings="bindings"
        label="of"
        @update:modelValue="patch({ of: $event })"
      />
    </div>

    <div v-else-if="currentSource.shape === 'nary'" class="nested">
      <div v-for="(item, i) in naryItems" :key="i" class="row">
        <ExpressionEditor
          :modelValue="item"
          :bindings="bindings"
          expect="Int"
          @update:modelValue="setNary(i, $event)"
        />
        <button type="button" class="remove" @click="removeNary(i)">×</button>
      </div>
      <button type="button" class="add" @click="addNary">+ Value</button>
    </div>

    <!-- What can be done to whatever the source is, in the order it happens. -->
    <div class="pipeline">
      <div v-for="(stage, at) in pipeline.stages" :key="at" class="pipe-stage">
        <div class="pipe-step">
          <span class="pipe-arrow" aria-hidden="true">
            <svg viewBox="0 0 14 16" width="14" height="16">
              <!-- Down out of the step above, then right into the step this is:
                   the chain turns rather than merely descends. -->
              <path
                d="M3.4 1.5 V8.6 Q3.4 11.4 6.2 11.4 H10.4"
                fill="none"
                stroke="currentColor"
                stroke-width="2.4"
                stroke-linecap="round"
                stroke-linejoin="round"
              />
              <path
                d="M8.6 9.2 L11.2 11.4 L8.6 13.6"
                fill="none"
                stroke="currentColor"
                stroke-width="2.4"
                stroke-linecap="round"
                stroke-linejoin="round"
              />
            </svg>
          </span>
          <select
            :value="keyAt(at)"
            @change="setStage(at, ($event.target as HTMLSelectElement).value)"
          >
            <option v-for="option in stageOptions(at)" :key="option.name" :value="option.name">
              {{ option.label }}
            </option>
          </select>
          <select
            v-if="keyAt(at) === 'filter'"
            :value="stagePredicateKey(at)"
            @change="setStagePredicate(at, ($event.target as HTMLSelectElement).value)"
          >
            <option v-for="p in PREDICATES" :key="p.key" :value="p.key">{{ p.label }}</option>
          </select>
          <!-- What this link hands to the next one. Said here rather than only at
               the foot of the chain, so a transform that narrowed the type to
               something unusable is visible at the step that did it. -->
          <code class="stage-type" :class="{ unsure: !pipelineTypes[at + 1] }">
            {{ pipelineTypes[at + 1] ?? '?' }}
          </code>
          <button
            type="button"
            class="pipe-remove"
            title="Remove this transform"
            aria-label="Remove this transform"
            @click="removeStage(at)"
          >
            ×
          </button>
        </div>
        <div v-if="keyAt(at) === 'filter'" class="pipe-operand">
          <ExpressionEditor
            :modelValue="stagePredicateOperand(at)"
            :bindings="bindings"
            label="compared with"
            @update:modelValue="setStagePredicateOperand(at, $event)"
          />
        </div>
      </div>

      <button v-if="canAddStage" type="button" class="add-transform" @click="addStage">
        <span class="add-glyph" aria-hidden="true">+</span> transform
      </button>
    </div>
  </div>
</template>

<style scoped lang="scss">
/* Same field furniture as the rest of the builder: a caption above its control,
 * controls sharing one row and one border treatment. An expression is a field
 * like any other and should not announce itself as a different kind of thing. */
.expr {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  min-width: 0;
}

.expr-label {
  font-size: 0.75rem;
  opacity: 0.9;
}

.row > .grow {
  flex: 1 1 180px;
}

.row {
  align-items: flex-end;
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;

  > label {
    flex: 1 1 120px;
  }

  > .expr {
    flex: 1 1 100%;
  }
}

label {
  display: flex;
  flex-direction: column;
  font-size: 0.75rem;
  gap: 0.2rem;
  min-width: 0;
  opacity: 0.9;
}

input.binding {
  background: rgba(170, 221, 255, 0.12);
  border-color: #adf;
  color: #adf;
  font-family: monospace;
}

input.unknown {
  border-color: #f88;
  color: #f88;
  font-family: monospace;
}

.origin {
  color: #9ca3af;
  font-size: 0.72rem;
  margin: 0;

  code {
    color: #adf;
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

/* The marker is drawn rather than left to the browser, so it matches the one the
 * custom pickers show and sits in from the edge instead of flush against it.
 * Selects only -- `appearance: none` on an input takes a checkbox's box away. */
select {
  -webkit-appearance: none;
  appearance: none;
  background: #111827 var(--select-caret) no-repeat right 0.6rem center;
  background-size: var(--select-caret-size);
  padding: 0.3rem 1.6rem 0.3rem 0.4rem;
}

/* The operand of an operator, indented so the shape of a nested expression is
 * visible without reading it. */
.nested {
  border-left: 2px solid #374151;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  margin-left: 0.35rem;
  min-width: 0;
  padding-left: 0.5rem;
}

.pipeline {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

/* What a stage needs beyond its name, kept under it rather than beside it so
   the chain still reads as one column. */
.pipe-operand {
  border-left: 2px solid #374151;
  margin: 0 0 0.1rem 1.25rem;
  padding-left: 0.6rem;
}

.pipe-stage {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.pipe-step {
  align-items: center;
  display: flex;
  gap: 0.35rem;

  > select {
    flex: 1 1 auto;
    min-width: 0;
  }
}

/* Drawn rather than set in type: a glyph can only get taller, and what this
 * wants is a thicker stroke. Present enough to read as the spine of the chain,
 * quiet enough not to compete with the steps hanging off it. */
.pipe-arrow {
  align-items: center;
  color: #94a3b8;
  display: flex;
  flex: none;
  justify-content: center;
  width: 1.25rem;
}

/* What a link in the chain hands on, sitting at the end of the link's own row.
 * A quiet tag rather than a band: it is a note on the step, not another control. */
.stage-type {
  background: rgba(20, 184, 166, 0.1);
  border-radius: 3px;
  color: #5eead4;
  flex: none;
  font-family: monospace;
  font-size: 0.72rem;
  padding: 0.1rem 0.35rem;

  // Nothing here is wrong -- an entity Field's type is simply not known to the
  // editor -- so it is muted rather than alarming.
  &.unsure {
    background: rgba(148, 163, 184, 0.1);
    color: #9ca3af;
  }
}

/* At the right end of the step it removes, and bordered so it reads as a
 * control rather than as a stray glyph. */
.pipe-remove {
  align-items: center;
  background: none;
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #9ca3af;
  cursor: pointer;
  display: flex;
  flex: none;
  font-family: inherit;
  font-size: 0.85rem;
  height: 1.6rem;
  justify-content: center;
  line-height: 1;
  margin-left: auto;
  width: 1.6rem;

  &:hover {
    background: rgba(248, 113, 113, 0.12);
    border-color: #f87171;
    color: #fca5a5;
  }
}

.add-transform {
  align-items: center;
  align-self: flex-start;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #d1d5db;
  cursor: pointer;
  display: flex;
  font-family: inherit;
  font-size: 0.75rem;
  gap: 0.3rem;
  margin-top: 0.15rem;
  padding: 0.3rem 0.6rem;

  &:hover {
    background: rgba(20, 184, 166, 0.12);
    border-color: #14b8a6;
    color: #5eead4;
  }
}

.add-glyph {
  font-size: 0.95rem;
  font-weight: 700;
  line-height: 1;
}

.add,
.remove {
  background: none;
  border: none;
  color: #9ca3af;
  cursor: pointer;
  font-size: 0.75rem;
  padding: 0.2rem 0.3rem;

  &:hover {
    color: #eee;
  }
}

.add {
  align-self: flex-start;
}
</style>

<script lang="ts" setup>
/* Building an expression from "Arkham.Custom.Expr" without writing its JSON.
 *
 * An expression is one source -- a value, a property, something about the test
 * -- with any number of transforms hung off it. Transforms are not a kind of
 * expression you choose between; they are what you can do to whatever you have,
 * which is why they are offered on every expression and filtered by the type at
 * that point in the chain. */
import { computed, ref } from 'vue'
import BindingField from '@/arkham/components/debug/BindingField.vue'
import BindingToggle from '@/arkham/components/debug/BindingToggle.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'
import PropertyField from '@/arkham/components/debug/PropertyField.vue'
import type { Binding } from '@/arkham/customCardBindings'
import {
  SKILL_TEST_PROPS,
  expressionType,
  stageKey,
  stageResult,
  stagesFor,
  typeFits,
  QUERY_MODES,
  QUERY_NOUNS,
  entityOf,
  isNary,
  naryExtras,
  naryOperandProblem,
  RECORD_HOLDS,
  NARY_NAMES,
  propOptionsFor,
  stageProp,
  queryType,
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
    /* The matcher type behind each query kind, threaded down from the form the
     * same way the steps editor gets it. Without it a query cannot be built. */
    queryKinds?: Record<string, string>
  }>(),
  { expect: undefined },
)
const emit = defineEmits<{ 'update:modelValue': [v: any] }>()

/* Entity kinds name a Field on that entity, whose type the schema knows but
 * this editor does not. `card` is the one we can type. */
const KINDS = ['card', 'enemy', 'location', 'investigator', 'asset', 'act']

/* An example Field, for the box that is only reached before the schema has loaded
 * -- the names are the engine's own, so there is nothing to guess from. */
const FIELD_PLACEHOLDER: Record<string, string> = {
  investigator: 'InvestigatorTraits',
  enemy: 'EnemyHealth',
  location: 'LocationShroud',
  asset: 'AssetUses',
  act: 'ActClues',
}

/* What an expression can start as. Anything that takes exactly one operand is a
 * transform instead, so it is not repeated here. */
type Source = {
  key: string
  label: string
  shape: 'literal' | 'prop' | 'skillTest' | 'query' | 'record'
}

/* Named the way the step kinds are: the short word for the thing, with the
 * controls underneath saying what it does. A sentence in the dropdown says it
 * twice and makes the list slower to scan. */

const SOURCES: Source[] = [
  { key: '', label: 'Value', shape: 'literal' },
  { key: 'get', label: 'Property', shape: 'prop' },
  { key: 'skillTest', label: 'Skill test', shape: 'skillTest' },
  { key: 'query', label: 'Query', shape: 'query' },
  { key: 'recordSet', label: 'Campaign log set', shape: 'record' },
  { key: 'recordCount', label: 'Campaign log count', shape: 'record' },
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
  if (chosen.shape === 'query')
    return rebuild({ query: { kind: 'enemy', matcher: null }, mode: 'all' }, stages)
  if (chosen.shape === 'record') return rebuild({ [key]: homebrewKey('') }, stages)
  return rebuild({ [key]: [kept ?? null, null] }, stages)
}

/* A key the card writes for itself. A custom card's own log entries are homebrew
 * ones by definition, and the wrapper is what makes an arbitrary name a
 * CampaignLogKey; an official campaign's key is a constructor, and reaching one
 * of those is a raw JSON job. */
const homebrewKey = (name: string) => ({ tag: 'HomebrewCampaignLogKey', contents: name })

const recordKeyName = computed(() => {
  const v = source.value?.[currentSource.value.key]
  return v && typeof v === 'object' && v.tag === 'HomebrewCampaignLogKey' ? (v.contents ?? '') : ''
})

const setRecordKey = (name: string) => patch({ [currentSource.value.key]: homebrewKey(name) })

const recordIsHomebrew = computed(() => {
  const v = source.value?.[currentSource.value.key]
  return !v || (typeof v === 'object' && v.tag === 'HomebrewCampaignLogKey')
})

/* Whether the value field has been swapped for the binding picker. Only needed
 * while nothing is chosen yet: once a `$name` is in the value, that is what the
 * field holds and `isBindingText` says so on its own. */
const bindingInput = ref(false)

function useBinding(name: string | null) {
  setLiteral(name ?? '')
  // Cleared back to a value, so the field goes back to being one.
  if (!name) bindingInput.value = false
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

/* Whether a stage can take what the one before it hands over.
 *
 * An unknown incoming type fits everything, so this only fires where the editor
 * is sure -- reading `modifiedCost` off an Int, say, after a query was switched
 * from `first` to `count`. The stage is left in place rather than dropped: the
 * fix is the author's to make, and quietly rewriting it would move the surprise
 * from here to the game. */
const stageFits = (at: number) => {
  const key = keyAt(at)
  return !!key && stagesFor(pipelineTypes.value[at]).some((st) => st.name === key)
}

/* The operands a nary stage carries besides the one it is handed, edited in place.
 * Stored as the engine stores them -- one list, the handed value first -- so the
 * hole at index 0 is left alone. */
const setNaryExtra = (at: number, which: number, value: any) => {
  const stage = stageAt(at)
  const key = NARY_NAMES.find((n) => n in (stage ?? {}))
  if (!key) return
  const held = [...(stage[key] as any[])]
  held[which + 1] = value
  patchStage(at, { [key]: held })
}

const addNaryExtra = (at: number) => {
  const stage = stageAt(at)
  const key = NARY_NAMES.find((n) => n in (stage ?? {}))
  if (!key) return
  patchStage(at, { [key]: [...(stage[key] as any[]), null] })
}

const removeNaryExtra = (at: number, which: number) => {
  const stage = stageAt(at)
  const key = NARY_NAMES.find((n) => n in (stage ?? {}))
  if (!key) return
  const held = (stage[key] as any[]).filter((_, i) => i !== which + 1)
  patchStage(at, { [key]: held })
}

/* What is wrong with a transform's own operands, if anything: said under the step
 * and marked on it, because a join between two different lists is something the
 * runner will do without complaint. */
const stageOperandProblems = (at: number): string[] =>
  naryExtras(stageAt(at))
    .map((extra) =>
      naryOperandProblem(keyAt(at), pipelineTypes.value[at], expressionType(extra, props.bindings ?? [])),
    )
    .filter((problem): problem is string => !!problem)

const stageIsWrong = (at: number) => !stageFits(at) || stageOperandProblems(at).length > 0

const stageLabel = (at: number) =>
  stageOptions(at).find((st) => st.name === keyAt(at))?.label ?? keyAt(at)

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

// Which property a `get` stage reads, chosen in a field of its own beside it.
const setStageProp = (at: number, prop: string) =>
  patchStage(at, { get: prop, kind: stageAt(at)?.kind ?? 'card' })

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

/* Why there is no transform to add, when there is not.
 *
 * Said on a dead button rather than by taking the button away: a control that
 * vanishes leaves you wondering whether you missed it, where a greyed one with a
 * reason answers the question -- most usefully when the reason is that nothing can
 * be read off the type you have arrived at. */
const noStageReason = computed(() => {
  if (source.value === null && pipeline.value.stages.length === 0) {
    return 'Give this a value first'
  }
  const incoming = pipelineTypes.value[pipeline.value.stages.length]
  if (stagesFor(incoming).length === 0) {
    return incoming
      ? `Nothing can be read off ${incoming}`
      : 'Nothing can be read off this yet'
  }
  return ''
})


// --- shapes with operands of their own ---








const propsFor = computed(() => propOptionsFor(source.value?.kind ?? 'card') ?? null)
</script>

<template>
  <div class="expr">
    <span v-if="label" class="expr-label">{{ label }}</span>

    <div class="row">
      <select
        class="source-kind"
        :value="currentSource.key"
        @change="pickSource(($event.target as HTMLSelectElement).value)"
      >
        <option v-for="o in SOURCES" :key="o.key" :value="o.key">{{ o.label }}</option>
      </select>

      <!-- A value or a binding, never both at once: the binding picker is what
           the field becomes when you ask for it, the way it is everywhere else in
           the builder. Showing the two side by side made every number look like it
           had a second, empty field attached. -->
      <template v-if="currentSource.shape === 'literal'">
        <BindingField
          v-if="isBindingText || bindingInput"
          clearable
          class="grow"
          :modelValue="isBindingText ? literalText.trim() : null"
          :applicable="applicableBindings"
          :inScope="bindings"
          :type="expect ?? 'anything'"
          @update:modelValue="useBinding($event)"
        />
        <div v-else class="value-box grow">
          <input
            :value="literalText"
            :placeholder="expect === 'Int' ? 'a number' : 'a value'"
            @input="setLiteral(($event.target as HTMLInputElement).value)"
            @keydown.stop
          />
          <BindingToggle
            :open="false"
            :count="applicableBindings.length"
            :type="expect ?? 'anything'"
            @toggle="bindingInput = true"
          />
        </div>
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
          :of="`a ${source?.kind ?? 'card'}`"
          :prefix="entityOf(source?.kind)"
          @update:modelValue="patch({ get: $event })"
        />
        <label v-else>
          Field
          <input
            :value="source?.get"
            :placeholder="FIELD_PLACEHOLDER[source?.kind ?? ''] ?? 'SomeField'"
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


      <template v-else-if="currentSource.shape === 'record'">
        <label v-if="recordIsHomebrew" class="grow">
          Key
          <input
            :value="recordKeyName"
            placeholder="TraitsLearned"
            @input="setRecordKey(($event.target as HTMLInputElement).value)"
            @keydown.stop
          />
        </label>
        <ValueEditor
          v-else
          class="grow"
          :bindings="bindings"
          type="CampaignLogKey"
          label="Key"
          :modelValue="source?.[currentSource.key]"
          @update:modelValue="patch({ [currentSource.key]: $event })"
        />
        <!-- Nothing can work out what a set holds: the log's generic entry is any
             JSON at all. Saying so is what lets a join be checked. -->
        <label v-if="currentSource.key === 'recordSet'">
          of
          <select
            :value="source?.holds ?? ''"
            @change="patch({ holds: ($event.target as HTMLSelectElement).value || undefined })"
          >
            <option value="">anything</option>
            <option v-for="h in RECORD_HOLDS" :key="h" :value="h">{{ h }}</option>
          </select>
        </label>
      </template>

    </div>

    <!-- A set and a count are two different things the log stores under a key,
         and picking the wrong one is silent, so the difference is spelled out. -->
    <p v-if="currentSource.shape === 'record'" class="record-hint">
      {{
        currentSource.key === 'recordSet'
          ? 'The entries recorded under that key, so they can be counted or filtered. Crossed-out entries are left out.'
          : 'The number recorded under that key — what the log counts, not how many entries the set has.'
      }}
    </p>

    <!-- "search: cards  get: first / that match ...", read left to right and then
         down. A block of its own because a query is a small thing entire, not two
         controls sharing a row with the field above. -->
    <div v-if="currentSource.shape === 'query'" class="query-block">
      <div class="phrase">
        <span class="phrase-label">search</span>
        <select
          :value="source?.query?.kind ?? 'enemy'"
          @change="patch({ query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
        >
          <option v-for="(_, kind) in queryKinds ?? {}" :key="kind" :value="kind">
            {{ QUERY_NOUNS[kind] ?? kind }}
          </option>
        </select>
        <span class="phrase-label">get</span>
        <select
          :value="source?.mode ?? 'all'"
          @change="patch({ mode: ($event.target as HTMLSelectElement).value })"
        >
          <option v-for="m in QUERY_MODES" :key="m.key" :value="m.key">{{ m.label }}</option>
        </select>
        <code class="stage-type" :class="{ unsure: !pipelineTypes[0] }">
          {{ pipelineTypes[0] ?? '?' }}
        </code>
      </div>
      <ValueEditor
        :type="(queryKinds ?? {})[source?.query?.kind ?? 'enemy'] ?? 'EnemyMatcher'"
        label="that match"
        :bindings="bindings"
        :modelValue="source?.query?.matcher"
        @update:modelValue="patch({ query: { ...(source?.query ?? { kind: 'enemy' }), matcher: $event } })"
      />
    </div>

    <div v-if="currentSource.shape === 'prop'" class="nested">
      <ExpressionEditor
        :modelValue="source?.of"
        :bindings="bindings"
        label="of"
        @update:modelValue="patch({ of: $event })"
      />
    </div>


    <!-- What can be done to whatever the source is, in the order it happens. -->
    <div class="pipeline">
      <div v-for="(stage, at) in pipeline.stages" :key="at" class="pipe-stage">
        <div class="pipe-step" :class="{ invalid: stageIsWrong(at) }">
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
          <PropertyField
            v-if="keyAt(at) === 'get' && propOptionsFor(stageAt(at)?.kind)"
            class="fit"
            :modelValue="stageProp(stageAt(at))"
            :options="propOptionsFor(stageAt(at)?.kind)!"
            :of="`a ${stageAt(at)?.kind ?? 'card'}`"
            :prefix="entityOf(stageAt(at)?.kind)"
            @update:modelValue="setStageProp(at, $event)"
          />
          <!-- An entity's Field, which the served schema does not carry, so it is
               typed rather than chosen. Named the way the engine names it. -->
          <input
            v-else-if="keyAt(at) === 'get'"
            class="fit"
            :value="stageProp(stageAt(at)) ?? ''"
            :placeholder="FIELD_PLACEHOLDER[stageAt(at)?.kind ?? ''] ?? 'SomeField'"
            @input="setStageProp(at, ($event.target as HTMLInputElement).value)"
            @keydown.stop
          />
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
        <p v-if="!stageFits(at)" class="pipe-error">
          <code>{{ pipelineTypes[at] }}</code> is not something “{{ stageLabel(at) }}” can be
          asked for.
        </p>
        <p v-for="problem in stageOperandProblems(at)" :key="problem" class="pipe-error">
          {{ problem }}.
        </p>

        <div v-if="keyAt(at) === 'filter'" class="pipe-operand">
          <ExpressionEditor
            :modelValue="stagePredicateOperand(at)"
            :bindings="bindings"
            label="compared with"
            @update:modelValue="setStagePredicateOperand(at, $event)"
          />
        </div>

        <!-- What it is added to, divided by, joined with. The value handed down the
             chain is the first operand and is not shown again here; these are the
             rest of them. -->
        <div v-if="isNary(keyAt(at))" class="pipe-operand">
          <div v-for="(extra, which) in naryExtras(stageAt(at))" :key="which" class="extra">
            <ExpressionEditor
              :modelValue="extra"
              :bindings="bindings"
              :queryKinds="queryKinds"
              :expect="keyAt(at) === 'concat' ? undefined : 'Int'"
              :label="stageLabel(at)"
              @update:modelValue="setNaryExtra(at, which, $event)"
            />
            <button
              v-if="naryExtras(stageAt(at)).length > 1"
              type="button"
              class="remove"
              title="Remove this value"
              @click="removeNaryExtra(at, which)"
            >
              ×
            </button>
          </div>
          <button type="button" class="add" @click="addNaryExtra(at)">+ Value</button>
        </div>
      </div>

      <button
        type="button"
        class="add-transform"
        :disabled="!!noStageReason"
        :title="noStageReason || 'Do something to what you have'"
        @click="addStage"
      >
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

// Sized to its own words rather than sharing the row equally: it names the kind
// of expression, and the controls it brings with it are the point.
.source-kind {
  flex: 0 1 auto;
  width: auto;
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

/* The box the toggle is a segment of.
 *
 * The border and the padding belong to this wrapper, not to the input inside it.
 * The toggle's own negative margins are cut to exactly this padding, which is
 * what lets it reach the box's edges and read as a segment divided from the value
 * -- outside a box like this it is a button parked next to the field. Same
 * metrics as ValueEditor's field box, because it is the same control. */
.value-box {
  align-items: stretch;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  display: flex;
  min-width: 0;
  overflow: hidden;
  padding: 0.35rem 0.5rem;

  > input {
    background: transparent;
    border-color: transparent;
    flex: 1 1 auto;
    min-width: 0;
    padding: 0;
  }

  &:focus-within {
    border-color: #6b7280;
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

/* A query set apart from the row above it: its own edge, its own ground, and the
 * phrase and the matcher inside it rather than split across the boundary. */
.query-block {
  background: rgba(148, 163, 184, 0.06);
  border: 1px solid #374151;
  border-left: 2px solid #64748b;
  border-radius: 0 4px 4px 0;
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
  padding: 0.5rem 0.6rem;
}

/* Sized to their words, so the pair reads as one line rather than as two fields
 * that happen to sit side by side. */
.phrase {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;

  > select {
    flex: 0 1 auto;
    width: auto;
  }
}

/* Beside its control rather than above it: these are two words in a line, not
 * captioned fields like the rest of the form. */
.phrase-label {
  color: #9ca3af;
  font-size: 0.72rem;

  &:not(:first-child) {
    margin-left: 0.35rem;
  }
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

/* A property step reads as two things in sequence -- "a property of it", then
 * the property -- so each takes the width of its own words. Left to stretch, the
 * select shoves the property to the far edge of the step and the pair stops
 * reading as one phrase. */
.pipe-step:has(> .fit) > select {
  flex: 0 1 auto;
  width: auto;
}

.pipe-step > .fit {
  flex: 0 1 auto;
  min-width: 0;
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

/* A stage handed something it cannot take. Marked rather than corrected, and
 * marked on the control that is wrong, not on the whole expression. */
.pipe-step.invalid > select {
  border-color: #f87171;
}

.pipe-error {
  color: #fca5a5;
  font-size: 0.72rem;
  margin: 0;
  // Clears the arrow column, so the note lines up under the stage it is about.
  padding-left: 1.6rem;

  code {
    font-family: monospace;
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

.add-transform:disabled {
  border-style: dashed;
  color: #4b5563;
  cursor: not-allowed;

  &:hover {
    background: #111827;
    border-color: #4b5563;
    color: #4b5563;
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

/* A transform's own operands, under the step that uses them: what it is added to,
   divided by, joined with. One each, with a way to add more where the operator
   takes any number. */
.extra {
  align-items: flex-start;
  display: flex;
  gap: 0.25rem;
  min-width: 0;

  > .expr {
    flex: 1;
    min-width: 0;
  }
}

.record-hint {
  color: #9ca3af;
  font-size: 0.72rem;
  margin: 0.15rem 0 0;
}

.add-glyph {
  font-size: 0.95rem;
  font-weight: 700;
  line-height: 1;
}

.add,
/* Beside the item it removes, and the same height as it: a bare glyph in a row
 * of fields shrinks to the size of the character. */
.remove {
  align-items: center;
  align-self: flex-start;
  background: none;
  border: 1px solid transparent;
  border-radius: 4px;
  color: #9ca3af;
  cursor: pointer;
  display: flex;
  flex: none;
  font-size: 0.85rem;
  line-height: 1;
  height: 1.9rem;
  padding: 0 0.4rem;

  &:hover {
    border-color: #f87171;
    color: #fca5a5;
  }
}

.add {
  align-self: flex-start;
}
</style>

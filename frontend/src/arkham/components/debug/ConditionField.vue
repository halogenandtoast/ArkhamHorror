<script setup lang="ts">
/* What an If, a When or a Case branch asks.
 *
 * `runReadCondition` reads a condition five ways, and the editor only ever
 * offered one of them — the query — so a card written with a criterion, a
 * source test or a comparison could be saved but not read back, and opening it
 * in the builder would quietly rewrite it to an empty query.
 *
 * The shape says which form is meant, exactly as the engine decides: a
 * `criteria` key is a criterion, a `source`/`matches` pair is a question about
 * a source, a comparison key is two values compared, and anything else is a
 * query. So nothing is stored alongside to remember the choice.
 */
import type { Binding } from '@/arkham/customCardBindings'
import ExpressionEditor from '@/arkham/components/debug/ExpressionEditor.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'

const props = defineProps<{
  modelValue: any
  bindings?: Binding[]
  queryKinds: Record<string, string>
}>()

const emit = defineEmits<{ 'update:modelValue': [v: any] }>()

/* In the order the engine tries them, so a condition that names two keys reads
 * here the way it will run. */
const COMPARISONS = ['eq', 'ne', 'lt', 'lte', 'gt', 'gte'] as const

const FORMS = [
  { key: 'query', label: 'something is on the table' },
  { key: 'criteria', label: 'a criterion holds' },
  { key: 'source', label: 'the source matches' },
  { key: 'compare', label: 'two values compare' },
] as const

const COMPARISON_LABELS: Record<string, string> = {
  eq: 'is',
  ne: 'is not',
  lt: 'is less than',
  lte: 'is at most',
  gt: 'is more than',
  gte: 'is at least',
}

const value = () => props.modelValue ?? {}

const formOf = (): string => {
  const v = value()
  if ('criteria' in v) return 'criteria'
  // A bare criterion: nothing else here is a tagged object, and the engine
  // reads one as a criterion, so the editor has to as well or it rewrites it
  // to an empty query on save.
  if ('tag' in v) return 'criteria'
  if ('source' in v || 'matches' in v) return 'source'
  if (COMPARISONS.some((c) => c in v)) return 'compare'
  return 'query'
}

const criterionOf = () => {
  const v = value()
  return 'criteria' in v ? v.criteria : 'tag' in v ? v : null
}

const comparisonOf = (): string => COMPARISONS.find((c) => c in value()) ?? 'eq'

const pair = (): [any, any] => {
  const held = value()[comparisonOf()]
  return Array.isArray(held) ? [held[0] ?? null, held[1] ?? null] : [null, null]
}

const blank = (form: string) => {
  switch (form) {
    case 'criteria':
      return { criteria: null }
    case 'source':
      return { source: '$source', matches: null }
    case 'compare':
      return { eq: [null, null] }
    default:
      return { kind: 'enemy', matcher: null }
  }
}

const setForm = (form: string) => emit('update:modelValue', blank(form))

const setComparison = (comparison: string) =>
  emit('update:modelValue', { [comparison]: pair() })

const setSide = (side: 0 | 1, v: any) => {
  const [left, right] = pair()
  emit('update:modelValue', { [comparisonOf()]: side === 0 ? [v, right] : [left, v] })
}

const matcherType = (kind: string | undefined) => props.queryKinds[kind ?? 'enemy'] ?? 'EnemyMatcher'
</script>

<template>
  <div class="condition">
    <label>
      Asks whether
      <select :value="formOf()" @change="setForm(($event.target as HTMLSelectElement).value)">
        <option v-for="form in FORMS" :key="form.key" :value="form.key">{{ form.label }}</option>
      </select>
    </label>

    <template v-if="formOf() === 'query'">
      <label>
        Kind
        <select
          :value="modelValue?.kind ?? 'enemy'"
          @change="emit('update:modelValue', { kind: ($event.target as HTMLSelectElement).value, matcher: null })"
        >
          <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
        </select>
      </label>
      <ValueEditor
        :bindings="bindings"
        :type="matcherType(modelValue?.kind)"
        label="Matcher"
        :modelValue="modelValue?.matcher"
        @update:modelValue="emit('update:modelValue', { ...modelValue, matcher: $event })"
      />
      <p class="hint">Holds when the matcher finds anything at all.</p>
    </template>

    <template v-else-if="formOf() === 'criteria'">
      <ValueEditor
        :bindings="bindings"
        type="Criterion"
        label="Criterion"
        :modelValue="criterionOf()"
        @update:modelValue="emit('update:modelValue', { criteria: $event })"
      />
      <p class="hint">
        What the card says about the situation — "if you are engaged with an enemy", "during your
        turn" — rather than what is on the table.
      </p>
    </template>

    <template v-else-if="formOf() === 'source'">
      <ExpressionEditor
        :queryKinds="queryKinds"
        :bindings="bindings"
        label="Which source"
        expect="Source"
        :modelValue="modelValue?.source"
        @update:modelValue="emit('update:modelValue', { ...modelValue, source: $event })"
      />
      <ValueEditor
        :bindings="bindings"
        type="SourceMatcher"
        label="Matches"
        :modelValue="modelValue?.matches"
        @update:modelValue="emit('update:modelValue', { ...modelValue, matches: $event })"
      />
      <p class="hint">
        A window carries the source of what it is reacting to, and no matcher kind selects sources,
        so this is the only way to ask about one.
      </p>
    </template>

    <template v-else>
      <ExpressionEditor
        :queryKinds="queryKinds"
        :bindings="bindings"
        label="This"
        :modelValue="pair()[0]"
        @update:modelValue="setSide(0, $event)"
      />
      <label>
        <select :value="comparisonOf()" @change="setComparison(($event.target as HTMLSelectElement).value)">
          <option v-for="c in COMPARISONS" :key="c" :value="c">{{ COMPARISON_LABELS[c] }}</option>
        </select>
      </label>
      <ExpressionEditor
        :queryKinds="queryKinds"
        :bindings="bindings"
        label="That"
        :modelValue="pair()[1]"
        @update:modelValue="setSide(1, $event)"
      />
      <p class="hint">
        A property read off a binding is not something the game can be asked about, so comparing one
        is the only way to branch on it. The orderings read both sides as numbers.
      </p>
    </template>
  </div>
</template>

<style scoped>
.condition {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}

.hint {
  color: #9ca3af;
  font-size: 0.72rem;
  margin: 0;
}
</style>

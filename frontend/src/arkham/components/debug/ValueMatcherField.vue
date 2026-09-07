<script lang="ts" setup>
/* A test against a number: "at least 2", "between 1 and 3", "any value".
 *
 * Stored as a comparison wrapping a `GameValue`, which in practice is always
 * `Static n` -- three constructors deep for three words. Here it is one field:
 * what kind of test on the left, and whatever that test needs on the right.
 *
 * Every shape a ValueMatcher has is in the dropdown, so the field never hands
 * the author back to the raw constructor picker. Between grows a second number,
 * any value has none, and one-of nests this same control.
 */
import { computed, ref } from 'vue'
import BindingField from '@/arkham/components/debug/BindingField.vue'
import BindingToggle from '@/arkham/components/debug/BindingToggle.vue'
import type { Binding } from '@/arkham/customCardBindings'
import { typeFits } from '@/arkham/customCardExpressions'

const props = defineProps<{ modelValue: any; bindings?: Binding[] }>()
const emit = defineEmits<{ 'update:modelValue': [v: any] }>()

const COMPARISONS = ['GreaterThanOrEqualTo', 'LessThanOrEqualTo', 'EqualTo', 'GreaterThan', 'LessThan']

const OPTIONS = [
  { tag: 'GreaterThanOrEqualTo', label: 'at least' },
  { tag: 'LessThanOrEqualTo', label: 'at most' },
  { tag: 'EqualTo', label: 'exactly' },
  { tag: 'GreaterThan', label: 'more than' },
  { tag: 'LessThan', label: 'fewer than' },
  { tag: 'Between', label: 'between' },
  { tag: 'AnyValue', label: 'any value' },
  { tag: 'GameValueOneOf', label: 'one of' },
]

const tag = computed<string>(() => {
  const v = props.modelValue
  return v && typeof v === 'object' && OPTIONS.some((o) => o.tag === v.tag)
    ? v.tag
    : 'GreaterThanOrEqualTo'
})

const label = computed(() => OPTIONS.find((o) => o.tag === tag.value)?.label ?? '')

/* A GameValue is `Static n`, and the n may be a binding: the name goes inside
 * the Static because a bare number is not a GameValue and would not decode. */
const gv = (v: any) => (v && typeof v === 'object' && v.tag === 'Static' ? v.contents : null)
const mkGv = (amount: any) => ({ tag: 'Static', contents: amount })

const operands = computed<any[]>(() => {
  const v = props.modelValue
  if (!v || typeof v !== 'object') return []
  if (COMPARISONS.includes(v.tag)) return [gv(v.contents)]
  if (v.tag === 'Between') return (v.contents ?? []).map(gv)
  return []
})

const branches = computed<any[]>(() =>
  tag.value === 'GameValueOneOf' && Array.isArray(props.modelValue?.contents)
    ? props.modelValue.contents
    : [],
)

const isBinding = (v: any) => typeof v === 'string' && v.startsWith('$')

const intBindings = computed(() => (props.bindings ?? []).filter((b) => typeFits(b.type, 'Int')))

/* One toggle per number, so "between $low and 3" is sayable. Kept by position,
 * which is enough for a field that never has more than two. */
const asBinding = ref<boolean[]>([])
const showBinding = (at: number) => asBinding.value[at] || isBinding(operands.value[at])

function write(nextTag: string, amounts: any[]) {
  if (nextTag === 'AnyValue') return emit('update:modelValue', { tag: 'AnyValue', contents: [] })
  if (nextTag === 'GameValueOneOf')
    return emit('update:modelValue', { tag: 'GameValueOneOf', contents: branches.value })
  if (nextTag === 'Between')
    return emit('update:modelValue', {
      tag: 'Between',
      contents: [mkGv(amounts[0] ?? 1), mkGv(amounts[1] ?? 1)],
    })
  emit('update:modelValue', { tag: nextTag, contents: mkGv(amounts[0] ?? 1) })
}

const setTag = (next: string) => write(next, operands.value)
const setAmount = (at: number, text: string) => {
  const next = [...operands.value]
  next[at] = text.trim() === '' ? 0 : Number(text)
  write(tag.value, next)
}
const setBinding = (at: number, name: string | null) => {
  const next = [...operands.value]
  if (!name) asBinding.value[at] = false
  next[at] = name ?? 1
  write(tag.value, next)
}

const setBranch = (at: number, v: any) =>
  emit('update:modelValue', {
    tag: 'GameValueOneOf',
    contents: branches.value.map((b, i) => (i === at ? v : b)),
  })
const addBranch = () =>
  emit('update:modelValue', {
    tag: 'GameValueOneOf',
    contents: [...branches.value, { tag: 'GreaterThanOrEqualTo', contents: mkGv(1) }],
  })
const removeBranch = (at: number) =>
  emit('update:modelValue', {
    tag: 'GameValueOneOf',
    contents: branches.value.filter((_, i) => i !== at),
  })
</script>

<template>
  <div class="matcher-wrap">
    <div class="matcher">
      <select
        class="op"
        :value="tag"
        :title="label"
        @change="setTag(($event.target as HTMLSelectElement).value)"
      >
        <option v-for="o in OPTIONS" :key="o.tag" :value="o.tag">{{ o.label }}</option>
      </select>

      <template v-if="tag !== 'AnyValue' && tag !== 'GameValueOneOf'">
        <span class="rule" aria-hidden="true"></span>

        <template v-for="(_, at) in tag === 'Between' ? 2 : 1" :key="at">
          <span v-if="at > 0" class="joiner">and</span>
          <BindingField
            v-if="showBinding(at)"
            class="grow"
            :modelValue="isBinding(operands[at]) ? operands[at] : null"
            :applicable="intBindings"
            :inScope="bindings"
            type="Int"
            @update:modelValue="setBinding(at, $event)"
          />
          <input
            v-else
            type="number"
            :value="operands[at] ?? ''"
            placeholder="a number"
            @input="setAmount(at, ($event.target as HTMLInputElement).value)"
            @keydown.stop
          />
          <!-- A segment of the field, like it is everywhere else in the builder. -->
          <BindingToggle
            :open="!!showBinding(at)"
            :count="intBindings.length"
            type="Int"
            @toggle="asBinding[at] = !showBinding(at)"
          />
        </template>
      </template>
    </div>

    <!-- One-of holds tests of its own, so it nests this same control. -->
    <div v-if="tag === 'GameValueOneOf'" class="branches">
      <div v-for="(branch, at) in branches" :key="at" class="branch">
        <ValueMatcherField
          class="grow"
          :modelValue="branch"
          :bindings="bindings"
          @update:modelValue="setBranch(at, $event)"
        />
        <button type="button" class="remove" title="Remove this one" @click="removeBranch(at)">
          ×
        </button>
      </div>
      <button type="button" class="add" @click="addBranch">+ Add</button>
    </div>
  </div>
</template>

<style scoped lang="scss">
.matcher-wrap {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  min-width: 0;
}

/* One field: the border is on the row and everything inside goes transparent, so
 * the test and its numbers read as parts of a control rather than as controls
 * that happen to touch. */
.matcher {
  align-items: stretch;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  display: flex;
  gap: 0.25rem;
  overflow: hidden;
  padding: 0.35rem 0.5rem;

  &:focus-within {
    border-color: #6b7280;
  }

  > input {
    background: transparent;
    border: 1px solid transparent;
    color: #eee;
    flex: 1 1 auto;
    font-family: inherit;
    min-width: 3rem;
    padding: 0;

    &:focus {
      outline: none;
    }
  }

  > .grow {
    flex: 1 1 auto;
    min-width: 0;
  }
}

/* Sized to its own words, and keeping its caret: unlike the numbers beside it
 * this one opens something, and the marker is what says so. */
.op {
  -webkit-appearance: none;
  appearance: none;
  background: transparent var(--select-caret) no-repeat right 0 center;
  background-size: var(--select-caret-size);
  border: 1px solid transparent;
  color: #eee;
  cursor: pointer;
  flex: 0 1 auto;
  font-family: inherit;
  font-size: inherit;
  padding: 0 0.9rem 0 0;
  width: auto;

  &:focus {
    outline: none;
  }
}

// Divides the test from what it tests against.
.rule {
  align-self: center;
  background: #374151;
  flex: none;
  height: 1.1rem;
  width: 1px;
}

.joiner {
  align-self: center;
  color: #9ca3af;
  flex: none;
  font-size: 0.75rem;
}

.branches {
  border-left: 2px solid #374151;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  margin-left: 0.2rem;
  padding-left: 0.6rem;
}

.branch {
  align-items: flex-start;
  display: flex;
  gap: 0.25rem;

  > .grow {
    flex: 1 1 auto;
    min-width: 0;
  }
}

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
  height: 1.9rem;
  line-height: 1;
  padding: 0 0.4rem;

  &:hover {
    border-color: #f87171;
    color: #fca5a5;
  }
}

.add {
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  font-family: inherit;
  font-size: 0.72rem;
  padding: 0.2rem 0.5rem;
}
</style>

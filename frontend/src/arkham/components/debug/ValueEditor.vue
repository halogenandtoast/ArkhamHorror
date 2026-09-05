<script lang="ts" setup>
/* One node of the ability editor: renders a value of a named Haskell type.
 *
 * A sum type becomes a searchable constructor picker plus an editor per field,
 * each of which is another ValueEditor — that recursion is what lets you drill
 * from an ability down through a window into the matchers inside it. Types the
 * schema does not cover fall back to a raw JSON field, so nothing is a dead end.
 *
 * A handful of types carry hand-written JSON instances that do not follow the
 * generic encoding (Actions, Trait, CardCode). Those come through as raw fields;
 * the encoding here is the generic one. */
import { computed, ref } from 'vue'
import { onClickOutside, useEventListener } from '@vueuse/core'
import { decodeConstructor, encodeConstructor, shapeOf, type ConSchema } from '@/arkham/schema'

const props = defineProps<{ type: string; modelValue: any; label?: string }>()
const emit = defineEmits<{ 'update:modelValue': [value: any] }>()

const shape = computed(() => shapeOf(props.type))
const search = ref('')
const open = ref(false)
const pickerEl = ref<HTMLElement | null>(null)

// A dropdown should close when you look away from it, or press escape. The key
// is watched on the document rather than the search box, so it works wherever
// focus happens to be -- and because two @keydown bindings on one element would
// silently drop one of them.
onClickOutside(pickerEl, () => closePicker())

useEventListener(document, 'keydown', (event: KeyboardEvent) => {
  if (event.key !== 'Escape' || !open.value) return
  event.stopPropagation()
  event.preventDefault()
  closePicker()
})

function closePicker() {
  open.value = false
  search.value = ''
}

// Every optional value can be taken back out.
const hasValue = computed(() => props.modelValue !== null && props.modelValue !== undefined)

/* A field can hold a binding instead of a value -- $iid, $source, or anything a
 * query step bound. It is substituted for the real thing before the card is
 * decoded, so the editor shows it as itself rather than trying and failing to
 * read it as a constructor. */
const binding = computed(() =>
  typeof props.modelValue === 'string' && props.modelValue.startsWith('$') ? props.modelValue : null,
)

const bindingInput = ref(false)

function setBinding(name: string) {
  const trimmed = name.trim()
  emit('update:modelValue', trimmed ? (trimmed.startsWith('$') ? trimmed : `$${trimmed}`) : null)
  bindingInput.value = false
}

function clear() {
  emit('update:modelValue', null)
  closePicker()
}

const current = computed(() =>
  shape.value.kind === 'sum' ? decodeConstructor(shape.value.schema, props.modelValue) : null,
)

/* Constructors are CamelCase but people search for them the way the card reads
 * — "game ends", "victory display". Match on the squashed form so spacing and
 * punctuation never matter, and require every word rather than the whole
 * phrase so word order does not either. */
const humanize = (name: string) => name.replace(/([a-z0-9])([A-Z])/g, '$1 $2').replace(/_/g, ' ')
const squash = (text: string) => text.toLowerCase().replace(/[^a-z0-9]/g, '')

const constructors = computed(() => {
  if (shape.value.kind !== 'sum') return []
  const all = shape.value.schema.constructors
  const words = search.value.trim().split(/\s+/).map(squash).filter(Boolean)
  if (!words.length) return all
  return all.filter((c) => {
    const haystack = squash(c.name)
    return words.every((word) => haystack.includes(word))
  })
})

function pick(con: ConSchema) {
  if (shape.value.kind !== 'sum') return
  // Keep any field values that carry over to the new constructor by name.
  const previous = current.value?.values ?? {}
  const values: Record<string, any> = {}
  con.fields.forEach((field, index) => {
    const key = field.name ?? String(index)
    values[key] = previous[key] ?? null
  })
  emit('update:modelValue', encodeConstructor(shape.value.schema, con, values))
  closePicker()
}

function setField(key: string, value: any) {
  if (!current.value) return
  if (shape.value.kind !== 'sum') return
  emit(
    'update:modelValue',
    encodeConstructor(shape.value.schema, current.value.con, { ...current.value.values, [key]: value }),
  )
}

const fieldKey = (field: { name: string | null }, index: number) => field.name ?? String(index)

// --- list ---

const items = computed<any[]>(() => (Array.isArray(props.modelValue) ? props.modelValue : []))

function setItem(index: number, value: any) {
  const next = [...items.value]
  next[index] = value
  emit('update:modelValue', next)
}

function addItem() {
  emit('update:modelValue', [...items.value, null])
}

function removeItem(index: number) {
  emit('update:modelValue', items.value.filter((_, i) => i !== index))
}

// --- raw ---

const rawText = ref<string | null>(null)
const rawError = ref(false)

const rawValue = computed(() =>
  rawText.value ?? (props.modelValue === undefined ? '' : JSON.stringify(props.modelValue)),
)

function setRaw(text: string) {
  rawText.value = text
  try {
    emit('update:modelValue', text.trim() === '' ? null : JSON.parse(text))
    rawError.value = false
  } catch {
    rawError.value = true
  }
}
</script>

<template>
  <div class="value-editor">
    <label v-if="label" class="value-label">{{ label }}</label>

    <div v-if="binding" class="picked-row">
      <span class="binding">{{ binding }}</span>
      <button type="button" class="clear-value" title="Clear" @click="clear">×</button>
    </div>

    <div v-else-if="bindingInput" class="picked-row">
      <input
        type="text"
        placeholder="iid, source, chosen…"
        autofocus
        @keydown.enter.prevent="setBinding(($event.target as HTMLInputElement).value)"
        @blur="setBinding(($event.target as HTMLInputElement).value)"
        @keydown.stop
      />
    </div>

    <template v-else>

    <template v-if="shape.kind === 'sum'">
      <div ref="pickerEl" class="picker">
        <div class="picked-row">
          <button type="button" class="picked" @click="open ? closePicker() : (open = true)">
            {{ current ? humanize(current.con.name) : `Choose ${shape.schema.name}…` }}
          </button>
          <button v-if="hasValue" type="button" class="clear-value" title="Clear" @click="clear">×</button>
        </div>
        <div v-if="open" class="picker-menu">
          <input
            v-model="search"
            type="search"
            :placeholder="`Search ${shape.schema.constructors.length} options`"
            autofocus
            @keydown.stop
          />
          <ul>
            <li v-for="con in constructors" :key="con.name">
              <button type="button" @click="pick(con)">
                {{ humanize(con.name) }}
                <small>
                  {{ con.name }}<template v-if="con.fields.length">
                    · {{ con.fields.map((f) => f.type).join(', ') }}</template>
                </small>
              </button>
            </li>
          </ul>
        </div>
      </div>

      <div v-if="current && current.con.fields.length" class="fields">
        <ValueEditor
          v-for="(field, index) in current.con.fields"
          :key="fieldKey(field, index)"
          :type="field.type"
          :label="field.name ?? field.type"
          :modelValue="current.values[fieldKey(field, index)]"
          @update:modelValue="setField(fieldKey(field, index), $event)"
        />
      </div>
    </template>

    <template v-else-if="shape.kind === 'list'">
      <div class="list">
        <div v-for="(item, index) in items" :key="index" class="list-item">
          <ValueEditor :type="shape.inner" :modelValue="item" @update:modelValue="setItem(index, $event)" />
          <button type="button" class="remove" @click="removeItem(index)">×</button>
        </div>
        <button type="button" class="add" @click="addItem">+ Add</button>
      </div>
    </template>

    <template v-else-if="shape.kind === 'maybe'">
      <!-- An optional field is simply unset when its value is null, which is
           what the inner editor produces before anything is chosen. -->
      <div class="maybe">
        <ValueEditor
          :type="shape.inner"
          :modelValue="modelValue"
          @update:modelValue="emit('update:modelValue', $event)"
        />
        <button
          v-if="modelValue !== null && modelValue !== undefined"
          type="button"
          class="clear"
          @click="emit('update:modelValue', null)"
        >
          Clear
        </button>
      </div>
    </template>

    <div v-else-if="shape.kind === 'text'" class="picked-row">
      <input
        type="text"
        :value="modelValue ?? ''"
        @input="emit('update:modelValue', ($event.target as HTMLInputElement).value)"
        @keydown.stop
      />
      <button v-if="hasValue" type="button" class="clear-value" title="Clear" @click="clear">×</button>
    </div>

    <div v-else-if="shape.kind === 'number'" class="picked-row">
      <input
        type="number"
        :value="modelValue ?? ''"
        @input="emit('update:modelValue', Number(($event.target as HTMLInputElement).value))"
        @keydown.stop
      />
      <button v-if="hasValue" type="button" class="clear-value" title="Clear" @click="clear">×</button>
    </div>

    <label v-else-if="shape.kind === 'bool'" class="inline">
      <input
        type="checkbox"
        :checked="!!modelValue"
        @change="emit('update:modelValue', ($event.target as HTMLInputElement).checked)"
      />
      {{ modelValue ? 'true' : 'false' }}
    </label>

    <div v-else class="picked-row">
      <input
        type="text"
        class="raw"
        :class="{ invalid: rawError }"
        :placeholder="shape.type"
        :value="rawValue"
        @input="setRaw(($event.target as HTMLInputElement).value)"
        @keydown.stop
      />
      <button v-if="hasValue" type="button" class="clear-value" title="Clear" @click="rawText = null; clear()">×</button>
    </div>
    </template>

    <button v-if="!binding && !bindingInput" type="button" class="use-binding" @click="bindingInput = true">
      use a binding
    </button>
  </div>
</template>

<style scoped lang="scss">
.value-editor {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  min-width: 0;
}

.value-label {
  font-size: 0.75rem;
  opacity: 0.7;
}

.picker {
  position: relative;
}

.picked-row {
  align-items: stretch;
  display: flex;
  gap: 0.25rem;
}

.binding {
  background: rgba(170, 221, 255, 0.12);
  border: 1px solid #adf;
  border-radius: 4px;
  color: #adf;
  flex: 1 1 auto;
  font-family: monospace;
  padding: 0.35rem 0.5rem;
}

.use-binding {
  align-self: flex-start;
  background: none;
  border: none;
  color: #9ca3af;
  cursor: pointer;
  font-size: 0.7rem;
  padding: 0;

  &:hover {
    color: #adf;
  }
}

.clear-value {
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  flex: 0 0 auto;
  padding: 0 0.5rem;
}

.picked {
  flex: 1 1 auto;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  padding: 0.35rem 0.5rem;
  text-align: left;
  width: 100%;
}

.picker-menu {
  position: absolute;
  z-index: 10;
  top: 100%;
  left: 0;
  right: 0;
  background: #0f1422;
  border: 1px solid var(--button-highlight);
  border-radius: 4px;
  max-height: 260px;
  overflow: auto;
  padding: 0.35rem;

  input {
    width: 100%;
    margin-bottom: 0.35rem;
  }

  ul {
    list-style: none;
    margin: 0;
    padding: 0;
  }

  button {
    background: none;
    border: none;
    color: #eee;
    cursor: pointer;
    display: flex;
    flex-direction: column;
    padding: 0.3rem 0.4rem;
    text-align: left;
    width: 100%;

    &:hover {
      background: rgba(255, 255, 255, 0.1);
    }

    small {
      opacity: 0.6;
    }
  }
}

.fields {
  border-left: 2px solid #374151;
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  margin-left: 0.3rem;
  padding-left: 0.6rem;
}

.list {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}

.list-item {
  align-items: flex-start;
  display: flex;
  gap: 0.35rem;

  > .value-editor {
    flex: 1 1 auto;
  }
}

.maybe {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.clear {
  align-self: flex-start;
  background: none;
  border: none;
  color: #adf;
  cursor: pointer;
  font-size: 0.75rem;
  padding: 0;
}

.inline {
  align-items: center;
  display: flex;
  flex-direction: row;
  gap: 0.35rem;
  font-size: 0.8rem;
}

input {
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  padding: 0.35rem;
  width: 100%;
}

input[type='checkbox'] {
  width: auto;
}

.raw.invalid {
  border-color: #f88;
}

.add,
.remove {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  padding: 0.25rem 0.5rem;
}

.add {
  align-self: flex-start;
}
</style>

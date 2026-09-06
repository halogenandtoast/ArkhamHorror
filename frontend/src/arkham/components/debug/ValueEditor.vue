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
import {
  decodeConstructor,
  encodeConstructor,
  shapeOf,
  typeSchema,
  type ConSchema,
} from '@/arkham/schema'
import { bindingFits, jumpToBinding, type Binding } from '@/arkham/customCardBindings'
import BindingToggle from '@/arkham/components/debug/BindingToggle.vue'

const props = defineProps<{
  type: string
  modelValue: any
  label?: string
  bindings?: Binding[]
  /* Whether the value can be taken away again. Most fields are part of the
   * constructor they belong to and clearing one only leaves a hole, so the
   * clear button is for the ones that really are removable: an optional field,
   * or one the caller says is optional. */
  optional?: boolean
}>()
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
const bindingSearch = ref('')
const bindingEl = ref<HTMLElement | null>(null)

onClickOutside(bindingEl, () => (bindingInput.value = false))

/* What this field may refer to, and where each name came from. Handed down
 * rather than looked up, because scope depends on where the field sits: a step
 * only sees what the steps before it bound. */
const inScope = computed(() => props.bindings ?? [])

// Shapes that draw a single bordered row, which is where the segment lives.
const hasOwnRow = computed(() => ['sum', 'text', 'number', 'raw'].includes(shape.value.kind))

const showClear = computed(() => !!props.optional && hasValue.value)

// Radios only behave as a group when they share a name, and every bool field on
// the page is its own group.
const boolGroup = `bool-${Math.random().toString(36).slice(2, 9)}`

function clearAll() {
  rawText.value = null
  clear()
}

/* Only the bindings that could actually go in this field. A `$w3 :: Int` is no
 * use to an EnemyMatcher, and offering it there is how you get a card that
 * silently does nothing. */
const resolveAlias = (type: string) => typeSchema(type)?.alias ?? type

const applicable = computed(() =>
  inScope.value.filter((b) => bindingFits(b, props.type, resolveAlias)),
)

const matchingBindings = computed(() => {
  const search = bindingSearch.value.trim().toLowerCase().replace(/^\$/, '')
  if (!search) return applicable.value
  return applicable.value.filter(
    (b) =>
      b.name.toLowerCase().includes(search) ||
      (b.detail ?? '').toLowerCase().includes(search) ||
      b.origin.toLowerCase().includes(search),
  )
})

// The binding currently set, matched back to its origin so the field can say
// where it came from and offer to jump there.
const boundTo = computed(() =>
  inScope.value.find((b) => `$${b.name}` === (binding.value ?? '')),
)

function setBinding(name: string) {
  const trimmed = name.trim()
  emit('update:modelValue', trimmed ? (trimmed.startsWith('$') ? trimmed : `$${trimmed}`) : null)
  bindingInput.value = false
  bindingSearch.value = ''
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

    <!-- The picker takes over even when a binding is already set, so clicking
         the value swaps it rather than making you clear it first. -->
    <div v-if="binding && !bindingInput" class="picked-row">
      <!-- One light-blue control. Where the binding came from is said where it
           is bound and again in the selector, so the field itself carries only
           a way back to that -- a segment at its head -- and the name. -->
      <div class="binding" :class="{ unknown: inScope.length && !boundTo }">
        <button
          v-if="boundTo?.anchor"
          type="button"
          class="jump-segment"
          :title="`Bound by ${boundTo.origin} — click to show`"
          @click="jumpToBinding(boundTo.anchor)"
        >
          ↗
        </button>
        <button
          type="button"
          class="binding-name"
          :title="`${boundTo ? `${boundTo.detail ?? ''} · ${boundTo.origin} — ` : ''}click to choose another`"
          @click="bindingInput = true"
        >
          {{ binding }}
        </button>
      </div>
      <span
        v-if="inScope.length && !boundTo"
        class="from unknown"
        title="Nothing in scope binds this name"
      >
        not bound
      </span>
      <button type="button" class="clear-value" title="Clear" @click="clear">×</button>
    </div>

    <div v-else ref="bindingEl" class="field-row">
      <div v-if="bindingInput" class="field-body with-toggle binding-open">
        <input
          v-model="bindingSearch"
          type="search"
          :placeholder="`Search the ${applicable.length} bindings that fit ${type}`"
          autofocus
          @keydown.enter.prevent="setBinding(bindingSearch)"
          @keydown.esc="bindingInput = false"
          @keydown.stop
        />
        <ul class="binding-menu">
          <li v-for="bound in matchingBindings" :key="bound.name">
            <button type="button" class="binding-option" @click="setBinding(bound.name)">
              <code class="option-name">${{ bound.name }}</code>
              <span v-if="bound.detail" class="option-detail">{{ bound.detail }}</span>
              <span class="option-origin">{{ bound.origin }}</span>
            </button>
            <button
              v-if="bound.anchor"
              type="button"
              class="option-jump"
              title="Show where this was bound"
              @click.stop="jumpToBinding(bound.anchor)"
            >
              ↗
            </button>
          </li>
          <li v-if="!matchingBindings.length" class="muted">
            Nothing in scope matches — press enter to use what you typed anyway.
          </li>
        </ul>
      </div>

      <div v-else class="field-body" :class="{ 'with-toggle': applicable.length }">

    <template v-if="shape.kind === 'sum'">
      <div ref="pickerEl" class="picker">
        <div class="picked-row">
          <button type="button" class="picked" @click="open ? closePicker() : (open = true)">
            <span class="picked-label">
              {{ current ? humanize(current.con.name) : `Choose ${shape.schema.name}…` }}
            </span>
            <!-- It opens a menu, so it says so; the binding segment sits to the
                 right of this, past the field's divider. -->
            <span class="caret" aria-hidden="true">▾</span>
          </button>
        <BindingToggle
          :open="bindingInput"
          :count="applicable.length"
          :type="type"
          @toggle="bindingInput = !bindingInput"
        />
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
          :bindings="bindings"
          :modelValue="current.values[fieldKey(field, index)]"
          @update:modelValue="setField(fieldKey(field, index), $event)"
        />
      </div>
    </template>

    <template v-else-if="shape.kind === 'list'">
      <div class="list">
        <div v-for="(item, index) in items" :key="index" class="list-item">
          <ValueEditor
            :type="shape.inner"
            :bindings="bindings"
            :modelValue="item"
            @update:modelValue="setItem(index, $event)"
          />
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
          :bindings="bindings"
          optional
          :modelValue="modelValue"
          @update:modelValue="emit('update:modelValue', $event)"
        />
      </div>
    </template>

    <div v-else-if="shape.kind === 'text'" class="picked-row">
      <input
        type="text"
        :value="modelValue ?? ''"
        @input="emit('update:modelValue', ($event.target as HTMLInputElement).value)"
        @keydown.stop
      />
      <BindingToggle
        :open="bindingInput"
        :count="applicable.length"
        :type="type"
        @toggle="bindingInput = !bindingInput"
      />
    </div>

    <div v-else-if="shape.kind === 'number'" class="picked-row">
      <input
        type="number"
        :value="modelValue ?? ''"
        @input="emit('update:modelValue', Number(($event.target as HTMLInputElement).value))"
        @keydown.stop
      />
      <BindingToggle
        :open="bindingInput"
        :count="applicable.length"
        :type="type"
        @toggle="bindingInput = !bindingInput"
      />
    </div>

    <!-- Two named choices rather than a checkbox: a bare box leaves you reading
         the label to work out which way is on, and an unset field looks the
         same as a false one. -->
    <div v-else-if="shape.kind === 'bool'" class="bool-toggle">
      <span class="bool-thumb" :class="{ on: !!modelValue }" aria-hidden="true"></span>
      <label class="bool-option" :class="{ active: !modelValue }">
        <input
          type="radio"
          :name="boolGroup"
          :checked="!modelValue"
          @change="emit('update:modelValue', false)"
        />
        false
      </label>
      <label class="bool-option" :class="{ active: !!modelValue }">
        <input
          type="radio"
          :name="boolGroup"
          :checked="!!modelValue"
          @change="emit('update:modelValue', true)"
        />
        true
      </label>
    </div>

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
      <BindingToggle
        :open="bindingInput"
        :count="applicable.length"
        :type="type"
        @toggle="bindingInput = !bindingInput"
      />
    </div>
      </div>

      <!-- A list, an optional or a checkbox has no row to sit in, so the
           segment falls back to the corner of the field. -->
      <BindingToggle
        v-if="!hasOwnRow"
        class="floating"
        :open="bindingInput"
        :count="applicable.length"
        :type="type"
        @toggle="bindingInput = !bindingInput"
      />

      <!-- Outside the field, because it removes the field's value rather than
           editing it. -->
      <button v-if="showClear" type="button" class="clear-value" title="Clear" @click="clearAll">
        ×
      </button>
    </div>
  </div>
</template>

<style scoped lang="scss">
/* The binding toggle sits inside the field's own box, at its right edge, rather
 * than beside or under it: it is part of the control, and turns the field into
 * the picker in place until something is chosen. The body reserves room for it
 * so nothing -- an input's text, a clear button -- ends up underneath. */
/* The field is the bordered row; the clear button sits beside it because it
 * removes the value rather than editing it. */
.field-row {
  align-items: flex-start;
  display: flex;
  gap: 0.25rem;
  min-width: 0;
  position: relative;
}

.field-body {
  flex: 1;
  min-width: 0;
}

// The fallback segment for shapes with no row of their own.
.floating {
  border: 1px solid #4b5563;
  border-radius: 3px;
  margin: 0;
  padding: 0.25rem 0.5rem;
  position: absolute;
  right: 0;
  top: 0;
}

/* The field's box is the row, not the control sitting in it: the border moves
 * out to the row and the control goes transparent inside, so the binding toggle
 * lands within the same outline rather than alongside it. Anything else in the
 * row -- the clear button -- stops before the toggle's reserved strip. */
.field-body > .picked-row,
.field-body > .picker > .picked-row {
  align-items: stretch;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  overflow: hidden;
  padding: 0.35rem 0.5rem;

  > input,
  > .picked,
  > .raw {
    background: transparent;
    border-color: transparent;
    padding: 0;
  }

  > input:focus,
  > .raw:focus {
    outline: none;
  }
}

.field-body:focus-within > .picked-row,
.field-body:focus-within > .picker > .picked-row {
  border-color: #6b7280;
}

/* Only the shapes with no row of their own still need room reserved, since
 * their segment is laid over the corner rather than sitting in a row. */
.with-toggle:not(:has(> .picked-row)):not(:has(> .picker > .picked-row)) {
  padding-right: 2rem;
}

.binding-open {
  position: relative;

  input {
    background: #0b1220;
    border: 1px solid #14b8a6;
    border-radius: 4px;
    color: #eee;
    padding: 0.35rem 0.5rem;
    width: 100%;

    &::placeholder {
      color: #6b7280;
    }
  }
}

/* Floated over the form rather than laid out in it: opening the picker in a
 * nested field would otherwise push everything below it down the page, which
 * moves the very field you were aiming at. */
.binding-menu {
  background: #0b1220;
  border: 1px solid #374151;
  border-radius: 5px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.45);
  left: 0;
  list-style: none;
  margin: 0.3rem 0 0;
  max-height: 15rem;
  overflow-y: auto;
  padding: 0.2rem;
  position: absolute;
  right: 0;
  top: 100%;
  z-index: 30;

  li {
    align-items: stretch;
    border-radius: 4px;
    display: flex;
    gap: 0.15rem;

    &:hover {
      background: rgba(20, 184, 166, 0.1);
    }
  }

  .muted {
    color: #9ca3af;
    font-size: 0.75rem;
    padding: 0.45rem 0.5rem;
  }
}

/* Name, what it holds, where it came from — three columns, so a list of them
 * reads down rather than as a run-on line. */
.binding-option {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  flex: 1;
  gap: 0.5rem;
  min-width: 0;
  padding: 0.35rem 0.45rem;
  text-align: left;
}

.option-name {
  color: #5eead4;
  flex: none;
  font-family: monospace;
  font-size: 0.82rem;
}

.option-detail {
  color: #d1d5db;
  flex: 1;
  font-size: 0.75rem;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.option-origin {
  color: #6b7280;
  flex: none;
  font-size: 0.72rem;
}

.option-jump {
  background: none;
  border: none;
  color: #6b7280;
  cursor: pointer;
  flex: none;
  padding: 0 0.4rem;

  &:hover {
    color: #5eead4;
  }
}

.jump {
  background: none;
  border: none;
  color: #5eead4;
  cursor: pointer;
  font-size: 0.72rem;
  padding: 0 0.25rem;
  white-space: nowrap;

  &:hover {
    text-decoration: underline;
  }
}

.from {
  color: #9ca3af;
  font-size: 0.72rem;

  &.unknown {
    color: #fca5a5;
  }
}

.binding.unknown {
  color: #fca5a5;
}

/* A slider between the two values: the thumb moves, so which one is chosen is
 * legible at a glance rather than read off a label. */
.bool-toggle {
  align-self: flex-start;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 999px;
  display: inline-flex;
  padding: 2px;
  position: relative;
}

.bool-thumb {
  background: #374151;
  border-radius: 999px;
  bottom: 2px;
  left: 2px;
  position: absolute;
  top: 2px;
  transition: transform 0.15s ease, background 0.15s ease;
  width: calc(50% - 2px);

  &.on {
    background: #134e4a;
    transform: translateX(100%);
  }
}

.bool-option {
  border-radius: 999px;
  color: #9ca3af;
  cursor: pointer;
  font-size: 0.75rem;
  min-width: 3.2rem;
  padding: 0.2rem 0.6rem;
  position: relative;
  text-align: center;
  user-select: none;
  z-index: 1;

  &.active {
    color: #eee;
  }

  &:last-child.active {
    color: #5eead4;
  }

  input {
    position: absolute;
    opacity: 0;
    pointer-events: none;
  }
}

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
  align-items: stretch;
  background: rgba(170, 221, 255, 0.12);
  border: 1px solid #adf;
  border-radius: 4px;
  color: #adf;
  display: flex;
  flex: 1 1 auto;
  font-family: monospace;
  min-width: 0;
  overflow: hidden;

  &.unknown {
    border-color: #fca5a5;
    color: #fca5a5;
  }
}

.binding-name {
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  flex: 1;
  font-family: inherit;
  font-size: inherit;
  min-width: 0;
  overflow: hidden;
  padding: 0.35rem 0.5rem;
  text-align: left;
  text-overflow: ellipsis;
  white-space: nowrap;
}

// A segment at the head of the field, back to whatever bound it.
.jump-segment {
  background: rgba(170, 221, 255, 0.22);
  border: none;
  border-right: 1px solid #adf;
  color: #dceeff;
  cursor: pointer;
  flex: none;
  font-family: inherit;
  padding: 0 0.5rem;

  &:hover {
    background: rgba(170, 221, 255, 0.4);
    color: #fff;
  }
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
  align-items: center;
  background: #111827;
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  display: flex;
  flex: 1 1 auto;
  gap: 0.5rem;
  justify-content: space-between;
  min-width: 0;
  padding: 0.35rem 0.5rem;
  text-align: left;
}

.picked-label {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.caret {
  color: #9ca3af;
  flex: none;
  font-size: 0.7rem;
  line-height: 1;
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

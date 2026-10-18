<script lang="ts" setup>
/* A prompt the player reads, which may be words or may be this card's own text.
 *
 * `$.again` is a key the card answers for, and the engine turns it into the
 * address the client looks the words up by. A key is not a label, so it does
 * not look like one: it takes the same chip a binding takes -- in its own
 * colour -- and what it says rides in a segment of that chip, where it can be
 * read and written without the key leaving the screen.
 */
import { computed, inject, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import SharedTextPicker from '@/arkham/components/debug/SharedTextPicker.vue'
import SharedTextValues from '@/arkham/components/debug/SharedTextValues.vue'
import type { Binding } from '@/arkham/customCardBindings'
import {
  formatSharedLabel,
  isOwnTextKey,
  isSharedTextKey,
  ownTextKey,
  parseSharedLabel,
  readableParams,
  sharedTextCatalogue,
  type CardTextAccess,
  CARD_TEXT,
} from '@/arkham/customText'

const props = defineProps<{
  label: string
  modelValue?: string | null
  placeholder?: string
  /* What this step has in scope. A label is a string the engine substitutes
   * into before it is shown, so a name bound here can stand in it. */
  bindings?: Binding[]
}>()

const emit = defineEmits<{ 'update:modelValue': [value: string] }>()

const text = inject<CardTextAccess | null>(CARD_TEXT, null)

const key = computed(() =>
  isOwnTextKey(props.modelValue) ? ownTextKey(props.modelValue as string) : null,
)
const says = computed(() => (key.value ? (text?.read(key.value) ?? '') : ''))

/* ------------------------------------------------- the words already here ---

   A key out of the app's own vocabulary, which every language it has been
   translated into answers for. The field holds the key and whatever the
   sentence leaves blank; what it says is read back through the same `t` the
   game reads it through. */
const { t, te } = useI18n()

const picking = ref(false)
const filling = ref(false)

const shared = computed(() =>
  isSharedTextKey(props.modelValue) ? parseSharedLabel(props.modelValue as string) : null,
)

const sharedSays = computed(() => {
  const held = shared.value
  if (!held) return ''
  return te(held.key) ? t(held.key, readableParams(held.params)) : ''
})

// What the sentence leaves blank, as the message itself says. A key nothing
// knows falls back to whatever the label already carries.
const blanks = computed<string[]>(() => {
  const held = shared.value
  if (!held) return []
  return sharedTextCatalogue().find((entry) => entry.key === held.key)?.params ?? Object.keys(held.params)
})

const setParam = (name: string, value: string) => {
  const held = shared.value
  if (!held) return
  emit('update:modelValue', formatSharedLabel(held.key, { ...held.params, [name]: value }))
}

const setKey = (key: string) => {
  emit('update:modelValue', formatSharedLabel(key, shared.value?.params ?? {}))
  picking.value = false
  /* A prompt with a hole in it is not finished being chosen. Asking straight
   * away is the difference between picking "Add {name} to hand" and picking
   * "Add  to hand", which is what it says until somebody fills it in. */
  const needs = sharedTextCatalogue().find((entry) => entry.key === key)?.params ?? []
  if (needs.length) filling.value = true
}

/* Said once, where the question comes up: a field that takes words does not
 * announce that it takes a key instead unless something says so. */
const HOW = {
  content: [
    'Words here are shown as written.',
    "$.name is this card's own text instead — the words go in Text below,",
    'where another language can be added.',
  ].join(' '),
  popperClass: 'label-how',
}

const write = (event: Event) => emit('update:modelValue', (event.target as HTMLInputElement).value)
</script>

<template>
  <label class="label-field">
    <span class="caption">
      {{ label }}
      <button type="button" class="how" v-tooltip="HOW" aria-label="How this label works" @click.prevent>
        <font-awesome-icon icon="circle-question" />
      </button>
    </span>

    <!-- One control, divided: whatever the label is made of, and the two ways
         to put something else in it. The dividers are the same ones a binding
         chip wears, so this reads as a field with segments rather than as a
         field with buttons parked beside it. -->
    <div class="field-wrap">
      <div class="field" :class="{ keyed: !!key || !!shared }">
        <input
          class="label-part"
          :value="modelValue"
          :placeholder="placeholder"
          @input="write"
          @keydown.stop
        />

        <!-- What this card's own key says, which is written here. -->
        <input
          v-if="key"
          class="says-part"
          :value="says"
          placeholder="no text yet"
          :disabled="!text"
          @input="text?.write(key, ($event.target as HTMLInputElement).value)"
          @keydown.stop
        />

        <!-- What a shared key says, which is not. -->
        <span v-else-if="shared" class="says-part reads">{{ sharedSays || 'no such key' }}</span>

        <button
          type="button"
          class="segment shared"
          v-tooltip="'Shared text'"
          aria-label="Shared text"
          @click.prevent="picking = true"
        >
          <font-awesome-icon icon="book" />
        </button>

        <!-- Only a prompt that leaves something blank has anything to fill
             in, and filling one in is where a binding in scope belongs: as a
             value for a named hole, not spliced into the middle of a string
             the engine substitutes by whole words. -->
        <button
          v-if="shared && blanks.length"
          type="button"
          class="segment values"
          :class="{ empty: blanks.some((name) => !shared?.params[name]) }"
          v-tooltip="'Fill in what it leaves blank'"
          aria-label="Values"
          @click.prevent="filling = true"
        >
          $
        </button>
      </div>
    </div>

    <SharedTextPicker v-if="picking" :chosen="modelValue" @pick="setKey" @close="picking = false" />

    <SharedTextValues
      v-if="filling && shared"
      :label="modelValue ?? ''"
      :bindings="bindings"
      @update:label="emit('update:modelValue', $event)"
      @close="filling = false"
    />
  </label>
</template>

<!-- The popper is teleported out of this component, so the one rule it needs is
     not scoped to it. Tooltips here are a few words; this one is a sentence and
     would otherwise lay itself across the whole window. -->
<style lang="scss">
.v-popper--theme-tooltip.label-how .v-popper__inner {
  max-width: 24rem;
}
</style>

<style lang="scss" scoped>
.label-field {
  display: flex;
  flex: 1;
  flex-direction: column;
  text-align: left;
}

.caption {
  align-items: center;
  display: flex;
  gap: 0.3rem;
}

/* A mark, not a control: it answers a question and does nothing else. */
.how {
  background: none;
  border: none;
  color: #6b7280;
  cursor: help;
  font-size: 0.72rem;
  padding: 0;

  &:hover {
    color: #9ca3af;
  }
}

.field-wrap {
  position: relative;
}

/* One box, divided into segments -- built like `BindingField`'s chip, so a
   field with parts looks the same wherever the builder has one. */
.field {
  align-items: stretch;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  display: flex;
  font-size: 1rem;
  min-width: 0;
  overflow: hidden;

  &:focus-within {
    border-color: var(--spooky-green);
  }

  input,
  .says-part {
    background: none;
    border: none;
    border-radius: 0;
    color: #eee;
    font-family: monospace;
    font-size: inherit;
    line-height: 1.4;
    min-width: 0;
    padding: 0.35rem 0.5rem;

    &:focus {
      outline: none;
    }
  }
}

.label-part {
  flex: 1 1 auto;
}

.says-part {
  border-left: 1px solid var(--box-border);
  flex: 1 1 45%;
}

/* The one colour in the builder that means "this is text, looked up by key". */
.field.keyed {
  background: rgba(240, 171, 252, 0.12);
  border-color: #f0abfc;

  &:focus-within {
    border-color: #f0abfc;
  }

  .label-part {
    color: #f0abfc;
  }

  .says-part {
    background: rgba(240, 171, 252, 0.18);
    border-left-color: #f0abfc;

    &::placeholder {
      color: #f0abfc99;
    }
  }
}

.says-part.reads {
  align-items: center;
  display: flex;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

/* The two ways to put something in the field, as segments of it.
 *
 * Square on every edge: a button is rounded by default, and a rounded one
 * inside a rounded box leaves a notch where the two curves disagree. The box
 * keeps its own corners and clips theirs. */
.segment {
  background: rgba(255, 255, 255, 0.04);
  border: none;
  border-left: 1px solid var(--box-border);
  border-radius: 0;
  color: #9ca3af;
  flex: none;
  font-family: monospace;
  font-size: 0.82rem;
  padding: 0 0.55rem;

  &:hover:not(:disabled) {
    color: #eee;
  }

  &:disabled {
    color: #4b5563;
  }
}

.segment.shared:hover:not(:disabled) {
  background: rgba(240, 171, 252, 0.24);
  color: #f0abfc;
}

/* A prompt with a hole in it says so: the segment is lit while anything it
   leaves blank is still blank. */
.segment.values.empty {
  background: rgba(252, 211, 77, 0.2);
  color: #fcd34d;
}

.field.keyed .segment {
  border-left-color: #f0abfc66;
}

.blanks {
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
  margin-top: 0.25rem;
}

.blank {
  align-items: center;
  display: flex;
  flex-direction: row;
  gap: 0.3rem;

  code {
    color: #f0abfc;
    font-size: 0.72rem;
  }
}

</style>

<script lang="ts" setup>
/* A prompt the player reads, which may be words or may be this card's own text.
 *
 * `$.again` is a key the card answers for, and the engine turns it into the
 * address the client looks the words up by. A key is not a label, so it does
 * not look like one: it takes the same chip a binding takes -- in its own
 * colour -- and what it says rides in a segment of that chip, where it can be
 * read and written without the key leaving the screen.
 */
import { computed, inject } from 'vue'
import { isOwnTextKey, ownTextKey, type CardTextAccess, CARD_TEXT } from '@/arkham/customText'

const props = defineProps<{
  label: string
  modelValue?: string | null
  placeholder?: string
}>()

const emit = defineEmits<{ 'update:modelValue': [value: string] }>()

const text = inject<CardTextAccess | null>(CARD_TEXT, null)

const key = computed(() =>
  isOwnTextKey(props.modelValue) ? ownTextKey(props.modelValue as string) : null,
)
const says = computed(() => (key.value ? (text?.read(key.value) ?? '') : ''))

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

    <!-- The key and what it says, as two segments of one chip. -->
    <div v-if="key" class="own-text">
      <input class="own-key" :value="modelValue" @input="write" @keydown.stop />
      <input
        class="own-says"
        :value="says"
        placeholder="no text yet"
        :disabled="!text"
        @input="text?.write(key, ($event.target as HTMLInputElement).value)"
        @keydown.stop
      />
    </div>

    <input v-else :value="modelValue" :placeholder="placeholder" @input="write" @keydown.stop />
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

/* The same chip a binding wears, in the one colour that means "text, looked up
   by key" -- nothing else in the builder uses it. Built like 'BindingField's
   `.binding`, so a key and a binding read as two of a kind rather than as two
   different ideas of what a field is. */
.own-text {
  align-items: stretch;
  background: rgba(240, 171, 252, 0.12);
  border: 1px solid #f0abfc;
  border-radius: 4px;
  color: #f0abfc;
  display: flex;
  font-family: monospace;
  /* The size a binding chip is. The fields around it are smaller, but this is
     one of those, and standing a row apart in height is what made it read as
     something else. */
  font-size: 1rem;
  min-width: 0;
  overflow: hidden;

  input {
    background: none;
    border: none;
    border-radius: 0;
    color: inherit;
    font-family: inherit;
    font-size: inherit;
    line-height: 1.4;
    min-width: 0;
    padding: 0.35rem 0.5rem;

    &:focus {
      outline: none;
    }
  }
}

.own-text .own-key {
  flex: 1 1 auto;
}

/* What it says, in the segment the chip's other halves use: tinted, divided,
   and readable rather than key-coloured -- these are the words themselves.
   Written against the chip, since the rule that strips the inputs bare is. */
.own-text .own-says {
  background: rgba(240, 171, 252, 0.18);
  border-left: 1px solid #f0abfc;
  color: #eee;
  flex: 1 1 45%;

  &::placeholder {
    color: #f0abfc99;
  }

  &:focus {
    background: rgba(240, 171, 252, 0.28);
  }
}
</style>

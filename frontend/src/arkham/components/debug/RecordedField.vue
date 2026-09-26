<script lang="ts" setup>
/* One entry in a campaign-log set.
 *
 * Stored as a `SomeRecorded`: a recordable type paired with a `Recorded` wrapping
 * the value, which is three nested objects and two names nobody outside the
 * engine has met -- `RecordableGeneric`, `recordVal`. Here it is the three
 * questions it actually asks: what kind of thing, what it says, and whether it
 * still counts.
 *
 * Most cards never need this: the Record step writes entries without naming the
 * shape at all. It is what the field becomes when a card pushes the message
 * itself.
 */
import { computed, ref } from 'vue'
import BindingField from '@/arkham/components/debug/BindingField.vue'
import BindingToggle from '@/arkham/components/debug/BindingToggle.vue'
import CardCodeField from '@/arkham/components/debug/CardCodeField.vue'
import { bindingFits, type Binding } from '@/arkham/customCardBindings'

const props = defineProps<{ modelValue: any; bindings?: Binding[] }>()
const emit = defineEmits<{ 'update:modelValue': [v: any] }>()

/* The four `Recordable` instances. A memento and a memory belong to one campaign
 * each and are plain constructor names; the other two are what a custom card
 * records. */
const KINDS = [
  { tag: 'RecordableGeneric', label: 'a value', hint: 'A trait, a name, a number — anything.' },
  { tag: 'RecordableCardCode', label: 'a card', hint: 'The log prints it by name and can hand it back as a card.' },
  { tag: 'RecordableMemento', label: 'a memento', hint: "A Circle Undone memento, by its own name." },
  { tag: 'RecordableMemory', label: 'a memory', hint: 'An Innsmouth memory, by its own name.' },
] as const

/* Recorded, or crossed out — the log's way of saying it is no longer true.
 * Circled is an annotation on top of either, not a third state. */
const STATES = [
  { tag: 'Recorded', label: 'recorded' },
  { tag: 'CrossedOut', label: 'crossed out' },
] as const

const kind = computed<string>(() => props.modelValue?.recordType ?? 'RecordableGeneric')
const kindHint = computed(() => KINDS.find((k) => k.tag === kind.value)?.hint ?? '')
const recordVal = computed<any>(() => props.modelValue?.recordVal ?? {})
const state = computed<string>(() => (recordVal.value.tag === 'CrossedOut' ? 'CrossedOut' : 'Recorded'))
const circled = computed(() => recordVal.value.circled === true)
const contents = computed(() => recordVal.value.contents)

const asText = computed(() =>
  contents.value === undefined || contents.value === null ? '' : String(contents.value),
)
const isBinding = computed(() => asText.value.trim().startsWith('$'))

const applicable = computed(() =>
  (props.bindings ?? []).filter((b) => bindingFits(b, 'Trait', (t) => t)),
)
const bindingInput = ref(false)

function write(changes: { kind?: string; state?: string; circled?: boolean; contents?: any }) {
  const next: any = {
    recordType: changes.kind ?? kind.value,
    recordVal: {
      tag: changes.state ?? state.value,
      contents: 'contents' in changes ? changes.contents : (contents.value ?? null),
    },
  }
  // Absent rather than false: the engine reads it with `.:? "circled" .!= False`,
  // and a key that is only ever true is one less thing in the saved JSON.
  if (changes.circled ?? circled.value) next.recordVal.circled = true
  emit('update:modelValue', next)
}

/* A number typed into a value entry is stored as one, since `Recordable Value`
 * takes any JSON and "3" and 3 are not the same entry. A `$binding` is not valid
 * JSON, so it stays the string it has to be. */
function setText(text: string) {
  const trimmed = text.trim()
  if (!trimmed) return write({ contents: null })
  if (/^-?\d+(\.\d+)?$/.test(trimmed)) return write({ contents: Number(trimmed) })
  write({ contents: text })
}

function setBinding(name: string | null) {
  write({ contents: name ?? null })
  if (!name) bindingInput.value = false
}
</script>

<template>
  <div class="recorded">
    <div class="row">
      <label>
        Record
        <select :value="kind" @change="write({ kind: ($event.target as HTMLSelectElement).value })">
          <option v-for="k in KINDS" :key="k.tag" :value="k.tag">{{ k.label }}</option>
        </select>
      </label>
      <label>
        as
        <select :value="state" @change="write({ state: ($event.target as HTMLSelectElement).value })">
          <option v-for="st in STATES" :key="st.tag" :value="st.tag">{{ st.label }}</option>
        </select>
      </label>
      <label class="check" title="A circle on an entry that is still recorded">
        <input
          type="checkbox"
          :checked="circled"
          @change="write({ circled: ($event.target as HTMLInputElement).checked })"
        />
        circled
      </label>
    </div>

    <CardCodeField
      v-if="kind === 'RecordableCardCode'"
      :modelValue="typeof contents === 'string' ? contents : null"
      @update:modelValue="write({ contents: $event })"
    />
    <BindingField
      v-else-if="isBinding || bindingInput"
      clearable
      :modelValue="isBinding ? asText.trim() : null"
      :applicable="applicable"
      :inScope="bindings"
      type="a value"
      @update:modelValue="setBinding($event)"
    />
    <div v-else class="value-box">
      <input
        :value="asText"
        :placeholder="kind === 'RecordableGeneric' ? 'Tome' : 'its own name'"
        @input="setText(($event.target as HTMLInputElement).value)"
        @keydown.stop
      />
      <BindingToggle
        :open="false"
        :count="applicable.length"
        type="a value"
        @toggle="bindingInput = true"
      />
    </div>
    <p class="hint">{{ kindHint }}</p>
  </div>
</template>

<style scoped lang="scss">
.recorded {
  border: 1px solid #374151;
  border-radius: 4px;
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  min-width: 0;
  padding: 0.35rem 0.45rem 0.4rem;
}

.row {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
  min-width: 0;
}

label {
  align-items: center;
  color: #9ca3af;
  display: flex;
  font-size: 0.72rem;
  gap: 0.25rem;
}

.check {
  gap: 0.2rem;
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

.value-box > input {
  color: #e5e7eb;
  font-family: inherit;
  font-size: 0.76rem;
}

.hint {
  color: #6b7280;
  font-size: 0.68rem;
  margin: 0;
}
</style>

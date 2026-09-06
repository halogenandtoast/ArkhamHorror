<script lang="ts" setup>
/* Builds the `_abilities` and `_handlers` blocks of a custom card's meta.
 *
 * Each ability is an AbilityType plus optional criteria/limit, and a list of
 * steps. A step either runs a query and binds the result, or pushes a message —
 * the two operations the runner supports. Anything a step binds is available to
 * later steps as $name, alongside $id, $source, $target and $iid. */
import { computed, onMounted } from 'vue'
import { loadSchema, messageConstructors, schemaLoaded, type FieldSchema } from '@/arkham/schema'
import StepsEditor from '@/arkham/components/debug/StepsEditor.vue'
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

/* Naming a message tag also names its fields, which is what a handler binds
 * $0, $1, … to. Showing them with their types is the difference between
 * guessing at a position and knowing it. */
const messages = computed(() =>
  schemaLoaded.value ? messageConstructors() : new Map<string, FieldSchema[]>(),
)
const messageTags = computed(() => [...messages.value.keys()].sort())
const messageFields = (tag: string) => messages.value.get(tag) ?? []
const knownMessage = (tag: string) => messages.value.has(tag)

/* What a handler's steps can name. The card's own bindings are always there;
 * the numbered ones come from the message it listens for. */
const CARD_BINDINGS = [
  '$message',
  '$source',
  '$target',
  '$id',
  '$iid',
  '$controller',
  '$owner',
  '$investigator',
]

const bindingsFor = (tag: string) => [
  ...CARD_BINDINGS,
  ...messageFields(tag).map((_, at) => `$${at}`),
]

const isBinding = (value: string) => value.trim().startsWith('$')
const isKnownBinding = (tag: string, value: string) => bindingsFor(tag).includes(value.trim())

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

const addHandler = () =>
  emit('update:handlers', [...handlers.value, { on: '', requires: [], steps: [] }])

/* A message that merely mentions this card is not necessarily about it: an
 * enemy defeated by someone else still names this card if it was the target.
 * A requirement pins the field that has to be this card down. */
const requiresOf = (handler: any): [string, string][] => handler.requires ?? []

const setRequirement = (index: number, at: number, side: 0 | 1, value: string) => {
  const requires = requiresOf(handlers.value[index]).map((pair, i) =>
    i === at ? (side === 0 ? [value, pair[1]] : [pair[0], value]) : pair,
  )
  setHandler(index, { requires })
}

const addRequirement = (index: number) =>
  setHandler(index, { requires: [...requiresOf(handlers.value[index]), ['$2', '$source']] })

const removeRequirement = (index: number, at: number) =>
  setHandler(index, { requires: requiresOf(handlers.value[index]).filter((_, i) => i !== at) })

const removeHandler = (i: number) =>
  emit('update:handlers', handlers.value.filter((_, j) => j !== i))

// --- modifiers ---

const setModifier = (i: number, changes: Record<string, any>) =>
  emit('update:modifiers', patch(modifiers.value, i, changes))

const addModifier = () =>
  emit('update:modifiers', [...modifiers.value, { kind: 'enemy', matcher: null, modifiers: [] }])

const removeModifier = (i: number) =>
  emit('update:modifiers', modifiers.value.filter((_, j) => j !== i))

/* Where the card has to be for the ability to be usable. A card out of play is
 * only built as an entity when its def asks for it, so naming a zone here also
 * puts the card in that zone's entity list. */
const ZONES: Record<string, string> = {
  '': 'In play',
  hand: 'In your hand',
  discard: 'In your discard',
  search: 'While searching',
  topOfDeck: 'On top of your deck',
}

const stepsOf = (item: any): any[] => item.steps ?? []

</script>

<template>
  <div class="ability-editor">
    <p v-if="!schemaLoaded" class="loading">Loading type schema…</p>

    <template v-else>
      <datalist id="custom-message-tags">
        <option v-for="tag in messageTags" :key="tag" :value="tag" />
      </datalist>

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
        <label class="zone">
          Active
          <select
            :value="ability.zone ?? ''"
            @change="setAbility(index, { zone: ($event.target as HTMLSelectElement).value || undefined })"
          >
            <option v-for="(text, zone) in ZONES" :key="zone" :value="zone">{{ text }}</option>
          </select>
        </label>

        <StepsEditor
          :queryKinds="QUERY_KINDS"
          :modelValue="stepsOf(ability)"
          @update:modelValue="setAbility(index, { steps: $event })"
        />
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
            list="custom-message-tags"
            placeholder="EnemyDamaged"
            @input="setHandler(index, { on: ($event.target as HTMLInputElement).value })"
            @keydown.stop
          />
        </label>
        <p class="hint">
          Fires when a message with this tag mentions this card. The whole message is
          <code>$message</code>.
        </p>
        <ul v-if="knownMessage(handler.on)" class="bindings">
          <li v-for="(field, at) in messageFields(handler.on)" :key="at">
            <code>${{ at }}</code> {{ field.name ? `${field.name} ::` : '::' }} {{ field.type }}
          </li>
          <li v-if="!messageFields(handler.on).length" class="muted">no fields</li>
        </ul>
        <p v-else-if="handler.on" class="hint muted">Not a message the engine sends.</p>

        <div v-for="(pair, at) in requiresOf(handler)" :key="at" class="row">
          <label>
            Only when
            <input
              :value="pair[0]"
              :class="{
                binding: isKnownBinding(handler.on, pair[0]),
                unknown: isBinding(pair[0]) && !isKnownBinding(handler.on, pair[0]),
              }"
              list="custom-handler-bindings"
              placeholder="$2"
              @input="setRequirement(index, at, 0, ($event.target as HTMLInputElement).value)"
              @keydown.stop
            />
          </label>
          <label>
            is
            <input
              :value="pair[1]"
              :class="{
                binding: isKnownBinding(handler.on, pair[1]),
                unknown: isBinding(pair[1]) && !isKnownBinding(handler.on, pair[1]),
              }"
              list="custom-handler-bindings"
              placeholder="$source"
              @input="setRequirement(index, at, 1, ($event.target as HTMLInputElement).value)"
              @keydown.stop
            />
          </label>
          <button type="button" @click="removeRequirement(index, at)">×</button>
        </div>
        <datalist id="custom-handler-bindings">
          <option v-for="name in bindingsFor(handler.on)" :key="name" :value="name" />
        </datalist>
        <button type="button" class="add" @click="addRequirement(index)">+ Requirement</button>
        <p class="hint">
          Mentioning this card is not the same as being about it — an enemy someone else defeated
          still names this card if it was the target. A requirement pins down which field has to be
          this card, the way a hand-written card matches on its source.
        </p>

        <StepsEditor
          :queryKinds="QUERY_KINDS"
          :modelValue="stepsOf(handler)"
          @update:modelValue="setHandler(index, { steps: $event })"
        />
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

  &.inline {
    align-items: center;
    flex-direction: row;
    gap: 0.3rem;
  }
}

input[type='checkbox'] {
  width: auto;
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

.bindings {
  display: flex;
  flex-wrap: wrap;
  font-size: 0.75rem;
  gap: 0.15rem 0.8rem;
  list-style: none;
  margin: 0;
  opacity: 0.85;
  padding: 0;
}

.bindings code {
  color: #adf;
}

/* Same blue a binding wears everywhere else in the editor. */
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

.muted {
  opacity: 0.6;
}

.loading {
  opacity: 0.7;
}
</style>

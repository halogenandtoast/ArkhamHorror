<script lang="ts" setup>
/* Builds the `_abilities` and `_handlers` blocks of a custom card's meta.
 *
 * Each ability is an AbilityType plus optional criteria/limit, and a list of
 * steps. A step either runs a query and binds the result, or pushes a message —
 * the two operations the runner supports. Anything a step binds is available to
 * later steps as $name, alongside $id, $source, $target and $iid. */
import { computed, onMounted, ref } from 'vue'
import {
  encodeConstructor,
  loadSchema,
  messageConstructors,
  schemaLoaded,
  typeSchema,
  windowsForMatcher,
  type FieldSchema,
} from '@/arkham/schema'
import StepsEditor from '@/arkham/components/debug/StepsEditor.vue'
import {
  cardBindings,
  paymentBindings,
  messageBindings,
  windowBindings,
  type Binding,
} from '@/arkham/customCardBindings'
import BoolField from '@/arkham/components/debug/BoolField.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'

const props = defineProps<{
  abilities: any[]
  handlers: any[]
  modifiers: any[]
  /** So `$id` knows what kind of id it is. */
  cardType?: string
  /* Which half to render. A listener is not an ability, so it belongs in a box of
   * its own -- but it shares this component's bindings, steps and block styling,
   * so it stays the same component rather than a copy of all of that. */
  section: 'abilities' | 'listeners'
  
  /* Revelation and the elder sign are two more things a card does, so they are
   * listed with the abilities rather than kept in fieldsets of their own. What
   * they are allowed to be is the form's business, which is why it is passed in
   * rather than worked out again here. */
  canRevelation?: boolean
  revelationImplied?: boolean
  hasRevelationPlacement?: boolean
  revelationPlacements?: { value: string; label: string }[]
  isInvestigator?: boolean
}>()
const emit = defineEmits<{
  'update:abilities': [v: any[]]
  'update:handlers': [v: any[]]
  'update:modifiers': [v: any[]]
}>()

const revelation = defineModel<boolean>('revelation', { default: false })
const revelationPlacement = defineModel<string>('revelationPlacement', { default: '' })
const revelationSteps = defineModel<any[]>('revelationSteps', { default: () => [] })
const elderSign = defineModel<string>('elderSign', { default: '' })
const elderSignRevealSteps = defineModel<any[]>('elderSignRevealSteps', { default: () => [] })
const elderSignSteps = defineModel<any[]>('elderSignSteps', { default: () => [] })
const elderSignSuccessSteps = defineModel<any[]>('elderSignSuccessSteps', { default: () => [] })

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

/* An ability that triggers on a window reads that window's fields as $w0, $w1,
 * … the way a handler reads a message's — that is how "heal that many" gets its
 * number.
 *
 * Which window a matcher fires on is not derivable from the two types (a third
 * of the names differ: InvestigatorHealed fires on Healed, PlacedCounter on
 * PlacedToken), so it is recorded in Arkham.Custom.Schema.Windows and served
 * with the schema. */
const windows = computed(() => typeSchema('WindowType')?.constructors ?? [])
const windowNames = computed(() => windows.value.map((c) => c.name).sort())
const windowFields = (name: string) => windows.value.find((c) => c.name === name)?.fields ?? []
const knownWindow = (name: string) => windows.value.some((c) => c.name === name)

/* The windows a matcher fires on, and everything else after them.
 *
 * The mapping is served with the schema (Arkham.Custom.Schema.Windows), so this
 * no longer guesses -- but a matcher that fires on several still needs a choice,
 * and one the table has nothing for still needs the full list. */
const windowsFor = (matcher: string | null) => (matcher ? windowsForMatcher(matcher) : [])

const windowCandidates = (matcher: string | null) => {
  const known = windowsFor(matcher)
  if (!known.length) return windowNames.value
  return [...known, ...windowNames.value.filter((n) => !known.includes(n))]
}

/* The matcher an ability triggers on, if its AbilityType carries one. The
 * constructors that do name the field `window`. */
function abilityWindowMatcher(ability: any): string | null {
  const tag = ability?.type?.window?.tag
  return typeof tag === 'string' ? tag : null
}

/* Which window's fields to show. The table answers it outright when a matcher
 * fires on exactly one; a matcher that fires on several is the only case left
 * that needs asking. An explicit choice still wins, and is kept on the ability
 * so it survives reopening the card -- a note to the next author rather than
 * anything the engine reads, since `AbilitySpec` ignores keys it does not name.
 */
/* Chosen deliberately to mean "this never fires on a window". Kept on the
 * ability like any other hint, so the answer survives reopening the card. */
const NO_WINDOW = '—none—'

/* Whether the window question has been answered, one way or the other. A
 * matcher that cannot fire answers it by itself: `NotAnyWindow` is how an
 * ability says it is reached some other way -- an elder sign, another card's
 * useAbility -- and asking which window it triggers on has no answer to give. */
function windowSettled(ability: any): boolean {
  if (abilityWindowMatcher(ability) === 'NotAnyWindow') return true
  return !!abilityWindow(ability) || ability?.windowHint === NO_WINDOW
}

function abilityWindow(ability: any): string {
  if (ability?.windowHint === NO_WINDOW) return ''
  if (typeof ability.windowHint === 'string' && ability.windowHint) return ability.windowHint
  const known = windowsFor(abilityWindowMatcher(ability))
  return known.length === 1 && knownWindow(known[0]) ? known[0] : ''
}

// -------------------------------------------------------------- the kinds ---

/* The four an ability almost always is. Picking one pre-fills the AbilityType
 * with that constructor; the editor below is still there, so it can be changed
 * to any of the others afterwards. `Other…` starts with nothing chosen, which is
 * how every ability used to start. */
const ABILITY_KINDS: { label: string; tags: string[]; icon?: string }[] = [
  // Forced is a word printed on the card, not a symbol, so it has no glyph.
  { label: 'Forced', tags: ['ForcedAbility'] },
  { label: 'Reaction', tags: ['ReactionAbility'], icon: 'reaction-icon' },
  { label: 'Free trigger', tags: ["FastAbility'", 'FastAbility'], icon: 'fast-icon' },
  { label: 'Action', tags: ['ActionAbility'], icon: 'action-icon' },
]

function blankAbilityType(tags: string[]) {
  const schema = typeSchema('AbilityType')
  const con = schema?.constructors.find((c) => tags.includes(c.name))
  if (!schema || !con) return null
  const values: Record<string, any> = {}
  con.fields.forEach((field, at) => (values[field.name ?? String(at)] = null))
  return encodeConstructor(schema, con, values)
}

const abilityKind = (ability: any) =>
  ABILITY_KINDS.find((k) => k.tags.includes(ability?.type?.tag))

function abilityLabel(ability: any) {
  const named = abilityKind(ability)
  if (named) return named.label
  const tag = ability?.type?.tag
  return typeof tag === 'string' && tag ? tag : 'Ability'
}

const abilities = computed(() => props.abilities ?? [])
const handlers = computed(() => props.handlers ?? [])
const modifiers = computed(() => props.modifiers ?? [])

function patch(list: any[], index: number, changes: Record<string, any>) {
  return list.map((item, i) => (i === index ? { ...item, ...changes } : item))
}

/* An elder sign or a revelation is present or it is not; there is no flag for
 * it in the saved card, so its own contents are what say so. */
const hasElderSign = computed(
  () =>
    !!props.isInvestigator &&
    (elderSign.value !== '' ||
      elderSignRevealSteps.value.length > 0 ||
      elderSignSteps.value.length > 0 ||
      elderSignSuccessSteps.value.length > 0),
)

const hasRevelation = computed(
  () => !!props.canRevelation && (!!props.revelationImplied || revelation.value),
)

type EntryKind = 'ability' | 'constant' | 'revelation' | 'elderSign'
type Entry = {
  key: string
  label: string
  kind: EntryKind
  index: number
  fixed: boolean
  /** The card symbol for this kind, where it has one. */
  icon?: string
}

/* Everything this card does, in one list. Repeated labels are numbered, because
 * two abilities both called Forced are otherwise two identical tabs. */
const entries = computed<Entry[]>(() => {
  const out: Entry[] = []
  abilities.value.forEach((ability, index) =>
    out.push({
      key: `ability:${index}`,
      label: abilityLabel(ability),
      kind: 'ability',
      index,
      fixed: false,
      icon: abilityKind(ability)?.icon,
    }),
  )
  modifiers.value.forEach((_, index) =>
    out.push({ key: `constant:${index}`, label: 'Constant', kind: 'constant', index, fixed: false }),
  )
  if (hasRevelation.value) {
    out.push({
      key: 'revelation',
      label: 'Revelation',
      kind: 'revelation',
      index: 0,
      fixed: !!props.revelationImplied,
    })
  }
  if (hasElderSign.value) {
    out.push({
      key: 'elderSign',
      label: 'Elder sign',
      kind: 'elderSign',
      index: 0,
      fixed: false,
      icon: 'elder-sign',
    })
  }

  const seen = new Map<string, number>()
  const totals = new Map<string, number>()
  for (const entry of out) totals.set(entry.label, (totals.get(entry.label) ?? 0) + 1)
  return out.map((entry) => {
    if ((totals.get(entry.label) ?? 0) < 2) return entry
    const at = (seen.get(entry.label) ?? 0) + 1
    seen.set(entry.label, at)
    return { ...entry, label: `${entry.label} ${at}` }
  })
})

/* Which tab is open. Held as the entry's key rather than a position so removing
 * one does not silently open a different one; an unknown key falls back to the
 * first, which is what happens after a removal. */
const openKey = ref('')
const open = computed(() => entries.value.find((e) => e.key === openKey.value) ?? entries.value[0])
const isOpen = (kind: EntryKind, index = 0) =>
  open.value?.kind === kind && open.value.index === index

// --- reordering ---

/* Tabs drag to reorder, within their own kind: an ability's position is its
 * number on the card, and a constant's is the order its modifiers are collected
 * in. Revelation and the elder sign are one each, so there is nothing to order
 * and they do not drag. */
const canOrder = (entry: Entry) => entry.kind === 'ability' || entry.kind === 'constant'

const dragKey = ref('')
const dropKey = ref('')
const dragged = computed(() => entries.value.find((e) => e.key === dragKey.value))

const dropAllowed = (target: Entry) => {
  const from = dragged.value
  return !!from && from.key !== target.key && from.kind === target.kind && canOrder(target)
}

function onDragStart(event: DragEvent, entry: Entry) {
  if (!canOrder(entry)) return event.preventDefault()
  dragKey.value = entry.key
  if (event.dataTransfer) event.dataTransfer.effectAllowed = 'move'
}

function onDragOver(entry: Entry) {
  dropKey.value = dropAllowed(entry) ? entry.key : ''
}

function endDrag() {
  dragKey.value = ''
  dropKey.value = ''
}

function onDrop(target: Entry) {
  const from = dragged.value
  if (!from || !dropAllowed(target)) return endDrag()

  const list = from.kind === 'ability' ? abilities.value : modifiers.value
  const next = [...list]
  const [item] = next.splice(from.index, 1)
  // The target's index shifts left by one when the dragged item came before it.
  const at = target.index > from.index ? target.index - 1 : target.index
  next.splice(at, 0, item)

  if (from.kind === 'ability') emit('update:abilities', next)
  else emit('update:modifiers', next)
  openKey.value = `${from.kind}:${at}`
  endDrag()
}

// --- adding ---

const adding = ref(false)

const addable = computed(() => {
  const out = ABILITY_KINDS.map((k) => ({ key: `type:${k.label}`, label: k.label }))
  out.push({ key: 'type:other', label: 'Other…' })
  if (props.canRevelation && !hasRevelation.value) out.push({ key: 'revelation', label: 'Revelation' })
  if (props.isInvestigator && !hasElderSign.value) out.push({ key: 'elderSign', label: 'Elder sign' })
  out.push({ key: 'constant', label: 'Constant' })
  return out
})

function add(key: string) {
  adding.value = false
  if (key === 'revelation') {
    revelation.value = true
    openKey.value = 'revelation'
    return
  }
  if (key === 'elderSign') {
    elderSign.value = '1'
    openKey.value = 'elderSign'
    return
  }
  if (key === 'constant') {
    // The list this reads is still the old one, so its length is the new index.
    openKey.value = `constant:${modifiers.value.length}`
    emit('update:modifiers', [...modifiers.value, { kind: 'enemy', matcher: null, modifiers: [] }])
    return
  }
  const kind = ABILITY_KINDS.find((k) => `type:${k.label}` === key)
  openKey.value = `ability:${abilities.value.length}`
  emit('update:abilities', [
    ...abilities.value,
    { type: kind ? blankAbilityType(kind.tags) : null, steps: [] },
  ])
}

/* The panel's own Remove button: only the open entry has one on screen. */
const removeOpen = () => {
  if (open.value) removeEntry(open.value)
}

function removeEntry(entry: Entry) {
  // Falls back to the first entry: whatever was open has just gone.
  openKey.value = ''
  if (entry.kind === 'ability') return removeAbility(entry.index)
  if (entry.kind === 'constant') return removeModifier(entry.index)
  if (entry.kind === 'revelation') {
    revelation.value = false
    revelationPlacement.value = ''
    revelationSteps.value = []
    return
  }
  elderSign.value = ''
  elderSignRevealSteps.value = []
  elderSignSteps.value = []
  elderSignSuccessSteps.value = []
}

// --- abilities ---

const setAbility = (i: number, changes: Record<string, any>) =>
  emit('update:abilities', patch(abilities.value, i, changes))

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

const removeModifier = (i: number) =>
  emit('update:modifiers', modifiers.value.filter((_, j) => j !== i))

/* A modifier can be gated two ways. `if` asks the game a question; `requires`
 * only compares bindings, which is what you need when the question itself would
 * ask for modifiers while modifiers are being collected -- telling a card in
 * hand from the same card committed, say. */
const modifierRequires = (modifier: any): [string, string][] => modifier.requires ?? []

const setModifierRequirement = (index: number, at: number, side: 0 | 1, value: string) => {
  const requires = modifierRequires(modifiers.value[index]).map((pair, i) =>
    i === at ? (side === 0 ? [value, pair[1]] : [pair[0], value]) : pair,
  )
  setModifier(index, { requires })
}

const addModifierRequirement = (index: number) =>
  setModifier(index, { requires: [...modifierRequires(modifiers.value[index]), ['$placement', '']] })

const removeModifierRequirement = (index: number, at: number) =>
  setModifier(index, {
    requires: modifierRequires(modifiers.value[index]).filter((_, i) => i !== at),
  })

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

/* What an ability's or a handler's steps start with in scope. The card's own
 * bindings are always there; the rest come from whatever the ability triggers
 * on or the handler listens for, anchored so a field can jump back to it. */
const abilityAnchor = (index: number) => `ccb-ability-${index}-window`
const handlerAnchor = (index: number) => `ccb-handler-${index}-message`

/* Until a window is chosen there is nothing truthful to say about `$w0`… —
 * not even how many there are — so nothing is contributed. The field will then
 * report an unresolved `$w3` as unbound, which is the right prompt: pick the
 * window. */
function abilityScope(ability: any, index: number): Binding[] {
  const name = abilityWindow(ability)
  const base = [...cardBindings(props.cardType), ...paymentBindings()]
  if (!name) return base
  return [...base, ...windowBindings(windowFields(name), name, abilityAnchor(index))]
}

function handlerScope(handler: any, index: number): Binding[] {
  const tag = handler.on
  if (!knownMessage(tag)) return cardBindings(props.cardType)
  return [...cardBindings(props.cardType), ...messageBindings(messageFields(tag), tag, handlerAnchor(index))]
}

</script>

<template>
  <div class="ability-editor">
    <p v-if="!schemaLoaded" class="loading">Loading type schema…</p>

    <template v-else>
      <template v-if="props.section === 'abilities'">
      <!-- Sits on top of the open panel, joined to it: the active tab has no
           bottom edge, so the two read as one box with a tab row. -->
      <div class="entry-tabs" role="tablist" aria-label="What this card does">
        <button
          v-for="entry in entries"
          :key="entry.key"
          type="button"
          role="tab"
          :class="{
            on: open?.key === entry.key,
            orderable: canOrder(entry),
            dragging: dragKey === entry.key,
            'drop-into': dropKey === entry.key,
          }"
          :aria-selected="open?.key === entry.key"
          :draggable="canOrder(entry)"
          @click="openKey = entry.key"
          @dragstart="onDragStart($event, entry)"
          @dragover.prevent="onDragOver(entry)"
          @dragleave="dropKey = ''"
          @drop.prevent="onDrop(entry)"
          @dragend="endDrag"
        >
          <i v-if="entry.icon" :class="entry.icon" aria-hidden="true" />{{ entry.label }}
        </button>
        <button
          type="button"
          class="entry-add"
          :aria-expanded="adding"
          @click="adding = !adding"
        >
          + Add
        </button>
      </div>

      <select v-if="adding" class="entry-menu" @change="add(($event.target as HTMLSelectElement).value)">
        <option value="">Ability type</option>
        <option v-for="choice in addable" :key="choice.key" :value="choice.key">
          {{ choice.label }}
        </option>
      </select>

      <p v-if="!entries.length" class="hint muted">
        Nothing yet. A card with no abilities, no modifiers and no revelation just sits there.
      </p>

      <div
        v-for="(ability, index) in abilities"
        :key="index"
        v-show="isOpen('ability', index)"
        class="block"
      >
        <div class="block-head">
          <strong
            ><i v-if="abilityKind(ability)?.icon" :class="abilityKind(ability)?.icon" aria-hidden="true" />{{
              abilityLabel(ability)
            }}</strong
          >
          <button type="button" @click="removeAbility(index)">Remove</button>
        </div>

        <ValueEditor
          type="AbilityType"
          label="When / how it is used"
          :bindings="cardBindings(props.cardType)"
          :modelValue="ability.type"
          @update:modelValue="setAbility(index, { type: $event })"
        />
        <ValueEditor
          optional
          type="Criterion"
          label="Criteria (optional) — gates whether the ability is available"
          :bindings="cardBindings(props.cardType)"
          :modelValue="ability.criteria"
          @update:modelValue="setAbility(index, { criteria: $event })"
        />
        <ValueEditor
          optional
          type="AbilityLimit"
          label="Limit (optional)"
          :bindings="cardBindings(props.cardType)"
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
        <label>
          Tooltip (optional)
          <input
            :value="ability.tooltip ?? ''"
            placeholder="Forced - When you suffer any number of horror…"
            @input="setAbility(index, { tooltip: ($event.target as HTMLInputElement).value || undefined })"
            @keydown.stop
          />
        </label>

        <template v-if="abilityWindowMatcher(ability)">
          <label :id="abilityAnchor(index)" :class="{ needed: !windowSettled(ability) }">
            Triggers on window
            <select
              :value="ability.windowHint === NO_WINDOW ? NO_WINDOW : abilityWindow(ability)"
              :class="{ needed: !windowSettled(ability) }"
              @change="setAbility(index, { windowHint: ($event.target as HTMLSelectElement).value })"
            >
              <option v-if="!windowSettled(ability)" value="">
                — pick one: $w bindings stay unnamed until you do —
              </option>
              <option :value="NO_WINDOW">— never fires on a window —</option>
              <option
                v-for="name in windowCandidates(abilityWindowMatcher(ability))"
                :key="name"
                :value="name"
              >
                {{ name }}
              </option>
            </select>
          </label>
          <ul v-if="abilityWindow(ability)" class="bindings">
            <li><code>$window</code> the whole window</li>
            <li v-for="(field, at) in windowFields(abilityWindow(ability))" :key="at">
              <code>$w{{ at }}</code> {{ field.name ? `${field.name} ::` : '::' }} {{ field.type }}
            </li>
          </ul>
          <p v-if="windowSettled(ability) && !abilityWindow(ability)" class="hint muted">
            <code>{{ abilityWindowMatcher(ability) }}</code> never fires on its own, so there are
            no <code>$w</code> bindings. The ability is reached another way — an elder sign, or
            another card's Use an ability step.
          </p>
          <p v-else-if="!abilityWindow(ability)" class="hint needed">
            <code>{{ abilityWindowMatcher(ability) }}</code>
            <template v-if="windowsFor(abilityWindowMatcher(ability)).length > 1">
              fires on more than one window, so which fields <code>$w0</code>… are depends on
              which. Its own are listed first.
            </template>
            <template v-else>
              fires on no window of its own, so there are no <code>$wN</code> to describe.
            </template>
          </p>
        </template>

        <StepsEditor
          :queryKinds="QUERY_KINDS"
          :bindings="abilityScope(ability, index)"
          :path="`ability${index}`"
          :modelValue="stepsOf(ability)"
          @update:modelValue="setAbility(index, { steps: $event })"
        />
      </div>


      <div
        v-for="(modifier, index) in modifiers"
        :key="`m${index}`"
        v-show="isOpen('constant', index)"
        class="block"
      >
        <div class="block-head">
          <strong>Constant — gives modifiers to</strong>
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
          :bindings="cardBindings(props.cardType)"
          label="Matcher"
          :modelValue="modifier.matcher"
          @update:modelValue="setModifier(index, { matcher: $event })"
        />
        <ValueEditor
          type="[ModifierType]"
          label="Modifiers"
          :bindings="cardBindings(props.cardType)"
          :modelValue="modifier.modifiers"
          @update:modelValue="setModifier(index, { modifiers: $event })"
        />
        <ValueEditor
          optional
          type="Criterion"
          label="Only if (optional) — a question asked of the game"
          :bindings="cardBindings(props.cardType)"
          :modelValue="modifier.if"
          @update:modelValue="setModifier(index, { if: $event })"
        />

        <div v-for="(pair, at) in modifierRequires(modifier)" :key="at" class="row">
          <label>
            Only when
            <input
              :value="pair[0]"
              :class="{ binding: isBinding(pair[0]) }"
              placeholder="$placement"
              @input="setModifierRequirement(index, at, 0, ($event.target as HTMLInputElement).value)"
              @keydown.stop
            />
          </label>
          <label>
            is
            <input
              :value="pair[1]"
              :class="{ binding: isBinding(pair[1]) }"
              placeholder="$source"
              @input="setModifierRequirement(index, at, 1, ($event.target as HTMLInputElement).value)"
              @keydown.stop
            />
          </label>
          <button type="button" @click="removeModifierRequirement(index, at)">×</button>
        </div>
        <button type="button" class="add" @click="addModifierRequirement(index)">
          + Requirement
        </button>

        <p class="hint">
          Applies while this card is in play, to everything the matcher selects. Match
          <strong>card</strong> rather than an entity to reach a card before it is in play — that is
          what a keyword needs when the engine reads it at draw or spawn time.
        </p>
        <p class="hint muted">
          A requirement only compares bindings, so it can gate on where the card is. Use it rather
          than <em>Only if</em> when the question would ask for modifiers while modifiers are being
          collected.
        </p>
      </div>

      <!-- Revelation: what the card does as it is drawn. Only here at all once
           it has one, which is also how the saved card records it. -->
      <div v-if="hasRevelation" v-show="isOpen('revelation')" class="block">
        <div class="block-head">
          <strong>Revelation</strong>
          <button v-if="!revelationImplied" type="button" @click="removeOpen">Remove</button>
        </div>
        <p v-if="revelationImplied" class="hint">
          This card always resolves as it is drawn, so it always has a revelation.
        </p>
        <label v-if="hasRevelationPlacement">
          Where it ends up
          <select v-model="revelationPlacement">
            <option v-for="place in revelationPlacements ?? []" :key="place.value" :value="place.value">
              {{ place.label }}
            </option>
          </select>
        </label>
        <p class="hint">What it does when it is revealed:</p>
        <StepsEditor
          :queryKinds="QUERY_KINDS"
          :bindings="cardBindings(props.cardType)"
          :path="'revelation'"
          :modelValue="revelationSteps"
          @update:modelValue="revelationSteps = $event"
        />
      </div>

      <div v-if="hasElderSign" v-show="isOpen('elderSign')" class="block">
        <div class="block-head">
          <strong><i class="elder-sign" aria-hidden="true" />Elder sign</strong>
          <button type="button" @click="removeOpen">Remove</button>
        </div>
        <label>
          Modifier
          <input v-model="elderSign" type="number" @keydown.stop />
        </label>
        <p class="hint">
          What happens the moment it is drawn, before anything can react to the reveal — where a
          flag this card's own abilities read has to be set:
        </p>
        <StepsEditor
          :queryKinds="QUERY_KINDS"
          :bindings="cardBindings(props.cardType)"
          :path="'elderSignReveal'"
          :modelValue="elderSignRevealSteps"
          @update:modelValue="elderSignRevealSteps = $event"
        />
        <p class="hint">What it does when it resolves, beyond the modifier:</p>
        <StepsEditor
          :queryKinds="QUERY_KINDS"
          :bindings="cardBindings(props.cardType)"
          :path="'elderSign'"
          :modelValue="elderSignSteps"
          @update:modelValue="elderSignSteps = $event"
        />
        <p class="hint">
          And what it does only if you then succeed — success is not known when the token resolves,
          so these run when the test is passed:
        </p>
        <StepsEditor
          :queryKinds="QUERY_KINDS"
          :bindings="cardBindings(props.cardType)"
          :path="'elderSignSuccess'"
          :modelValue="elderSignSuccessSteps"
          @update:modelValue="elderSignSuccessSteps = $event"
        />
      </div>

      </template>

      <div v-else class="listeners">
        <datalist id="custom-message-tags">
          <option v-for="tag in messageTags" :key="tag" :value="tag" />
        </datalist>
      <div v-for="(handler, index) in handlers" :key="`h${index}`" class="block">
        <div class="block-head">
          <strong>Listener {{ index + 1 }}</strong>
          <button type="button" @click="removeHandler(index)">Remove</button>
        </div>
        <label :id="handlerAnchor(index)">
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
          Fires when a message with this tag mentions this card &mdash; by its target, its source
          or its id. The whole message is <code>$message</code>.
        </p>
        <BoolField
          label="fires for messages that do not mention this card"
          :modelValue="!!handler.global"
          @update:modelValue="setHandler(index, { global: $event || undefined })"
        />
        <p v-if="handler.global" class="hint muted">
          Now runs for every message with this tag, so gate it with a requirement below (an id
          from the message compared against one of this card's, say) or it will fire for
          everyone.
        </p>
        <!-- A message with no fields simply lists nothing. The warning below is
             about the message not being known, which is a different thing. -->
        <ul v-if="messageFields(handler.on).length" class="bindings">
          <li v-for="(field, at) in messageFields(handler.on)" :key="at">
            <code>${{ at }}</code> {{ field.name ? `${field.name} ::` : '::' }} {{ field.type }}
          </li>
        </ul>
        <p v-if="handler.on && !knownMessage(handler.on)" class="hint muted">
          Not a message the engine sends.
        </p>

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
          :bindings="handlerScope(handler, index)"
          :path="`handler${index}`"
          :modelValue="stepsOf(handler)"
          @update:modelValue="setHandler(index, { steps: $event })"
        />
      </div>

      <button type="button" class="add" @click="addHandler">+ Listener</button>
      </div>
    </template>
  </div>
</template>

<style scoped lang="scss">
.ability-editor {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

/* The same quiet tabs the form uses one level out, so a nested strip does not
   read as a second, louder control. */
/* The card symbols come from the icon font, which renders them through `i` --
   reset the italics it would otherwise inherit. */
.entry-tabs i,
.block-head i {
  font-style: normal;
  margin-right: 0.3em;
}

/* The elder sign draws at 1.3em (icons.css), so it overruns the `i`'s own advance
   and swallows a margin set in the `i`'s smaller em. The gap goes on the glyph
   itself, where the em is the one it is drawn at. */
.entry-tabs i.elder-sign::before,
.block-head i.elder-sign::before {
  padding-right: 0.3em;
}

/* Violet: the one hue this form was not already using. Light blue means a live
   binding, lime means a card code that resolves, red means something wrong --
   the open tab needed a colour of its own rather than borrowing one of those. */
/* Attached to the panel below it, so no rule of its own and no gap: the strip
   cancels the column gap it would otherwise inherit, and the open tab drops its
   bottom edge to join the box. */
.entry-tabs {
  align-items: flex-end;
  display: flex;
  flex-wrap: wrap;
  gap: 0.1rem;
  margin-bottom: -0.6rem;
  padding: 0 0.35rem;
  position: relative;
  z-index: 1;

  button {
    background: none;
    border: none;
    border-radius: 4px 4px 0 0;
    color: #9ca3af;
    cursor: pointer;
    font-size: 0.78rem;
    padding: 0.2rem 0.55rem 0.5rem;
    transition: background 0.12s ease, color 0.12s ease;

    &:hover {
      color: #cbd2dd;
    }

    /* Same edges as the panel, minus the one they share. */
    &.on {
      background: rgba(196, 181, 253, 0.12);
      border: 1px solid #374151;
      border-bottom: none;
      color: #c4b5fd;
      margin-bottom: -1px;
      padding-bottom: calc(0.5rem + 1px);
    }

    &:focus-visible {
      outline: 1px solid #c4b5fd;
      outline-offset: -1px;
    }

    &.orderable {
      cursor: grab;
    }

    &.dragging {
      cursor: grabbing;
      opacity: 0.4;
    }

    /* Where it would land: the dragged tab takes this one's place. */
    &.drop-into {
      box-shadow: inset 2px 0 0 #c4b5fd;
    }
  }
}

/* Not a tab: this one does something, so it is shaped like a button rather than
   borrowing the strip's flat treatment. */
.entry-tabs .entry-add {
  background: rgba(255, 255, 255, 0.07);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #d1d5db;
  font-weight: 600;
  /* Lifted off the panel's edge: the tabs join the box, this does not. The strip
     grows to fit, which is where the clearance comes from. */
  margin-bottom: 0.3rem;
  margin-left: 0.4rem;
  padding: 0.2rem 0.7rem;

  &:hover {
    background: rgba(255, 255, 255, 0.14);
    border-color: #6b7280;
    color: #eee;
  }

  &:focus-visible {
    outline: 1px solid #6b7280;
    outline-offset: 1px;
  }
}

.entry-menu {
  align-self: flex-start;
}

.listeners {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

/* Flashed when a field jumps here to show where a binding came from. The class
 * is set from outside this component, which scoped styles still match: the rule
 * keys off the element's own attribute, not on who added the class. */
.binding-flash {
  animation: binding-flash 1.4s ease-out;
  border-radius: 4px;
}

@keyframes binding-flash {
  0%,
  55% {
    box-shadow: 0 0 0 2px #14b8a6;
  }
  100% {
    box-shadow: 0 0 0 2px transparent;
  }
}

.needed {
  color: #fbbf24;

  select {
    border-color: #b45309;
  }
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

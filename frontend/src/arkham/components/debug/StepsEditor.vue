<script lang="ts" setup>
/* A list of steps, and the steps inside them.
 *
 * Branches and choices carry their own steps, so this renders itself for those
 * — which is what lets an ability say "if it is ready, attack; otherwise ready
 * it" or "choose an event, then play it". */
import { computed, ref } from 'vue'
import { vFocus } from '@/arkham/components/debug/vFocus'
import { onClickOutside } from '@vueuse/core'
import ExpressionEditor from '@/arkham/components/debug/ExpressionEditor.vue'
import BoolField from '@/arkham/components/debug/BoolField.vue'
import CardCodeField from '@/arkham/components/debug/CardCodeField.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'
import {
  jumpToBinding,
  scopeAt,
  scopeInside,
  stepAnchor,
  stepBindings,
  type Binding,
} from '@/arkham/customCardBindings'

const props = defineProps<{
  modelValue: any[]
  queryKinds: Record<string, string>
  /** What is already in scope where this list of steps sits. */
  bindings?: Binding[]
  /** Identifies this list so each step gets a stable anchor to jump to. */
  path?: string
  /* What to announce at the top. Defaults to everything in scope, which is what
   * a top-level list wants; a nested list passes only what its enclosing step
   * adds, since the rest was already announced further up. */
  announce?: Binding[]
}>()
const emit = defineEmits<{ 'update:modelValue': [v: any[]] }>()

const steps = computed(() => props.modelValue ?? [])

type StepKind =
  | 'query'
  | 'let'
  | 'push'
  | 'if'
  | 'when'
  | 'case'
  | 'forEach'
  | 'modify'
  | 'withSkillTest'
  | 'withLocationOf'
  | 'choose'
  | 'chooseFrom'
  | 'playCard'
  | 'useAbility'
  | 'cancelBatch'
  | 'fight'
  | 'investigate'
  | 'evade'
  | 'parley'
  | 'attack'
  | 'ready'
  | 'draw'
  | 'gather'
  | 'customize'

const KIND_LABELS: Record<StepKind, string> = {
  query: 'Query',
  let: 'Let',
  push: 'Push',
  if: 'If',
  when: 'When',
  case: 'Case',
  forEach: 'For each',
  modify: 'Modify',
  withSkillTest: 'With skill test',
  withLocationOf: 'With location of',
  choose: 'Choose',
  chooseFrom: 'Choose from',
  playCard: 'Play a card',
  useAbility: 'Use an ability',
  cancelBatch: 'Cancel what would happen',
  fight: 'Fight',
  investigate: 'Investigate',
  evade: 'Evade',
  parley: 'Parley',
  attack: 'Attack',
  ready: 'Ready',
  draw: 'Draw cards',
  gather: 'Gather',
  customize: 'Customize',
}

function kindOf(step: any): StepKind {
  const kinds = [
    'query',
    'let',
    'push',
    'when',
    'if',
    'case',
    'forEach',
    'modify',
    'withSkillTest',
    'withLocationOf',
    'choose',
    'chooseFrom',
    'playCard',
    'useAbility',
    'cancelBatch',
    'fight',
    'investigate',
    'evade',
    'parley',
    'attack',
    'ready',
    'draw',
    'gather',
    'customize',
  ] as StepKind[]
  for (const kind of kinds) {
    if (kind in (step ?? {})) return kind
  }
  return 'push'
}

const blankStep = (kind: StepKind) =>
  ({
    query: { query: { kind: 'enemy', matcher: null }, bind: '', mode: 'all' },
    let: { let: '', be: null },
    push: { push: null },
    if: { if: { kind: 'enemy', matcher: null }, then: [], else: [] },
    when: { when: { kind: 'enemy', matcher: null }, then: [] },
    case: { case: [{ if: { kind: 'enemy', matcher: null }, steps: [] }], else: [] },
    forEach: { forEach: { query: { kind: 'enemy', matcher: null }, bind: 'each', steps: [] } },
    modify: { modify: { target: null, modifiers: [] } },
    withSkillTest: { withSkillTest: { bind: 'skillTestId', steps: [] } },
    withLocationOf: { withLocationOf: { kind: 'investigator', of: '$iid', bind: 'location', steps: [] } },
    choose: { choose: { options: [{ label: '', steps: [] }] } },
    chooseFrom: { chooseFrom: { query: { kind: 'enemy', matcher: null }, bind: 'chosen', steps: [] } },
    playCard: { playCard: { optional: true, matcher: null } },
    useAbility: { useAbility: { index: 1, optional: true } },
    cancelBatch: { cancelBatch: true },
    fight: { fight: { matcher: null, modifiers: [] } },
    investigate: { investigate: { modifiers: [] } },
    evade: { evade: { matcher: null, modifiers: [] } },
    parley: { parley: { target: null, modifiers: [] } },
    attack: { attack: {} },
    ready: { ready: {} },
    draw: { draw: { amount: 1 } },
    gather: { gather: { cardCode: '' } },
    customize: { customize: { optional: true } },
  })[kind]

/* One line each, taken from what the step's own editor says once it is there.
 * Picking a step means knowing what it does before you have one, which a list of
 * bare names cannot tell you -- Gather and Draw are both about cards, Choose and
 * Choose from are a word apart and are not the same thing at all. */
const KIND_HELP: Record<StepKind, string> = {
  query: 'Superseded — a Let can bind a query directly.',
  let: 'Works something out and binds it to a name for the steps after this one.',
  push: 'Puts a message on the queue, written as the engine spells it.',
  if: 'Runs a matcher and takes the first branch when it finds anything.',
  when: 'Runs its steps only when a matcher finds something. An If with no else.',
  case: 'Takes the first branch whose condition holds.',
  forEach: 'Runs its steps once per thing found, with that thing bound inside.',
  modify: 'Gives something modifiers for as long as a window lasts.',
  withSkillTest: 'Runs its steps during a skill test, with that test bound inside.',
  withLocationOf: 'Runs its steps where something is, with that location bound inside.',
  choose: 'Offers the player named options, each with steps of its own.',
  chooseFrom: 'One option per thing a matcher finds, with it bound for the steps below.',
  playCard: 'Offers the cards that could be played and pays for the one chosen.',
  useAbility: "Resolves one of this card's own abilities, paying its cost.",
  cancelBatch: 'Stops what the ability is reacting to. Needs a "would" window.',
  fight: 'Fights an enemy. The test it starts is $sid.',
  investigate: 'Investigates, the way the action does. The test it starts is $sid.',
  evade: 'Evades an enemy. The test it starts is $sid.',
  parley: 'Parleys against something, naming the target, skill and difficulty itself.',
  attack: 'This card attacks — an enemy making an immediate attack.',
  ready: 'Readies this card, or the one chosen.',
  draw: 'Draws cards. Nothing is drawn when the amount works out to zero or less.',
  gather: 'Shuffles a card into the encounter deck.',
  customize: 'Marks a checkbox on the upgrade sheet of a customizable card.',
}

/* A query is an expression now, so binding one is what a Let does and a Query
 * step adds nothing a Let cannot say. The kind stays known -- cards already
 * written hold Query steps, and those keep running and keep their editor -- it
 * is simply not offered for anything new. */
const ADDABLE = (Object.keys(KIND_LABELS) as StepKind[])
  .filter((k) => k !== 'query')
  // By what the menu shows, not by the key behind it, since that is what is read.
  .sort((a, b) => KIND_LABELS[a].localeCompare(KIND_LABELS[b]))

const set = (index: number, step: any) =>
  emit('update:modelValue', steps.value.map((s, i) => (i === index ? step : s)))

const add = (kind: StepKind) => emit('update:modelValue', [...steps.value, blankStep(kind)])

/* Swapping between the two is only ever moving the condition between keys, and
 * dropping an else that was empty anyway. Offered rather than done automatically
 * because an empty else is a legitimate thing to be part way through writing. */
const toWhen = (index: number, step: any) =>
  set(index, { when: step.if, then: step.then ?? [] })

const toIf = (index: number, step: any) =>
  set(index, { if: step.when, then: step.then ?? [], else: [] })

const elseIsEmpty = (step: any) => !step.else || step.else.length === 0

/* Lift a body out of its condition, leaving the steps where the block was. For
 * when the check has moved somewhere else -- an ability's criteria, say -- and
 * the block is now a wrapper around nothing. */
const promote = (index: number, step: any) => {
  const body = step.then ?? []
  emit('update:modelValue', [...steps.value.slice(0, index), ...body, ...steps.value.slice(index + 1)])
}
const remove = (index: number) => emit('update:modelValue', steps.value.filter((_, i) => i !== index))

const SKILLS = ['SkillWillpower', 'SkillIntellect', 'SkillCombat', 'SkillAgility']

/* A step's own fields see what the steps before it bound; the steps nested
 * inside it also see whatever it binds for them. */
const base = computed(() => props.bindings ?? [])
const path = computed(() => props.path ?? 'steps')
const anchorFor = (index: number) => stepAnchor(path.value, index)
const scopeFor = (index: number) => scopeAt(base.value, steps.value, index, path.value)
const innerScope = (index: number) => scopeInside(base.value, steps.value, index, path.value)
const innerPath = (index: number, branch = '') =>
  `${path.value}-${index}${branch ? `-${branch}` : ''}`

const announced = computed(() => props.announce ?? base.value)

// What an enclosing step adds for the steps nested in it, so the nested list
// announces `$chosen` without repeating everything it inherited.
const addedInside = (index: number) =>
  stepBindings(steps.value[index], anchorFor(index)).inside

/* "If such a chaos token is revealed during this test, …" — a rider on the step
 * that starts the test, so it can name that test. Its steps see $sid, which the
 * step binds. */
const onReveal = (step: any, kind: string) => step[kind]?.onReveal ?? null

const setOnReveal = (index: number, kind: string, value: any) =>
  set(index, { ...steps.value[index], [kind]: { ...steps.value[index][kind], onReveal: value } })

const revealScope = (index: number) => [
  ...scopeFor(index),
  ...stepBindings(steps.value[index], anchorFor(index)).after,
]

/* The Locateable instances the runner dispatches on. Which one it is has to be
 * said, because an id is a bare uuid and the instance cannot be chosen from it. */
const LOCATEABLE = ['investigator', 'enemy', 'asset', 'treachery']

const matcherType = (kind: string | undefined) => props.queryKinds[kind ?? 'enemy'] ?? 'EnemyMatcher'

/* The names a step introduces, shown on the step that introduces them.
 *
 * A binding is just a string somewhere in a later step, so nothing about the
 * JSON says where one came from -- and a name that is never bound fails
 * silently rather than complaining. Naming them at the point they are created
 * is the only thing that makes them discoverable.
 *
 * `forEach` and `chooseFrom` bind inside their own steps rather than after
 * themselves, which is why they say so. */
function bindsOf(
  step: any,
  index: number,
): { name: string; scope: string; type?: string }[] {
  // Given what is in scope here, so a `let` can say what its expression came
  // out as rather than only that it bound something.
  const { after, inside } = stepBindings(step, '', scopeFor(index))
  return [
    ...after.map((b) => ({ name: b.name, scope: 'later steps', type: b.type })),
    ...inside.map((b) => ({ name: b.name, scope: 'the steps inside', type: b.type })),
  ]
}

// --- choose options ---

const addingStep = ref(false)
// Clicking away closes it, the way every other menu in the builder behaves.
const addEl = ref<HTMLElement | null>(null)
onClickOutside(addEl, () => (addingStep.value = false))

function addAndClose(kind: StepKind) {
  add(kind)
  addingStep.value = false
  addSearch.value = ''
}

/* Typed against both the name and what it does, so "during a test" finds
 * With skill test without knowing it is called that. */
const addSearch = ref('')

const matchingKinds = computed(() => {
  const needle = addSearch.value.trim().toLowerCase()
  if (!needle) return ADDABLE
  return ADDABLE.filter(
    (k) =>
      KIND_LABELS[k].toLowerCase().includes(needle) || KIND_HELP[k].toLowerCase().includes(needle),
  )
})

function toggleAdd() {
  addSearch.value = ''
  addingStep.value = !addingStep.value
}

// Enter takes the first match, which is the one the list marks.
const addFirstMatch = () => {
  const [first] = matchingKinds.value
  if (first) addAndClose(first)
}

const optionsOf = (step: any): any[] => step.choose?.options ?? []

const setOption = (step: any, index: number, option: any, at: number) =>
  set(at, { ...step, choose: { ...step.choose, options: optionsOf(step).map((o, i) => (i === index ? option : o)) } })

const addOption = (step: any, at: number) =>
  set(at, { ...step, choose: { ...step.choose, options: [...optionsOf(step), { label: '', steps: [] }] } })

const removeOption = (step: any, index: number, at: number) =>
  set(at, { ...step, choose: { ...step.choose, options: optionsOf(step).filter((_, i) => i !== index) } })
</script>

<template>
  <div class="steps">
    <p v-if="announced.length" class="scope-bar">
      <span class="scope-label">In scope here:</span>
      <button
        v-for="bound in announced"
        :key="bound.name"
        type="button"
        class="scope-chip"
        :class="{ jumpable: !!bound.anchor }"
        :title="`${bound.detail ? bound.detail + ' · ' : ''}${bound.origin}${bound.anchor ? ' — click to show' : ''}`"
        @click="jumpToBinding(bound.anchor)"
      >
        ${{ bound.name }}
      </button>
    </p>
    <div v-for="(step, index) in steps" :key="index" :id="anchorFor(index)" class="step">
      <div class="step-head">
        <span class="step-kind">{{ KIND_LABELS[kindOf(step)] }}</span>
        <button type="button" class="step-remove" title="Remove this step" @click="remove(index)">
          ×
        </button>
      </div>

      <div class="step-body">

      <template v-if="kindOf(step) === 'query'">
        <p class="hint">
          A Let step can bind a query directly, which is how new ones are written.
          This still works; there is nothing to fix here.
        </p>
        <div class="row">
          <label>
            Kind
            <select
              :value="step.query?.kind"
              @change="set(index, { ...step, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            Bind to
            <input
              :value="step.bind"
              placeholder="enemies"
              @input="set(index, { ...step, bind: ($event.target as HTMLInputElement).value })"
              @keydown.stop
            />
          </label>
          <label>
            Mode
            <select :value="step.mode ?? 'all'" @change="set(index, { ...step, mode: ($event.target as HTMLSelectElement).value })">
              <option value="all">all</option>
              <option value="first">first</option>
              <option value="count">count</option>
            </select>
          </label>
        </div>
        <ValueEditor
          :bindings="scopeFor(index)"
          :type="matcherType(step.query?.kind)"
          label="Matcher"
          :modelValue="step.query?.matcher"
          @update:modelValue="set(index, { ...step, query: { ...step.query, matcher: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'let'">
        <label>
          Name
          <input
            :value="step.let"
            placeholder="icons"
            @input="set(index, { ...step, let: ($event.target as HTMLInputElement).value })"
            @keydown.stop
          />
        </label>
        <ExpressionEditor
          :queryKinds="queryKinds"
          label="Expression"
          :bindings="scopeFor(index)"
          :modelValue="step.be"
          @update:modelValue="set(index, { ...step, be: $event })"
        />

      </template>

      <ValueEditor
          :bindings="scopeFor(index)"
        v-else-if="kindOf(step) === 'push'"
        type="Message"
        label="Message"
        :modelValue="step.push"
        @update:modelValue="set(index, { ...step, push: $event })"
      />

      <template v-else-if="kindOf(step) === 'if'">
        <p class="hint">Runs the matcher; takes the first branch when it finds anything.</p>
        <label>
          Kind
          <select
            :value="step.if?.kind"
            @change="set(index, { ...step, if: { kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
          >
            <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
          </select>
        </label>
        <ValueEditor
          :bindings="scopeFor(index)"
          :type="matcherType(step.if?.kind)"
          label="Matcher"
          :modelValue="step.if?.matcher"
          @update:modelValue="set(index, { ...step, if: { ...step.if, matcher: $event } })"
        />
        <!-- Both branches belong to the If above them, so they are bounded and
             indented under it rather than left as two labelled step lists that
             read as siblings of the matcher. -->
        <div class="branch">
          <span class="branch-label">Then</span>
          <StepsEditor
            :bindings="innerScope(index)"
            :path="innerPath(index, 'then')"
            :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="step.then ?? []"
            @update:modelValue="set(index, { ...step, then: $event })"
          />
        </div>
        <div class="branch">
          <span class="branch-label">Else</span>
          <StepsEditor
            :bindings="innerScope(index)"
            :path="innerPath(index, 'else')"
            :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="step.else ?? []"
            @update:modelValue="set(index, { ...step, else: $event })"
          />
        </div>
        <button
          v-if="elseIsEmpty(step)"
          type="button"
          class="convert"
          title="Nothing happens otherwise, so the else has nothing to say"
          @click="toWhen(index, step)"
        >
          Make this a When
        </button>
      </template>

      <template v-else-if="kindOf(step) === 'when'">
        <p class="hint">Runs the steps below only when the matcher finds something.</p>
        <label>
          Kind
          <select
            :value="step.when?.kind"
            @change="set(index, { ...step, when: { kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
          >
            <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
          </select>
        </label>
        <ValueEditor
          :bindings="scopeFor(index)"
          :type="matcherType(step.when?.kind)"
          label="Matcher"
          :modelValue="step.when?.matcher"
          @update:modelValue="set(index, { ...step, when: { ...step.when, matcher: $event } })"
        />
        <div class="branch">
          <span class="branch-label">Then</span>
          <StepsEditor
            :bindings="innerScope(index)"
            :path="innerPath(index, 'then')"
            :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="step.then ?? []"
            @update:modelValue="set(index, { ...step, then: $event })"
          />
        </div>
        <div class="step-actions">
          <button
            type="button"
            class="convert"
            title="Say what happens when it finds nothing"
            @click="toIf(index, step)"
          >
            Make this an If
          </button>
          <button
            type="button"
            class="convert"
            title="Drop the condition and leave the steps where this block was"
            @click="promote(index, step)"
          >
            Drop the condition
          </button>
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'case'">
        <p class="hint">The first branch whose condition holds.</p>
        <div v-for="(b, bi) in step.case ?? []" :key="bi" class="option">
          <div class="row">
            <label>
              Kind
              <select
                :value="b.if?.kind"
                @change="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, if: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } : x) })"
              >
                <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
              </select>
            </label>
            <button type="button" @click="set(index, { ...step, case: step.case.filter((_: any, i: number) => i !== bi) })">×</button>
          </div>
          <ValueEditor
          :bindings="scopeFor(index)"
            :type="matcherType(b.if?.kind)"
            label="Matcher"
            :modelValue="b.if?.matcher"
            @update:modelValue="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, if: { ...x.if, matcher: $event } } : x) })"
          />
          <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, `case${bi}`)"
          :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="b.steps ?? []"
            @update:modelValue="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, steps: $event } : x) })"
          />
        </div>
        <button type="button" class="add" @click="set(index, { ...step, case: [...(step.case ?? []), { if: { kind: 'enemy', matcher: null }, steps: [] }] })">
          + Branch
        </button>
        <div class="branch">
          <span class="branch-label">Else</span>
          <StepsEditor
            :bindings="innerScope(index)"
            :path="innerPath(index, 'else')"
            :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="step.else ?? []"
            @update:modelValue="set(index, { ...step, else: $event })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'forEach'">
        <p class="hint">Runs the steps once per thing found, with it bound below.</p>
        <div class="row">
          <label>
            Kind
            <select
              :value="step.forEach?.query?.kind"
              @change="set(index, { ...step, forEach: { ...step.forEach, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            Bind to
            <input
              :value="step.forEach?.bind"
              @input="set(index, { ...step, forEach: { ...step.forEach, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <ValueEditor
          :bindings="scopeFor(index)"
          :type="matcherType(step.forEach?.query?.kind)"
          label="Matcher"
          :modelValue="step.forEach?.query?.matcher"
          @update:modelValue="set(index, { ...step, forEach: { ...step.forEach, query: { ...step.forEach.query, matcher: $event } } })"
        />
        <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, 'forEach')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.forEach?.steps ?? []"
          @update:modelValue="set(index, { ...step, forEach: { ...step.forEach, steps: $event } })"
        />
      </template>

      <!-- Blocks: what they bind is in scope only for the steps inside them,
           because outside the block there may be no such thing to name. -->
      <template v-else-if="kindOf(step) === 'withSkillTest'">
        <p class="hint">
          Runs the steps below only while a skill test is being resolved, with that test bound
          for them. Nothing happens when there is no test.
        </p>
        <label>
          Bind to
          <input
            :value="step.withSkillTest?.bind"
            placeholder="skillTestId"
            @input="set(index, { ...step, withSkillTest: { ...step.withSkillTest, bind: ($event.target as HTMLInputElement).value } })"
            @keydown.stop
          />
        </label>
        <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, 'withSkillTest')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.withSkillTest?.steps ?? []"
          @update:modelValue="set(index, { ...step, withSkillTest: { ...step.withSkillTest, steps: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'withLocationOf'">
        <p class="hint">
          Runs the steps below where something is, with that location bound for them. Nothing
          happens when it is nowhere.
        </p>
        <div class="row">
          <label>
            Of what
            <select
              :value="step.withLocationOf?.kind ?? 'investigator'"
              @change="set(index, { ...step, withLocationOf: { ...step.withLocationOf, kind: ($event.target as HTMLSelectElement).value } })"
            >
              <option v-for="k in LOCATEABLE" :key="k" :value="k">{{ k }}</option>
            </select>
          </label>
          <label>
            Bind to
            <input
              :value="step.withLocationOf?.bind"
              placeholder="location"
              @input="set(index, { ...step, withLocationOf: { ...step.withLocationOf, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <ExpressionEditor
          :queryKinds="queryKinds"
          label="Which one"
          :bindings="scopeFor(index)"
          :modelValue="step.withLocationOf?.of"
          @update:modelValue="set(index, { ...step, withLocationOf: { ...step.withLocationOf, of: $event } })"
        />
        <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, 'withLocationOf')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.withLocationOf?.steps ?? []"
          @update:modelValue="set(index, { ...step, withLocationOf: { ...step.withLocationOf, steps: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'modify'">
        <p class="hint">
          The source is this card and the modifiers carry no card of their own, so only what is
          modified, for how long, and with what are asked for. Left alone, the window is the
          skill test being resolved — what "+2 for this test" means.
        </p>
        <ValueEditor
          type="Target"
          label="What is modified"
          :bindings="scopeFor(index)"
          :modelValue="step.modify?.target"
          @update:modelValue="set(index, { ...step, modify: { ...step.modify, target: $event } })"
        />
        <ValueEditor
          optional
          type="EffectWindow"
          label="For how long (defaults to this skill test)"
          :bindings="scopeFor(index)"
          :modelValue="step.modify?.window"
          @update:modelValue="set(index, { ...step, modify: { ...step.modify, window: $event } })"
        />
        <ValueEditor
          type="[ModifierType]"
          label="Modifiers"
          :bindings="scopeFor(index)"
          :modelValue="step.modify?.modifiers"
          @update:modelValue="set(index, { ...step, modify: { ...step.modify, modifiers: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'choose'">
        <div v-for="(option, oi) in optionsOf(step)" :key="oi" class="option">
          <div class="row">
            <label>
              Option label
              <input
                :value="option.label"
                @input="setOption(step, oi, { ...option, label: ($event.target as HTMLInputElement).value }, index)"
                @keydown.stop
              />
            </label>
            <button type="button" @click="removeOption(step, oi, index)">×</button>
          </div>
          <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, `option${oi}`)"
          :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="option.steps ?? []"
            @update:modelValue="setOption(step, oi, { ...option, steps: $event }, index)"
          />
        </div>
        <button type="button" class="add" @click="addOption(step, index)">+ Option</button>
      </template>

      <p v-else-if="kindOf(step) === 'cancelBatch'" class="hint">
        Stops the thing this ability is reacting to, for effects that say "instead". Only works
        in a <code>would</code> window, which is what carries the batch to cancel — and only
        cancels what that batch holds, so anything else the card means to do it must push itself.
      </p>

      <template v-else-if="kindOf(step) === 'useAbility'">
        <p class="hint">
          Resolves one of this card's own abilities, offered the way using it normally would be
          so its cost is paid. Abilities are numbered from 1, in the order they are written.
        </p>
        <div class="row">
          <label>
            Ability
            <input
              type="number"
              min="1"
              :value="step.useAbility?.index ?? 1"
              @input="set(index, { ...step, useAbility: { ...step.useAbility, index: Number(($event.target as HTMLInputElement).value) } })"
              @keydown.stop
            />
          </label>
        </div>
        <!-- The label only exists while declining is allowed, so it shares the
             line with the toggle that decides that. -->
        <div class="row">
          <BoolField
            label="may decline"
            :modelValue="!!step.useAbility?.optional"
            @update:modelValue="set(index, { ...step, useAbility: { ...step.useAbility, optional: $event } })"
          />
          <label v-if="step.useAbility?.optional">
            Decline label
            <input
              :value="step.useAbility?.declineLabel"
              placeholder="Do not"
              @input="set(index, { ...step, useAbility: { ...step.useAbility, declineLabel: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <div class="row">
          <BoolField
            label="ignore its limit"
            :modelValue="!!step.useAbility?.ignoreLimit"
            @update:modelValue="set(index, { ...step, useAbility: { ...step.useAbility, ignoreLimit: $event } })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'playCard'">
        <p class="hint">
          Offers the cards you could play, paying the cost, each shown as the card itself. A
          discount is worked out before the choice, since a card is only playable if you can
          afford it. Naming a card plays that one instead of offering a choice; free skips
          payment entirely, for "without paying its cost".
        </p>
        <div class="row">
          <label>
            This card (optional)
            <input
              :value="step.playCard?.card"
              placeholder="$card"
              @input="set(index, { ...step, playCard: { ...step.playCard, card: ($event.target as HTMLInputElement).value || undefined } })"
              @keydown.stop
            />
          </label>
          <BoolField
            label="without paying its cost"
            :modelValue="!!step.playCard?.free"
            @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, free: $event } })"
          />
        </div>
        <div class="row">
          <label>
            Discount
            <input
              type="number"
              :value="step.playCard?.discount ?? 0"
              @input="set(index, { ...step, playCard: { ...step.playCard, discount: Number(($event.target as HTMLInputElement).value) } })"
              @keydown.stop
            />
          </label>
          <BoolField
            label="may decline"
            :modelValue="!!step.playCard?.optional"
            @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, optional: $event } })"
          />
          <label v-if="step.playCard?.optional">
            Decline label
            <input
              :value="step.playCard?.declineLabel"
              placeholder="Do not"
              @input="set(index, { ...step, playCard: { ...step.playCard, declineLabel: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="CardMatcher"
          label="Which cards"
          :modelValue="step.playCard?.matcher"
          @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, matcher: $event } })"
        />
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Criterion"
          label="Discount only when (optional)"
          :modelValue="step.playCard?.discountIf?.criteria"
          @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, discountIf: $event ? { criteria: $event } : undefined } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'fight'">
        <p class="hint">
          Fight an enemy. Whether the card itself is a fight action comes from its Actions, not
          from here — this is the attack it makes.
        </p>
        <BoolField
          label="a basic fight action instead"
          :modelValue="!!step.fight?.basic"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, basic: $event } })"
        />
        <p v-if="step.fight?.basic" class="hint">
          The enemy's own attack ability, granted so it costs no action. No card can be a basic
          fight action, so modifiers "for this attack" have nowhere to go here.
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="EnemyMatcher"
          label="Which enemies (optional)"
          :modelValue="step.fight?.matcher"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, matcher: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          v-if="!step.fight?.basic"
          type="[ModifierType]"
          label="For this attack"
          :modelValue="step.fight?.modifiers"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, modifiers: $event } })"
        />

        <fieldset class="on-reveal">
          <legend>If a chaos token is revealed during this test</legend>
          <BoolField
            label="it does something"
            :modelValue="!!onReveal(step, 'fight')"
            @update:modelValue="setOnReveal(index, 'fight', $event ? { tokens: null, steps: [] } : undefined)"
          />
          <template v-if="onReveal(step, 'fight')">
            <ValueEditor
              type="ChaosTokenMatcher"
              label="Which tokens"
              :bindings="scopeFor(index)"
              :modelValue="onReveal(step, 'fight').tokens"
              @update:modelValue="setOnReveal(index, 'fight', { ...onReveal(step, 'fight'), tokens: $event })"
            />
            <StepsEditor
              :queryKinds="queryKinds"
              :bindings="revealScope(index)"
              :path="innerPath(index, 'fightreveal')"
              :announce="[]"
              :modelValue="onReveal(step, 'fight').steps ?? []"
              @update:modelValue="setOnReveal(index, 'fight', { ...onReveal(step, 'fight'), steps: $event })"
            />
          </template>
        </fieldset>
      </template>

      <template v-else-if="kindOf(step) === 'investigate'">
        <p class="summary">
          Investigate, the way the action does. The test it starts is
          <code>$sid</code>, so a modifier "for this investigation" goes in the box below or is
          pushed against that.
        </p>
        <div class="row">
          <label>
            Using
            <select
              :value="step.investigate?.skill ?? ''"
              @change="set(index, { ...step, investigate: { ...step.investigate, skill: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">the location's own skill</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
          <label v-if="step.investigate?.skill">
            instead of
            <select
              :value="step.investigate?.insteadOf ?? ''"
              @change="set(index, { ...step, investigate: { ...step.investigate, insteadOf: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">— always use it —</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
        </div>
        <p v-if="step.investigate?.skill && step.investigate?.insteadOf" class="hint">
          Swaps only when the test would have used that skill, and can be declined by anything
          that ignores the substitution — the difference between "uses willpower" and "uses
          willpower instead of intellect".
        </p>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="[ModifierType]"
          label="For this investigation"
          :modelValue="step.investigate?.modifiers"
          @update:modelValue="set(index, { ...step, investigate: { ...step.investigate, modifiers: $event } })"
        />

        <fieldset class="on-reveal">
          <legend>If a chaos token is revealed during this test</legend>
          <BoolField
            label="it does something"
            :modelValue="!!onReveal(step, 'investigate')"
            @update:modelValue="setOnReveal(index, 'investigate', $event ? { tokens: null, steps: [] } : undefined)"
          />
          <template v-if="onReveal(step, 'investigate')">
            <ValueEditor
              type="ChaosTokenMatcher"
              label="Which tokens"
              :bindings="scopeFor(index)"
              :modelValue="onReveal(step, 'investigate').tokens"
              @update:modelValue="setOnReveal(index, 'investigate', { ...onReveal(step, 'investigate'), tokens: $event })"
            />
            <StepsEditor
              :queryKinds="queryKinds"
              :bindings="revealScope(index)"
              :path="innerPath(index, 'investigatereveal')"
              :announce="[]"
              :modelValue="onReveal(step, 'investigate').steps ?? []"
              @update:modelValue="setOnReveal(index, 'investigate', { ...onReveal(step, 'investigate'), steps: $event })"
            />
          </template>
        </fieldset>
      </template>

      <template v-else-if="kindOf(step) === 'evade'">
        <p class="summary">Evade an enemy. The test it starts is <code>$sid</code>.</p>
        <div class="row">
          <label>
            Using
            <select
              :value="step.evade?.skill ?? ''"
              @change="set(index, { ...step, evade: { ...step.evade, skill: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">the enemy's own skill</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
          <label v-if="step.evade?.skill">
            instead of
            <select
              :value="step.evade?.insteadOf ?? ''"
              @change="set(index, { ...step, evade: { ...step.evade, insteadOf: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">— always use it —</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
        </div>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="EnemyMatcher"
          label="Which enemies (optional)"
          :modelValue="step.evade?.matcher"
          @update:modelValue="set(index, { ...step, evade: { ...step.evade, matcher: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          type="[ModifierType]"
          label="For this evasion"
          :modelValue="step.evade?.modifiers"
          @update:modelValue="set(index, { ...step, evade: { ...step.evade, modifiers: $event } })"
        />

        <fieldset class="on-reveal">
          <legend>If a chaos token is revealed during this test</legend>
          <BoolField
            label="it does something"
            :modelValue="!!onReveal(step, 'evade')"
            @update:modelValue="setOnReveal(index, 'evade', $event ? { tokens: null, steps: [] } : undefined)"
          />
          <template v-if="onReveal(step, 'evade')">
            <ValueEditor
              type="ChaosTokenMatcher"
              label="Which tokens"
              :bindings="scopeFor(index)"
              :modelValue="onReveal(step, 'evade').tokens"
              @update:modelValue="setOnReveal(index, 'evade', { ...onReveal(step, 'evade'), tokens: $event })"
            />
            <StepsEditor
              :queryKinds="queryKinds"
              :bindings="revealScope(index)"
              :path="innerPath(index, 'evadereveal')"
              :announce="[]"
              :modelValue="onReveal(step, 'evade').steps ?? []"
              @update:modelValue="setOnReveal(index, 'evade', { ...onReveal(step, 'evade'), steps: $event })"
            />
          </template>
        </fieldset>
      </template>

      <template v-else-if="kindOf(step) === 'parley'">
        <p class="summary">
          Parley against something. Unlike the others there is no action to derive the test from,
          so the target, skill and difficulty are all named here. The test is <code>$sid</code>.
        </p>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="Target"
          label="Against"
          :modelValue="step.parley?.target"
          @update:modelValue="set(index, { ...step, parley: { ...step.parley, target: $event } })"
        />
        <label>
          Using
          <select
            :value="step.parley?.skill ?? 'SkillWillpower'"
            @change="set(index, { ...step, parley: { ...step.parley, skill: ($event.target as HTMLSelectElement).value } })"
          >
            <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
          </select>
        </label>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="GameCalculation"
          label="Difficulty"
          :modelValue="step.parley?.difficulty"
          @update:modelValue="set(index, { ...step, parley: { ...step.parley, difficulty: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          type="[ModifierType]"
          label="For this parley"
          :modelValue="step.parley?.modifiers"
          @update:modelValue="set(index, { ...step, parley: { ...step.parley, modifiers: $event } })"
        />

        <fieldset class="on-reveal">
          <legend>If a chaos token is revealed during this test</legend>
          <BoolField
            label="it does something"
            :modelValue="!!onReveal(step, 'parley')"
            @update:modelValue="setOnReveal(index, 'parley', $event ? { tokens: null, steps: [] } : undefined)"
          />
          <template v-if="onReveal(step, 'parley')">
            <ValueEditor
              type="ChaosTokenMatcher"
              label="Which tokens"
              :bindings="scopeFor(index)"
              :modelValue="onReveal(step, 'parley').tokens"
              @update:modelValue="setOnReveal(index, 'parley', { ...onReveal(step, 'parley'), tokens: $event })"
            />
            <StepsEditor
              :queryKinds="queryKinds"
              :bindings="revealScope(index)"
              :path="innerPath(index, 'parleyreveal')"
              :announce="[]"
              :modelValue="onReveal(step, 'parley').steps ?? []"
              @update:modelValue="setOnReveal(index, 'parley', { ...onReveal(step, 'parley'), steps: $event })"
            />
          </template>
        </fieldset>
      </template>

      <template v-else-if="kindOf(step) === 'attack'">
        <p class="summary">
          <template v-if="step.attack?.target">This card attacks the chosen target.</template>
          <template v-else>
            This card attacks whoever triggered the ability — "it makes an immediate attack
            against you".
          </template>
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          label="Attack someone else instead (optional)"
          :modelValue="step.attack?.target"
          @update:modelValue="set(index, { ...step, attack: { ...step.attack, target: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'ready'">
        <p class="summary">
          <template v-if="step.ready?.target">Readies the chosen card.</template>
          <template v-else>Readies this card.</template>
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          label="Ready something else instead (optional)"
          :modelValue="step.ready?.target"
          @update:modelValue="set(index, { ...step, ready: { ...step.ready, target: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'draw'">
        <ExpressionEditor
          :queryKinds="queryKinds"
          label="How many"
          expect="int"
          :bindings="scopeFor(index)"
          :modelValue="step.draw?.amount ?? 1"
          @update:modelValue="set(index, { ...step, draw: { ...step.draw, amount: $event } })"
        />
        <p class="hint">Nothing is drawn when this works out to zero or less.</p>
      </template>

      <template v-else-if="kindOf(step) === 'gather'">
        <p class="hint">
          Shuffles a card into the encounter deck. "Gather during setup" is over by the time a card
          in play can act, so this is the nearest a card can get to it.
        </p>
        <label class="cap">Which card</label>
        <CardCodeField
          :modelValue="step.gather?.cardCode"
          @update:modelValue="set(index, { ...step, gather: { ...step.gather, cardCode: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'customize'">
        <p class="hint">
          Mark a checkbox on an upgrade sheet for a customizable card you own — pick the card and
          which customization, and answer whatever that customization asks for.
        </p>
        <div class="row">
          <label>
            Who (optional)
            <input
              :value="step.customize?.iid"
              placeholder="$iid"
              @input="set(index, { ...step, customize: { ...step.customize, iid: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <BoolField
            label="may decline"
            :modelValue="step.customize?.optional !== false"
            @update:modelValue="set(index, { ...step, customize: { ...step.customize, optional: $event } })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'chooseFrom'">
        <p class="hint">One option per thing the matcher finds, with it bound for the steps below.</p>
        <div class="row">
          <label>
            Kind
            <select
              :value="step.chooseFrom?.query?.kind"
              @change="set(index, { ...step, chooseFrom: { ...step.chooseFrom, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            Bind to
            <input
              :value="step.chooseFrom?.bind"
              @input="set(index, { ...step, chooseFrom: { ...step.chooseFrom, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <BoolField
            label="may decline"
            :modelValue="!!step.chooseFrom?.optional"
            @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, optional: $event } })"
          />
          <label v-if="step.chooseFrom?.optional">
            Decline label
            <input
              :value="step.chooseFrom?.declineLabel"
              placeholder="Do not"
              @input="set(index, { ...step, chooseFrom: { ...step.chooseFrom, declineLabel: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <ValueEditor
          :bindings="scopeFor(index)"
          :type="matcherType(step.chooseFrom?.query?.kind)"
          label="Matcher"
          :modelValue="step.chooseFrom?.query?.matcher"
          @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, query: { ...step.chooseFrom.query, matcher: $event } } })"
        />
        <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, 'chooseFrom')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.chooseFrom?.steps ?? []"
          @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, steps: $event } })"
        />
      </template>
      </div>

      <div v-if="bindsOf(step, index).length" class="step-foot">
        <!-- Read as a declaration, the way a binding is written everywhere else
             in the builder, rather than as a sentence with code spans dropped
             into it -- with a type that made two of them run together. -->
        <span v-for="bound in bindsOf(step, index)" :key="bound.name" class="binds">
          <span class="binds-label">binds</span>
          <code class="binds-name"
            >${{ bound.name }}<span v-if="bound.type" class="binds-type"> :: {{ bound.type }}</span></code
          >
          <span class="binds-scope">for {{ bound.scope }}</span>
        </span>
      </div>
    </div>

    <!-- A menu rather than a row of buttons: twenty names side by side is a wall
         to read, and the name alone does not say what the step does. -->
    <div ref="addEl" class="step-actions">
      <button type="button" @click="toggleAdd">+ Step</button>
      <div v-if="addingStep" class="kind-menu">
        <input
          v-model="addSearch"
          type="search"
          class="kind-search"
          placeholder="Type to filter, enter to pick"
          v-focus
          @keydown.enter.prevent="addFirstMatch"
          @keydown.esc="addingStep = false"
          @keydown.stop
        />
        <ul>
          <li v-for="(kind, at) in matchingKinds" :key="kind" :class="{ first: at === 0 }">
            <button type="button" class="kind-option" @click="addAndClose(kind)">
              <span class="kind-name">{{ KIND_LABELS[kind] }}</span>
              <span class="kind-help">{{ KIND_HELP[kind] }}</span>
            </button>
          </li>
          <li v-if="!matchingKinds.length" class="muted">Nothing matches.</li>
        </ul>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.steps {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}

/* Three bands, so a step reads as what it is, then what it does, then what it
 * leaves behind. Flat colour and rules only -- the separation should come from
 * the edges, not from a wash. */
/* The bands round their own outer corners rather than the step clipping them:
 * `overflow: hidden` here would also cut off every dropdown a field opens, since
 * those are laid over the step rather than inside its flow. */
.step {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid #374151;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
}

.step-body {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  padding: 0.55rem 0.6rem;
}

.step-foot {
  align-items: center;
  background: #131a27;
  border-radius: 0 0 4px 4px;
  border-top: 1px solid #374151;
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem 0.6rem;
  padding: 0.3rem 0.6rem;
}

.binds {
  align-items: baseline;
  display: inline-flex;
  font-size: 0.72rem;
  gap: 0.3rem;
}

.binds-label,
.binds-scope {
  color: #6b7280;
}

.binds-name {
  color: #5eead4;
  font-family: monospace;
}

.binds-type {
  opacity: 0.65;
}

/* Flashed when a field jumps here to show where a binding came from. The class
 * is set from outside this component, which scoped styles still match: the rule
 * keys off the element's own attribute, not on who added the class. */
.on-reveal {
  border: 1px solid #374151;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  padding: 0.4rem 0.6rem;

  legend {
    color: #9ca3af;
    font-size: 0.72rem;
    padding: 0 0.3rem;
  }
}

.scope-bar {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;
  margin: 0 0 0.2rem;
}

.scope-label {
  color: #9ca3af;
  font-size: 0.72rem;
}

.scope-chip {
  background: #1f2937;
  border: 1px solid #374151;
  border-radius: 999px;
  color: #5eead4;
  cursor: default;
  font-family: inherit;
  font-size: 0.72rem;
  padding: 0.05rem 0.45rem;

  &.jumpable {
    cursor: pointer;

    &:hover {
      border-color: #14b8a6;
    }
  }
}

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

.step-head {
  align-items: center;
  background: #1b2436;
  border-bottom: 1px solid #374151;
  border-radius: 4px 4px 0 0;
  display: flex;
  gap: 0.5rem;
  justify-content: space-between;
  padding: 0.3rem 0.6rem;
}

/* What the step is. Set apart from the fields below it rather than sitting in
 * the same voice as them. */
.step-kind {
  color: #d1d5db;
  font-size: 0.72rem;
  font-weight: 600;
  letter-spacing: 0.04em;
  text-transform: uppercase;
}

.step-remove {
  background: none;
  border: 1px solid transparent;
  border-radius: 4px;
  color: #9ca3af;
  cursor: pointer;
  font-size: 0.85rem;
  line-height: 1;
  padding: 0.1rem 0.35rem;

  &:hover {
    border-color: #f87171;
    color: #fca5a5;
  }
}

.option {
  border-left: 2px solid #374151;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  padding-left: 0.5rem;
}

/* A branch of an If: its own edge and indent, so what belongs to which is read
 * off the shape rather than off the word above it. */
.branch {
  border-left: 2px solid #374151;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  margin-left: 0.2rem;
  padding: 0.15rem 0 0.15rem 0.6rem;
}

/* Changes the shape of the step rather than its contents, so it sits apart from
 * the fields and reads as something done to the block. */
.convert {
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #9ca3af;
  cursor: pointer;
  font-family: inherit;
  font-size: 0.72rem;
  padding: 0.2rem 0.5rem;

  &:hover {
    border-color: #6b7280;
    color: #5eead4;
  }
}

.branch-label {
  color: #9ca3af;
  font-size: 0.72rem;
  letter-spacing: 0.04em;
  text-transform: uppercase;
}

.row {
  display: flex;
  gap: 0.4rem;
  flex-wrap: wrap;
  align-items: flex-end;

  > label {
    flex: 1 1 100px;
  }
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


.step-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
  position: relative;
}

/* The same menu the binding and property fields open: a name, and beside it the
 * one line that says what picking it would do. */
.kind-menu {
  background: #0b1220;
  border: 1px solid #374151;
  border-radius: 5px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.45);
  left: 0;
  margin: 0.3rem 0 0;
  min-width: 26rem;
  padding: 0.2rem;
  position: absolute;
  top: 100%;
  z-index: 30;

  // The search stays put while the list under it scrolls.
  ul {
    list-style: none;
    margin: 0;
    max-height: 18rem;
    overflow-y: auto;
    padding: 0;
  }

  li:hover {
    background: rgba(20, 184, 166, 0.1);
    border-radius: 4px;
  }

  /* What enter would take. Marked so the key does something visible rather than
   * something you have to guess at. */
  li.first {
    background: rgba(20, 184, 166, 0.08);
    border-radius: 4px;
    box-shadow: inset 2px 0 0 #14b8a6;
  }

  .muted {
    color: #9ca3af;
    font-size: 0.75rem;
    padding: 0.45rem 0.5rem;
  }
}

.kind-search {
  background: #0b1220;
  border: 1px solid #14b8a6;
  border-radius: 4px;
  color: #eee;
  margin-bottom: 0.3rem;
  padding: 0.35rem 0.5rem;
  width: 100%;

  &::placeholder {
    color: #6b7280;
  }
}

.kind-option {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  gap: 0.5rem;
  padding: 0.35rem 0.45rem;
  text-align: left;
  width: 100%;
}

.kind-name {
  color: #5eead4;
  flex: none;
  font-size: 0.78rem;
  min-width: 7rem;
}

.kind-help {
  color: #9ca3af;
  font-size: 0.72rem;
}

.cancel-add {
  opacity: 0.7;
}

.step-actions button,
.add {
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid #4b5563;
  border-radius: 4px;
  color: #eee;
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.25rem 0.6rem;
}

.summary {
  font-size: 0.8rem;
  margin: 0;
}

.hint {
  font-size: 0.75rem;
  margin: 0;
  opacity: 0.7;
}
</style>

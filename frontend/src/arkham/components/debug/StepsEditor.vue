<script lang="ts" setup>
/* A list of steps, and the steps inside them.
 *
 * Branches and choices carry their own steps, so this renders itself for those
 * — which is what lets an ability say "if it is ready, attack; otherwise ready
 * it" or "choose an event, then play it". */
import { computed, ref } from 'vue'
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
  | 'case'
  | 'forEach'
  | 'choose'
  | 'chooseFrom'
  | 'playCard'
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
  case: 'Case',
  forEach: 'For each',
  choose: 'Choose',
  chooseFrom: 'Choose from',
  playCard: 'Play a card',
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
    'if',
    'case',
    'forEach',
    'choose',
    'chooseFrom',
    'playCard',
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
    case: { case: [{ if: { kind: 'enemy', matcher: null }, steps: [] }], else: [] },
    forEach: { forEach: { query: { kind: 'enemy', matcher: null }, bind: 'each', steps: [] } },
    choose: { choose: { options: [{ label: '', steps: [] }] } },
    chooseFrom: { chooseFrom: { query: { kind: 'enemy', matcher: null }, bind: 'chosen', steps: [] } },
    playCard: { playCard: { optional: true, matcher: null } },
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

const set = (index: number, step: any) =>
  emit('update:modelValue', steps.value.map((s, i) => (i === index ? step : s)))

const add = (kind: StepKind) => emit('update:modelValue', [...steps.value, blankStep(kind)])
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
function bindsOf(step: any): { name: string; scope: string }[] {
  const { after, inside } = stepBindings(step, '')
  return [
    ...after.map((b) => ({ name: b.name, scope: 'later steps' })),
    ...inside.map((b) => ({ name: b.name, scope: 'the steps inside' })),
  ]
}

// --- choose options ---

const addingStep = ref(false)

function addAndClose(kind: StepKind) {
  add(kind)
  addingStep.value = false
}

/* An expression is structural JSON rather than a value with a schema, so it is
 * edited as text. Bad JSON is kept as typed instead of thrown away, otherwise
 * the field fights you halfway through a brace. */
const drafts = ref<Record<number, string>>({})

const exprText = (step: any, index: number) =>
  drafts.value[index] ?? JSON.stringify(step.be ?? null, null, 2)

function setExpr(step: any, index: number, text: string) {
  drafts.value = { ...drafts.value, [index]: text }
  try {
    set(index, { ...step, be: JSON.parse(text) })
  } catch {
    /* left in the draft until it parses */
  }
}

const exprValid = (index: number) => {
  const text = drafts.value[index]
  if (text === undefined) return true
  try {
    JSON.parse(text)
    return true
  } catch {
    return false
  }
}

const drawText = (step: any, index: number) =>
  drafts.value[index] ?? JSON.stringify(step.draw?.amount ?? 1, null, 2)

function setDraw(step: any, index: number, text: string) {
  drafts.value = { ...drafts.value, [index]: text }
  try {
    set(index, { ...step, draw: { ...step.draw, amount: JSON.parse(text) } })
  } catch {
    /* left in the draft until it parses */
  }
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
        <span>{{ KIND_LABELS[kindOf(step)] }}</span>
        <span v-for="bound in bindsOf(step)" :key="bound.name" class="binds">
          binds <code>${{ bound.name }}</code> for {{ bound.scope }}
        </span>
        <button type="button" @click="remove(index)">×</button>
      </div>

      <template v-if="kindOf(step) === 'query'">
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
        <label>
          Expression
          <textarea
            :class="{ invalid: !exprValid(index) }"
            :value="exprText(step, index)"
            rows="5"
            spellcheck="false"
            @input="setExpr(step, index, ($event.target as HTMLTextAreaElement).value)"
            @keydown.stop
          />
        </label>
        <p class="hint">
          A literal, or one of <code>get</code>/<code>map</code> (with <code>kind</code> and
          <code>of</code>), <code>filter</code>, <code>unique</code>, <code>concat</code>,
          <code>count</code>, <code>sum</code>, <code>max</code>, <code>min</code>,
          <code>first</code>, <code>reverse</code>, <code>add</code>, <code>subtract</code>,
          <code>multiply</code>, <code>divide</code>. Anything <code>of</code> takes a list is
          applied to each of its elements.
        </p>
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
        <span class="branch-label">Then</span>
        <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, 'then')"
          :announce="[]"
          :queryKinds="queryKinds"
          :modelValue="step.then ?? []"
          @update:modelValue="set(index, { ...step, then: $event })"
        />
        <span class="branch-label">Otherwise</span>
        <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, 'else')"
          :announce="[]"
          :queryKinds="queryKinds"
          :modelValue="step.else ?? []"
          @update:modelValue="set(index, { ...step, else: $event })"
        />
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
        <span class="branch-label">Otherwise</span>
        <StepsEditor
          :bindings="innerScope(index)"
          :path="innerPath(index, 'else')"
          :announce="[]"
          :queryKinds="queryKinds"
          :modelValue="step.else ?? []"
          @update:modelValue="set(index, { ...step, else: $event })"
        />
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

      <template v-else-if="kindOf(step) === 'playCard'">
        <p class="hint">
          Offers the cards you could play, paying the cost, each shown as the card itself. A
          discount is worked out before the choice, since a card is only playable if you can
          afford it.
        </p>
        <div class="row">
          <label v-if="step.playCard?.optional">
            Decline label
            <input
              :value="step.playCard?.declineLabel"
              placeholder="Do not"
              @input="set(index, { ...step, playCard: { ...step.playCard, declineLabel: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <label>
            Discount
            <input
              type="number"
              :value="step.playCard?.discount ?? 0"
              @input="set(index, { ...step, playCard: { ...step.playCard, discount: Number(($event.target as HTMLInputElement).value) } })"
              @keydown.stop
            />
          </label>
          <label class="inline">
            <input
              type="checkbox"
              :checked="!!step.playCard?.optional"
              @change="set(index, { ...step, playCard: { ...step.playCard, optional: ($event.target as HTMLInputElement).checked } })"
            />
            may decline
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
        <label class="inline">
          <input
            type="checkbox"
            :checked="!!step.fight?.basic"
            @change="set(index, { ...step, fight: { ...step.fight, basic: ($event.target as HTMLInputElement).checked } })"
          />
          a basic fight action instead
        </label>
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
        <label>
          How many
          <textarea
            :class="{ invalid: !exprValid(index) }"
            :value="drawText(step, index)"
            rows="2"
            spellcheck="false"
            @input="setDraw(step, index, ($event.target as HTMLTextAreaElement).value)"
            @keydown.stop
          />
        </label>
        <p class="hint">A number, or an expression — the same forms a Let takes.</p>
      </template>

      <template v-else-if="kindOf(step) === 'gather'">
        <p class="hint">
          Shuffles a card into the encounter deck. "Gather during setup" is over by the time a card
          in play can act, so this is the nearest a card can get to it.
        </p>
        <label>
          Card code
          <input
            :value="step.gather?.cardCode"
            placeholder="09752"
            @input="set(index, { ...step, gather: { ...step.gather, cardCode: ($event.target as HTMLInputElement).value } })"
            @keydown.stop
          />
        </label>
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
          <label class="inline">
            <input
              type="checkbox"
              :checked="step.customize?.optional !== false"
              @change="set(index, { ...step, customize: { ...step.customize, optional: ($event.target as HTMLInputElement).checked } })"
            />
            may decline
          </label>
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
          <label class="inline">
            <input
              type="checkbox"
              :checked="!!step.chooseFrom?.optional"
              @change="set(index, { ...step, chooseFrom: { ...step.chooseFrom, optional: ($event.target as HTMLInputElement).checked } })"
            />
            may decline
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

    <div class="step-actions">
      <button v-if="!addingStep" type="button" @click="addingStep = true">+ Step</button>
      <template v-else>
        <button v-for="(label, kind) in KIND_LABELS" :key="kind" type="button" @click="addAndClose(kind as StepKind)">
          {{ label }}
        </button>
        <button type="button" class="cancel-add" @click="addingStep = false">×</button>
      </template>
    </div>
  </div>
</template>

<style scoped lang="scss">
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

.binds {
  background: #1f2937;
  border: 1px solid #374151;
  border-radius: 999px;
  color: #9ca3af;
  font-size: 0.72rem;
  margin-left: auto;
  padding: 0.05rem 0.5rem;

  code {
    color: #d1d5db;
  }
}

/* Flashed when a field jumps here to show where a binding came from. The class
 * is set from outside this component, which scoped styles still match: the rule
 * keys off the element's own attribute, not on who added the class. */
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

textarea {
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid #374151;
  border-radius: 4px;
  color: inherit;
  font-family: monospace;
  font-size: 0.75rem;
  padding: 0.3rem;
  resize: vertical;
  width: 100%;

  &.invalid {
    border-color: #b45309;
  }
}

.option {
  border-left: 2px solid #374151;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  padding-left: 0.5rem;
}

.branch-label {
  font-size: 0.75rem;
  opacity: 0.7;
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

  &.inline {
    align-items: center;
    flex: 0 0 auto;
    flex-direction: row;
    gap: 0.3rem;
  }
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

input[type='checkbox'] {
  width: auto;
}

.step-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
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

<script lang="ts" setup>
/* A list of steps, and the steps inside them.
 *
 * Branches and choices carry their own steps, so this renders itself for those
 * — which is what lets an ability say "if it is ready, attack; otherwise ready
 * it" or "choose an event, then play it". */
import { computed, inject, reactive, ref } from 'vue'
import { vFocus } from '@/arkham/components/debug/vFocus'
import { onClickOutside } from '@vueuse/core'
import ExpressionEditor from '@/arkham/components/debug/ExpressionEditor.vue'
import BoolField from '@/arkham/components/debug/BoolField.vue'
import LabelField from '@/arkham/components/debug/LabelField.vue'
import CardCodeField from '@/arkham/components/debug/CardCodeField.vue'
import ConditionField from '@/arkham/components/debug/ConditionField.vue'
import ScopeBar from '@/arkham/components/debug/ScopeBar.vue'
import ValueEditor from '@/arkham/components/debug/ValueEditor.vue'
import ValueMatcherField from '@/arkham/components/debug/ValueMatcherField.vue'
import {
  scopeAt,
  scopeInside,
  stepAnchor,
  stepBindings,
  type Binding,
} from '@/arkham/customCardBindings'
import { QUERY_TYPES } from '@/arkham/customCardExpressions'
import { useI18n } from 'vue-i18n'
import {
  isOwnTextKey,
  isSharedTextKey,
  ownTextKey,
  parseSharedLabel,
  readableParams,
  type CardTextAccess,
  CARD_TEXT,
} from '@/arkham/customText'

const { t: uiT } = useI18n()

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
  /* Slots that stand for a list of steps, offered as a step of their own. Only
   * a KEYWORD's behaviour has any: a card's steps are its own, while a
   * keyword's are written once and have to leave a place for each card's. */
  slotSteps?: Binding[]
}>()
const emit = defineEmits<{ 'update:modelValue': [v: any[]] }>()

const steps = computed(() => props.modelValue ?? [])

type StepKind =
  | 'query'
  | 'let'
  | 'random'
  | 'push'
  | 'if'
  | 'when'
  | 'case'
  | 'forEach'
  | 'repeat'
  | 'call'
  | 'distribute'
  | 'search'
  | 'request'
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
  | 'test'
  | 'attack'
  | 'ready'
  | 'discardCard'
  | 'record'
  | 'takeAction'
  | 'draw'
  | 'gather'
  | 'customize'
  | 'place'
  | 'discover'
  | 'setAside'
  | 'activateAbility'
  | 'repeatAbilities'
  | 'notifyResult'
  | 'skillTestOption'
  | 'seal'
  /* Not a step the engine runs, but a slot standing where a list of steps goes.
   * Only ever offered while editing a KEYWORD's behaviour, where the steps a
   * card supplies are spliced in at this point. */
  | 'slot'

const KIND_LABELS: Record<StepKind, string> = {
  query: 'Query',
  let: 'Let',
  random: 'Random',
  push: 'Push',
  if: 'If',
  when: 'When',
  case: 'Case',
  forEach: uiT('ui.forEach'),
  repeat: 'Repeat',
  call: 'Again',
  distribute: 'Distribute',
  search: 'Search',
  request: uiT('ui.askTheGame'),
  modify: 'Modify',
  withSkillTest: uiT('ui.withSkillTest'),
  withLocationOf: uiT('ui.withLocationOf'),
  choose: 'Choose',
  chooseFrom: uiT('ui.chooseFrom'),
  playCard: 'Play a card',
  useAbility: uiT('ui.useAnAbility'),
  cancelBatch: uiT('ui.cancelWhatWouldHappen'),
  fight: 'Fight',
  investigate: 'Investigate',
  evade: 'Evade',
  parley: 'Parley',
  test: uiT('ui.skillTest'),
  attack: 'Attack',
  ready: 'Ready',
  discardCard: 'Discard a card',
  record: uiT('ui.recordInTheCampaignLog'),
  takeAction: uiT('ui.takeAnAction'),
  draw: uiT('ui.drawCards'),
  gather: 'Gather',
  customize: 'Customize',
  place: 'Place a card',
  discover: uiT('ui.discoverClues'),
  setAside: uiT('ui.setCardsAside'),
  activateAbility: uiT('ui.activateAnotherCardSAbility'),
  repeatAbilities: uiT('ui.repeatWhatACardSAbilitiesDid'),
  notifyResult: uiT('ui.tellACardTheTestSResultAgain'),
  skillTestOption: uiT('ui.offerThisAtTheEndOfTheTest'),
  seal: uiT('ui.sealChaosTokens'),
  slot: uiT('ui.whateverTheCardFillsIn'),
}

function kindOf(step: any): StepKind {
  const kinds = [
    'query',
    'let',
    'random',
    'push',
    'when',
    'if',
    'case',
    'forEach',
    'repeat',
    'call',
    'distribute',
    'search',
    'request',
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
    'test',
    'attack',
    'ready',
    'discardCard',
    'record',
    'takeAction',
    'draw',
    'gather',
    'customize',
    'place',
    'discover',
    'setAside',
    'activateAbility',
    'repeatAbilities',
    'notifyResult',
    'skillTestOption',
    'seal',
  ] as StepKind[]
  // A slot is written as the bare `$name` the template substitutes, so it is a
  // string where every other step is an object.
  if (typeof step === 'string') return 'slot'
  for (const kind of kinds) {
    if (kind in (step ?? {})) return kind
  }
  return 'push'
}

const blankStep = (kind: StepKind) =>
  ({
    query: { query: { kind: 'enemy', matcher: null }, bind: '', mode: 'all' },
    let: { let: '', be: null },
    random: { random: { bind: 'sid' } },
    push: { push: null },
    if: { if: { kind: 'enemy', matcher: null }, then: [], else: [] },
    when: { when: { kind: 'enemy', matcher: null }, then: [] },
    case: { case: [{ if: { kind: 'enemy', matcher: null }, steps: [] }], else: [] },
    forEach: { forEach: { query: { kind: 'enemy', matcher: null }, bind: 'each', steps: [] } },
    modify: { modify: { target: null, modifiers: [] } },
    repeat: { repeat: { times: 1, bind: 'i', steps: [] } },
    call: { call: '' },
    distribute: { distribute: { total: 1, among: { kind: 'investigator', matcher: null }, label: '', bind: 'who', amount: 'amount', steps: [] } },
    search: {
      search: {
        how: 'search',
        zones: [{ zone: 'topOfDeck', amount: 1, return: 'ShuffleBackIn' }],
        matcher: null,
        found: 'draw',
        amount: 1,
      },
    },
    request: { request: { push: null, on: '', steps: [] } },
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
    test: { test: { skill: 'SkillWillpower', difficulty: 0, modifiers: [] } },
    attack: { attack: {} },
    ready: { ready: {} },
    discardCard: { discardCard: { card: '$card' } },
    record: { record: { key: { tag: 'HomebrewCampaignLogKey', contents: '' }, mode: 'flag' } },
    takeAction: { takeAction: true },
    draw: { draw: { amount: 1 } },
    gather: { gather: { cardCode: '' } },
    customize: { customize: { optional: true } },
    place: { place: { placement: null } },
    discover: { discover: { amount: 1 } },
    setAside: { setAside: {} },
    activateAbility: { activateAbility: { matcher: null, modifiers: [] } },
    repeatAbilities: { repeatAbilities: { source: null } },
    notifyResult: { notifyResult: { target: null } },
    skillTestOption: { skillTestOption: { label: '', steps: [] } },
    seal: { seal: { tokens: null } },
    slot: `$${props.slotSteps?.[0]?.name ?? ''}`,
  })[kind]

/* One line each, taken from what the step's own editor says once it is there.
 * Picking a step means knowing what it does before you have one, which a list of
 * bare names cannot tell you -- Gather and Draw are both about cards, Choose and
 * Choose from are a word apart and are not the same thing at all. */
const KIND_HELP: Record<StepKind, string> = {
  query: uiT('ui.supersededALetCanBindAQueryDirectly'),
  let: uiT('ui.worksSomethingOutAndBindsItToAName'),
  random: uiT('ui.bindsAFreshIdOrOneThingPickedAt'),
  push: uiT('ui.putsAMessageOnTheQueueWrittenAsThe'),
  if: uiT('ui.runsAMatcherAndTakesTheFirstBranchWhen'),
  when: uiT('ui.runsItsStepsOnlyWhenAMatcherFindsSomething'),
  case: uiT('ui.takesTheFirstBranchWhoseConditionHolds'),
  forEach: uiT('ui.runsItsStepsOncePerThingFoundFromA'),
  repeat: uiT('ui.runsItsStepsANumberOfTimesWithThe'),
  call: uiT('ui.runsAnotherPassOfANamedRepeatNothingHappens'),
  distribute: uiT('ui.splitsATotalBetweenInvestigatorsThenRunsItsSteps'),
  search: uiT('ui.searchesAZoneForCardsAndSaysWhatHappens'),
  request: uiT('ui.pushesAMessageThatIsAnsweredLaterWithThe'),
  modify: uiT('ui.givesSomethingModifiersForAsLongAsAWindow'),
  withSkillTest: uiT('ui.runsItsStepsDuringASkillTestWithThat'),
  withLocationOf: uiT('ui.runsItsStepsWhereSomethingIsWithThatLocation'),
  choose: uiT('ui.offersThePlayerNamedOptionsEachWithStepsOf'),
  chooseFrom: uiT('ui.oneOptionPerThingFoundFromAMatcherOr'),
  playCard: uiT('ui.offersTheCardsThatCouldBePlayedAndPays'),
  useAbility: uiT('ui.resolvesOneOfThisCardSOwnAbilitiesPaying'),
  cancelBatch: uiT('ui.stopsWhatTheAbilityIsReactingToNeedsA'),
  fight: uiT('ui.fightsAnEnemyTheTestItStartsIsSid'),
  investigate: uiT('ui.investigatesTheWayTheActionDoesTheTestIt'),
  evade: uiT('ui.evadesAnEnemyTheTestItStartsIsSid'),
  parley: uiT('ui.parleysAgainstSomethingNamingTheTargetSkillAndDifficulty'),
  test: uiT('ui.beginsAPlainSkillTestTestWillpower3And'),
  attack: uiT('ui.thisCardAttacksAnEnemyMakingAnImmediateAttack'),
  ready: uiT('ui.readiesThisCardOrTheOneChosen'),
  discardCard: uiT('ui.discardsACardFromHandOrFromPlayWhichever'),
  record: uiT('ui.writesInTheCampaignLogThatSomethingHappenedA'),
  takeAction: uiT('ui.takesOneImmediateActionAsIfItWereYour'),
  draw: uiT('ui.drawsCardsNothingIsDrawnWhenTheAmountWorks'),
  gather: uiT('ui.shufflesACardIntoTheEncounterDeck'),
  customize: uiT('ui.marksACheckboxOnTheUpgradeSheetOfA'),
  place: uiT('ui.putsACardSomewhereAttachedInAPlayArea'),
  discover: uiT('ui.discoversCluesWithTheIdEverythingReactingToA'),
  setAside: uiT('ui.makesCopiesOfNamedCardsInTheSetAside2'),
  activateAbility: uiT('ui.offersEveryAbilityOfANamedShapeOnOther'),
  repeatAbilities:
    uiT('ui.pushesEveryAbilityUseAlreadyRecordedForASource'),
  notifyResult:
    uiT('ui.saysTheTestSResultToACardA'),
  skillTestOption:
    uiT('ui.registersTheseStepsAsOneOfTheOptionsThe'),
  seal: uiT('ui.sealsChaosTokensOnThisCardOrReleasesThe'),
  slot: uiT('ui.splicesInTheStepsWhicheverCardPrintsThisKeyword'),
}

/* A query is an expression now, so binding one is what a Let does and a Query
 * step adds nothing a Let cannot say. The kind stays known -- cards already
 * written hold Query steps, and those keep running and keep their editor -- it
 * is simply not offered for anything new. */
/* `slot` is left out unless there is a slot to insert: on a card there is never
 * one, and offering a step that can only produce `$` would be offering a way to
 * write a step that does nothing. */
const ADDABLE = computed(() =>
  (Object.keys(KIND_LABELS) as StepKind[])
    .filter((k) => k !== 'query')
    .filter((k) => k !== 'slot' || (props.slotSteps?.length ?? 0) > 0)
    // By what the menu shows, not by the key behind it, since that is what is read.
    .sort((a, b) => KIND_LABELS[a].localeCompare(KIND_LABELS[b])),
)

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

/* What ONE option of a Choose sees.
 *
 * An option carrying a query stands for one option per thing found, bound for
 * that option's steps alone -- so it cannot come from the step's own `inside`,
 * which every option shares. Without this the steps under such an option
 * reported their own binding as unbound, which is the editor telling an author
 * their card is broken when it is not. Mirrors `runChoose`. */
function optionBinding(index: number, option: any, oi: number): Binding | null {
  if (!option?.query) return null
  return {
    name: typeof option.bind === 'string' && option.bind ? option.bind : 'chosen',
    detail: uiT('ui.whatWasChosen'),
    type: QUERY_TYPES[option.query?.kind],
    origin: uiT('ui.queryOnOption', { number: oi + 1 }),
    anchor: anchorFor(index),
  }
}

function optionScope(index: number, option: any, oi: number): Binding[] {
  const bound = optionBinding(index, option, oi)
  return bound ? [...innerScope(index), bound] : innerScope(index)
}

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

/* Every step that starts a test takes the same two riders — what a token revealed
 * during it does, and what its result does — so they are asked for once, below
 * whatever the step itself needs, rather than five times over.
 *
 * A basic fight action is the exception: the enemy's own ability makes its own
 * test, and there is no test of this step's to hang anything on. */
/* Switching a repeat between its two readings. The name IS the switch -- the
 * engine runs the steps where they stand unless one is bound to call -- so
 * dropping it is how "all of it now" is said. */
function setRepeatMode(index: number, step: any, mode: string) {
  const repeat = { ...step.repeat }
  if (mode === 'called') repeat.as = repeat.as || 'again'
  else delete repeat.as
  set(index, { ...step, repeat })
}

// The repeats a step here could ask for another pass of.
const repeatsInScope = (index: number) => scopeFor(index).filter((b) => b.type === 'Repeat')

const TEST_KINDS = ['test', 'fight', 'investigate', 'evade', 'parley']

const startsTest = (step: any) => TEST_KINDS.includes(kindOf(step)) && !step.fight?.basic

/* Which steps can carry "if you succeed" and "if you fail": the ones that START
 * a test, and `withSkillTest`, which names one already under way. */
const hasOutcomes = (step: any) => startsTest(step) || kindOf(step) === 'withSkillTest'

/* "If you succeed, …" and "If you fail, …". Kept on the step that started the
 * test because only it knows which test, and only here can the steps still see
 * what this run chose.
 *
 * A card may say it twice — "if you succeed, evade the target; if you succeed by
 * 2 or more, discover a clue" — so an outcome holds a LIST of riders, each with
 * its own margin. One rider stays written as the bare block it has always been,
 * so nothing already saved changes shape. */
const outcomes = (step: any, which: 'onSuccess' | 'onFailure'): any[] => {
  const held = step[kindOf(step)]?.[which]
  if (Array.isArray(held)) return held
  return held ? [held] : []
}

/* Both give v-for a definite array: iterating `step.x ?? []` off an `any`
 * types the loop index `string | number`, which then will not do arithmetic. */
const branches = (step: any): any[] => step.case ?? []

const setAsideCards = (step: any): string[] => step.setAside?.cards ?? []

const setOutcomes = (index: number, which: 'onSuccess' | 'onFailure', riders: any[]) => {
  const step = steps.value[index]
  const kind = kindOf(step)
  const value = riders.length === 0 ? undefined : riders.length === 1 ? riders[0] : riders
  set(index, { ...step, [kind]: { ...step[kind], [which]: value } })
}

const setRider = (index: number, which: 'onSuccess' | 'onFailure', at: number, rider: any) =>
  setOutcomes(index, which, outcomes(steps.value[index], which).map((r, i) => (i === at ? rider : r)))

/* One entry of a setAside list, with null meaning "take it out". Kept here
 * rather than inline because the list may not exist yet. */
const setAsideCard = (index: number, at: number, code: string | null) => {
  const step = steps.value[index]
  const cards = [...(step.setAside?.cards ?? [])]
  if (code === null) cards.splice(at, 1)
  else cards[at] = code
  set(index, { ...step, setAside: { ...step.setAside, cards } })
}

const addRider = (index: number, which: 'onSuccess' | 'onFailure') =>
  setOutcomes(index, which, [...outcomes(steps.value[index], which), { steps: [] }])

const removeRider = (index: number, which: 'onSuccess' | 'onFailure', at: number) =>
  setOutcomes(index, which, outcomes(steps.value[index], which).filter((_, i) => i !== at))

/* Which of the two difficulty fields is in use. A calculation is a tagged
 * object; a number or an expression is not, so the shape says which was written
 * and nothing has to be stored alongside it. */
const calculatedDifficulty = (value: any) =>
  !!value && typeof value === 'object' && !Array.isArray(value) && 'tag' in value

const setDifficultyKind = (index: number, kind: string) => {
  const step = steps.value[index]
  set(index, { ...step, test: { ...step.test, difficulty: kind === 'calculation' ? null : 0 } })
}

/* An id or a pick from a list. `from` being absent is what says "an id", so
 * switching back removes the key rather than emptying it. */
const setRandomKind = (index: number, kind: string) => {
  const step = steps.value[index]
  const { from: _from, ...rest } = step.random ?? {}
  set(index, { ...step, random: kind === 'from' ? { ...rest, from: null } : rest })
}

/* What the log can be told, in the words a card uses rather than the seven
 * messages the engine reaches it through. Each maps to one of those. */
const RECORD_MODES = [
  { key: 'flag', label: uiT('ui.thatItHappened'), help: uiT('ui.recordsTheKeyRecordThat') },
  { key: 'crossOut', label: uiT('ui.crossItOut'), help: uiT('ui.crossesTheKeyOutUndoingARecord') },
  { key: 'count', label: uiT('ui.setANumber'), help: uiT('ui.setsTheNumberKeptUnderTheKey') },
  { key: 'increase', label: uiT('ui.increaseTheNumber'), help: uiT('ui.addsToTheNumberKeptUnderTheKey') },
  { key: 'decrease', label: uiT('ui.decreaseTheNumber'), help: uiT('ui.takesAwayFromTheNumberKeptUnderTheKey') },
  {
    key: 'entries',
    label: uiT('ui.addToTheList'),
    help: uiT('ui.addsEntriesToTheSetKeptUnderTheKey'),
  },
  {
    key: 'crossOutEntries',
    label: uiT('ui.crossOutEntriesInTheList'),
    help: uiT('ui.marksEntriesAlreadyInTheSetAsNoLonger'),
  },
] as const

const recordMode = (step: any) => step.record?.mode ?? 'flag'
const recordModeHelp = (step: any) =>
  RECORD_MODES.find((m) => m.key === recordMode(step))?.help ?? ''
const recordTakesAmount = (step: any) =>
  ['count', 'increase', 'decrease'].includes(recordMode(step))
const recordTakesValues = (step: any) =>
  ['entries', 'crossOutEntries'].includes(recordMode(step))

/* A custom card's own log entries are homebrew ones, and the wrapper is what makes
 * an arbitrary name a CampaignLogKey. An official campaign's key is a constructor,
 * so that stays a raw JSON job. */
const recordKeyName = (step: any) => {
  const k = step.record?.key
  return k && typeof k === 'object' && k.tag === 'HomebrewCampaignLogKey' ? (k.contents ?? '') : ''
}

const recordKeyIsHomebrew = (step: any) => {
  const k = step.record?.key
  return !k || (typeof k === 'object' && k.tag === 'HomebrewCampaignLogKey')
}

const setRecord = (index: number, changes: Record<string, any>) => {
  const step = steps.value[index]
  set(index, { ...step, record: { ...step.record, ...changes } })
}

const OUTCOME_LEGENDS: Record<string, string> = {
  onSuccess: uiT('ui.ifYouSucceed'),
  onFailure: uiT('ui.ifYouFail'),
}

/* A list to choose or loop over can come from a matcher or from a binding. Which
 * one is stored is the presence of `over`, so switching drops the other rather
 * than leaving both for the runner to pick between. */
const overSource = (block: any) => (block?.over === undefined ? 'query' : 'over')

/* What an `over` list holds, so each option can show as that thing. Clearing it
 * drops the key rather than storing an empty one, and takes the per-option
 * label with it — a kind names every option on its own. */
const setOverKind = (index: number, step: any, kind: string) => {
  const chooseFrom = { ...step.chooseFrom }
  if (kind) {
    chooseFrom.kind = kind
    delete chooseFrom.label
  } else {
    delete chooseFrom.kind
  }
  set(index, { ...step, chooseFrom })
}

const setSource = (index: number, kind: 'chooseFrom' | 'forEach', source: string) => {
  const step = steps.value[index]
  const { over: _over, query: _query, ...rest } = step[kind] ?? {}
  set(index, {
    ...step,
    [kind]:
      source === 'over' ? { ...rest, over: null } : { ...rest, query: { kind: 'enemy', matcher: null } },
  })
}

/* The Locateable instances the runner dispatches on. Which one it is has to be
 * said, because an id is a bare uuid and the instance cannot be chosen from it. */
const LOCATEABLE = ['investigator', 'enemy', 'asset', 'treachery']

/* The answers the engine sends back. Each is the second half of a pair whose
 * first half is an ordinary message, and each routes by source or target -- so
 * a card only ever sees the answers to its own questions. */
const ANSWERS = [
  'RequestedChaosTokens_',
  'RequestedPlayerCard',
  'RequestedSetAsideCard',
  'RequestedEncounterCard',
  'RequestedEncounterCards',
  'FoundEncounterCard',
  'FoundAndDrewEncounterCard',
  'FoundCards_',
  'ChosenRandomLocation',
  'ChoseCards',
  'ResolveAmounts',
]

const matcherType = (kind: string | undefined) => props.queryKinds[kind ?? 'enemy'] ?? 'EnemyMatcher'

/* How a search treats the deck it reads. The same machinery with three manners:
 * a search is a search as far as everything that reacts to one is concerned,
 * looking leaves the deck alone, revealing shows the card to the table. */
const SEARCH_HOWS: [string, string][] = [
  ['search', uiT('ui.searchIt')],
  ['look', uiT('ui.lookAtIt')],
  ['reveal', uiT('ui.revealIt')],
]

/* The zones a card names, and how deep. Only the two deck zones take a depth --
 * a hand or a discard pile is read whole. */
const SEARCH_ZONES: [string, string][] = [
  ['topOfDeck', uiT('ui.theTopOfYourDeck')],
  ['bottomOfDeck', uiT('ui.theBottomOfYourDeck')],
  ['deck', uiT('ui.yourWholeDeck')],
  ['hand', uiT('ui.yourHand')],
  ['discard', uiT('ui.yourDiscardPile')],
]

const ZONE_TAKES_DEPTH = ['topOfDeck', 'bottomOfDeck']

/* What becomes of the cards it did NOT take. */
const SEARCH_RETURNS: [string, string][] = [
  ['ShuffleBackIn', uiT('ui.shuffleTheRestBackIn')],
  ['PutBack', uiT('ui.putTheRestBackAsTheyWere')],
  ['PutBackInAnyOrder', uiT('ui.putTheRestBackInAnyOrder')],
  ['PutBackInAnyOrderBothTopAndBottom', uiT('ui.putTheRestBackTopOrBottom')],
  ['DiscardRest', uiT('ui.discardTheRest')],
  ['RemoveRestFromGame', uiT('ui.removeTheRestFromTheGame')],
  ['DoNothing', uiT('ui.leaveTheRestAlone')],
]

/* What becomes of what it DID find. Everything but the last is one of the
 * engine's own strategies, which handle the prompt themselves; the last hands
 * the result back to this card so its own steps can say. */
const SEARCH_FOUND: [string, string][] = [
  ['draw', uiT('ui.drawIt')],
  ['drawUpTo', uiT('ui.drawUpToThatMany')],
  ['drawAll', uiT('ui.drawAllOfThem')],
  ['hand', uiT('ui.addItToYourHand')],
  ['play', uiT('ui.playItPayingItsCost')],
  ['playFree', uiT('ui.playItWithoutPayingItsCost')],
  ['handOrPlay', uiT('ui.addItToYourHandOrPlayIt')],
  ['drawOrCommit', uiT('ui.drawItOrCommitIt')],
  ['removeFromGame', uiT('ui.removeItFromTheGame')],
  ['return', uiT('ui.nothingJustLook')],
  ['steps', uiT('ui.whateverTheStepsBelowSay')],
]

/* Whether this search hands its result back to the card. Writing either branch
 * is what asks for that, so the strategy follows from the steps rather than
 * having to be kept in step with them by hand. */
const searchDefers = (step: any) =>
  step.search?.found === 'steps' ||
  step.search?.onFound !== undefined ||
  step.search?.onNoneFound !== undefined

const SEARCH_COUNTED = ['draw', 'drawUpTo', 'hand', 'play', 'playFree', 'handOrPlay', 'drawOrCommit', 'removeFromGame']

function setSearch(index: number, step: any, changes: Record<string, any>) {
  set(index, { ...step, search: { ...step.search, ...changes } })
}

const searchZones = (step: any): any[] => step.search?.zones ?? []

function setSearchZone(index: number, step: any, zi: number, changes: Record<string, any>) {
  const zones = searchZones(step).map((z, i) => (i === zi ? { ...z, ...changes } : z))
  setSearch(index, step, { zones })
}

const addSearchZone = (index: number, step: any) =>
  setSearch(index, step, {
    zones: [...searchZones(step), { zone: 'deck', return: 'ShuffleBackIn' }],
  })

const removeSearchZone = (index: number, step: any, zi: number) =>
  setSearch(index, step, { zones: searchZones(step).filter((_, i) => i !== zi) })

/* Turning the branches on and off, rather than leaving an author to guess that
 * an absent key and an empty list mean different things to the engine. */
function toggleSearchBranch(index: number, step: any, which: 'onFound' | 'onNoneFound') {
  const on = step.search?.[which] !== undefined
  setSearch(index, step, { [which]: on ? undefined : [], found: on ? step.search?.found : 'steps' })
}


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
  // A name bound for both is one name, not two: a named repeat is in scope for
  // the steps it repeats AND for the ones that start it.
  const both = new Set(after.map((b) => b.name).filter((n) => inside.some((b) => b.name === n)))
  return [
    ...after.map((b) => ({
      name: b.name,
      scope: both.has(b.name) ? uiT('ui.theStepsInsideAndAfter') : uiT('ui.laterSteps'),
      type: b.type,
    })),
    ...inside
      .filter((b) => !both.has(b.name))
      .map((b) => ({ name: b.name, scope: uiT('ui.theStepsInside'), type: b.type })),
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
  if (!needle) return ADDABLE.value
  return ADDABLE.value.filter(
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

/* Switching an option between the two things it can be. The keys the other kind
 * reads are dropped rather than left behind: a `query` beside a `label` is not
 * half of each, it is a query, and the label would simply never be shown. */
function setOptionSource(step: any, oi: number, at: number, kind: string) {
  const option = optionsOf(step)[oi] ?? {}
  const next =
    kind === 'query'
      ? { ...option, label: undefined, query: { kind: 'enemy', matcher: null }, bind: 'chosen' }
      : { ...option, query: undefined, bind: undefined, label: option.label ?? '' }
  setOption(step, oi, next, at)
}

const addOption = (step: any, at: number) =>
  set(at, { ...step, choose: { ...step.choose, options: [...optionsOf(step), { label: '', steps: [] }] } })

const removeOption = (step: any, index: number, at: number) => {
  set(at, { ...step, choose: { ...step.choose, options: optionsOf(step).filter((_, i) => i !== index) } })
  showOption(at, Math.max(index - 1, 0))
}

/* One option at a time, under a strip of tabs.
 *
 * Every option carries steps of its own, and steps nest, so a Choose with
 * three of them used to run down the page with nothing to say where one
 * stopped and the next began. A tab is that line. */
const shownOption = reactive<Record<string, number>>({})

const activeOption = (step: any, at: number) => {
  const held = shownOption[innerPath(at, 'choose')] ?? 0
  return Math.min(held, Math.max(optionsOf(step).length - 1, 0))
}

const showOption = (at: number, oi: number) => {
  shownOption[innerPath(at, 'choose')] = oi
}

const addAndShowOption = (step: any, at: number) => {
  addOption(step, at)
  showOption(at, optionsOf(step).length)
}

/* What to call an option without asking: what it says, or what it stands for.
 * Written text is read as written, a key is read through whatever answers it,
 * and an option that is one-per-thing is named for the thing. */
const { t, te } = useI18n()
const cardText = inject<CardTextAccess | null>(CARD_TEXT, null)

const saidBy = (label: string): string => {
  if (isOwnTextKey(label)) return cardText?.read(ownTextKey(label)) ?? ''
  if (isSharedTextKey(label)) {
    const held = parseSharedLabel(label)
    return held && te(held.key) ? t(held.key, readableParams(held.params)) : ''
  }
  return label
}

const optionName = (option: any, oi: number): string => {
  if (option?.query) return `each ${option.query.kind || 'thing'}`
  const said = saidBy(option?.label ?? '')
    .replace(/[*_]/g, '')
    .replace(/\s+/g, ' ')
    .trim()
  return said || `Option ${oi + 1}`
}

/* Two options can say the same thing and differ only in a matcher further
 * down, which is more than a tab can hold. They are counted off instead, so
 * the strip still says which is which. */
const optionTitle = (step: any, option: any, oi: number): string => {
  const name = optionName(option, oi)
  const alike = optionsOf(step).filter((o, i) => optionName(o, i) === name)
  if (alike.length < 2) return name
  const nth = optionsOf(step).filter((o, i) => i <= oi && optionName(o, i) === name).length
  return `${name} (${nth})`
}
</script>

<template>
  <div class="steps">
    <ScopeBar :bindings="announced" />
    <div v-for="(step, index) in steps" :key="index" :id="anchorFor(index)" class="step">
      <div class="step-head">
        <span class="step-kind">{{ KIND_LABELS[kindOf(step)] }}</span>
        <button type="button" class="step-remove" :title="$t('ui.removeThisStep')" @click="remove(index)">
          ×
        </button>
      </div>

      <div class="step-body">

      <template v-if="kindOf(step) === 'query'">
        <p class="hint">
          {{ $t('ui.aLetStepCanBindAQueryDirectlyWhich') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.kind') }}
            <select
              :value="step.query?.kind"
              @change="set(index, { ...step, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            {{ $t('ui.bindTo') }}
            <input
              :value="step.bind"
              :placeholder="$t('ui.enemies')"
              @input="set(index, { ...step, bind: ($event.target as HTMLInputElement).value })"
              @keydown.stop
            />
          </label>
          <label>
            {{ $t('ui.mode') }}
            <select :value="step.mode ?? 'all'" @change="set(index, { ...step, mode: ($event.target as HTMLSelectElement).value })">
              <option value="all">{{ $t('ui.all') }}</option>
              <option value="first">{{ $t('ui.first') }}</option>
              <option value="count">{{ $t('ui.queryCount') }}</option>
            </select>
          </label>
        </div>
        <ValueEditor
          :bindings="scopeFor(index)"
          :type="matcherType(step.query?.kind)"
          :label="$t('ui.matcher')"
          :modelValue="step.query?.matcher"
          @update:modelValue="set(index, { ...step, query: { ...step.query, matcher: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'let'">
        <label>
          {{ $t('ui.name') }}
          <input
            :value="step.let"
            :placeholder="$t('ui.icons')"
            @input="set(index, { ...step, let: ($event.target as HTMLInputElement).value })"
            @keydown.stop
          />
        </label>
        <ExpressionEditor
          :queryKinds="queryKinds"
          :label="$t('ui.expression')"
          :bindings="scopeFor(index)"
          :modelValue="step.be"
          @update:modelValue="set(index, { ...step, be: $event })"
        />

      </template>

      <template v-else-if="kindOf(step) === 'random'">
        <p class="summary">
          <template v-if="step.random?.from === undefined">
            {{ $t('ui.aFreshIdForTheStepsThatHaveTo') }}
          </template>
          <template v-else>{{ $t('ui.oneThingOutOfTheListPickedAtRandom') }}</template>
        </p>
        <div class="row">
          <label>
            {{ $t('ui.bindTo') }}
            <input
              :value="step.random?.bind"
              placeholder="sid"
              @input="set(index, { ...step, random: { ...step.random, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <label>
            {{ $t('ui.what') }}
            <select
              :value="step.random?.from === undefined ? 'id' : 'from'"
              @change="setRandomKind(index, ($event.target as HTMLSelectElement).value)"
            >
              <option value="id">{{ $t('ui.aFreshId') }}</option>
              <option value="from">{{ $t('ui.oneOfAList') }}</option>
            </select>
          </label>
        </div>
        <ExpressionEditor
          v-if="step.random?.from !== undefined"
          :queryKinds="queryKinds"
          :label="$t('ui.outOf')"
          :bindings="scopeFor(index)"
          :modelValue="step.random?.from"
          @update:modelValue="set(index, { ...step, random: { ...step.random, from: $event } })"
        />
        <p v-if="step.random?.from !== undefined" class="hint">
          {{ $t('ui.aMatcherCanBePickedFromDirectlyAQuery') }}
        </p>
      </template>

      <ValueEditor
          :bindings="scopeFor(index)"
        v-else-if="kindOf(step) === 'push'"
        type="Message"
        :label="$t('ui.message')"
        :modelValue="step.push"
        @update:modelValue="set(index, { ...step, push: $event })"
      />

      <template v-else-if="kindOf(step) === 'if'">
        <p class="hint">{{ $t('ui.takesTheFirstBranchWhenTheConditionHolds') }}</p>
        <ConditionField
          :bindings="scopeFor(index)"
          :queryKinds="queryKinds"
          :modelValue="step.if"
          @update:modelValue="set(index, { ...step, if: $event })"
        />
        <!-- Both branches belong to the If above them, so they are bounded and
             indented under it rather than left as two labelled step lists that
             read as siblings of the matcher. -->
        <div class="branch">
          <span class="branch-label">{{ $t('ui.then') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
            :bindings="innerScope(index)"
            :path="innerPath(index, 'then')"
            :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="step.then ?? []"
            @update:modelValue="set(index, { ...step, then: $event })"
          />
        </div>
        <div class="branch">
          <span class="branch-label">{{ $t('ui.else') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
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
          :title="$t('ui.nothingHappensOtherwiseSoTheElseHasNothingTo')"
          @click="toWhen(index, step)"
        >
          {{ $t('ui.makeThisAWhen') }}
        </button>
      </template>

      <template v-else-if="kindOf(step) === 'when'">
        <p class="hint">{{ $t('ui.runsTheStepsBelowOnlyWhenTheConditionHolds') }}</p>
        <ConditionField
          :bindings="scopeFor(index)"
          :queryKinds="queryKinds"
          :modelValue="step.when"
          @update:modelValue="set(index, { ...step, when: $event })"
        />
        <div class="branch">
          <span class="branch-label">{{ $t('ui.then') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
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
            :title="$t('ui.sayWhatHappensWhenItFindsNothing')"
            @click="toIf(index, step)"
          >
            {{ $t('ui.makeThisAnIf') }}
          </button>
          <button
            type="button"
            class="convert"
            :title="$t('ui.dropTheConditionAndLeaveTheStepsWhereThis')"
            @click="promote(index, step)"
          >
            {{ $t('ui.dropTheCondition') }}
          </button>
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'case'">
        <p class="hint">{{ $t('ui.theFirstBranchWhoseConditionHolds') }}</p>
        <div v-for="(b, bi) in branches(step)" :key="bi" class="option">
          <div class="row">
            <span class="branch-label">{{ $t('ui.branch') }} {{ bi + 1 }}</span>
            <button type="button" @click="set(index, { ...step, case: step.case.filter((_: any, i: number) => i !== bi) })">×</button>
          </div>
          <ConditionField
            :bindings="scopeFor(index)"
            :queryKinds="queryKinds"
            :modelValue="b.if"
            @update:modelValue="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, if: $event } : x) })"
          />
          <StepsEditor
          :slotSteps="slotSteps"
          :bindings="innerScope(index)"
          :path="innerPath(index, `case${bi}`)"
          :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="b.steps ?? []"
            @update:modelValue="set(index, { ...step, case: step.case.map((x: any, i: number) => i === bi ? { ...x, steps: $event } : x) })"
          />
        </div>
        <button type="button" class="add" @click="set(index, { ...step, case: [...(step.case ?? []), { if: { kind: 'enemy', matcher: null }, steps: [] }] })">
          {{ $t('ui.branch2') }}
        </button>
        <div class="branch">
          <span class="branch-label">{{ $t('ui.else') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
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
        <p class="hint">{{ $t('ui.runsTheStepsOncePerThingFoundWithIt') }}</p>
        <div class="row">
          <label>
            {{ $t('ui.over') }}
            <select
              :value="overSource(step.forEach)"
              @change="setSource(index, 'forEach', ($event.target as HTMLSelectElement).value)"
            >
              <option value="query">{{ $t('ui.whatAMatcherFinds') }}</option>
              <option value="over">{{ $t('ui.whatIsInAList') }}</option>
            </select>
          </label>
          <label v-if="overSource(step.forEach) === 'query'">
            {{ $t('ui.kind') }}
            <select
              :value="step.forEach?.query?.kind"
              @change="set(index, { ...step, forEach: { ...step.forEach, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            {{ $t('ui.bindTo') }}
            <input
              :value="step.forEach?.bind"
              @input="set(index, { ...step, forEach: { ...step.forEach, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <ValueEditor
          v-if="overSource(step.forEach) === 'query'"
          :bindings="scopeFor(index)"
          :type="matcherType(step.forEach?.query?.kind)"
          :label="$t('ui.matcher')"
          :modelValue="step.forEach?.query?.matcher"
          @update:modelValue="set(index, { ...step, forEach: { ...step.forEach, query: { ...step.forEach.query, matcher: $event } } })"
        />
        <ExpressionEditor
          v-else
          :queryKinds="queryKinds"
          :label="$t('ui.theList')"
          :bindings="scopeFor(index)"
          :modelValue="step.forEach?.over"
          @update:modelValue="set(index, { ...step, forEach: { ...step.forEach, over: $event } })"
        />
        <StepsEditor
          :slotSteps="slotSteps"
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
          {{ $t('ui.runsTheStepsBelowOnlyWhileASkillTest') }}
        </p>
        <label>
          {{ $t('ui.bindTo') }}
          <input
            :value="step.withSkillTest?.bind"
            placeholder="skillTestId"
            @input="set(index, { ...step, withSkillTest: { ...step.withSkillTest, bind: ($event.target as HTMLInputElement).value } })"
            @keydown.stop
          />
        </label>
        <StepsEditor
          :slotSteps="slotSteps"
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
          {{ $t('ui.runsTheStepsBelowWhereSomethingIsWithThat') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.ofWhat') }}
            <select
              :value="step.withLocationOf?.kind ?? 'investigator'"
              @change="set(index, { ...step, withLocationOf: { ...step.withLocationOf, kind: ($event.target as HTMLSelectElement).value } })"
            >
              <option v-for="k in LOCATEABLE" :key="k" :value="k">{{ k }}</option>
            </select>
          </label>
          <label>
            {{ $t('ui.bindTo') }}
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
          :label="$t('ui.whichOne')"
          :bindings="scopeFor(index)"
          :modelValue="step.withLocationOf?.of"
          @update:modelValue="set(index, { ...step, withLocationOf: { ...step.withLocationOf, of: $event } })"
        />
        <StepsEditor
          :slotSteps="slotSteps"
          :bindings="innerScope(index)"
          :path="innerPath(index, 'withLocationOf')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.withLocationOf?.steps ?? []"
          @update:modelValue="set(index, { ...step, withLocationOf: { ...step.withLocationOf, steps: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'request'">
        <p class="hint">
          {{ $t('ui.someMessagesAreAnsweredByAnotherMessageATurn') }}
          <code>RequestChaosTokens_</code> by <code>RequestedChaosTokens_</code>,
          <code>FindEncounterCard</code> by <code>FoundEncounterCard</code>{{ $t('ui.everyOneOfThemSendsTheAnswerBackBy') }}
        </p>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="Message"
          :label="$t('ui.ask')"
          :modelValue="step.request?.push"
          @update:modelValue="set(index, { ...step, request: { ...step.request, push: $event } })"
        />
        <label>
          {{ $t('ui.answeredBy') }}
          <select
            :value="step.request?.on"
            @change="set(index, { ...step, request: { ...step.request, on: ($event.target as HTMLSelectElement).value } })"
          >
            <option value="">{{ $t('ui.choose2') }}</option>
            <option v-for="m in ANSWERS" :key="m" :value="m">{{ m }}</option>
          </select>
        </label>
        <p v-if="step.request?.on" class="hint">
          {{ $t('ui.boundForTheStepsBelow') }} <code>$message</code>{{ $t('ui.and') }} <code>$0</code>,
          <code>$1</code>{{ $t('ui.forWhatTheAnswerCarriesNotWhatEarlierSteps') }}
        </p>
        <div class="branch">
          <span class="branch-label">{{ $t('ui.withTheAnswer') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
            :bindings="innerScope(index)"
            :path="innerPath(index, 'request')"
            :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="step.request?.steps ?? []"
            @update:modelValue="set(index, { ...step, request: { ...step.request, steps: $event } })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'search'">
        <p class="hint">
          {{ $t('ui.searchesAZoneAndSaysWhatHappensToWhat') }} <em>{{ $t('ui.whatHappensToWhatYouFind') }}</em> {{ $t('ui.isOneOfTheEngineSOwnWaysOf') }} <em>{{ $t('ui.whateverTheStepsBelowSay') }}</em> {{ $t('ui.orWritingEitherBranchHandsTheResultBackTo') }}
          <em>{{ $t('ui.foundNothing') }}</em> {{ $t('ui.branchWithIt') }}
        </p>

        <label>
          {{ $t('ui.how') }}
          <select
            :value="step.search?.how ?? 'search'"
            @change="setSearch(index, step, { how: ($event.target as HTMLSelectElement).value })"
          >
            <option v-for="[value, label] in SEARCH_HOWS" :key="value" :value="value">{{ label }}</option>
          </select>
        </label>

        <label>
          {{ $t('ui.whoseZones') }} <span class="muted">{{ $t('ui.defaultsToWhoeverUsedThis') }}</span>
          <input
            type="text"
            placeholder="$iid"
            :value="step.search?.of ?? ''"
            @input="setSearch(index, step, { of: ($event.target as HTMLInputElement).value || undefined })"
            @keydown.stop
          />
        </label>

        <h6>{{ $t('ui.whereToLook') }}</h6>
        <div v-for="(zone, zi) in searchZones(step)" :key="zi" class="zone">
          <select
            :value="zone.zone"
            @change="setSearchZone(index, step, zi, { zone: ($event.target as HTMLSelectElement).value })"
          >
            <option v-for="[value, label] in SEARCH_ZONES" :key="value" :value="value">{{ label }}</option>
          </select>
          <!-- A depth is an expression: "the top X cards, where X is your
               location's shroud" has no number to write down. -->
          <ExpressionEditor
            v-if="ZONE_TAKES_DEPTH.includes(zone.zone)"
            :label="$t('ui.howMany')"
            :bindings="scopeFor(index)"
            :modelValue="zone.amount ?? 1"
            @update:modelValue="setSearchZone(index, step, zi, { amount: $event })"
          />
          <select
            :value="zone.return ?? 'ShuffleBackIn'"
            @change="setSearchZone(index, step, zi, { return: ($event.target as HTMLSelectElement).value })"
          >
            <option v-for="[value, label] in SEARCH_RETURNS" :key="value" :value="value">{{ label }}</option>
          </select>
          <button type="button" class="drop" @click="removeSearchZone(index, step, zi)">
            <font-awesome-icon icon="trash" />
          </button>
        </div>
        <button type="button" class="add-zone" @click="addSearchZone(index, step)">
          <font-awesome-icon icon="plus" /> {{ $t('ui.anotherZone') }}
        </button>

        <ValueEditor
          optional
          :label="$t('ui.onlyCardsMatching')"
          type="ExtendedCardMatcher"
          :bindings="scopeFor(index)"
          :modelValue="step.search?.matcher"
          @update:modelValue="setSearch(index, step, { matcher: $event })"
        />

        <label>
          {{ $t('ui.whatHappensToWhatYouFind') }}
          <select
            :value="step.search?.found ?? 'return'"
            @change="setSearch(index, step, { found: ($event.target as HTMLSelectElement).value })"
          >
            <option v-for="[value, label] in SEARCH_FOUND" :key="value" :value="value">{{ label }}</option>
          </select>
        </label>

        <ExpressionEditor
          v-if="SEARCH_COUNTED.includes(step.search?.found)"
          :label="$t('ui.howMany')"
          :bindings="scopeFor(index)"
          :modelValue="step.search?.amount ?? 1"
          @update:modelValue="setSearch(index, step, { amount: $event })"
        />

        <label class="inline">
          <input
            type="checkbox"
            :checked="step.search?.asDraw === true"
            @change="setSearch(index, step, { asDraw: ($event.target as HTMLInputElement).checked || undefined })"
          />
          {{ $t('ui.findingACardCountsAsDrawingIt') }}
        </label>

        <template v-if="searchDefers(step)">
          <label>
            {{ $t('ui.bindTheCardTo') }}
            <input
              type="text"
              placeholder="card"
              :value="step.search?.bind ?? ''"
              @input="setSearch(index, step, { bind: ($event.target as HTMLInputElement).value || undefined })"
              @keydown.stop
            />
          </label>
          <p class="declares">
            {{ $t('ui.binds') }} <code>${{ step.search?.bind || 'card' }}</code> {{ $t('ui.cardAnd') }}
            <code>${{ step.search?.bind || 'card' }}s</code> {{ $t('ui.cardForThe') }}
            <em>found</em> {{ $t('ui.steps') }}
          </p>
        </template>

        <div v-if="step.search?.onFound !== undefined" class="branch">
          <span class="branch-label">{{ $t('ui.whenItFindsSomething') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
            :bindings="innerScope(index)"
            :path="innerPath(index, 'onFound')"
            :announce="addedInside(index)"
            :queryKinds="queryKinds"
            :modelValue="step.search?.onFound ?? []"
            @update:modelValue="setSearch(index, step, { onFound: $event })"
          />
        </div>

        <!-- The other branch, which the engine only ever takes for a deferred
             search: "If you cannot discard an event, shuffle this back in". -->
        <div v-if="step.search?.onNoneFound !== undefined" class="branch">
          <span class="branch-label">{{ $t('ui.whenItFindsNothing') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
            :bindings="scopeFor(index)"
            :path="innerPath(index, 'onNoneFound')"
            :announce="[]"
            :queryKinds="queryKinds"
            :modelValue="step.search?.onNoneFound ?? []"
            @update:modelValue="setSearch(index, step, { onNoneFound: $event })"
          />
        </div>

        <div class="branch-toggles">
          <button type="button" @click="toggleSearchBranch(index, step, 'onFound')">
            {{ step.search?.onFound === undefined ? $t('ui.foundSomething') : $t('ui.foundSomething2') }}
          </button>
          <button type="button" @click="toggleSearchBranch(index, step, 'onNoneFound')">
            {{ step.search?.onNoneFound === undefined ? $t('ui.foundNothing2') : $t('ui.foundNothing3') }}
          </button>
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'distribute'">
        <p class="hint">
          {{ $t('ui.asksOnceSplittingATotalBetweenInvestigatorsAndRuns') }}
        </p>
        <ExpressionEditor
          :queryKinds="queryKinds"
          :label="$t('ui.totalToSplit')"
          expect="Int"
          :bindings="scopeFor(index)"
          :modelValue="step.distribute?.total"
          @update:modelValue="set(index, { ...step, distribute: { ...step.distribute, total: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          type="InvestigatorMatcher"
          :label="$t('ui.among')"
          :modelValue="step.distribute?.among?.matcher"
          @update:modelValue="set(index, { ...step, distribute: { ...step.distribute, among: { kind: 'investigator', matcher: $event } } })"
        />
        <div class="row">
          <label>
            {{ $t('ui.prompt') }}
            <input
              :value="step.distribute?.label"
              :placeholder="$t('ui.howManyEach')"
              @input="set(index, { ...step, distribute: { ...step.distribute, label: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <label>
            {{ $t('ui.bindTheInvestigatorTo') }}
            <input
              :value="step.distribute?.bind"
              :placeholder="$t('ui.who')"
              @input="set(index, { ...step, distribute: { ...step.distribute, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <label>
            {{ $t('ui.bindShareTo') }}
            <input
              :value="step.distribute?.amount"
              :placeholder="$t('ui.amount2')"
              @input="set(index, { ...step, distribute: { ...step.distribute, amount: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <div class="branch">
          <span class="branch-label">{{ $t('ui.forEachShare') }}</span>
          <StepsEditor
            :slotSteps="slotSteps"
            :bindings="innerScope(index)"
            :path="innerPath(index, 'distribute')"
            :announce="addedInside(index)"
            :queryKinds="queryKinds"
            :modelValue="step.distribute?.steps ?? []"
            @update:modelValue="set(index, { ...step, distribute: { ...step.distribute, steps: $event } })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'repeat'">
        <p class="hint">
          {{ $t('ui.runsTheStepsBelowOverAndOverATotal') }}
        </p>
        <!-- The two readings of "repeat": all of it now, or a pass at a time
             when something asks. The second is a name, because the steps that
             ask for a pass have to say which repeat they mean. -->
        <div class="row">
          <label>
            {{ $t('ui.runs') }}
            <select
              :value="step.repeat?.as ? 'called' : 'now'"
              @change="setRepeatMode(index, step, ($event.target as HTMLSelectElement).value)"
            >
              <option value="now">{{ $t('ui.everyPassOneAfterAnother') }}</option>
              <option value="called">{{ $t('ui.onePassEachTimeItIsCalled') }}</option>
            </select>
          </label>
          <label v-if="step.repeat?.as !== undefined">
            {{ $t('ui.calledAs') }}
            <input
              :value="step.repeat?.as"
              :placeholder="$t('ui.again')"
              @input="set(index, { ...step, repeat: { ...step.repeat, as: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
        </div>
        <ExpressionEditor
          :queryKinds="queryKinds"
          :label="step.repeat?.as ? $t('ui.howManyPassesAtMost') : $t('ui.howManyTimes')"
          expect="Int"
          :bindings="scopeFor(index)"
          :modelValue="step.repeat?.times"
          @update:modelValue="set(index, { ...step, repeat: { ...step.repeat, times: $event } })"
        />
        <label>
          {{ $t('ui.bindThePassNumberTo') }}
          <input
            :value="step.repeat?.bind"
            placeholder="i"
            @input="set(index, { ...step, repeat: { ...step.repeat, bind: ($event.target as HTMLInputElement).value } })"
            @keydown.stop
          />
        </label>
        <StepsEditor
          :slotSteps="slotSteps"
          :bindings="innerScope(index)"
          :path="innerPath(index, 'repeat')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.repeat?.steps ?? []"
          @update:modelValue="set(index, { ...step, repeat: { ...step.repeat, steps: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'call'">
        <label>
          {{ $t('ui.anotherPassOf') }}
          <select
            :value="typeof step.call === 'string' ? step.call : ''"
            @change="set(index, { call: ($event.target as HTMLSelectElement).value })"
          >
            <option value="">{{ $t('ui.chooseARepeat') }}</option>
            <option v-for="loop in repeatsInScope(index)" :key="loop.name" :value="`$${loop.name}`">
              ${{ loop.name }}
            </option>
          </select>
        </label>
        <p v-if="!repeatsInScope(index).length" class="hint">
          {{ $t('ui.noRepeatInScopeIsCalledByNameSet') }}
        </p>
      </template>

      <template v-else-if="kindOf(step) === 'modify'">
        <p class="hint">
          {{ $t('ui.theSourceIsThisCardAndTheModifiersCarry') }}
        </p>
        <ValueEditor
          type="Target"
          :label="$t('ui.whatIsModified')"
          :bindings="scopeFor(index)"
          :modelValue="step.modify?.target"
          @update:modelValue="set(index, { ...step, modify: { ...step.modify, target: $event } })"
        />
        <ValueEditor
          optional
          type="EffectWindow"
          :label="$t('ui.forHowLongDefaultsToThisSkillTest')"
          :bindings="scopeFor(index)"
          :modelValue="step.modify?.window"
          @update:modelValue="set(index, { ...step, modify: { ...step.modify, window: $event } })"
        />
        <ValueEditor
          type="[ModifierType]"
          :label="$t('ui.modifiers')"
          :bindings="scopeFor(index)"
          :modelValue="step.modify?.modifiers"
          @update:modelValue="set(index, { ...step, modify: { ...step.modify, modifiers: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'choose'">
        <p class="hint">
          {{ $t('ui.namedOptionsEachWithStepsOfItsOwnAn') }}
        </p>
        <!-- One tab per option, named for what the option says. The panel below
             is the one option being written, so where it begins and ends is the
             edge of the panel rather than a reader's guess. -->
        <div class="tabbed">
          <div class="tabs" role="tablist">
            <button
              v-for="(option, oi) in optionsOf(step)"
              :key="oi"
              type="button"
              class="tab"
              role="tab"
              :class="{ on: activeOption(step, index) === oi }"
              :aria-selected="activeOption(step, index) === oi"
              :title="optionTitle(step, option, oi)"
              @click="showOption(index, oi)"
            >
              <span class="tab-name">{{ optionTitle(step, option, oi) }}</span>
              <span
                class="tab-remove"
                :title="$t('ui.removeThisOption')"
                @click.stop="removeOption(step, oi, index)"
                >&times;</span
              >
            </button>
            <button type="button" class="tab add-tab" :title="$t('ui.addAnOption')" @click="addAndShowOption(step, index)">
              +
            </button>
          </div>
          <div
            v-for="(option, oi) in optionsOf(step)"
            v-show="activeOption(step, index) === oi"
            :key="oi"
            class="option tab-panel"
          >
            <div class="row">
              <label>
                {{ $t('ui.thisOptionIs') }}
                <select
                  :value="option.query ? 'query' : 'one'"
                  @change="setOptionSource(step, oi, index, ($event.target as HTMLSelectElement).value)"
                >
                  <option value="one">{{ $t('ui.oneNamedOption') }}</option>
                  <option value="query">{{ $t('ui.onePerThingAMatcherFinds') }}</option>
                </select>
              </label>
              <LabelField
                v-if="!option.query"
                :label="$t('ui.optionLabel')"
                :bindings="optionScope(index, option, oi)"
                :modelValue="option.label"
                @update:modelValue="setOption(step, oi, { ...option, label: $event }, index)"
              />
              <label v-if="option.query">
                {{ $t('ui.kind') }}
                <select
                  :value="option.query?.kind"
                  @change="setOption(step, oi, { ...option, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } }, index)"
                >
                  <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
                </select>
              </label>
              <label v-if="option.query">
                {{ $t('ui.bindTo') }}
                <input
                  :value="option.bind"
                  placeholder="chosen"
                  @input="setOption(step, oi, { ...option, bind: ($event.target as HTMLInputElement).value }, index)"
                  @keydown.stop
                />
              </label>
            </div>

            <ValueEditor
              v-if="option.query"
              :bindings="innerScope(index)"
              :type="matcherType(option.query?.kind)"
              :label="$t('ui.matcher')"
              :modelValue="option.query?.matcher"
              @update:modelValue="setOption(step, oi, { ...option, query: { ...option.query, matcher: $event } }, index)"
            />

            <!-- Whether this option is on the table at all. Read before the query
                 runs, so it cannot name what the option binds. -->
            <ValueEditor
              optional
              type="Criterion"
              :label="$t('ui.onlyOfferedWhenOptional')"
              :bindings="innerScope(index)"
              :modelValue="option.criteria?.criteria ?? (option.criteria?.tag ? option.criteria : undefined)"
              @update:modelValue="setOption(step, oi, { ...option, criteria: $event ? { criteria: $event } : undefined }, index)"
            />
            <!-- Said where it is created, the way every other binding in the
                 builder is: an option with a query stands for one option per thing
                 found, and that thing is what its steps are written against. -->
            <div v-if="optionBinding(index, option, oi)" class="step-foot">
              <span class="binds">
                <span class="binds-label">{{ $t('ui.binds') }}</span>
                <code class="binds-name"
                  >${{ optionBinding(index, option, oi)!.name
                  }}<span v-if="optionBinding(index, option, oi)!.type" class="binds-type">
                    :: {{ optionBinding(index, option, oi)!.type }}</span
                  ></code
                >
                <span class="binds-scope">{{ $t('ui.forThisOptionSSteps') }}</span>
              </span>
            </div>
            <StepsEditor
            :slotSteps="slotSteps"
            :bindings="optionScope(index, option, oi)"
            :path="innerPath(index, `option${oi}`)"
            :announce="optionBinding(index, option, oi) ? [optionBinding(index, option, oi)!] : []"
              :queryKinds="queryKinds"
              :modelValue="option.steps ?? []"
              @update:modelValue="setOption(step, oi, { ...option, steps: $event }, index)"
            />
          </div>
        </div>
      </template>

      <p v-else-if="kindOf(step) === 'cancelBatch'" class="hint">
        {{ $t('ui.stopsTheThingThisAbilityIsReactingToFor') }} <code>{{ $t('ui.would') }}</code> {{ $t('ui.windowWhichIsWhatCarriesTheBatchToCancel') }}
      </p>

      <template v-else-if="kindOf(step) === 'useAbility'">
        <p class="hint">
          {{ $t('ui.resolvesOneOfThisCardSOwnAbilitiesOffered') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.ability') }}
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
            :label="$t('ui.mayDecline')"
            :modelValue="!!step.useAbility?.optional"
            @update:modelValue="set(index, { ...step, useAbility: { ...step.useAbility, optional: $event } })"
          />
          <LabelField
            v-if="step.useAbility?.optional"
            :label="$t('ui.declineLabel')"
            :placeholder="$t('ui.doNot')"
            :bindings="scopeFor(index)"
            :modelValue="step.useAbility?.declineLabel"
            @update:modelValue="set(index, { ...step, useAbility: { ...step.useAbility, declineLabel: $event } })"
          />
        </div>
        <div class="row">
          <BoolField
            :label="$t('ui.ignoreItsLimit')"
            :modelValue="!!step.useAbility?.ignoreLimit"
            @update:modelValue="set(index, { ...step, useAbility: { ...step.useAbility, ignoreLimit: $event } })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'playCard'">
        <p class="hint">
          {{ $t('ui.offersTheCardsYouCouldPlayPayingTheCost') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.playedBy') }} <span class="muted">{{ $t('ui.defaultsToWhoeverIsResolvingThis') }}</span>
            <input
              :value="step.playCard?.iid"
              placeholder="$iid"
              @input="set(index, { ...step, playCard: { ...step.playCard, iid: ($event.target as HTMLInputElement).value || undefined } })"
              @keydown.stop
            />
          </label>
          <label>
            {{ $t('ui.thisCardOptional') }}
            <input
              :value="step.playCard?.card"
              placeholder="$card"
              @input="set(index, { ...step, playCard: { ...step.playCard, card: ($event.target as HTMLInputElement).value || undefined } })"
              @keydown.stop
            />
          </label>
          <BoolField
            :label="$t('ui.withoutPayingItsCost')"
            :modelValue="!!step.playCard?.free"
            @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, free: $event } })"
          />
        </div>
        <div class="row">
          <BoolField
            :label="$t('ui.mayDecline')"
            :modelValue="!!step.playCard?.optional"
            @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, optional: $event } })"
          />
          <LabelField
            v-if="step.playCard?.optional"
            :label="$t('ui.declineLabel')"
            :placeholder="$t('ui.doNot')"
            :bindings="scopeFor(index)"
            :modelValue="step.playCard?.declineLabel"
            @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, declineLabel: $event } })"
          />
        </div>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="CardMatcher"
          :label="$t('ui.whichCards')"
          :modelValue="step.playCard?.matcher"
          @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, matcher: $event } })"
        />
        <ExpressionEditor
          :queryKinds="queryKinds"
          :bindings="scopeFor(index)"
          :label="$t('ui.discount')"
          expect="Int"
          :modelValue="step.playCard?.discount ?? 0"
          @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, discount: $event } })"
        />
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Criterion"
          :label="$t('ui.discountOnlyWhenOptional')"
          :modelValue="step.playCard?.discountIf?.criteria"
          @update:modelValue="set(index, { ...step, playCard: { ...step.playCard, discountIf: $event ? { criteria: $event } : undefined } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'fight'">
        <p class="hint">
          {{ $t('ui.fightAnEnemyWhetherTheCardItselfIsA') }}
        </p>
        <BoolField
          :label="$t('ui.aBasicFightActionInstead')"
          :modelValue="!!step.fight?.basic"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, basic: $event } })"
        />
        <p v-if="step.fight?.basic" class="hint">
          {{ $t('ui.theEnemySOwnAttackAbilityGrantedSoIt') }}
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="EnemyMatcher"
          :label="$t('ui.whichEnemiesOptional')"
          :modelValue="step.fight?.matcher"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, matcher: $event } })"
        />
        <BoolField
          v-if="!step.fight?.basic"
          :label="$t('ui.ignoreTheAloofKeywordForThisAttack')"
          :modelValue="!!step.fight?.ignoreAloof"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, ignoreAloof: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          v-if="!step.fight?.basic"
          type="[ModifierType]"
          :label="$t('ui.forThisAttack')"
          :modelValue="step.fight?.modifiers"
          @update:modelValue="set(index, { ...step, fight: { ...step.fight, modifiers: $event } })"
        />

      </template>

      <template v-else-if="kindOf(step) === 'investigate'">
        <p class="summary">
          {{ $t('ui.investigateTheWayTheActionDoesTheTestIt') }}
          <code>$sid</code>{{ $t('ui.soAModifierForThisInvestigationGoesInThe') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.using') }}
            <select
              :value="step.investigate?.skill ?? ''"
              @change="set(index, { ...step, investigate: { ...step.investigate, skill: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">{{ $t('ui.theLocationSOwnSkill') }}</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
          <label v-if="step.investigate?.skill">
            {{ $t('ui.insteadOf') }}
            <select
              :value="step.investigate?.insteadOf ?? ''"
              @change="set(index, { ...step, investigate: { ...step.investigate, insteadOf: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">{{ $t('ui.alwaysUseIt') }}</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
        </div>
        <p v-if="step.investigate?.skill && step.investigate?.insteadOf" class="hint">
          {{ $t('ui.swapsOnlyWhenTheTestWouldHaveUsedThat') }}
        </p>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="[ModifierType]"
          :label="$t('ui.forThisInvestigation')"
          :modelValue="step.investigate?.modifiers"
          @update:modelValue="set(index, { ...step, investigate: { ...step.investigate, modifiers: $event } })"
        />

      </template>

      <template v-else-if="kindOf(step) === 'evade'">
        <p class="summary">{{ $t('ui.evadeAnEnemyTheTestItStartsIs') }} <code>$sid</code>.</p>
        <div class="row">
          <label>
            {{ $t('ui.using') }}
            <select
              :value="step.evade?.skill ?? ''"
              @change="set(index, { ...step, evade: { ...step.evade, skill: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">{{ $t('ui.theEnemySOwnSkill') }}</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
          <label v-if="step.evade?.skill">
            {{ $t('ui.insteadOf') }}
            <select
              :value="step.evade?.insteadOf ?? ''"
              @change="set(index, { ...step, evade: { ...step.evade, insteadOf: ($event.target as HTMLSelectElement).value || undefined } })"
            >
              <option value="">{{ $t('ui.alwaysUseIt') }}</option>
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
        </div>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="EnemyMatcher"
          :label="$t('ui.whichEnemiesOptional')"
          :modelValue="step.evade?.matcher"
          @update:modelValue="set(index, { ...step, evade: { ...step.evade, matcher: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          type="[ModifierType]"
          :label="$t('ui.forThisEvasion')"
          :modelValue="step.evade?.modifiers"
          @update:modelValue="set(index, { ...step, evade: { ...step.evade, modifiers: $event } })"
        />

      </template>

      <template v-else-if="kindOf(step) === 'parley'">
        <p class="summary">
          {{ $t('ui.parleyAgainstSomethingUnlikeTheOthersThereIsNo') }} <code>$sid</code>.
        </p>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="Target"
          :label="$t('ui.against')"
          :modelValue="step.parley?.target"
          @update:modelValue="set(index, { ...step, parley: { ...step.parley, target: $event } })"
        />
        <label>
          {{ $t('ui.using') }}
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
          :label="$t('ui.difficulty')"
          :modelValue="step.parley?.difficulty"
          @update:modelValue="set(index, { ...step, parley: { ...step.parley, difficulty: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          type="[ModifierType]"
          :label="$t('ui.forThisParley')"
          :modelValue="step.parley?.modifiers"
          @update:modelValue="set(index, { ...step, parley: { ...step.parley, modifiers: $event } })"
        />

      </template>

      <template v-else-if="kindOf(step) === 'test'">
        <p class="summary">
          {{ $t('ui.aPlainSkillTestTheCardNamesItselfThe') }} <code>$sid</code>{{ $t('ui.andWhatItsResultDoesGoesInTheBoxes') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.using') }}
            <select
              :value="step.test?.skill ?? 'SkillWillpower'"
              @change="set(index, { ...step, test: { ...step.test, skill: ($event.target as HTMLSelectElement).value } })"
            >
              <option v-for="sk in SKILLS" :key="sk" :value="sk">{{ sk.replace('Skill', '') }}</option>
            </select>
          </label>
        </div>
        <!-- Two ways to say how hard it is, and they are not the same thing. A
             calculation is re-read as the test goes on; an expression is worked
             out once, when the test begins, and can do arithmetic over bindings. -->
        <div class="row">
          <label>
            {{ $t('ui.difficulty') }}
            <select
              :value="calculatedDifficulty(step.test?.difficulty) ? 'calculation' : 'expression'"
              @change="setDifficultyKind(index, ($event.target as HTMLSelectElement).value)"
            >
              <option value="expression">{{ $t('ui.aNumberTheCardWorksOut') }}</option>
              <option value="calculation">{{ $t('ui.somethingTheEngineCounts') }}</option>
            </select>
          </label>
        </div>
        <ExpressionEditor
          v-if="!calculatedDifficulty(step.test?.difficulty)"
          :queryKinds="queryKinds"
          :label="$t('ui.difficulty')"
          expect="Int"
          :bindings="scopeFor(index)"
          :modelValue="step.test?.difficulty ?? 0"
          @update:modelValue="set(index, { ...step, test: { ...step.test, difficulty: $event } })"
        />
        <ValueEditor
          v-else
          :bindings="scopeFor(index)"
          type="GameCalculation"
          :label="$t('ui.difficulty')"
          :modelValue="step.test?.difficulty"
          @update:modelValue="set(index, { ...step, test: { ...step.test, difficulty: $event } })"
        />
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          :label="$t('ui.aboutWhatOptionalYouByDefault')"
          :modelValue="step.test?.target"
          @update:modelValue="set(index, { ...step, test: { ...step.test, target: $event } })"
        />
        <ValueEditor
          :bindings="scopeFor(index)"
          type="[ModifierType]"
          :label="$t('ui.forThisTest')"
          :modelValue="step.test?.modifiers"
          @update:modelValue="set(index, { ...step, test: { ...step.test, modifiers: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'attack'">
        <p class="summary">
          <template v-if="step.attack?.target">{{ $t('ui.thisCardAttacksTheChosenTarget') }}</template>
          <template v-else>
            {{ $t('ui.thisCardAttacksWhoeverTriggeredTheAbilityItMakes') }}
          </template>
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          :label="$t('ui.attackSomeoneElseInsteadOptional')"
          :modelValue="step.attack?.target"
          @update:modelValue="set(index, { ...step, attack: { ...step.attack, target: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'ready'">
        <p class="summary">
          <template v-if="step.ready?.target">{{ $t('ui.readiesTheChosenCard') }}</template>
          <template v-else>{{ $t('ui.readiesThisCard') }}</template>
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          :label="$t('ui.readySomethingElseInsteadOptional')"
          :modelValue="step.ready?.target"
          @update:modelValue="set(index, { ...step, ready: { ...step.ready, target: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'discardCard'">
        <p class="summary">
          {{ $t('ui.discardsTheCardACardInHandGoesTo') }}
        </p>
        <ExpressionEditor
          :queryKinds="queryKinds"
          :label="$t('ui.whichCard')"
          expect="Card"
          :bindings="scopeFor(index)"
          :modelValue="step.discardCard?.card"
          @update:modelValue="set(index, { ...step, discardCard: { card: $event } })"
        />
        <p class="hint">
          {{ $t('ui.orNameTheEntityOutrightWhichIsWhatDiscard') }}
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          :label="$t('ui.theEntityInsteadOptional')"
          :modelValue="step.discardCard?.target"
          @update:modelValue="set(index, { ...step, discardCard: { target: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'record'">
        <p class="summary">
          {{ $t('ui.theCampaignLogKeepsThreeDifferentThingsUnderA') }}
        </p>
        <div class="row">
          <label v-if="recordKeyIsHomebrew(step)">
            {{ $t('ui.key') }}
            <input
              :value="recordKeyName(step)"
              placeholder="TraitsLearned"
              @input="setRecord(index, { key: { tag: 'HomebrewCampaignLogKey', contents: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <label>
            {{ $t('ui.record') }}
            <select
              :value="recordMode(step)"
              @change="setRecord(index, { mode: ($event.target as HTMLSelectElement).value })"
            >
              <option v-for="m in RECORD_MODES" :key="m.key" :value="m.key">{{ m.label }}</option>
            </select>
          </label>
        </div>
        <ValueEditor
          v-if="!recordKeyIsHomebrew(step)"
          :bindings="scopeFor(index)"
          type="CampaignLogKey"
          :label="$t('ui.key')"
          :modelValue="step.record?.key"
          @update:modelValue="setRecord(index, { key: $event })"
        />
        <p class="hint">{{ recordModeHelp(step) }}</p>
        <ExpressionEditor
          v-if="recordTakesAmount(step)"
          :queryKinds="queryKinds"
          :label="$t('ui.howMuch')"
          expect="Int"
          :bindings="scopeFor(index)"
          :modelValue="step.record?.amount ?? 1"
          @update:modelValue="setRecord(index, { amount: $event })"
        />
        <template v-if="recordTakesValues(step)">
          <ExpressionEditor
            :queryKinds="queryKinds"
            :label="$t('ui.whatToRecord')"
            :bindings="scopeFor(index)"
            :modelValue="step.record?.values"
            @update:modelValue="setRecord(index, { values: $event })"
          />
          <div class="row">
            <label>
              {{ $t('ui.eachOneIs') }}
              <select
                :value="step.record?.kind ?? 'value'"
                @change="setRecord(index, { kind: ($event.target as HTMLSelectElement).value })"
              >
                <option value="value">{{ $t('ui.aValueATraitANameANumber') }}</option>
                <option value="cardCode">{{ $t('ui.aCard') }}</option>
              </select>
            </label>
          </div>
          <p class="hint">
            {{ $t('ui.aListRecordsEveryEntryInItOneEach') }}
          </p>
        </template>
      </template>

      <template v-else-if="kindOf(step) === 'takeAction'">
        <p class="summary">{{ $t('ui.takeOneImmediateActionAsIfItWereYour') }}</p>
      </template>

      <template v-else-if="kindOf(step) === 'draw'">
        <ExpressionEditor
          :queryKinds="queryKinds"
          :label="$t('ui.howMany')"
          expect="Int"
          :bindings="scopeFor(index)"
          :modelValue="step.draw?.amount ?? 1"
          @update:modelValue="set(index, { ...step, draw: { ...step.draw, amount: $event } })"
        />
        <p class="hint">{{ $t('ui.nothingIsDrawnWhenThisWorksOutToZero') }}</p>
      </template>

      <template v-else-if="kindOf(step) === 'gather'">
        <p class="hint">
          {{ $t('ui.shufflesACardIntoTheEncounterDeckGatherDuring') }}
        </p>
        <label class="cap">{{ $t('ui.whichCard') }}</label>
        <CardCodeField
          :modelValue="step.gather?.cardCode"
          @update:modelValue="set(index, { ...step, gather: { ...step.gather, cardCode: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'discover'">
        <p class="summary">
          {{ $t('ui.discoversCluesNotAPushADiscoveryCarriesAn') }}
        </p>
        <ExpressionEditor
          :queryKinds="queryKinds"
          :label="$t('ui.howMany')"
          expect="Int"
          :bindings="scopeFor(index)"
          :modelValue="step.discover?.amount ?? 1"
          @update:modelValue="set(index, { ...step, discover: { ...step.discover, amount: $event } })"
        />
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="InvestigatorId"
          :label="$t('ui.whoDiscoversThemDefaultsToYou')"
          :modelValue="step.discover?.iid"
          @update:modelValue="set(index, { ...step, discover: { ...step.discover, iid: $event } })"
        />
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="LocationId"
          :label="$t('ui.fromWhereDefaultsToYourLocation')"
          :modelValue="step.discover?.location"
          @update:modelValue="set(index, { ...step, discover: { ...step.discover, location: $event } })"
        />
        <BoolField
          :label="$t('ui.countsAsInvestigating')"
          :modelValue="step.discover?.via === 'investigate'"
          @update:modelValue="set(index, { ...step, discover: { ...step.discover, via: $event ? 'investigate' : undefined } })"
        />
        <p class="hint">
          {{ $t('ui.whichDecidesWhetherDiscover1AdditionalClueWhenYou') }}
        </p>
      </template>

      <template v-else-if="kindOf(step) === 'place'">
        <p class="summary">
          {{ $t('ui.putsACardSomewhereAttachToYourLocationAnd') }}
        </p>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="Placement"
          :label="$t('ui.whereItGoes')"
          :modelValue="step.place?.placement"
          @update:modelValue="set(index, { ...step, place: { ...step.place, placement: $event } })"
        />
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          :label="$t('ui.whatIsPlacedDefaultsToThisCard')"
          :modelValue="step.place?.target"
          @update:modelValue="set(index, { ...step, place: { ...step.place, target: $event } })"
        />
        <p class="hint">
          {{ $t('ui.anEventThatPlacesItselfStaysInPlayThe') }}
        </p>
      </template>

      <template v-else-if="kindOf(step) === 'setAside'">
        <p class="summary">
          {{ $t('ui.makesCopiesOfNamedCardsInTheSetAside') }}
        </p>
        <label class="cap">{{ $t('ui.whichCards') }}</label>
        <div v-for="(code, ci) in setAsideCards(step)" :key="ci" class="row">
          <CardCodeField
            :modelValue="code"
            @update:modelValue="setAsideCard(index, ci, $event)"
          />
          <button type="button" class="drop" @click="setAsideCard(index, ci, null)">{{ $t('ui.remove2') }}</button>
        </div>
        <button type="button" class="add-rider" @click="setAsideCard(index, setAsideCards(step).length, '')">
          {{ $t('ui.aCard2') }}
        </button>
        <p class="hint">
          {{ $t('ui.leftEmptyItUsesThisCardSOwnSet') }}
        </p>
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="InvestigatorId"
          :label="$t('ui.whoOwnsThemDefaultsToYou')"
          :modelValue="step.setAside?.iid"
          @update:modelValue="set(index, { ...step, setAside: { ...step.setAside, iid: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'repeatAbilities'">
        <p class="summary">
          {{ $t('ui.doesAgainWhateverACardSAbilitiesAlreadyDid') }}
          <em>{{ $t('ui.effects') }}</em>{{ $t('ui.butTheGameRecordsWhichOfItsAbilitiesWere') }} <em>{{ $t('ui.used') }}</em> {{ $t('ui.soEveryRecordedUseOfThisSourceIsPushed') }}
        </p>
        <ExpressionEditor
          :queryKinds="queryKinds"
          :bindings="scopeFor(index)"
          :label="$t('ui.whoseAbilities')"
          expect="Source"
          :modelValue="step.repeatAbilities?.source"
          @update:modelValue="set(index, { ...step, repeatAbilities: { ...step.repeatAbilities, source: $event } })"
        />
        <p class="hint">
          {{ $t('ui.theSourceIsTheEntityNotTheCardA') }}
        </p>
      </template>

      <template v-else-if="kindOf(step) === 'notifyResult'">
        <p class="summary">
          {{ $t('ui.tellsACardTheCurrentTestSResultAgain') }}
        </p>
        <ExpressionEditor
          :queryKinds="queryKinds"
          :bindings="scopeFor(index)"
          :label="$t('ui.whichCardToTellDefaultsToThisOne')"
          expect="Target"
          :modelValue="step.notifyResult?.target"
          @update:modelValue="set(index, { ...step, notifyResult: { ...step.notifyResult, target: $event } })"
        />
        <p class="hint">
          {{ $t('ui.theMessageIsTheTestSOwnResultSo') }} <em>skill</em> {{ $t('ui.targetStandingInLimbo') }}
        </p>
      </template>

      <template v-else-if="kindOf(step) === 'skillTestOption'">
        <p class="summary">
          {{ $t('ui.holdsTheseStepsBackAndOffersThemAsOne') }}
        </p>
        <LabelField
          :label="$t('ui.label')"
          :placeholder="$t('ui.resolve')"
          :bindings="scopeFor(index)"
          :modelValue="step.skillTestOption?.label"
          @update:modelValue="set(index, { ...step, skillTestOption: { ...step.skillTestOption, label: $event } })"
        />
        <StepsEditor
          :slotSteps="slotSteps"
          :bindings="innerScope(index)"
          :path="innerPath(index, 'skillTestOption')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.skillTestOption?.steps ?? []"
          @update:modelValue="set(index, { ...step, skillTestOption: { ...step.skillTestOption, steps: $event } })"
        />
        <p class="hint">
          {{ $t('ui.anOptionResolvesAfterEveryCommittedCardHasBeen') }}
        </p>
      </template>

      <template v-else-if="kindOf(step) === 'activateAbility'">
        <p class="summary">
          {{ $t('ui.usesAnAbilityThatIsNotThisCardS') }}
        </p>
        <ValueEditor
          :bindings="scopeFor(index)"
          type="AbilityMatcher"
          :label="$t('ui.whichAbilities')"
          :modelValue="step.activateAbility?.matcher"
          @update:modelValue="set(index, { ...step, activateAbility: { ...step.activateAbility, matcher: $event } })"
        />
        <ValueEditor
          type="[ModifierType]"
          :label="$t('ui.modifiersOnThoseAbilities')"
          :bindings="scopeFor(index)"
          :modelValue="step.activateAbility?.modifiers"
          @update:modelValue="set(index, { ...step, activateAbility: { ...step.activateAbility, modifiers: $event } })"
        />
        <p class="hint">
          {{ $t('ui.theseGoOnTheAbilitiesThemselvesNotOnAnything') }}
        </p>
        <div class="row">
          <BoolField
            :label="$t('ui.doesNotProvokeAttacksOfOpportunity')"
            :modelValue="!!step.activateAbility?.noAttacksOfOpportunity"
            @update:modelValue="set(index, { ...step, activateAbility: { ...step.activateAbility, noAttacksOfOpportunity: $event } })"
          />
        </div>
        <div class="row">
          <BoolField
            :label="$t('ui.mayDecline')"
            :modelValue="!!step.activateAbility?.optional"
            @update:modelValue="set(index, { ...step, activateAbility: { ...step.activateAbility, optional: $event } })"
          />
          <LabelField
            v-if="step.activateAbility?.optional"
            :label="$t('ui.declineLabel')"
            :placeholder="$t('ui.doNot')"
            :bindings="scopeFor(index)"
            :modelValue="step.activateAbility?.declineLabel"
            @update:modelValue="set(index, { ...step, activateAbility: { ...step.activateAbility, declineLabel: $event } })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'seal'">
        <p class="summary">
          {{ $t('ui.sealsChaosTokensOnThisCardOrLetsGo') }}
        </p>
        <BoolField
          :label="$t('ui.releaseWhatItIsHolding')"
          :modelValue="!!step.seal?.release"
          @update:modelValue="set(index, { ...step, seal: $event ? { release: true } : { tokens: null } })"
        />
        <ExpressionEditor
          v-if="!step.seal?.release"
          :queryKinds="queryKinds"
          :label="$t('ui.whichTokens')"
          expect="[ChaosToken]"
          :bindings="scopeFor(index)"
          :modelValue="step.seal?.tokens"
          @update:modelValue="set(index, { ...step, seal: { ...step.seal, tokens: $event } })"
        />
        <p v-else class="hint">
          {{ $t('ui.everyTokenThisCardIsHoldingSoReleaseEach') }}
        </p>
        <ExpressionEditor
          v-if="step.seal?.release"
          :queryKinds="queryKinds"
          :label="$t('ui.onlyTheseOptional')"
          expect="[ChaosToken]"
          :bindings="scopeFor(index)"
          :modelValue="step.seal?.tokens"
          @update:modelValue="set(index, { ...step, seal: { ...step.seal, tokens: $event } })"
        />
        <ValueEditor
          optional
          :bindings="scopeFor(index)"
          type="Target"
          :label="$t('ui.onWhatDefaultsToThisCard')"
          :modelValue="step.seal?.on"
          @update:modelValue="set(index, { ...step, seal: { ...step.seal, on: $event } })"
        />
      </template>

      <template v-else-if="kindOf(step) === 'slot'">
        <p class="hint">
          {{ $t('ui.whateverTheCardPrintingThisKeywordPutInThat') }}
        </p>
        <label class="cap">{{ $t('ui.whichSlot') }}</label>
        <select
          :value="String(step).replace(/^\$/, '')"
          @change="set(index, `$${($event.target as HTMLSelectElement).value}`)"
        >
          <option v-for="slot in slotSteps ?? []" :key="slot.name" :value="slot.name">
            {{ slot.detail || slot.name }}
          </option>
        </select>
      </template>

      <template v-else-if="kindOf(step) === 'customize'">
        <p class="hint">
          {{ $t('ui.markACheckboxOnAnUpgradeSheetForA') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.whoOptional') }}
            <input
              :value="step.customize?.iid"
              placeholder="$iid"
              @input="set(index, { ...step, customize: { ...step.customize, iid: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <BoolField
            :label="$t('ui.mayDecline')"
            :modelValue="step.customize?.optional !== false"
            @update:modelValue="set(index, { ...step, customize: { ...step.customize, optional: $event } })"
          />
        </div>
      </template>

      <template v-else-if="kindOf(step) === 'chooseFrom'">
        <p class="hint">
          {{ $t('ui.oneOptionPerThingFoundWithItBoundFor') }}
        </p>
        <div class="row">
          <label>
            {{ $t('ui.between') }}
            <select
              :value="overSource(step.chooseFrom)"
              @change="setSource(index, 'chooseFrom', ($event.target as HTMLSelectElement).value)"
            >
              <option value="query">{{ $t('ui.whatAMatcherFinds') }}</option>
              <option value="over">{{ $t('ui.whatIsInAList') }}</option>
            </select>
          </label>
          <label v-if="overSource(step.chooseFrom) === 'query'">
            {{ $t('ui.kind') }}
            <select
              :value="step.chooseFrom?.query?.kind"
              @change="set(index, { ...step, chooseFrom: { ...step.chooseFrom, query: { kind: ($event.target as HTMLSelectElement).value, matcher: null } } })"
            >
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label>
            {{ $t('ui.bindTo') }}
            <input
              :value="step.chooseFrom?.bind"
              @input="set(index, { ...step, chooseFrom: { ...step.chooseFrom, bind: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <BoolField
            :label="$t('ui.mayDecline')"
            :modelValue="!!step.chooseFrom?.optional"
            @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, optional: $event } })"
          />
          <LabelField
            v-if="step.chooseFrom?.optional"
            :label="$t('ui.declineLabel')"
            :placeholder="$t('ui.doNot')"
            :bindings="scopeFor(index)"
            :modelValue="step.chooseFrom?.declineLabel"
            @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, declineLabel: $event } })"
          />
        </div>
        <ValueEditor
          v-if="overSource(step.chooseFrom) === 'query'"
          :bindings="scopeFor(index)"
          :type="matcherType(step.chooseFrom?.query?.kind)"
          :label="$t('ui.matcher')"
          :modelValue="step.chooseFrom?.query?.matcher"
          @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, query: { ...step.chooseFrom.query, matcher: $event } } })"
        />
        <template v-else>
          <ExpressionEditor
            :queryKinds="queryKinds"
            :label="$t('ui.theList')"
            :bindings="scopeFor(index)"
            :modelValue="step.chooseFrom?.over"
            @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, over: $event } })"
          />
          <label>
            {{ $t('ui.holding') }}
            <select
              :value="step.chooseFrom?.kind ?? ''"
              @change="setOverKind(index, step, ($event.target as HTMLSelectElement).value)"
            >
              <option value="">{{ $t('ui.plainWordsATraitASkillANumber') }}</option>
              <option v-for="(_, kind) in queryKinds" :key="kind" :value="kind">{{ kind }}</option>
            </select>
          </label>
          <label v-if="!step.chooseFrom?.kind">
            {{ $t('ui.labelEachOption') }}
            <input
              :value="step.chooseFrom?.label"
              :placeholder="`$${step.chooseFrom?.bind || 'chosen'}`"
              @input="set(index, { ...step, chooseFrom: { ...step.chooseFrom, label: ($event.target as HTMLInputElement).value } })"
              @keydown.stop
            />
          </label>
          <p class="hint">
            {{ $t('ui.aMatcherSOptionsShowAsTheThingItself') }}
          </p>
        </template>
        <StepsEditor
          :slotSteps="slotSteps"
          :bindings="innerScope(index)"
          :path="innerPath(index, 'chooseFrom')"
          :announce="addedInside(index)"
          :queryKinds="queryKinds"
          :modelValue="step.chooseFrom?.steps ?? []"
          @update:modelValue="set(index, { ...step, chooseFrom: { ...step.chooseFrom, steps: $event } })"
        />
      </template>

      <!-- The riders every test-starting step shares, below whatever that step
           asked for. Not part of the chain above: they belong to all five. -->
      <template v-if="startsTest(step)">
        <fieldset class="on-reveal">
          <legend>{{ $t('ui.ifAChaosTokenIsRevealedDuringThisTest') }}</legend>
          <BoolField
            :label="$t('ui.itDoesSomething')"
            :modelValue="!!onReveal(step, kindOf(step))"
            @update:modelValue="setOnReveal(index, kindOf(step), $event ? { tokens: null, steps: [] } : undefined)"
          />
          <template v-if="onReveal(step, kindOf(step))">
            <ValueEditor
              type="ChaosTokenMatcher"
              :label="$t('ui.whichTokens')"
              :bindings="scopeFor(index)"
              :modelValue="onReveal(step, kindOf(step)).tokens"
              @update:modelValue="setOnReveal(index, kindOf(step), { ...onReveal(step, kindOf(step)), tokens: $event })"
            />
            <BoolField
              :label="$t('ui.onlyOnceTheTestHasSucceeded')"
              :modelValue="!!onReveal(step, kindOf(step)).whenPassed"
              @update:modelValue="setOnReveal(index, kindOf(step), { ...onReveal(step, kindOf(step)), whenPassed: $event })"
            />
            <StepsEditor
              :slotSteps="slotSteps"
              :queryKinds="queryKinds"
              :bindings="revealScope(index)"
              :path="innerPath(index, 'reveal')"
              :announce="[]"
              :modelValue="onReveal(step, kindOf(step)).steps ?? []"
              @update:modelValue="setOnReveal(index, kindOf(step), { ...onReveal(step, kindOf(step)), steps: $event })"
            />
          </template>
        </fieldset>

      </template>

      <!-- "If you succeed, …" belongs to any step that can name a test, not just
           one that started it: a card played into someone else's test says it
           too, and by then its own steps have long finished. -->
      <template v-if="hasOutcomes(step)">
        <fieldset v-for="which in (['onSuccess', 'onFailure'] as const)" :key="which" class="outcome">
          <legend>{{ OUTCOME_LEGENDS[which] }}</legend>
          <BoolField
            :label="$t('ui.somethingHappens')"
            :modelValue="outcomes(step, which).length > 0"
            @update:modelValue="setOutcomes(index, which, $event ? [{ steps: [] }] : [])"
          />
          <div v-for="(rider, ri) in outcomes(step, which)" :key="ri" class="rider">
            <div v-if="outcomes(step, which).length > 1" class="rider-head">
              <span class="rider-label">{{ $t('ui.and2') }}</span>
              <button type="button" class="drop" @click="removeRider(index, which, ri)">{{ $t('ui.remove2') }}</button>
            </div>
            <ValueMatcherField
              :bindings="scopeFor(index)"
              :modelValue="rider.by"
              @update:modelValue="setRider(index, which, ri, { ...rider, by: $event })"
            />
            <p class="hint">
              {{ $t('ui.byHowMuchWhenTheCardSaysSoIf') }}
            </p>
            <StepsEditor
              :slotSteps="slotSteps"
              :queryKinds="queryKinds"
              :bindings="kindOf(step) === 'withSkillTest' ? innerScope(index) : revealScope(index)"
              :path="innerPath(index, `${which}${ri}`)"
              :announce="[]"
              :modelValue="rider.steps ?? []"
              @update:modelValue="setRider(index, which, ri, { ...rider, steps: $event })"
            />
            <p class="hint">
              {{ $t('ui.workedOutWhenTheTestBeginsSoTheseCan') }}
            </p>
          </div>
          <button
            v-if="outcomes(step, which).length > 0"
            type="button"
            class="add-rider"
            @click="addRider(index, which)"
          >
            {{ $t('ui.anotherAtADifferentMargin') }}
          </button>
        </fieldset>
      </template>
      </div>

      <div v-if="bindsOf(step, index).length" class="step-foot">
        <!-- Read as a declaration, the way a binding is written everywhere else
             in the builder, rather than as a sentence with code spans dropped
             into it -- with a type that made two of them run together. -->
        <span v-for="bound in bindsOf(step, index)" :key="bound.name" class="binds">
          <span class="binds-label">{{ $t('ui.binds') }}</span>
          <code class="binds-name"
            >${{ bound.name }}<span v-if="bound.type" class="binds-type"> :: {{ bound.type }}</span></code
          >
          <span class="binds-scope">{{ $t('ui.for') }} {{ bound.scope }}</span>
        </span>
      </div>
    </div>

    <!-- A menu rather than a row of buttons: twenty names side by side is a wall
         to read, and the name alone does not say what the step does. -->
    <div ref="addEl" class="step-actions">
      <button type="button" @click="toggleAdd">{{ $t('ui.step') }}</button>
      <div v-if="addingStep" class="kind-menu">
        <input
          v-model="addSearch"
          type="search"
          class="kind-search"
          :placeholder="$t('ui.typeToFilterEnterToPick')"
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
          <li v-if="!matchingKinds.length" class="muted">{{ $t('ui.nothingMatches') }}</li>
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
/* The riders a test-starting step carries. Bounded and labelled, because each is
 * a different moment of the same test and they read as siblings otherwise. */
.on-reveal,
.outcome {
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

/* One rider of several. Separated only once there IS more than one, so the
   ordinary single "if you succeed" reads exactly as it did before. */
.rider {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;

  & + .rider {
    border-top: 1px dashed #374151;
    padding-top: 0.4rem;
  }
}

.rider-head {
  align-items: center;
  display: flex;
  gap: 0.5rem;
}

.rider-label {
  color: #9ca3af;
  font-size: 0.72rem;
}

.add-rider {
  align-self: flex-start;
  background: none;
  border: 1px dashed #374151;
  border-radius: 4px;
  color: #9ca3af;
  font-size: 0.72rem;
  padding: 0.2rem 0.5rem;
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

/* The panel under the strip is `.tab-panel`, in the stylesheet, since the
   elder sign's parts are tabbed the same way. */


/* What a step binds for the steps under it, said on the step that binds it --
   so a $name in scope can be traced back to where it came from. */
.declares {
  color: #9ca3af;
  font-size: 0.72rem;
  margin: 0;

  code {
    color: #5eead4;
  }
}

h6 {
  color: #9ca3af;
  font-size: 0.72rem;
  letter-spacing: 0.04em;
  margin: 0.2rem 0 0;
  text-transform: uppercase;
}

.muted {
  opacity: 0.6;
}

/* One zone of a Search, on a line: where, how deep, and what happens to the
   rest. Read left to right as the sentence a card prints. */
.zone {
  align-items: flex-end;
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
}

.add-zone,
.branch-toggles > button {
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

.branch-toggles {
  display: flex;
  gap: 0.4rem;
}

/* A checkbox reads as one line, not as a stacked label over a field. */
label.inline {
  align-items: center;
  flex-direction: row;
  gap: 0.35rem;
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

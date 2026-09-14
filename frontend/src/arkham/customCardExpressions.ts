/* The expression language of "Arkham.Custom.Expr", described well enough for the
 * editor to offer only what fits and for a `let` to say what it bound.
 *
 * Every entry names what it takes and what it gives back, so a binding made by
 * a step carries a type into the steps after it and the next transform can be
 * filtered to the ones that accept it. The engine is the authority; this is a
 * description of it, and the two are kept together by name.
 */
import type { Binding } from '@/arkham/customCardBindings'

export type Transform = {
  name: string
  label: string
  /** What it accepts. */
  from: string
  /** What it yields. */
  to: string
}

/* Reading a Payment apart. These mirror the helpers in `Arkham.Cost` that
 * `applyFn` dispatches to, which is why a cost that took no cards yields an
 * empty list rather than the binding being absent. */
export const TRANSFORMS: Transform[] = [
  { name: 'paidCards', label: 'the cards it discarded', from: 'Payment', to: '[Card]' },
  { name: 'discardedCard', label: 'the single card it discarded', from: 'Payment', to: 'Card' },
  { name: 'chosenCard', label: 'the card it chose', from: 'Payment', to: 'CardId' },
  { name: 'chosenEnemy', label: 'the enemy it chose', from: 'Payment', to: 'EnemyId' },
  { name: 'exhausted', label: 'what it exhausted', from: 'Payment', to: '[Target]' },
  { name: 'removed', label: 'what it removed', from: 'Payment', to: '[Target]' },
  { name: 'paymentTargets', label: 'everything it touched', from: 'Payment', to: '[Target]' },
  { name: 'sealedTokens', label: 'the tokens it sealed', from: 'Payment', to: '[ChaosToken]' },
  { name: 'actionsPaid', label: 'actions spent', from: 'Payment', to: 'Int' },
  { name: 'resourcesPaid', label: 'resources spent', from: 'Payment', to: 'Int' },
  { name: 'cluesPaid', label: 'clues spent', from: 'Payment', to: 'Int' },
  { name: 'usesPaid', label: 'uses spent', from: 'Payment', to: 'Int' },
  { name: 'damagePaid', label: 'damage taken', from: 'Payment', to: 'Int' },
  { name: 'horrorPaid', label: 'horror taken', from: 'Payment', to: 'Int' },
  { name: 'curseTokensPaid', label: 'curse tokens added', from: 'Payment', to: 'Int' },
  { name: 'cardsDiscarded', label: 'how many cards it discarded', from: 'Payment', to: 'Int' },
]

/* `fetchCard` in the engine: anything with a FetchCard instance can name the
 * card behind it.
 *
 * Which kind of id it is travels with the stage, because it cannot be read back
 * off the value -- every id serializes as a bare uuid, so the editor writes down
 * the type it was transforming at the time. */
export const FETCHABLE: Record<string, string> = {
  CardId: 'card',
  AssetId: 'asset',
  EventId: 'event',
  TreacheryId: 'treachery',
  EnemyId: 'enemy',
  LocationId: 'location',
  StoryId: 'story',
}

/* The kinds of thing a `get property` stage can read, and where the property
 * names for each come from. A card reads its def; a skill test reads the test
 * being resolved. */
export const PROP_KINDS: Record<string, string> = {
  card: 'Card',
  skillTest: 'SkillTestId',
}

export const CARD_PROPS: Record<string, string> = {
  icons: '[SkillIcon]',
  skills: '[SkillIcon]',
  traits: '[Trait]',
  class: '[ClassSymbol]',
  name: 'Text',
  cardCode: 'CardCode',
  cardType: 'CardType',
  level: 'Int',
  cost: 'CardCost',
  printedCost: 'Int',
  modifiedCost: 'Int',
  id: 'CardId',
}

export const SKILL_TEST_PROPS: Record<string, string> = {
  matchingIcons: '[SkillIcon]',
  difficulty: 'Int',
  action: 'Action',
  investigator: 'InvestigatorId',
  committedCards: '[Card]',
  id: 'SkillTestId',
}

/* What a query of each kind finds. A card query yields whole cards; everything
 * else yields the entity's id, which is what `select` gives back. */
export const QUERY_TYPES: Record<string, string> = {
  enemy: 'EnemyId',
  location: 'LocationId',
  investigator: 'InvestigatorId',
  asset: 'AssetId',
  treachery: 'TreacheryId',
  event: 'EventId',
  skill: 'SkillId',
  story: 'StoryId',
  act: 'ActId',
  agenda: 'AgendaId',
  card: 'Card',
}

/* A query reads as "search: cards  get: first", so the kind names what is being
 * searched and the mode names what is taken from what it found. */
export const QUERY_NOUNS: Record<string, string> = {
  enemy: 'enemies',
  location: 'locations',
  investigator: 'investigators',
  asset: 'assets',
  treachery: 'treacheries',
  event: 'events',
  skill: 'skills',
  story: 'stories',
  act: 'acts',
  agenda: 'agendas',
  card: 'cards',
}

export const QUERY_MODES = [
  { key: 'all', label: 'all' },
  { key: 'first', label: 'first' },
  { key: 'count', label: 'count' },
]

/** What a `{query, mode}` works out to, by kind and by how it is read. */
export function queryType(query: any, mode?: string): string | undefined {
  if (mode === 'count') return 'Int'
  const found = QUERY_TYPES[query?.kind]
  if (!found) return undefined
  return mode === 'first' ? found : listOf(found)
}

export const listOf = (type?: string) => (type ? `[${type}]` : undefined)
export const elementOf = (type?: string) =>
  type && type.startsWith('[') ? type.slice(1, -1) : undefined

/* An unknown type fits anywhere, in either direction: an entity `Field` and a
 * binding a `let` made from something we cannot read both have one, and
 * refusing them would block expressions that are perfectly good. */
export function typeFits(actual: string | undefined, expected: string | undefined): boolean {
  if (!actual || !expected || actual === 'any' || expected === 'any') return true
  if (expected === '[any]') return actual.startsWith('[')
  if (actual === '[any]') return expected.startsWith('[')
  return actual === expected
}

/** The transforms that accept a value of this type. */
export const transformsFor = (type?: string) =>
  TRANSFORMS.filter((t) => typeFits(type, t.from))

/** What an expression works out to, as far as can be told without running it. */
export function expressionType(expr: any, bindings: Binding[] = []): string | undefined {
  if (expr === null || expr === undefined) return undefined
  if (typeof expr === 'number') return 'Int'
  if (typeof expr === 'string') {
    if (!expr.startsWith('$')) return 'Text'
    return bindings.find((b) => `$${b.name}` === expr)?.type
  }
  if (typeof expr !== 'object') return undefined

  if (typeof expr.apply === 'string') {
    // Polymorphic in its input, so these have no single row in the table above.
    if (expr.apply === 'fetchCard') return 'Card'
    if (expr.apply === 'getSkillTest') return 'SkillTest'
    return TRANSFORMS.find((t) => t.name === expr.apply)?.to
  }
  if (typeof expr.skillTest === 'string') return SKILL_TEST_PROPS[expr.skillTest]
  // A query is an expression, so a `let` bound to one is typed like any other.
  if (expr.query) return queryType(expr.query, expr.mode)
  if (typeof expr.get === 'string' || typeof expr.map === 'string') {
    const prop = expr.get ?? expr.map
    // Broadcasting over a list gives a list of the property.
    const inner = expressionType(expr.of, bindings)
    const result =
      expr.kind === 'card'
        ? CARD_PROPS[prop]
        : expr.kind === 'skillTest'
          ? SKILL_TEST_PROPS[prop]
          : undefined
    return inner?.startsWith('[') ? listOf(result) : result
  }
  for (const key of ['count', 'sum', 'max', 'min', 'iconValue', 'add', 'subtract', 'multiply', 'divide']) {
    if (key in expr) return 'Int'
  }
  for (const key of ['unique', 'reverse']) {
    if (key in expr) return expressionType(expr[key], bindings)
  }
  if ('filter' in expr) return expressionType(expr.of, bindings)
  if ('concat' in expr) return elementOf(expressionType(expr.concat, bindings))
  if ('first' in expr) return elementOf(expressionType(expr.first, bindings))
  return undefined
}


/* A stage in a pipeline: one step that takes what it is handed and gives back
 * something else.
 *
 * A stage is stored as the wrapper it becomes, minus the operand -- `{first: _}`,
 * `{apply: 'paidCards'}`, `{get: 'icons', kind: 'card'}` -- so a stage that
 * needs more than a name (a property to read, a predicate to test) carries it. */
export type Stage = {
  name: string
  label: string
  to: string | undefined
  /** The wrapper this stage becomes, with its operand left out. */
  template: Record<string, any>
}

/** Where a stage's operand goes, which differs by which wrapper it is. */
const OPERAND_KEY: Record<string, string> = { apply: 'to', get: 'of', map: 'of', filter: 'of' }
const operandKeyFor = (name: string) => OPERAND_KEY[name] ?? name

const STAGE_NAMES = [
  'apply',
  'get',
  'map',
  'filter',
  'first',
  'count',
  'unique',
  'reverse',
  'concat',
  'sum',
  'max',
  'min',
  'iconValue',
]

export function stagesFor(from: string | undefined): Stage[] {
  const unknown = !from
  const list = from?.startsWith('[') ?? false
  const element = elementOf(from)
  const stages: Stage[] = transformsFor(from).map((t) => ({
    name: t.name,
    label: t.label,
    to: t.to,
    template: { apply: t.name },
  }))

  /* Reading a property works on one card or on many: applied to a list it is
   * applied to each, which is why the result is a list of the property.
   *
   * One stage rather than one per property. Twelve near-identical entries buried
   * the handful of stages that are actually different, and which property is
   * wanted is a second question, asked in a field of its own. What it yields
   * therefore depends on the answer, so `stageResult` works it out rather than
   * the stage declaring it -- and whether a list came back is read off the type
   * it reports rather than from the wording of the label. */
  const fetchKind = from ? FETCHABLE[from] : undefined
  if (unknown || fetchKind) {
    stages.push({
      name: 'fetchCard',
      label: 'get card',
      to: 'Card',
      template: { apply: 'fetchCard', kind: fetchKind ?? 'card' },
    })
  }

  if (unknown || typeFits(from, 'SkillTestId')) {
    stages.push({
      name: 'getSkillTest',
      label: 'get skill test',
      to: 'SkillTest',
      template: { apply: 'getSkillTest' },
    })
    stages.push({
      name: 'get',
      label: 'get property',
      to: undefined,
      template: { get: 'difficulty', kind: 'skillTest' },
    })
  }

  const propSubject = list ? element : from
  if (unknown || typeFits(propSubject, 'Card')) {
    stages.push({
      name: 'get',
      label: 'get property',
      to: undefined,
      template: { get: 'name', kind: 'card' },
    })
  }

  if (list || unknown) {
    stages.push(
      { name: 'first', label: 'the first of them', to: element, template: { first: null } },
      { name: 'count', label: 'how many there are', to: 'Int', template: { count: null } },
      { name: 'unique', label: 'without duplicates', to: from, template: { unique: null } },
      { name: 'reverse', label: 'reversed', to: from, template: { reverse: null } },
      {
        name: 'filter',
        label: 'only some of them',
        to: from,
        template: { filter: { eq: null } },
      },
    )
    if (unknown || element?.startsWith('[')) {
      stages.push({ name: 'concat', label: 'flattened', to: element, template: { concat: null } })
    }
    if (unknown || typeFits(element, 'Int')) {
      stages.push(
        { name: 'sum', label: 'added up', to: 'Int', template: { sum: null } },
        { name: 'max', label: 'the largest', to: 'Int', template: { max: null } },
        { name: 'min', label: 'the smallest', to: 'Int', template: { min: null } },
      )
    }
    if (unknown || typeFits(element, 'SkillIcon')) {
      stages.push({
        name: 'iconValue',
        label: 'what they are worth to this test',
        to: 'Int',
        template: { iconValue: null },
      })
    }
  }
  return stages
}

/** Which stage a stored wrapper is, matched back to the list for its type. */
export function stageKey(stage: any): string | undefined {
  if (!stage || typeof stage !== 'object') return undefined
  if (typeof stage.apply === 'string') return stage.apply
  // Which property is a parameter of the stage, not a stage of its own.
  if (typeof stage.get === 'string' || typeof stage.map === 'string') return 'get'
  const key = Object.keys(stage).find((k) => STAGE_NAMES.includes(k))
  return key
}

/** The property a `get` stage reads, however the stage spells it. */
export const stageProp = (stage: any): string | undefined => stage?.get ?? stage?.map

const isStage = (expr: any) =>
  !!expr && typeof expr === 'object' && !Array.isArray(expr) && !!stageKey(expr)

/** A pipeline read outward: the value it starts from, then each stage in order. */
export function unwindPipeline(expr: any): { source: any; stages: any[] } {
  const stages: any[] = []
  let cur = expr
  while (isStage(cur)) {
    const key = Object.keys(cur).find((k) => STAGE_NAMES.includes(k))!
    const operandKey = operandKeyFor(key)
    const { [operandKey]: operand, ...rest } = cur
    /* `{first: x}` holds its operand under its own name, so taking the operand
     * out would leave nothing behind to say which stage it was. */
    stages.unshift(operandKey === key ? { ...rest, [key]: null } : rest)
    cur = operand
  }
  return { source: cur ?? null, stages }
}

export const windPipeline = (source: any, stages: any[]): any =>
  stages.reduce((acc, stage) => {
    const key = Object.keys(stage).find((k) => STAGE_NAMES.includes(k))!
    return { ...stage, [operandKeyFor(key)]: acc }
  }, source)

/** What a stage gives back, given what it was handed. */
export const stageResult = (stage: any, incoming: string | undefined) => {
  const key = stageKey(stage)
  // A property stage yields whatever that property is, broadcast over a list.
  if (key === 'get') {
    const props = stage?.kind === 'skillTest' ? SKILL_TEST_PROPS : CARD_PROPS
    const type = props[stageProp(stage) ?? '']
    return incoming?.startsWith('[') ? listOf(type) : type
  }
  return stagesFor(incoming).find((st) => st.name === key)?.to
}

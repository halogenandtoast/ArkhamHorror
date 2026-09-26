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
  { name: 'chosenTrait', label: 'the trait it chose', from: 'Payment', to: 'Trait' },
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

/* Which `get property` kind reads a value of this type, mirroring the dispatch in
 * `getProp` ("Arkham.Custom.Expr"). An entity kind names a Field on that entity --
 * `InvestigatorTraits`, `EnemyHealth` -- and those names are not in the served
 * schema, so only `card` and `skillTest` have lists the editor can offer; the rest
 * are typed in.
 *
 * Without this the editor refused a property on anything but a card or a test,
 * while the runner was perfectly happy to read one off an investigator. */
export const PROP_KIND_FOR: Record<string, string> = {
  Card: 'card',
  SkillTestId: 'skillTest',
  EnemyId: 'enemy',
  LocationId: 'location',
  InvestigatorId: 'investigator',
  AssetId: 'asset',
  ActId: 'act',
}

/* Where an entity kind's properties come from. The backend reflects each entity's
 * `Field` GADT into the schema under this name, so the names and the types they
 * yield are the runner's own rather than a copy. */
export const FIELD_SCHEMA: Record<string, string> = {
  investigator: 'Field Investigator',
  enemy: 'Field Enemy',
  location: 'Field Location',
  asset: 'Field Asset',
  act: 'Field Act',
}

/** The entity behind a kind, which is the prefix all of its field names carry. */
export const entityOf = (kind: string | undefined) =>
  (FIELD_SCHEMA[kind ?? ''] ?? '').replace(/^Field /, '')

/* The entity property tables, handed in rather than imported.
 *
 * They come from the served schema, and importing that here would drag the API
 * client into everything that reads this module -- `customCardBindings` and so
 * every editor. `schema.ts` registers the lookup instead, which also keeps the
 * dependency pointing the sensible way: the schema knows about expressions, not
 * the other way round. Absent until it does, which reads as "type unknown". */
let entityProps: (kind: string) => Record<string, string> | undefined = () => undefined

export const setEntityProps = (lookup: (kind: string) => Record<string, string> | undefined) => {
  entityProps = lookup
}

/* A kind's properties, as name to the type reading it yields.
 *
 * `card` and `skillTest` are not entities and have no Field GADT -- their readings
 * are `cardProp`/`skillTestProp`, listed by hand on both sides. Everything else
 * comes from the schema, which is also what makes the type known: without it a
 * property read reported an unknown type, and an unknown type accepts every
 * transform there is -- so reading an investigator's traits offered to tell you
 * which cards a payment discarded. */
export const propOptionsFor = (kind: string | undefined): Record<string, string> | undefined => {
  if (kind === 'skillTest') return SKILL_TEST_PROPS
  if (kind === 'card') return CARD_PROPS
  return kind ? entityProps(kind) : undefined
}

const DEFAULT_PROP: Record<string, string> = { card: 'name', skillTest: 'difficulty' }

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

/* What the type holds, if it is something the runner walks as a list.
 *
 * `valueList` reads any JSON array as a list, and a Set serializes as one, so
 * `Set Trait` is as much a list of traits as `[Trait]` is -- treating only the
 * bracketed spelling as one offered nothing at all for an entity's traits. */
export const elementOf = (type?: string): string | undefined => {
  if (!type) return undefined
  const m = /^\[(.*)\]$/.exec(type.trim()) ?? /^(?:Set|NonEmpty) (.*)$/.exec(type.trim())
  return m ? m[1].trim() : undefined
}

/* An optional is whatever it holds: `valueList` of null is empty and `toInt` of
 * null is zero, so the runner reads `Maybe Int` wherever it reads an Int. */
export const withoutMaybe = (type?: string) =>
  type?.trim().startsWith('Maybe ') ? type.trim().slice(6).trim() : type

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

/* What the campaign log holds. A set's element type is not knowable — the log
 * stores card codes, mementos and bare values under different keys — so it
 * reports a list of anything, which every list transform accepts. */
/* What a record set holds, which nothing can work out for itself.
 *
 * The log stores four kinds of entry, and one of them -- the generic one -- is
 * arbitrary JSON: a trait, a name, a number. So the type is the author's to say.
 * Unset it is a list of anything, which accepts every join and is what made this
 * worth asking: a set of traits would happily be joined with a list of cards.
 *
 * Carried as `holds` beside the key. The runner reads only `recordSet`, so this is
 * inert to it -- a note for the editor, not part of the expression. */
export const RECORD_HOLDS = ['Trait', 'CardCode', 'Text', 'Int', 'Memento', 'Memory'] as const

export const recordSetHolds = (expr: any): string | undefined =>
  typeof expr?.holds === 'string' && expr.holds ? expr.holds : undefined

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
  if ('recordSet' in expr) return listOf(recordSetHolds(expr) ?? 'any')
  if ('recordCount' in expr) return 'Int'
  // A query is an expression, so a `let` bound to one is typed like any other.
  if (expr.query) return queryType(expr.query, expr.mode)
  if (typeof expr.get === 'string' || typeof expr.map === 'string') {
    const prop = expr.get ?? expr.map
    // Broadcasting over a list gives a list of the property.
    const inner = expressionType(expr.of, bindings)
    const result = (propOptionsFor(expr.kind) ?? {})[prop]
    return elementOf(inner) !== undefined ? listOf(result) : result
  }
  for (const key of ['count', 'sum', 'max', 'min', 'iconValue', 'add', 'subtract', 'multiply', 'divide']) {
    if (key in expr) return 'Int'
  }
  for (const key of ['unique', 'reverse']) {
    if (key in expr) return expressionType(expr[key], bindings)
  }
  if ('filter' in expr) return expressionType(expr.of, bindings)
  if ('concat' in expr) {
    // Joined lists: the parts are expressions, so nothing here knows what they
    // hold. Flattening one list of lists yields that list's element type.
    if (Array.isArray(expr.concat)) return undefined
    return elementOf(expressionType(expr.concat, bindings))
  }
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

/* Arithmetic and joining, which hold their operands in a list rather than under a
 * key of their own.
 *
 * As transforms the value they are handed is the first of those operands and the
 * rest are written beside them, which is what makes `(the traits you have learned
 * + 1) / 2` read as "how many -> plus 1 -> divided by 2" instead of three boxes
 * nested inside each other. The engine's JSON is unchanged: it was always a list,
 * and the first entry was always the thing being added to. */
export const NARY_NAMES = ['add', 'subtract', 'multiply', 'divide', 'concat']

export const isNary = (name: string | undefined) => !!name && NARY_NAMES.includes(name)

/* What a nary transform's own operands have to be, given what it was handed.
 *
 * Arithmetic wants numbers. A join wants more of the same list: joining a set of
 * traits to a list of cards is a mistake the runner cannot see -- `concatMap
 * valueList` will happily glue them together and hand you a list of two different
 * things -- so it is worth saying here. Unknown on either side fits, as everywhere
 * else: that is what unknown has to mean. */
export function naryOperandProblem(
  name: string | undefined,
  incoming: string | undefined,
  operand: string | undefined,
): string | undefined {
  if (!operand) return undefined
  if (name === 'concat') {
    if (elementOf(operand) === undefined) return `${operand} is not a list`
    const want = elementOf(withoutMaybe(incoming))
    const got = elementOf(operand)
    if (want && got && want !== 'any' && got !== 'any' && want !== got) {
      return `a list of ${got}, joined to a list of ${want}`
    }
    return undefined
  }
  if (isNary(name) && !typeFits(withoutMaybe(operand), 'Int')) {
    return `${operand} is not a number`
  }
  return undefined
}

/** The operands a nary stage carries besides the one handed to it. */
export const naryExtras = (stage: any): any[] => {
  const key = NARY_NAMES.find((n) => n in (stage ?? {}))
  const held = key ? stage[key] : undefined
  return Array.isArray(held) ? held.slice(1) : []
}

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
  'add',
  'subtract',
  'multiply',
  'divide',
]

export function stagesFor(from0: string | undefined): Stage[] {
  /* Read as the runner reads it: an optional is whatever it holds, and anything
   * that serializes as a JSON array is a list. Judged on the spelling instead,
   * `Maybe Int` took no arithmetic and `Set Trait` no list transform at all. */
  const from = withoutMaybe(from0)
  const unknown = !from
  const element = elementOf(from)
  const list = element !== undefined
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
  }

  /* One `get property` stage, whose kind follows from what it is handed -- there is
   * only ever one reading, so there is nothing to ask. Applied to a list it reads
   * the property of each, which is what the runner does. */
  const propSubject = list ? element : from
  const propKind = propSubject ? PROP_KIND_FOR[propSubject] : undefined
  if (unknown || propKind) {
    const kind = propKind ?? 'card'
    stages.push({
      name: 'get',
      label: 'get property',
      to: undefined,
      template: { get: DEFAULT_PROP[kind] ?? '', kind },
    })
  }

  /* Arithmetic, on anything the runner reads as a number. `toInt` treats a list as
   * its length and a bool as 0/1, but offering "plus" on a list of abilities would
   * be a trap, so this asks for something that really is a number. */
  if (unknown || typeFits(from, 'Int')) {
    stages.push(
      { name: 'add', label: 'plus', to: 'Int', template: { add: [null, null] } },
      { name: 'subtract', label: 'minus', to: 'Int', template: { subtract: [null, null] } },
      { name: 'multiply', label: 'times', to: 'Int', template: { multiply: [null, null] } },
      { name: 'divide', label: 'divided by', to: 'Int', template: { divide: [null, null] } },
    )
  }

  if (list || unknown) {
    stages.push(
      { name: 'concat', label: 'joined with', to: from, template: { concat: [null, null] } },
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
    if (unknown || elementOf(element) !== undefined) {
      stages.push({
        name: 'flatten',
        label: 'the lists inside joined into one',
        to: element,
        template: { concat: null },
      })
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
  /* The two readings of `concat` are two different transforms to choose between,
   * so they answer to different names even though the key is the same. */
  if ('concat' in stage && !Array.isArray(stage.concat)) return 'flatten'
  const key = Object.keys(stage).find((k) => STAGE_NAMES.includes(k))
  return key
}

/** The property a `get` stage reads, however the stage spells it. */
export const stageProp = (stage: any): string | undefined => stage?.get ?? stage?.map

/* `concat` wears two hats, told apart by its operand: given one list of lists it
 * flattens what it was handed, and given a list of expressions it joins them end to
 * end. Both are transforms now, so neither needs keeping out of the pipeline. */
const isStage = (expr: any) =>
  !!expr && typeof expr === 'object' && !Array.isArray(expr) && !!stageKey(expr)

/** A pipeline read outward: the value it starts from, then each stage in order. */
export function unwindPipeline(expr: any): { source: any; stages: any[] } {
  const stages: any[] = []
  let cur = expr
  while (isStage(cur)) {
    const key = Object.keys(cur).find((k) => STAGE_NAMES.includes(k))!
    /* A nary stage's operands are a list whose first entry is the value it was
     * handed, so that entry comes out and a hole is left in its place. */
    if (isNary(key) && Array.isArray(cur[key])) {
      const operands = cur[key]
      stages.unshift({ ...cur, [key]: [null, ...operands.slice(1)] })
      cur = operands[0]
      continue
    }
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
    // Back into the hole it came out of, which is the first operand.
    if (isNary(key) && Array.isArray(stage[key])) {
      return { ...stage, [key]: [acc, ...stage[key].slice(1)] }
    }
    return { ...stage, [operandKeyFor(key)]: acc }
  }, source)

/** What a stage gives back, given what it was handed. */
export const stageResult = (stage: any, incoming: string | undefined) => {
  const key = stageKey(stage)
  // A property stage yields whatever that property is, broadcast over a list.
  if (key === 'get') {
    // An entity's Field has no type the editor knows, which is an honest unknown.
    const props = propOptionsFor(stage?.kind) ?? {}
    const type = props[stageProp(stage) ?? '']
    return incoming?.startsWith('[') ? listOf(type) : type
  }
  return stagesFor(incoming).find((st) => st.name === key)?.to
}

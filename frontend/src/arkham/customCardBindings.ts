/* What a custom card's steps can refer to by `$name`, and where each name came
 * from.
 *
 * A binding is only a string once it is written down, so nothing in the JSON
 * says where one was introduced — and a name that was never bound fails
 * silently rather than complaining. Carrying the origin alongside the name is
 * what lets a field offer the names that are actually in scope where it sits,
 * and lets the author jump back to whatever bound one.
 */

import { QUERY_TYPES, expressionType } from '@/arkham/customCardExpressions'

export type Binding = {
  /** Without the `$`. */
  name: string
  /** The type or value behind it, when known. */
  detail?: string
  /* The Haskell type it holds, when that is known exactly. Absent means "could
   * be anything" -- a `let` binds whatever its expression worked out to -- and
   * such a binding is offered for every field rather than hidden from all of
   * them. */
  type?: string
  /** Where it came from, in words. */
  origin: string
  /** id of the element that introduced it, for jumping back to. */
  anchor?: string
}

/* Always available: the card the steps belong to, and whoever is acting.
 * `Arkham.Custom.Ability` puts these in before any step runs. */
/* What a card's own id is, by the kind of card being edited. Without this `$id`
 * has no type and would be offered for every field. */
const ID_TYPES: Record<string, string> = {
  AssetType: 'AssetId',
  EncounterAssetType: 'AssetId',
  EventType: 'EventId',
  SkillType: 'SkillId',
  EnemyType: 'EnemyId',
  PlayerEnemyType: 'EnemyId',
  TreacheryType: 'TreacheryId',
  PlayerTreacheryType: 'TreacheryId',
  LocationType: 'LocationId',
  StoryType: 'StoryId',
  InvestigatorType: 'InvestigatorId',
}

/* Which entities serialize the fields the steps read by name. `bindings` in
 * "Arkham.Custom.Ability" hands the attrs' own JSON over wholesale, so a field
 * is in scope exactly when that card's attrs carry it -- an enemy has no cardId,
 * only a treachery is drawn by anyone. Listing one that is not there would offer
 * a name that resolves to nothing; leaving one out reports a name that works as
 * out of scope, which is what sent authors looking for a bug that was not there. */
const HAS_CARD_ID = [
  'EventType',
  'AssetType',
  'EncounterAssetType',
  'TreacheryType',
  'PlayerTreacheryType',
  'SkillType',
  'StoryType',
  'ActType',
  'AgendaType',
]

const HAS_DRAWN_BY = ['TreacheryType', 'PlayerTreacheryType']

const on = (types: string[], cardType: string | undefined, binding: Binding): Binding[] =>
  !cardType || types.includes(cardType) ? [binding] : []

export const cardBindings = (cardType?: string): Binding[] => [
  {
    name: 'id',
    detail: cardType ? (ID_TYPES[cardType] ?? "this card's own id") : "this card's own id",
    type: cardType ? ID_TYPES[cardType] : undefined,
    origin: 'the card',
  },
  { name: 'source', detail: 'Source', type: 'Source', origin: 'the card' },
  { name: 'target', detail: 'Target', type: 'Target', origin: 'the card' },
  { name: 'iid', detail: 'InvestigatorId', type: 'InvestigatorId', origin: 'whoever is acting' },
  // These come from the entity's own serialized fields, so they exist only on a
  // card that has them -- an event has a controller, an investigator does not.
  {
    name: 'controller',
    detail: 'InvestigatorId, if the card has one',
    type: 'InvestigatorId',
    origin: 'the card',
  },
  {
    name: 'owner',
    detail: 'InvestigatorId, if the card has one',
    type: 'InvestigatorId',
    origin: 'the card',
  },
  {
    name: 'investigator',
    detail: 'InvestigatorId, on a signature card',
    type: 'InvestigatorId',
    origin: "whoever's signature this is",
  },
  ...on(HAS_CARD_ID, cardType, {
    name: 'cardId',
    detail: 'CardId',
    type: 'CardId',
    origin: 'the card',
  }),
  ...on(HAS_DRAWN_BY, cardType, {
    name: 'drawnBy',
    detail: 'InvestigatorId',
    type: 'InvestigatorId',
    origin: 'whoever drew it',
  }),
]

/* Meta the card is *built* from -- an enemy's prey, where it spawns -- is read
 * before any entity exists, so it cannot see the entity's own fields. Mirrors
 * `defBindings` in "Arkham.Card.CustomCard": the only name a def alone knows is
 * whose signature it is. */
export const defBindings = (): Binding[] => [
  {
    name: 'investigator',
    detail: 'InvestigatorId, on a signature card',
    type: 'InvestigatorId',
    origin: "whoever's signature this is",
  },
]

/* What paying for an ability gave up. Only an ability has this: a handler is a
 * reaction to a message and a modifier is read rather than used, so neither pays
 * for anything.
 *
 * Just the payment itself -- what was actually taken out of it (the cards, the
 * targets exhausted, the resources) is read with a transform in a `let`, since
 * which of those a payment holds depends on the cost. */
export const paymentBindings = (): Binding[] => [
  {
    name: 'payment',
    detail: 'Payment',
    type: 'Payment',
    origin: "this ability's cost",
  },
]

/** The window an ability triggered on, once the author has said which it is. */
export function windowBindings(
  fields: { name: string | null; type: string }[],
  windowName: string,
  anchor: string,
): Binding[] {
  return [
    { name: 'window', detail: windowName, type: 'Window', origin: `the ${windowName} window`, anchor },
    ...fields.map((field, at) => ({
      name: `w${at}`,
      detail: field.name ? `${field.name} :: ${field.type}` : field.type,
      type: field.type,
      origin: `the ${windowName} window`,
      anchor,
    })),
  ]
}

/** The message a handler listens for. */
export function messageBindings(
  fields: { name: string | null; type: string }[],
  tag: string,
  anchor: string,
): Binding[] {
  return [
    { name: 'message', detail: tag, type: 'Message', origin: `the ${tag} message`, anchor },
    ...fields.map((field, at) => ({
      name: String(at),
      detail: field.name ? `${field.name} :: ${field.type}` : field.type,
      type: field.type,
      origin: `the ${tag} message`,
      anchor,
    })),
  ]
}

const stepKinds = [
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
  'fight',
  'investigate',
  'evade',
  'parley',
  'attack',
  'ready',
  'draw',
  'gather',
  'customize',
]

export const stepKind = (step: any): string =>
  stepKinds.find((kind) => kind in (step ?? {})) ?? 'push'

const named = (value: any, fallback: string) =>
  typeof value === 'string' && value.trim() ? value.trim() : fallback

/* What a query of each kind binds. `card` is the whole card; everything else is
 * the id the matcher selected, which is what a Target or a field of that id
 * type wants. */
const queryType = (query: any): string | undefined => QUERY_TYPES[query?.kind]

/* What a step introduces, split by where it is visible.
 *
 * `after` is in scope for the steps that follow. `inside` is in scope only
 * within the step's own steps — a `forEach` binds each thing it found for the
 * body it repeats, and that name means nothing once the loop is over.
 */
export function stepBindings(
  step: any,
  anchor: string,
  /* What is already in scope where this step sits. Only a `let` needs it, to
   * read the type of a binding its expression refers to. */
  scope: Binding[] = [],
): { after: Binding[]; inside: Binding[] } {
  const kind = stepKind(step)
  const none = { after: [], inside: [] }
  const at = (name: string, detail: string, origin: string, type?: string): Binding => ({
    name,
    detail,
    type,
    origin,
    anchor,
  })

  switch (kind) {
    case 'query': {
      const name = named(step.query && step.bind, '')
      if (!name) return none
      const mode = step.mode ?? 'all'
      const found = queryType(step.query)
      const detail =
        mode === 'count' ? 'how many were found' : mode === 'first' ? 'the first found' : 'a list'
      const type =
        mode === 'count' ? 'Int' : found && (mode === 'first' ? found : `[${found}]`)
      return { after: [at(name, detail, 'a Query step', type || undefined)], inside: [] }
    }
    case 'let': {
      const name = named(step.let, '')
      if (!name) return none
      // What the expression works out to, so the steps after this one can be
      // filtered by it the same way any other binding is.
      const type = expressionType(step.be, scope)
      return {
        after: [at(name, type ? `an expression :: ${type}` : 'an expression', 'a Let step', type)],
        inside: [],
      }
    }
    /* A block, so what it binds is in scope only inside it -- there may be no
     * skill test, and nowhere to be, and a name that means nothing outside the
     * block should not be offered outside it. */
    case 'withSkillTest':
      return {
        after: [],
        inside: [
          at(
            named(step.withSkillTest?.bind, 'skillTestId'),
            'the test being resolved',
            'a With skill test step',
            'SkillTestId',
          ),
        ],
      }
    case 'withLocationOf':
      return {
        after: [],
        inside: [
          at(
            named(step.withLocationOf?.bind, 'location'),
            'where it is',
            'a With location of step',
            'LocationId',
          ),
        ],
      }
    case 'forEach':
      return {
        after: [],
        inside: [
          at(
            named(step.forEach?.bind, 'each'),
            'one of what was found',
            'a For each step',
            queryType(step.forEach?.query),
          ),
        ],
      }
    case 'chooseFrom':
      return {
        after: [],
        inside: [
          at(
            named(step.chooseFrom?.bind, 'chosen'),
            'what was chosen',
            'a Choose from step',
            queryType(step.chooseFrom?.query),
          ),
        ],
      }
    case 'fight':
      // A basic fight action makes its own test, so there is no id to hand on.
      return step.fight?.basic
        ? none
        : { after: [at('sid', 'SkillTestId', 'a Fight step', 'SkillTestId')], inside: [] }
    case 'investigate':
      return { after: [at('sid', 'SkillTestId', 'an Investigate step', 'SkillTestId')], inside: [] }
    case 'evade':
      return { after: [at('sid', 'SkillTestId', 'an Evade step', 'SkillTestId')], inside: [] }
    case 'parley':
      return { after: [at('sid', 'SkillTestId', 'a Parley step', 'SkillTestId')], inside: [] }
    default:
      return none
  }
}

/** A stable element id for the step at this path, so a binding can point at it. */
export const stepAnchor = (path: string, index: number) => `ccb-${path}-${index}`

/** Everything in scope for the step at `index`: the base, plus earlier steps. */
export function scopeAt(base: Binding[], steps: any[], index: number, path: string): Binding[] {
  const found = [...base]
  for (let i = 0; i < index; i++) {
    // Handed what is in scope so far, so a `let` can type itself from a binding
    // an earlier step made.
    found.push(...stepBindings(steps[i], stepAnchor(path, i), dedupe(found)).after)
  }
  return dedupe(found)
}

/** Everything in scope inside the step at `index` — its own bindings included. */
export function scopeInside(base: Binding[], steps: any[], index: number, path: string): Binding[] {
  const anchor = stepAnchor(path, index)
  const own = stepBindings(steps[index], anchor, scopeAt(base, steps, index, path))
  return dedupe([...scopeAt(base, steps, index, path), ...own.after, ...own.inside])
}

/* A later binding of the same name shadows an earlier one, which is what the
 * runner does: the environment is a map and a second `query` with the same
 * `bind` overwrites the first. */
function dedupe(bindings: Binding[]): Binding[] {
  const byName = new Map<string, Binding>()
  for (const binding of bindings) byName.set(binding.name, binding)
  return [...byName.values()]
}

/* Scroll to whatever introduced a binding and flash it.
 *
 * The flash class is added to an element owned by another component; Vue's
 * scoped styles key off an attribute on the element, not on who set the class,
 * so the rule still applies. */
export function jumpToBinding(anchor: string | undefined) {
  if (!anchor) return
  const element = document.getElementById(anchor)
  if (!element) return
  element.scrollIntoView({ behavior: 'smooth', block: 'center' })
  element.classList.add('binding-flash')
  window.setTimeout(() => element.classList.remove('binding-flash'), 1400)
}

/* Whether a binding can go in a field of this type.
 *
 * A `Maybe X` field takes an X, and an alias stands for what it aliases (`Who`
 * is an InvestigatorMatcher). A binding with no known type fits anywhere --
 * that is what "unknown" has to mean, since hiding it everywhere would make a
 * `let` unusable.
 */
export function bindingFits(
  binding: Binding,
  fieldType: string,
  resolveAlias: (type: string) => string,
): boolean {
  if (!binding.type) return true
  const strip = (t: string) => {
    const trimmed = t.trim()
    return resolveAlias(trimmed.startsWith('Maybe ') ? trimmed.slice(6).trim() : trimmed)
  }
  return strip(binding.type) === strip(fieldType)
}

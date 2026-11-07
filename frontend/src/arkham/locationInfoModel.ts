/**
 * What the map shows about one location: its name plate and its info card both read this, never the raw
 * location. Which side of the card is face up decides everything printed that is shown -- an unrevealed
 * location shows only its unrevealed side (ArkhamDB's `back_*` fields), never the revealed side's text,
 * shroud, clue value or victory points.
 *
 * Import-free on purpose (like locationGraph.ts) so tests can load it without the Vue toolchain: the card
 * record, the text formatter and the location's choices are handed in.
 */

/** The fields of an ArkhamDB card record this model reads. */
export type CardRecord = {
  code: string
  name: string
  traits?: string
  text?: string
  flavor?: string
  back_name?: string
  back_traits?: string
  back_text?: string
  back_flavor?: string
  shroud?: number | null
  clues?: number | null
  clues_fixed?: boolean
  victory?: number | null
}

/** The fields of a game location this model reads. */
export type InfoLocation = {
  id: string
  label: string
  cardCode: string
  revealed: boolean
  exhausted: boolean
  shroud: number | null
  /** 'PerPlayer' multiplies the printed shroud by the investigator count; 'ValueX' has no fixed number. */
  shroudKind?: string | null
  tokens: Partial<Record<string, number>>
  investigators: string[]
  enemies: string[]
  connectedLocations: string[]
  keys: unknown[]
  seals: unknown[]
  floodLevel: string | null
  brazier: string | null
  breaches: unknown
  modifiers: { type: { tag: string; contents?: unknown } }[]
}

/** A choice offered for the location (from `choicesFor`), with the index it is sent as. */
export type InfoChoice<C> = { index: number; contents: C }

export type LocationFace = {
  name: string
  /** False while no card record is loaded: only the fallback name is known. */
  known: boolean
  traits: string | null
  text: string | null
  flavor: string | null
}

export type LocationInfo<C> = {
  id: string
  revealed: boolean
  face: LocationFace
  /** Null until revealed: the shroud is printed on the revealed side. */
  shroud: { printed: number; current: number; x: boolean } | null
  clues: { left: number; printed: number | null }
  doom: number
  victory: number | null
  /** The face-up text has a forced effect on entering. */
  onEnter: boolean
  here: boolean
  adjacent: boolean
  targetable: boolean
  exhausted: boolean
  investigators: number
  enemies: number
  extras: string[]
  actions: { move: InfoChoice<C> | null; investigate: InfoChoice<C> | null; other: InfoChoice<C>[]; selectIndex: number }
}

export type LocationInfoContext<C> = {
  /** The card record of the revealed side, or null while the card list is loading. */
  front: CardRecord | null
  /** A record filed under the `b` code itself, when ArkhamDB has one; otherwise the front's `back_*` fields are used. */
  ownBack: CardRecord | null
  /** Name to show when no card record is available. */
  fallbackName: string
  playerCount: number
  myLocationId: string | null
  /** Enemies to count as standing here (engaged ones included). */
  enemyCount: number
  choices: InfoChoice<C>[]
  /** Index of the choice that picks this location as a target, or -1. */
  selectIndex: number
}

const blank = (s: string | undefined | null): string | null => (s && s.trim() ? s : null)

/** The side of the card that is face up. */
export function faceUp(revealed: boolean, front: CardRecord | null, ownBack: CardRecord | null, fallbackName: string): LocationFace {
  if (revealed || !front) {
    if (!front) return { name: fallbackName, known: false, traits: null, text: null, flavor: null }
    return { name: front.name, known: true, traits: blank(front.traits), text: blank(front.text), flavor: blank(front.flavor) }
  }
  if (ownBack) return { name: ownBack.name, known: true, traits: blank(ownBack.traits), text: blank(ownBack.text), flavor: blank(ownBack.flavor) }
  return {
    name: front.back_name || front.name,
    known: true,
    traits: blank(front.back_traits) ?? blank(front.traits),
    text: blank(front.back_text),
    flavor: blank(front.back_flavor),
  }
}

/** "Forced - After you enter ...", "Forced – When you enter ...": a hint only, it never blocks anything. */
export function hasEnterEffect(text: string | null): boolean {
  if (!text) return false
  return text.split(/\n|<br\s*\/?>/i).some((line) => /forced/i.test(line) && /\benter(s|ed|ing)?\b/i.test(line))
}

/** The engine's own order: BaseShroud replaces the printed value, ShroudModifiers add (only the raises
 * count while the shroud cannot be reduced), SetShroud overrides last, never below 0. */
export function currentShroud(printed: number, modifiers: InfoLocation['modifiers']): number {
  const tags = modifiers.map((m) => m.type)
  let value = printed
  for (const m of tags) if (m.tag === 'BaseShroud' && typeof m.contents === 'number') value = m.contents
  // A modifier without its own decoder arrives as OtherModifier carrying its tag.
  const floored = tags.some((m) => m.tag === 'ShroudCannotBeReduced' || (m.tag === 'OtherModifier' && m.contents === 'ShroudCannotBeReduced'))
  for (const m of tags) {
    if (m.tag !== 'ShroudModifier' || typeof m.contents !== 'number') continue
    if (floored && m.contents < 0) continue
    value += m.contents
  }
  for (const m of tags) if (m.tag === 'SetShroud' && typeof m.contents === 'number') value = m.contents
  return Math.max(0, value)
}

/** Which basic action an ability-label choice performs, read from the ability itself rather than its index.
 * Only the basic Move / Investigate count: a location's own "[action]: Move to a connecting location" or
 * special investigate is printed text, and stays with the other abilities under its own label. */
export function choiceAction(contents: unknown): string[] {
  const ability = (contents as { ability?: { basic?: boolean } })?.ability
  if (!ability?.basic) return []
  return abilityActions(contents)
}

/** Every action an ability-label choice performs, basic or printed (a card's own "[action]: Move ..."
 * counts as a Move here). Empty for anything that is not an action ability. */
export function abilityActions(contents: unknown): string[] {
  const ability = (contents as { ability?: { type?: { tag?: string; actions?: unknown } } })?.ability
  if (ability?.type?.tag !== 'ActionAbility') return []
  const walk = (a: unknown): string[] => {
    const node = a as { tag?: string; contents?: unknown }
    if (node?.tag === 'SingleAction' && typeof node.contents === 'string') return [node.contents]
    if ((node?.tag === 'AndActions' || node?.tag === 'OrActions') && Array.isArray(node.contents)) return node.contents.flatMap(walk)
    return []
  }
  return walk(ability.type.actions)
}

export function buildLocationInfo<C>(l: InfoLocation, ctx: LocationInfoContext<C>): LocationInfo<C> {
  const face = faceUp(l.revealed, ctx.front, ctx.ownBack, ctx.fallbackName)
  const base = l.shroud ?? ctx.front?.shroud ?? null
  const x = l.shroudKind === 'ValueX'
  const printedShroud = base === null ? null : l.shroudKind === 'PerPlayer' ? base * Math.max(1, ctx.playerCount) : base
  const shroud = l.revealed && printedShroud !== null
    ? { printed: printedShroud, current: x ? printedShroud : currentShroud(printedShroud, l.modifiers), x }
    : null
  const perCard = ctx.front?.clues ?? null
  const printedClues = l.revealed && perCard !== null
    ? (ctx.front?.clues_fixed ? perCard : perCard * Math.max(1, ctx.playerCount))
    : null

  let move: InfoChoice<C> | null = null
  let investigate: InfoChoice<C> | null = null
  const other: InfoChoice<C>[] = []
  for (const choice of ctx.choices) {
    const actions = choiceAction(choice.contents)
    if (!move && actions.includes('Move')) move = choice
    else if (!investigate && actions.includes('Investigate')) investigate = choice
    else other.push(choice)
  }

  const extras: string[] = []
  if (l.keys.length) extras.push(`keys:${l.keys.length}`)
  if (l.seals.length) extras.push(`seals:${l.seals.length}`)
  if (l.floodLevel && l.floodLevel !== 'Unflooded') extras.push(`flood:${l.floodLevel}`)
  if (l.brazier) extras.push(`brazier:${l.brazier}`)
  if (l.breaches) extras.push('breaches')

  return {
    id: l.id,
    revealed: l.revealed,
    face,
    shroud,
    clues: { left: l.tokens.Clue ?? 0, printed: printedClues },
    doom: l.tokens.Doom ?? 0,
    victory: l.revealed ? (ctx.front?.victory ?? null) : null,
    onEnter: hasEnterEffect(face.text),
    here: l.id === ctx.myLocationId,
    adjacent: ctx.myLocationId !== null && l.id !== ctx.myLocationId && l.connectedLocations.includes(ctx.myLocationId),
    targetable: ctx.selectIndex !== -1,
    exhausted: l.exhausted,
    investigators: l.investigators.length,
    enemies: ctx.enemyCount,
    extras,
    actions: { move, investigate, other, selectIndex: ctx.selectIndex },
  }
}

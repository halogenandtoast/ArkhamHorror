/* The number a stat finally has once every modifier on its holder has been applied, together with the
 * list of what changed it. Import-free on purpose so the tests can load it without the Vue toolchain.
 *
 * The engine only prints the base value on the entity; modifiers (a treachery attached to a location,
 * an asset in play, a scenario effect) arrive separately, each carrying its source. The scene shows the
 * final number and lets the player read where the difference came from. */

export type StatKey =
  | 'shroud' | 'fight' | 'evade' | 'enemyHealth' | 'damage' | 'horror'
  | 'willpower' | 'intellect' | 'combat' | 'agility' | 'health' | 'sanity'

/** One modifier's contribution. `base` replaces the printed value, `add` shifts it, `set` overrides it last. */
export type StatPart = { sourceId: string | null; delta: number; mode: 'base' | 'add' | 'set' }

export type Stat = {
  key: StatKey
  printed: number
  current: number
  parts: StatPart[]
  /** Which way the final value moved compared to the printed one. */
  trend: 'up' | 'down' | null
  /** True when the move is bad news for the investigators. */
  bad: boolean
}

/* The minimal shape of a decoded modifier this file reads; the real type is richer. */
type SourceLike = {
  sourceTag?: string
  contents?: unknown
  source?: SourceLike
  originalSource?: SourceLike
}
export type ModifierLike = {
  type: { tag: string; contents?: unknown; skillType?: string; value?: number }
  source?: SourceLike
  /** Set when an effect published the modifier: a "until end of turn / round" bonus, not a lasting one. */
  effect?: string
}

type Spec = {
  base?: string
  add: string[]
  /** Tags carrying `[value, minimum]`: the value is added and the result never drops below the minimum. */
  addWithMin?: string[]
  set?: string
  /** A tag (a plain modifier or an OtherModifier carrying it) that stops the stat being lowered. */
  cannotLower?: string
  skill?: string
  /** Only lasting modifiers count (an asset in play); bonuses an effect hands out for a turn or an attack do not. */
  lastingOnly?: boolean
  higherIsBad: boolean
}

const SPECS: Record<StatKey, Spec> = {
  shroud: { base: 'BaseShroud', add: ['ShroudModifier'], set: 'SetShroud', cannotLower: 'ShroudCannotBeReduced', higherIsBad: true },
  fight: { add: ['EnemyFight'], addWithMin: ['EnemyFightWithMin'], higherIsBad: true },
  evade: { add: ['EnemyEvade'], addWithMin: ['EnemyEvadeWithMin'], higherIsBad: true },
  enemyHealth: { add: ['HealthModifier'], addWithMin: ['HealthModifierWithMin'], higherIsBad: true },
  damage: { add: ['DamageDealt'], higherIsBad: true },
  horror: { add: ['HorrorDealt'], higherIsBad: true },
  willpower: { add: ['SkillModifier'], base: 'BaseSkillOf', set: 'SetSkillValue', skill: 'SkillWillpower', lastingOnly: true, higherIsBad: false },
  intellect: { add: ['SkillModifier'], base: 'BaseSkillOf', set: 'SetSkillValue', skill: 'SkillIntellect', lastingOnly: true, higherIsBad: false },
  combat: { add: ['SkillModifier'], base: 'BaseSkillOf', set: 'SetSkillValue', skill: 'SkillCombat', lastingOnly: true, higherIsBad: false },
  agility: { add: ['SkillModifier'], base: 'BaseSkillOf', set: 'SetSkillValue', skill: 'SkillAgility', lastingOnly: true, higherIsBad: false },
  health: { add: ['HealthModifier'], addWithMin: ['HealthModifierWithMin'], lastingOnly: true, higherIsBad: false },
  sanity: { add: ['SanityModifier'], lastingOnly: true, higherIsBad: false },
}

/** The id of the entity a modifier came from (the card that granted it), or null when it has no single card. */
export function sourceEntityId(source: SourceLike | undefined): string | null {
  if (!source) return null
  const c = source.contents
  switch (source.sourceTag) {
    case 'ProxySource': return sourceEntityId(source.originalSource) ?? sourceEntityId(source.source)
    case 'IndexedSource': return Array.isArray(c) ? sourceEntityId(c[1] as SourceLike) : null
    case 'AbilitySource': return Array.isArray(c) ? sourceEntityId(c[0] as SourceLike) : null
    case 'UseAbilitySource': return Array.isArray(c) ? sourceEntityId(c[1] as SourceLike) : null
    case 'PaymentSource': return sourceEntityId(c as SourceLike)
    case 'BothSource': return Array.isArray(c) ? sourceEntityId(c[0] as SourceLike) ?? sourceEntityId(c[1] as SourceLike) : null
    case 'OtherSource': return typeof c === 'string' ? c : null
    default: return null
  }
}

const numberOf = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null)

/** [value, minimum] as the server sends it; the minimum may arrive wrapped or missing. */
function valueWithMin(contents: unknown): { value: number; min: number | null } | null {
  if (!Array.isArray(contents)) return null
  const value = numberOf(contents[0])
  if (value === null) return null
  return { value, min: numberOf(contents[1]) }
}

function trendOf(key: StatKey, printed: number, current: number): Pick<Stat, 'trend' | 'bad'> {
  if (current === printed) return { trend: null, bad: false }
  const up = current > printed
  return { trend: up ? 'up' : 'down', bad: SPECS[key].higherIsBad ? up : !up }
}

/** Applies the engine's order — replace the base, add the shifts, override last — and never goes below 0. */
export function computeStat(key: StatKey, printed: number, modifiers: readonly ModifierLike[] | null | undefined): Stat {
  const spec = SPECS[key]
  const list = (modifiers ?? []).filter((m) =>
    (!spec.skill || m.type.skillType === spec.skill || m.type.skillType === undefined) && !(spec.lastingOnly && m.effect))
  const parts: StatPart[] = []
  let value = printed
  let minimum: number | null = null

  for (const m of list) {
    if (spec.base && m.type.tag === spec.base) {
      const v = spec.skill ? numberOf(m.type.value) : numberOf(m.type.contents)
      if (v === null || (spec.skill && m.type.skillType !== spec.skill)) continue
      parts.push({ sourceId: sourceEntityId(m.source), delta: v - value, mode: 'base' })
      value = v
    }
  }

  const floored = !!spec.cannotLower && list.some((m) => m.type.tag === spec.cannotLower || (m.type.tag === 'OtherModifier' && m.type.contents === spec.cannotLower))

  for (const m of list) {
    let delta: number | null = null
    if (spec.add.includes(m.type.tag)) {
      delta = spec.skill
        ? (m.type.skillType === spec.skill ? numberOf(m.type.value) : null)
        : numberOf(m.type.contents)
    } else if (spec.addWithMin?.includes(m.type.tag)) {
      const withMin = valueWithMin(m.type.contents)
      if (withMin) {
        delta = withMin.value
        if (withMin.min !== null) minimum = Math.max(minimum ?? withMin.min, withMin.min)
      }
    }
    if (delta === null || delta === 0 || (floored && delta < 0)) continue
    parts.push({ sourceId: sourceEntityId(m.source), delta, mode: 'add' })
    value += delta
  }

  for (const m of list) {
    if (!spec.set || m.type.tag !== spec.set) continue
    const v = spec.skill
      ? (m.type.skillType === spec.skill ? numberOf(m.type.value) : null)
      : numberOf(m.type.contents)
    if (v === null) continue
    parts.push({ sourceId: sourceEntityId(m.source), delta: v - value, mode: 'set' })
    value = v
  }

  if (minimum !== null) value = Math.max(minimum, value)
  value = Math.max(0, value)
  return { key, printed, current: value, parts, ...trendOf(key, printed, value) }
}

/** A modifier that is not a number: something the holder can no longer do, or can no longer be done to it. */
const FLAGS = new Set([
  'CannotEnter', 'CannotBeEnteredBy', 'Blank', 'BlankExceptForcedAbilities', 'CannotDiscoverCluesAt',
  'CannotPerformSkillTest', 'DoubleDifficulty', 'CannotBeDamaged', 'CannotMove', 'CannotInvestigate',
])

export type EffectLine =
  | { kind: 'stat'; stat: StatKey; delta: number; mode: StatPart['mode'] }
  | { kind: 'flag'; flag: string }

const STAT_OF_TAG: Record<string, StatKey> = {
  ShroudModifier: 'shroud', BaseShroud: 'shroud', SetShroud: 'shroud',
  EnemyFight: 'fight', EnemyFightWithMin: 'fight', EnemyEvade: 'evade', EnemyEvadeWithMin: 'evade',
  HealthModifier: 'enemyHealth', HealthModifierWithMin: 'enemyHealth', DamageDealt: 'damage', HorrorDealt: 'horror',
}
const SKILL_STAT: Record<string, StatKey> = {
  SkillWillpower: 'willpower', SkillIntellect: 'intellect', SkillCombat: 'combat', SkillAgility: 'agility',
}

/** What one card is doing to its host, as short lines for a tooltip. A card whose effect is not a stat or a
 * known flag yields nothing: its text is on the card, one hover away. */
export function effectsOfSource(modifiers: readonly ModifierLike[] | null | undefined, sourceId: string): EffectLine[] {
  const lines: EffectLine[] = []
  for (const m of modifiers ?? []) {
    if (sourceEntityId(m.source) !== sourceId) continue
    const tag = m.type.tag
    const mode: StatPart['mode'] = tag.startsWith('Base') ? 'base' : tag.startsWith('Set') ? 'set' : 'add'
    const stat = tag === 'SkillModifier' || tag === 'SetSkillValue' || tag === 'BaseSkillOf'
      ? SKILL_STAT[m.type.skillType ?? ''] : STAT_OF_TAG[tag]
    if (stat) {
      const raw = tag.endsWith('WithMin') ? valueWithMin(m.type.contents)?.value ?? null
        : numberOf(tag === 'SkillModifier' || tag === 'SetSkillValue' || tag === 'BaseSkillOf' ? m.type.value : m.type.contents)
      if (raw !== null && (raw !== 0 || mode !== 'add')) lines.push({ kind: 'stat', stat, delta: raw, mode })
    } else if (FLAGS.has(tag)) {
      lines.push({ kind: 'flag', flag: tag })
    } else if (tag === 'OtherModifier' && typeof m.type.contents === 'string' && FLAGS.has(m.type.contents)) {
      lines.push({ kind: 'flag', flag: m.type.contents })
    }
  }
  return lines
}

/** Whether something stops enemies or investigators entering the holder (a barricade, a locked door). */
export function blocksEntry(modifiers: readonly ModifierLike[] | null | undefined): boolean {
  return (modifiers ?? []).some((m) => m.type.tag === 'CannotEnter' || m.type.tag === 'CannotBeEnteredBy'
    || (m.type.tag === 'OtherModifier' && (m.type.contents === 'CannotEnter' || m.type.contents === 'CannotBeEnteredBy')))
}

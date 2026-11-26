import type { Game } from '@/arkham/types/Game'
import type { Modifier, ModifierType } from '@/arkham/types/Modifier'
import { cardArt, sourceCardCode } from '@/arkham/cardImages'
import { toCardContents } from '@/arkham/types/Card'

/* Everything here needs only a modifier's `type`, which is what lets the same
 * code describe a live `Modifier` and one that arrived over a data attribute. */
export type HasModifierType = { type: ModifierType }

/* One modifier, reduced to something that can be drawn.
 *
 * The engine has 432 ModifierType constructors and the UI used to render 17 of
 * them, so everything else was either a blank pill or a raw Haskell name. This
 * module is total: every modifier gets a group, a polarity and a glyph, and
 * says whether a glyph and a number can carry its meaning on their own.
 *
 * `needsLabel` is the whole point. "+2 health" is a picture of a heart and a
 * +2 in any language; "cards cannot leave your discard pile" is a sentence and
 * has to be translated. The first kind never needs `modifierTag.*`, the second
 * always does, and keeping them apart is what stops the panel turning into a
 * wall of English.
 */

export type ModifierPolarity = 'buff' | 'nerf' | 'ban' | 'grant' | 'neutral'

export type ModifierGroup =
  | 'skills'
  | 'stats'
  | 'combat'
  | 'chaos'
  | 'economy'
  | 'actions'
  | 'cards'
  | 'movement'
  | 'keywords'
  | 'permissions'
  | 'other'

/* The four skills and the stats an entity prints, as glyph names. */
export type SkillKey = 'willpower' | 'intellect' | 'combat' | 'agility' | 'wild'
export type StatKey = 'health' | 'sanity' | 'fight' | 'evade' | 'shroud'
export type TokenKey = 'damage' | 'horror' | 'clue' | 'doom' | 'resource'

export type ModifierGlyph =
  | { kind: 'skill'; skill: SkillKey }
  | { kind: 'stat'; stat: StatKey }
  | { kind: 'token'; token: TokenKey }
  /* An Arkham-font class name without the `-icon` suffix: 'action', 'fast', … */
  | { kind: 'arkham'; icon: string }
  /* One of the shapes ModifierGlyph.vue draws inline. */
  | { kind: 'shape'; shape: ShapeKey }

export type ShapeKey =
  | 'ban'
  | 'allow'
  | 'card'
  | 'deck'
  | 'hand'
  | 'slot'
  | 'move'
  | 'cost'
  | 'test'
  | 'keyword'
  | 'trait'
  | 'victory'
  | 'eye'
  | 'bolt'
  | 'spawn'
  | 'ui'
  | 'cog'

/* How the number beside the glyph should read. `delta` is signed (+2 / −1),
 * `set` replaces (=3), `scale` multiplies (×2), `none` has no number at all. */
export type ModifierOp = 'delta' | 'set' | 'scale' | 'none'

export interface ModifierDisplay {
  /* The constructor name; also the `modifierTag.<tag>` i18n key. */
  tag: string
  group: ModifierGroup
  polarity: ModifierPolarity
  glyph: ModifierGlyph
  op: ModifierOp
  /* The number to draw, already signed for `delta`. */
  value?: number
  /* A second, smaller glyph narrowing the first — the action a skill bonus is
   * limited to, the slot a count applies to. */
  qualifier?: ModifierGlyph
  /* Free text the engine supplied that is already localized or is a proper
   * noun (a keyword, a trait, a scenario label). Drawn as-is. */
  detail?: string
  /* True when glyph + number cannot say what this does, so the row must fall
   * back to the localized `modifierTag.<tag>` sentence. */
  needsLabel: boolean
}

const SKILLS: Record<string, SkillKey> = {
  SkillWillpower: 'willpower',
  SkillIntellect: 'intellect',
  SkillCombat: 'combat',
  SkillAgility: 'agility',
  SkillWild: 'wild',
  willpower: 'willpower',
  intellect: 'intellect',
  combat: 'combat',
  agility: 'agility',
  wild: 'wild',
}

const skillGlyph = (raw: unknown): ModifierGlyph => ({
  kind: 'skill',
  skill: (typeof raw === 'string' && SKILLS[raw]) || 'wild',
})

/* An action name ("Fight", "Investigate", …) has no icon of its own, so a
 * qualified bonus shows the generic action glyph and names the action in
 * `detail`. */
const ACTION_GLYPH: ModifierGlyph = { kind: 'arkham', icon: 'action' }

const num = (v: unknown): number | undefined => (typeof v === 'number' ? v : undefined)
const str = (v: unknown): string | undefined => (typeof v === 'string' ? v : undefined)

/* A signed delta is a buff when it helps whoever carries it. That is all the
 * polarity we can honestly compute: whether +2 enemy health is good news
 * depends on which side of the table you are on, so the panel colours the
 * direction of the change, never the player's fortune. */
const bySign = (n: number | undefined): ModifierPolarity =>
  n === undefined || n === 0 ? 'neutral' : n > 0 ? 'buff' : 'nerf'

type Shape = Omit<ModifierDisplay, 'tag'>

/* The modifiers a glyph and a number fully describe. Everything absent from
 * this table falls through to `inferred`, which groups it by name and shows the
 * localized sentence. */
function explicit(tag: string, type: Record<string, unknown>): Shape | null {
  /* `Int (Min Int)` arrives as a pair; the first element is the change, the
   * second only a floor, which the row does not draw. */
  const c = Array.isArray(type.contents) && type.contents.length === 2 && tag.endsWith('WithMin')
    ? type.contents[0]
    : type.contents

  switch (tag) {
    /* ---- skills ------------------------------------------------------- */
    case 'SkillModifier':
      return {
        group: 'skills',
        polarity: bySign(num(type.value)),
        glyph: skillGlyph(type.skillType),
        op: 'delta',
        value: num(type.value),
        needsLabel: false,
      }
    case 'ActionSkillModifier':
      return {
        group: 'skills',
        polarity: bySign(num(type.value)),
        glyph: skillGlyph(type.skillType),
        op: 'delta',
        value: num(type.value),
        qualifier: ACTION_GLYPH,
        detail: str(type.action),
        needsLabel: false,
      }
    case 'SetSkillValue':
      return {
        group: 'skills',
        polarity: 'neutral',
        glyph: skillGlyph(type.skillType),
        op: 'set',
        value: num(type.value),
        needsLabel: false,
      }
    case 'AnySkillValue':
      return {
        group: 'skills',
        polarity: bySign(num(c)),
        glyph: { kind: 'skill', skill: 'wild' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'BaseSkill':
      return {
        group: 'skills',
        polarity: 'neutral',
        glyph: { kind: 'skill', skill: 'wild' },
        op: 'set',
        value: num(c),
        needsLabel: false,
      }
    case 'BaseSkillOf':
      return {
        group: 'skills',
        polarity: 'neutral',
        glyph: skillGlyph(type.skillType),
        op: 'set',
        value: num(type.value),
        needsLabel: false,
      }
    case 'AddSkillValue':
      return {
        group: 'skills',
        polarity: 'buff',
        glyph: skillGlyph(c),
        op: 'none',
        needsLabel: true,
      }

    /* ---- printed stats ------------------------------------------------ */
    case 'HealthModifier':
    case 'HealthModifierWithMin':
      return {
        group: 'stats',
        polarity: bySign(num(c) ?? num(type.value)),
        glyph: { kind: 'stat', stat: 'health' },
        op: 'delta',
        value: num(c) ?? num(type.value),
        needsLabel: false,
      }
    case 'SanityModifier':
      return {
        group: 'stats',
        polarity: bySign(num(c) ?? num(type.value)),
        glyph: { kind: 'stat', stat: 'sanity' },
        op: 'delta',
        value: num(c) ?? num(type.value),
        needsLabel: false,
      }
    case 'EnemyFight':
    case 'EnemyFightWithMin':
      return {
        group: 'stats',
        polarity: bySign(num(c) ?? num(type.value)),
        glyph: { kind: 'stat', stat: 'fight' },
        op: 'delta',
        value: num(c) ?? num(type.value),
        needsLabel: false,
      }
    case 'EnemyEvade':
    case 'EnemyEvadeWithMin':
      return {
        group: 'stats',
        polarity: bySign(num(c) ?? num(type.value)),
        glyph: { kind: 'stat', stat: 'evade' },
        op: 'delta',
        value: num(c) ?? num(type.value),
        needsLabel: false,
      }
    case 'AsIfEnemyFight':
      return {
        group: 'stats',
        polarity: 'neutral',
        glyph: { kind: 'stat', stat: 'fight' },
        op: 'set',
        value: num(c),
        needsLabel: false,
      }
    case 'ShroudModifier':
      return {
        group: 'stats',
        polarity: bySign(num(c)),
        glyph: { kind: 'stat', stat: 'shroud' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'SetShroud':
    case 'BaseShroud':
      return {
        group: 'stats',
        polarity: 'neutral',
        glyph: { kind: 'stat', stat: 'shroud' },
        op: 'set',
        value: num(c),
        needsLabel: false,
      }

    /* ---- damage and horror dealt -------------------------------------- */
    case 'DamageDealt':
      return {
        group: 'combat',
        polarity: bySign(num(c)),
        glyph: { kind: 'token', token: 'damage' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'HorrorDealt':
      return {
        group: 'combat',
        polarity: bySign(num(c)),
        glyph: { kind: 'token', token: 'horror' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'GainVictory':
      return {
        group: 'cards',
        polarity: 'buff',
        glyph: { kind: 'shape', shape: 'victory' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }

    /* ---- the skill test band ------------------------------------------ */
    case 'Difficulty':
      return {
        group: 'combat',
        polarity: bySign(num(c)),
        glyph: { kind: 'shape', shape: 'test' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'DoubleDifficulty':
      return {
        group: 'combat',
        polarity: 'nerf',
        glyph: { kind: 'shape', shape: 'test' },
        op: 'scale',
        value: 2,
        needsLabel: false,
      }
    case 'SkillTestResultValueModifier':
      return {
        group: 'combat',
        polarity: bySign(num(c)),
        glyph: { kind: 'shape', shape: 'test' },
        op: 'delta',
        value: num(c),
        needsLabel: true,
      }
    case 'DiscoveredClues':
      return {
        group: 'economy',
        polarity: bySign(num(c)),
        glyph: { kind: 'token', token: 'clue' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }

    /* ---- economy ------------------------------------------------------ */
    case 'AdditionalTargets':
      return {
        group: 'combat',
        polarity: 'buff',
        glyph: { kind: 'shape', shape: 'bolt' },
        op: 'delta',
        value: num(c),
        needsLabel: true,
      }
    case 'AdditionalStartingUses':
      return {
        group: 'cards',
        polarity: 'buff',
        glyph: { kind: 'shape', shape: 'slot' },
        op: 'delta',
        value: num(c),
        needsLabel: true,
      }
    case 'AdditionalResources':
      return {
        group: 'economy',
        polarity: bySign(num(c)),
        glyph: { kind: 'token', token: 'resource' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'ActionCostModifier':
      return {
        group: 'economy',
        polarity: bySign(num(c)),
        glyph: { kind: 'arkham', icon: 'action' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'ActionCostSetToModifier':
      return {
        group: 'economy',
        polarity: 'neutral',
        glyph: { kind: 'arkham', icon: 'action' },
        op: 'set',
        value: num(c),
        needsLabel: false,
      }
    case 'AdditionalActions':
      return {
        group: 'actions',
        polarity: 'buff',
        glyph: { kind: 'arkham', icon: 'action' },
        op: 'delta',
        value: num(Array.isArray(c) ? c[2] : undefined),
        needsLabel: false,
      }
    case 'FewerActions':
      return {
        group: 'actions',
        polarity: 'nerf',
        glyph: { kind: 'arkham', icon: 'action' },
        op: 'delta',
        value: -(num(c) ?? 1),
        needsLabel: false,
      }
    case 'ActionsAreFree':
      return {
        group: 'actions',
        polarity: 'buff',
        glyph: { kind: 'arkham', icon: 'free' },
        op: 'none',
        needsLabel: false,
      }

    /* ---- hand and slots ----------------------------------------------- */
    case 'HandSize':
      return {
        group: 'cards',
        polarity: bySign(num(c)),
        glyph: { kind: 'shape', shape: 'hand' },
        op: 'delta',
        value: num(c),
        needsLabel: false,
      }
    case 'AdditionalSlot':
      return {
        group: 'cards',
        polarity: 'buff',
        glyph: { kind: 'shape', shape: 'slot' },
        op: 'delta',
        value: 1,
        detail: str(c),
        needsLabel: false,
      }
    case 'FewerSlots':
      return {
        group: 'cards',
        polarity: 'nerf',
        glyph: { kind: 'shape', shape: 'slot' },
        op: 'delta',
        value: -(num(Array.isArray(c) ? c[1] : undefined) ?? 1),
        detail: str(Array.isArray(c) ? c[0] : undefined),
        needsLabel: false,
      }

    /* ---- keywords and traits ------------------------------------------ */
    case 'AddKeyword':
      return {
        group: 'keywords',
        polarity: 'grant',
        glyph: { kind: 'shape', shape: 'keyword' },
        op: 'none',
        detail: str(c),
        needsLabel: false,
      }
    case 'RemoveKeyword':
      return {
        group: 'keywords',
        polarity: 'ban',
        glyph: { kind: 'shape', shape: 'keyword' },
        op: 'none',
        detail: str(c),
        needsLabel: false,
      }
    case 'AddTrait':
      return {
        group: 'keywords',
        polarity: 'grant',
        glyph: { kind: 'shape', shape: 'trait' },
        op: 'none',
        detail: str(c),
        needsLabel: false,
      }
    case 'RemoveTrait':
      return {
        group: 'keywords',
        polarity: 'ban',
        glyph: { kind: 'shape', shape: 'trait' },
        op: 'none',
        detail: str(c),
        needsLabel: false,
      }

    /* ---- blanket prohibitions ----------------------------------------- */
    case 'CannotBeDamaged':
      return {
        group: 'permissions',
        polarity: 'ban',
        glyph: { kind: 'token', token: 'damage' },
        op: 'none',
        needsLabel: false,
      }
    case 'Blank':
    case 'BlankExceptForcedAbilities':
      return {
        group: 'permissions',
        polarity: 'ban',
        glyph: { kind: 'shape', shape: 'card' },
        op: 'none',
        needsLabel: tag !== 'Blank',
      }

    default:
      return null
  }
}

/* Name-shape rules for the long tail. The engine names its constructors
 * consistently enough that the prefix says which bucket a modifier belongs in,
 * which beats hand-labelling 400 of them and then missing the 433rd. */
function inferred(tag: string): Shape {
  const has = (s: string) => tag.includes(s)
  const shape = (group: ModifierGroup, polarity: ModifierPolarity, sh: ShapeKey): Shape => ({
    group,
    polarity,
    glyph: { kind: 'shape', shape: sh },
    op: 'none',
    needsLabel: true,
  })

  /* Order matters: the first rule that matches wins, so the narrow senses
   * (a prohibition, an exemption, the chaos bag) come before the broad ones. */
  if (tag.startsWith('Cannot') || tag.startsWith('DoNot') || tag.startsWith('Prohibit')) {
    return shape('permissions', 'ban', 'ban')
  }
  /* "Ignore the Aloof keyword", "ignore the commit restriction": an exemption
   * from a rule, which is a permission granted, not a prohibition. */
  if (tag.startsWith('Ignore') || tag.startsWith('Can') || tag.startsWith('May')) {
    return shape('permissions', 'grant', 'allow')
  }
  if (tag === 'UIModifier') return shape('other', 'neutral', 'ui')
  if (has('ChaosToken') || has('ChaosBag') || has('RevealStrategy') || has('Bless') || has('Curse')) {
    return { group: 'chaos', polarity: 'neutral', glyph: { kind: 'arkham', icon: 'elder-thing' }, op: 'none', needsLabel: true }
  }
  if (has('Skill') || has('Willpower') || has('Intellect') || has('Combat') || has('Agility')) {
    return { group: 'skills', polarity: 'neutral', glyph: { kind: 'skill', skill: 'wild' }, op: 'none', needsLabel: true }
  }
  if (has('Damage') || has('Horror') || has('Attack') || has('Fight') || has('Evade') || has('Heal')) {
    return shape('combat', 'neutral', 'bolt')
  }
  if (has('Cost') || has('Resource') || has('Clue') || has('Doom')) {
    return shape('economy', 'neutral', 'cost')
  }
  if (has('Action') || has('Turn') || has('Phase')) {
    return { group: 'actions', polarity: 'neutral', glyph: { kind: 'arkham', icon: 'action' }, op: 'none', needsLabel: true }
  }
  if (has('Deck') || has('Discard') || has('Hand') || has('Card') || has('Draw') || has('Search') || has('Slot')) {
    return shape('cards', 'neutral', 'deck')
  }
  if (has('Keyword') || has('Trait')) return shape('keywords', 'neutral', 'keyword')
  /* Where things are and how they get there -- its own section rather than
   * the dumping ground, because a changed connection or spawn is a spatial
   * fact a player reads off the board. */
  if (
    has('Spawn')
    || has('Move')
    || has('Enter')
    || has('Location')
    || has('Connect')
    || has('Engage')
    || has('Place')
  ) {
    return shape('movement', 'neutral', 'move')
  }
  return shape('other', 'neutral', 'cog')
}

/* The constructor name a modifier should be looked up and grouped by. A nullary
 * constructor reaches the client through the catch-all in types/Modifier.ts, so
 * it arrives as OtherModifier carrying its real name. */
export function modifierTag(modifier: HasModifierType): string {
  const type = modifier.type as Record<string, unknown>
  if (type.tag === 'OtherModifier' && typeof type.contents === 'string') return type.contents
  if (type.tag === 'ScenarioModifier' && typeof type.contents === 'string') return type.contents
  return String(type.tag)
}

export function describeModifier(modifier: HasModifierType): ModifierDisplay {
  const type = modifier.type as Record<string, unknown>
  const tag = modifierTag(modifier)
  const shape = explicit(tag, type) ?? inferred(tag)

  /* A modifier shaped around a number whose number never arrived (its
   * constructor is not in the decoder tables) would draw a bare glyph and imply
   * a change of zero. Say it in words instead. */
  if (shape.op !== 'none' && shape.value === undefined) {
    return { tag, ...shape, op: 'none', needsLabel: true }
  }
  return { tag, ...shape }
}

/* `CannotLeaveYourHand` -> `Cannot leave your hand`. The last resort, for a
 * constructor the engine has added since the glossary was written and for a
 * ScenarioModifier, whose payload is a campaign's own key rather than a
 * constructor. Better than raw camel case, and never blank. */
export function humanizeTag(tag: string): string {
  const words = tag
    .replace(/([A-Z]+)(?=[A-Z][a-z])/g, '$1 ')
    .replace(/(?<=[a-z0-9])(?=[A-Z])/g, ' ')
    .replace(/(?<=[A-Za-z])(?=\d)/g, ' ')
    .trim()
  if (!words) return tag
  return words.charAt(0).toUpperCase() + words.slice(1).toLowerCase()
}

/* The localized name for a modifier's constructor. Extracted from the chip so
 * a test can run every one of the engine's constructors through it and prove
 * none comes out blank or as raw camel case. */
export function modifierLabel(
  tag: string,
  te: (key: string) => boolean,
  t: (key: string) => string,
): string {
  const key = `modifierTag.${tag}`
  return te(key) ? t(key) : humanizeTag(tag)
}

/* The art id for a modifier's source: the card it came in on if it has one,
 * otherwise whatever entity the source names. Same resolution Modifier.vue has
 * always used, kept here so every surface agrees. */
export function modifierSourceArt(modifier: Modifier, game: Game): string | null {
  if (modifier.card) return cardArt(toCardContents(modifier.card).cardCode)
  return sourceCardCode(modifier.source, game)
}

export const MODIFIER_GROUP_ORDER: ModifierGroup[] = [
  'stats',
  'skills',
  'combat',
  'chaos',
  'actions',
  'economy',
  'cards',
  'movement',
  'keywords',
  'permissions',
  'other',
]

export interface ModifierRow {
  display: ModifierDisplay
  /* The art id of the card that published this modifier, for the thumbnail and
   * for magnifying the source. */
  art: string | null
}

export interface ModifierSection {
  group: ModifierGroup
  rows: ModifierRow[]
}

/* What a card hands to the global magnifier.
 *
 * CardOverlay is mounted once per view, outside any game component, and there
 * is no game store to read -- so a card publishes its own modifiers on a data
 * attribute. The full `Modifier` carries an entire card object; only the
 * resolved art id is wanted, so that is all that travels. */
export interface WireModifier {
  type: ModifierType
  art: string | null
}

export function toWireModifiers(modifiers: Modifier[], game: Game): WireModifier[] {
  return modifiers.map((modifier) => ({
    type: modifier.type,
    art: modifierSourceArt(modifier, game),
  }))
}

export function modifierRows(modifiers: Modifier[], game: Game): ModifierRow[] {
  return modifiers.map((modifier) => ({
    display: describeModifier(modifier),
    art: modifierSourceArt(modifier, game),
  }))
}

export function wireModifierRows(wire: WireModifier[]): ModifierRow[] {
  return wire.map((w) => ({ display: describeModifier(w), art: w.art ?? null }))
}

/* Rows grouped for display, in a fixed order so the panel does not reshuffle as
 * modifiers come and go. Stats first: they are the ones that make a card's
 * printed numbers wrong, which is what sends anyone looking. */
export function groupModifierRows(rows: ModifierRow[]): ModifierSection[] {
  return MODIFIER_GROUP_ORDER.flatMap((group) => {
    const inGroup = rows.filter((r) => r.display.group === group)
    return inGroup.length ? [{ group, rows: inGroup }] : []
  })
}

/* The first `limit` rows of a grouped list, and how many were left out.
 *
 * For a panel that cannot scroll. The magnifier's copy is inside an overlay
 * that is `pointer-events: none`, so its scroll container could never be
 * reached: a long list had its tail silently unreachable, and made the overlay
 * taller than the window, which grows the document and lets the board itself
 * be scrolled away. A preview cuts the list instead and says how much it cut.
 * The pip's panel can be scrolled, so it passes no limit and keeps all of it.
 *
 * Rows are taken in the order the groups already put them, so what survives is
 * the most important end of the list rather than an arbitrary slice.
 */
export function limitModifierSections(
  sections: ModifierSection[],
  limit?: number,
): { sections: ModifierSection[]; hidden: number } {
  const total = sections.reduce((n, s) => n + s.rows.length, 0)
  // No limit at all, rather than an empty list, when nothing was asked for.
  if (limit === undefined || limit <= 0 || total <= limit) return { sections, hidden: 0 }

  const kept: ModifierSection[] = []
  let room = limit
  for (const section of sections) {
    if (room <= 0) break
    kept.push({ group: section.group, rows: section.rows.slice(0, room) })
    room -= section.rows.length
  }
  return { sections: kept, hidden: total - limit }
}

/* The polarity a single pip should wear when it stands for the whole list: the
 * most urgent thing in it. */
export function dominantPolarity(rows: ModifierRow[]): ModifierPolarity {
  const order: ModifierPolarity[] = ['ban', 'nerf', 'buff', 'grant', 'neutral']
  for (const p of order) if (rows.some((r) => r.display.polarity === p)) return p
  return 'neutral'
}

/* One printed stat and what the engine will actually use for it.
 *
 * The pair is the point: "+2" tells you something changed, "1 -> 3" tells you
 * what to do about it. Both numbers come from the server already resolved
 * (EnemyMetadata), because the printed value can be per-player and the
 * modified one carries clamps and overrides the client must not re-derive. */
export interface StatReading {
  /* Stable across renders, and the `modifierTag.*` key for the accessible
   * name, so the line needs no vocabulary of its own. */
  key: string
  labelKey: string
  glyph: ModifierGlyph
  printed: number
  modified: number
}

const reading = (
  key: string,
  labelKey: string,
  glyph: ModifierGlyph,
  printed: number | null | undefined,
  modified: number | null | undefined,
): StatReading[] =>
  typeof printed === 'number' && typeof modified === 'number' && printed !== modified
    ? [{ key, labelKey, glyph, printed, modified }]
    : []

/* An enemy's three printed stats. `unvalued` carries what the server cannot
 * know: a dash or an X resolves to 0 there, and only the calculation tag the
 * client still receives says it was never a number. */
export function enemyStatReadings(
  source: Partial<Record<`printed${Capitalize<StatKey>}` | `modified${Capitalize<StatKey>}`, number | null>>,
  unvalued: Partial<Record<StatKey, boolean>> = {},
): StatReading[] {
  return [
    ...(unvalued.fight ? [] : reading('fight', 'EnemyFight', { kind: 'stat', stat: 'fight' }, source.printedFight, source.modifiedFight)),
    ...(unvalued.health ? [] : reading('health', 'HealthModifier', { kind: 'stat', stat: 'health' }, source.printedHealth, source.modifiedHealth)),
    ...(unvalued.evade ? [] : reading('evade', 'EnemyEvade', { kind: 'stat', stat: 'evade' }, source.printedEvade, source.modifiedEvade)),
  ]
}

/* An investigator's four skills, printed beside what a test would use. A null
 * modified value means the server never sent one, and the skill is left off
 * the line rather than reported as a change to nothing. */
export function investigatorSkillReadings(source: {
  willpower: number
  intellect: number
  combat: number
  agility: number
  modifiedWillpower: number | null
  modifiedIntellect: number | null
  modifiedCombat: number | null
  modifiedAgility: number | null
}): StatReading[] {
  return (['willpower', 'intellect', 'combat', 'agility'] as const).flatMap((skill) => {
    const key = (skill.charAt(0).toUpperCase() + skill.slice(1)) as Capitalize<typeof skill>
    return reading(skill, 'SkillModifier', { kind: 'skill', skill }, source[skill], source[`modified${key}`])
  })
}

/* An asset's total health and sanity -- what it can take before it leaves
 * play. An ally's health is raised by The Star XVII (3) and by customizations,
 * and the board shows only the damage on it. */
export function assetStatReadings(source: {
  printedHealth?: number | null
  modifiedHealth?: number | null
  printedSanity?: number | null
  modifiedSanity?: number | null
}): StatReading[] {
  return [
    ...reading('health', 'HealthModifier', { kind: 'stat', stat: 'health' }, source.printedHealth, source.modifiedHealth),
    ...reading('sanity', 'SanityModifier', { kind: 'stat', stat: 'sanity' }, source.printedSanity, source.modifiedSanity),
  ]
}

/* The doom an agenda advances at. The Ultimatums and Boons move it, and the
 * board has never shown the number a player is being measured against. */
export function agendaThresholdReading(source: {
  printedDoomThreshold?: number | null
  modifiedDoomThreshold?: number | null
}): StatReading[] {
  return reading(
    'doomThreshold',
    'DoomThresholdModifier',
    { kind: 'token', token: 'doom' },
    source.printedDoomThreshold,
    source.modifiedDoomThreshold,
  )
}

/* A location's shroud. Both halves are absent together when the location is
 * unrevealed or prints no shroud at all. */
export function locationShroudReading(source: {
  printedShroud?: number | null
  modifiedShroud?: number | null
}): StatReading[] {
  return reading('shroud', 'ShroudModifier', { kind: 'stat', stat: 'shroud' }, source.printedShroud, source.modifiedShroud)
}

/* Did one of the card's printed numbers actually move?
 *
 * This decides how loud the on-card pip is: a card whose fight or health
 * really changed must stand out from one that merely behaves differently,
 * or a scenario-wide modifier puts an identical pip on every card in play.
 *
 * Asked of the resolved readings rather than of the modifier list, because
 * only the readings know the outcome -- two modifiers can cancel, and a
 * clamp can swallow one entirely. */
export const hasAlteredStat = (readings: StatReading[]) =>
  readings.some((r) => r.printed !== r.modified)

/* Engine bookkeeping with no meaning at the table. `UIModifier` is already on
 * screen as the thing it draws, and `Semaphore` is the mutex that stops an
 * ability reacting to itself -- a player seeing "Resolving" on a card would
 * learn nothing from it. */
const NOT_PRESENTABLE: ReadonlySet<string> = new Set(['UIModifier', 'Semaphore'])

/* The card's own printed text, restated.
 *
 * `printed` means the card gave this to ITSELF unconditionally, so a row for
 * it says nothing the card does not -- worst in the magnifier, where the card
 * text is sitting right beside the panel repeating it.
 *
 * It is dropped only when it carries no number. A computed value IS news: a
 * location printing "shroud +1 per Witch here" tells you the rule, not that
 * the shroud is currently 7. Those come through `modifySelf` too, so the
 * numeral is what separates "Cannot leave play" from "+3 shroud".
 *
 * A CONDITIONAL self-modifier is never `printed`, because its presence is
 * itself the news -- it says the condition is currently met. */
const isPrintedRestatement = (m: Modifier) =>
  m.printed === true && describeModifier(m).value === undefined

/* Bookkeeping that makes a composite entity and the cards it stands for act as
 * one. Cthulhu's three facets get "cannot attack during the enemy phase"
 * because CTHULHU does the attacking, and Cthulhu gets "cannot be attacked"
 * because you attack a facet. Neither is news about the card it sits on -- it
 * is the shape of the encounter, which is sitting on the board in front of
 * you, and restating it four times per facet buries whatever is.
 *
 * Dropped whether or not it carries a number: unlike a printed restatement,
 * there is no version of this that tells the player something. */
const isCompositeBookkeeping = (m: Modifier) => m.subsumed === true

export function presentableModifiers(modifiers: Modifier[] | undefined): Modifier[] {
  return (modifiers ?? []).filter(
    (m) =>
      !NOT_PRESENTABLE.has(modifierTag(m)) &&
      !isPrintedRestatement(m) &&
      !isCompositeBookkeeping(m),
  )
}

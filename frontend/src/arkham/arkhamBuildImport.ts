// Bridges arkham.build's ("Arkham Card Maker") own JSON export format into
// this app's custom-card model, and rewrites the same card codes wherever
// they show up in a deck, so a deck built on arkham.build against these same
// cards resolves against the copies imported here rather than going missing.
//
// Deliberately narrow: only the fields a card needs to look right and stat
// out correctly (name, type, class, cost, stats, traits, art, ...). Ability
// text/effects are out of scope -- this app has no free-text ability field at
// all, abilities are authored structurally through the builder's own
// AbilityEditor/StepsEditor -- so an imported card has no functional effect
// until someone builds that by hand.
import { arkhamBuildCustomCardCode, type CustomCard } from '@/arkham/customCards'

const FACTION_TO_CLASS: Record<string, string> = {
  guardian: 'Guardian',
  seeker: 'Seeker',
  rogue: 'Rogue',
  mystic: 'Mystic',
  survivor: 'Survivor',
  neutral: 'Neutral',
}

const SLOT_NAME_MAP: Record<string, string> = {
  hand: 'HandSlot',
  ally: 'AllySlot',
  body: 'BodySlot',
  accessory: 'AccessorySlot',
  arcane: 'ArcaneSlot',
  tarot: 'TarotSlot',
  head: 'HeadSlot',
}

const isWeaknessSubtype = (raw: any) =>
  raw.subtype_code === 'weakness' || raw.subtype_code === 'basicweakness'

/* Mirrors the reasoning `ALWAYS_WEAKNESS`/`isWeakness` already encode in
 * CustomCardForm.vue: a treachery or enemy is only ever a *player* card when
 * it's a weakness -- otherwise it's encounter-side. */
function mapCardType(raw: any): string {
  const weak = isWeaknessSubtype(raw)
  switch (raw.type_code) {
    case 'investigator':
      return 'InvestigatorType'
    case 'asset':
      return raw.faction_code === 'mythos' ? 'EncounterAssetType' : 'AssetType'
    case 'event':
      return 'EventType'
    case 'skill':
      return 'SkillType'
    case 'treachery':
      return weak ? 'PlayerTreacheryType' : 'TreacheryType'
    case 'enemy':
      return weak ? 'PlayerEnemyType' : 'EnemyType'
    default:
      return 'AssetType'
  }
}

function mapSlots(raw: any): string[] {
  if (!raw.slot) return []
  const found = new Set<string>()
  for (const piece of String(raw.slot).split(/[.,]| and /i)) {
    const key = SLOT_NAME_MAP[piece.trim().toLowerCase()]
    if (key) found.add(key)
  }
  return [...found]
}

/* On every card type except an investigator, `skill_*` is an icon *count* --
 * how many pips are printed -- not a stat. */
function mapIcons(raw: any): any[] {
  const icons: any[] = []
  const push = (n: unknown, tag: string) => {
    const count = typeof n === 'number' ? n : 0
    for (let i = 0; i < count; i++) {
      icons.push(tag === 'Wild' ? { tag: 'WildIcon', contents: [] } : { tag: 'SkillIcon', contents: tag })
    }
  }
  push(raw.skill_willpower, 'SkillWillpower')
  push(raw.skill_intellect, 'SkillIntellect')
  push(raw.skill_combat, 'SkillCombat')
  push(raw.skill_agility, 'SkillAgility')
  push(raw.skill_wild, 'Wild')
  return icons
}

const gameValue = (n: unknown) => (typeof n === 'number' ? { tag: 'Static', contents: n } : null)

const setIf = (obj: Record<string, any>, key: string, value: unknown) => {
  if (value !== null && value !== undefined) obj[key] = value
}

/* Accepts a full arkham.build card-pool export ({meta, data:{cards:[...]}}),
 * a bare {cards:[...]}, or a single card object on its own.
 *
 * `packCode` is the pack's own id, kept so that importing a corrected version of
 * the same pack later replaces the set it made rather than making a second one
 * beside it -- which a rename of the pack would otherwise cause. */
export function parseArkhamBuildCards(raw: string): {
  packName: string | null
  packCode: string | null
  cards: any[]
} {
  const parsed = JSON.parse(raw)
  const cards: any[] = Array.isArray(parsed?.data?.cards)
    ? parsed.data.cards
    : Array.isArray(parsed?.cards)
      ? parsed.cards
      : parsed?.name && parsed?.type_code
        ? [parsed]
        : []
  return {
    packName: parsed?.meta?.name ?? null,
    packCode: parsed?.meta?.code ?? null,
    cards,
  }
}

export function arkhamBuildCardToCustomCard(raw: any, packName: string | null): CustomCard {
  const cardCode = arkhamBuildCustomCardCode(raw.code)
  const cardType = mapCardType(raw)
  const isWeakness = isWeaknessSubtype(raw)
  const isEnemy = cardType === 'EnemyType' || cardType === 'PlayerEnemyType'
  const isAsset = cardType === 'AssetType' || cardType === 'EncounterAssetType'
  const isTreachery = cardType === 'TreacheryType' || cardType === 'PlayerTreacheryType'
  const isInvestigator = cardType === 'InvestigatorType'
  const isPlayerCard = ['AssetType', 'EventType', 'SkillType', 'PlayerTreacheryType', 'PlayerEnemyType'].includes(
    cardType,
  )
  const hasSkillIcons = !isInvestigator && !isEnemy && !isTreachery
  const hasClass = (isPlayerCard || isInvestigator) && !isWeakness
  const hasCost = ['AssetType', 'EventType'].includes(cardType) && !isWeakness
  const hasLevel = isPlayerCard && !isEnemy && !isTreachery && !isWeakness

  const def: Record<string, any> = {
    cardCode,
    art: cardCode,
    cardType,
    name: {
      title: (raw.name || '').trim() || 'Custom Card',
      subtitle: raw.subname?.trim() || null,
    },
    cardTraits: String(raw.traits || '')
      .split('.')
      .map((t: string) => t.trim())
      .filter(Boolean),
    skills: hasSkillIcons ? mapIcons(raw) : [],
    keywords: [],
    unique: !!raw.is_unique,
    permanent: !!raw.permanent,
    doubleSided: false,
    meta: { arkhamBuildCode: raw.code } as Record<string, any>,
  }

  if (hasClass) def.classSymbols = [FACTION_TO_CLASS[raw.faction_code] ?? 'Neutral']
  if (isWeakness) def.cardSubType = raw.subtype_code === 'basicweakness' ? 'BasicWeakness' : 'Weakness'

  if (hasCost) setIf(def, 'cost', typeof raw.cost === 'number' ? { tag: 'StaticCost', contents: raw.cost } : null)
  if (hasLevel) setIf(def, 'level', typeof raw.xp === 'number' ? raw.xp : null)

  if (isEnemy) {
    setIf(def, 'fight', gameValue(raw.enemy_fight))
    setIf(def, 'health', gameValue(raw.health))
    setIf(def, 'evade', gameValue(raw.enemy_evade))
    setIf(def, 'healthDamage', gameValue(raw.enemy_damage))
    setIf(def, 'sanityDamage', gameValue(raw.enemy_horror))
  }

  if (isAsset) {
    const slots = mapSlots(raw)
    if (slots.length) def.slots = slots
    setIf(def.meta, 'health', typeof raw.health === 'number' ? raw.health : null)
    setIf(def.meta, 'sanity', typeof raw.sanity === 'number' ? raw.sanity : null)
  }

  if (raw.position != null) def.meta.number = String(raw.position)
  if (packName) def.meta.set = packName

  if (isInvestigator) {
    def.meta.health = typeof raw.health === 'number' ? raw.health : 0
    def.meta.sanity = typeof raw.sanity === 'number' ? raw.sanity : 0
    def.meta.willpower = typeof raw.skill_willpower === 'number' ? raw.skill_willpower : 0
    def.meta.intellect = typeof raw.skill_intellect === 'number' ? raw.skill_intellect : 0
    def.meta.combat = typeof raw.skill_combat === 'number' ? raw.skill_combat : 0
    def.meta.agility = typeof raw.skill_agility === 'number' ? raw.skill_agility : 0
    if (raw.back_image_url) def.meta.backArt = raw.back_image_url
  }

  return { def: def as any, art: raw.image_url || null }
}

// ---------------------------------------------------------- deck codes ---

const UUID_RE = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i

const translateCode = (code: string): string => (UUID_RE.test(code) ? arkhamBuildCustomCardCode(code) : code)

const translateSlots = (slots?: Record<string, number>): Record<string, number> | undefined =>
  slots
    ? Object.fromEntries(Object.entries(slots).map(([code, qty]) => [translateCode(code), qty]))
    : slots

/* A deck exported from arkham.build names cards by the same bare UUIDs its
 * card-pool export uses. Nothing in this app's deck-lookup or backend
 * validation ever tries a custom-card match unless the code already starts
 * with `*` (isCustomCardCode), so a deck built against arkham.build custom
 * cards needs those UUIDs rewritten to this app's derived codes before it's
 * validated or created -- otherwise it is silently dropped from view (or
 * rejected outright as UnimplementedCard) even after the matching cards have
 * been imported. A decklist built entirely from official cards is untouched:
 * ArkhamDB codes never match UUID_RE. */
export function normalizeArkhamBuildDeckCodes<T extends Record<string, any>>(deck: T): T {
  const out: any = { ...deck }
  if (out.slots) out.slots = translateSlots(out.slots)
  if (out.sideSlots) out.sideSlots = translateSlots(out.sideSlots)
  if (typeof out.investigator_code === 'string') out.investigator_code = translateCode(out.investigator_code)

  const meta = typeof out.meta === 'string'
    ? (() => {
        try {
          return JSON.parse(out.meta)
        } catch {
          return null
        }
      })()
    : out.meta
  if (meta && typeof meta.alternate_front === 'string') {
    const patched = { ...meta, alternate_front: translateCode(meta.alternate_front) }
    out.meta = typeof out.meta === 'string' ? JSON.stringify(patched) : patched
  }

  return out
}

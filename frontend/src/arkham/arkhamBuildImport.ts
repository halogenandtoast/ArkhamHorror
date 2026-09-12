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
import {
  arkhamBuildCustomCardCode,
  bareCardCode,
  isArkhamBuildCardId,
  type CustomCard,
} from '@/arkham/customCards'

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

// --------------------------------------------------------- signatures ---

/* Looks like a card id rather than a restriction keyword. arkham.build ids are
 * UUIDs or bare hex; ArkhamDB codes are five or six digits. */
const looksLikeCardId = (piece: string) =>
  /^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(piece) ||
  /^[0-9a-f]{8}$/i.test(piece) ||
  /^[0-9a-f]{32}$/i.test(piece) ||
  /^\d{5,6}$/.test(piece)

/* A signature is recorded on the card, naming its investigator. arkham.build
 * writes that as a string -- `"restrictions": "investigator:<card id>"` -- and
 * ArkhamDB as an object, so all the shapes are read here. A card can name more
 * than one owner: the parallel versions of an investigator share signatures.
 *
 * Restrictions that are not about an investigator (`trait:`, `faction:`) are
 * dropped rather than guessed at. */
export function restrictedInvestigatorIds(restrictions: any): string[] {
  if (!restrictions) return []

  if (typeof restrictions === 'string') {
    return restrictions
      .split(/[,;]/)
      .map((piece) => piece.trim())
      .filter(Boolean)
      .flatMap((piece) => {
        const at = piece.indexOf(':')
        if (at === -1) return looksLikeCardId(piece) ? [piece] : []
        const key = piece.slice(0, at).trim().toLowerCase()
        const value = piece.slice(at + 1).trim()
        return key === 'investigator' && value ? [value] : []
      })
  }

  const investigator = restrictions.investigator
  if (!investigator) return []
  if (typeof investigator === 'string') return [investigator]
  if (Array.isArray(investigator)) return investigator.filter((x: any) => typeof x === 'string')
  // ArkhamDB's shape is a code -> code map.
  if (typeof investigator === 'object') return Object.keys(investigator)
  return []
}

/* arkham.build records the link one way only: on the signature, pointing at its
 * investigator. This app needs it the other way -- the investigator's
 * `_signatures` is what brings those cards along when a deck picks them, and
 * what binds `$investigator` for the signature's own abilities -- so the
 * pointers are followed back and written onto the investigators here.
 *
 * Mutates in place, like `attachInvestigatorPortraits`: these are the same card
 * objects the import is about to commit. Read the result back with
 * `summarizeSignatures`, which every import path uses whether or not the file
 * came from arkham.build. */
export function linkSignatures(cards: CustomCard[]): void {
  const investigators = new Map<string, CustomCard>()
  for (const card of cards) {
    if ((card.def as any).cardType === 'InvestigatorType') {
      investigators.set(bareCardCode(card.def.cardCode), card)
    }
  }

  const owned = new Map<string, string[]>()

  for (const card of cards) {
    const def = card.def as any
    if (def.cardType === 'InvestigatorType') continue

    const owners: string[] = (def.deckRestrictions ?? [])
      .filter((r: any) => r?.tag === 'Signature' && typeof r.contents === 'string')
      .map((r: any) => bareCardCode(r.contents))

    for (const code of owners.filter((c) => investigators.has(c))) {
      owned.set(code, [...(owned.get(code) ?? []), bareCardCode(def.cardCode)])
    }
  }

  for (const [code, signatures] of owned) {
    const def = investigators.get(code)!.def as any
    def.meta = def.meta ?? {}
    /* Replaced, not merged: the pack is the whole set, so it is the whole list
     * too, and re-importing a corrected pack has to be able to take one away. */
    def.meta._signatures = signatures
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

  /* A signature's own half of the link. `linkSignatures` writes the other half
   * onto the investigator; this half is what takes the card back out of a deck
   * when the investigator is swapped away from. */
  const owners = restrictedInvestigatorIds(raw.restrictions).map(translateCode)
  if (owners.length) {
    def.deckRestrictions = owners.map((contents) => ({ tag: 'Signature', contents }))
  }

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

// ----------------------------------------------------------- portraits ---

/* arkham.build's export has a card face and a card back and nothing else. An
 * investigator also needs a mini -- the portrait the game shows for that
 * investigator wherever there is no room for the card -- and there is no such
 * image in the export to map, so an imported investigator arrived with an empty
 * portrait and its whole landscape card squashed into a portrait-shaped hole
 * wherever one was asked for.
 *
 * The art is on the card, though. An investigator card is laid out the same way
 * every time: the illustration fills the left of the face, below the name plate,
 * top to bottom -- which is the shape a portrait wants. So the mini is cut out
 * of the face rather than left missing. It is an approximation: the official
 * minis are cropped from the original illustration and show more of it than the
 * card ever does. It is a recognisable one.
 *
 * The back mini is the same crop desaturated, which is what the official back
 * minis are -- the card's own back is the deckbuilding text and has no art on it
 * at all.
 *
 * Fractions rather than pixels: faces arrive at whatever size the pack was
 * rendered at (600x423 and 1050x750 both turn up in the official data alone).
 * The width is taken from the height so the result is 2:3 whatever aspect the
 * face itself has. */
const PORTRAIT_CROP = { left: 0.03, top: 0.15, height: 0.83, aspect: 2 / 3 }
const PORTRAIT_MAX_HEIGHT = 450

function desaturate(ctx: CanvasRenderingContext2D, width: number, height: number) {
  const image = ctx.getImageData(0, 0, width, height)
  const px = image.data
  for (let i = 0; i < px.length; i += 4) {
    const grey = 0.299 * px[i] + 0.587 * px[i + 1] + 0.114 * px[i + 2]
    px[i] = Math.min(255, grey * 1.07)
    px[i + 1] = grey * 0.97
    px[i + 2] = grey * 0.8
  }
  ctx.putImageData(image, 0, 0)
}

/* Cuts the portrait out of a card face. Null whenever it cannot be done, which
 * leaves the card exactly as it was: the image host may refuse the request, and
 * this runs in a browser or not at all. */
async function cropPortrait(url: string, grey = false): Promise<string | null> {
  if (typeof document === 'undefined') return null
  try {
    /* Fetched as bytes and decoded from the blob rather than pointed at with an
     * <img src>: a cross-origin image drawn onto a canvas taints it, and
     * `toDataURL` then throws rather than returning anything. Bytes carry no
     * origin, so the canvas stays clean -- and a host that will not serve the
     * fetch at all simply leaves the portrait unset. */
    const response = await fetch(url, { mode: 'cors' })
    if (!response.ok) return null
    const bitmap = await createImageBitmap(await response.blob())

    const sh = Math.round(bitmap.height * PORTRAIT_CROP.height)
    const sw = Math.round(sh * PORTRAIT_CROP.aspect)
    // Clamped so a face narrower than the crop yields a smaller portrait rather
    // than one padded out with transparent pixels.
    const sx = Math.min(Math.round(bitmap.width * PORTRAIT_CROP.left), Math.max(0, bitmap.width - sw))
    const sy = Math.round(bitmap.height * PORTRAIT_CROP.top)

    const scale = Math.min(1, PORTRAIT_MAX_HEIGHT / sh)
    const canvas = document.createElement('canvas')
    canvas.width = Math.max(1, Math.round(sw * scale))
    canvas.height = Math.max(1, Math.round(sh * scale))
    const ctx = canvas.getContext('2d')
    if (!ctx) return null

    ctx.drawImage(bitmap, sx, sy, sw, sh, 0, 0, canvas.width, canvas.height)
    bitmap.close()
    if (grey) desaturate(ctx, canvas.width, canvas.height)
    return canvas.toDataURL('image/jpeg', 0.85)
  } catch {
    return null
  }
}

/* Gives every imported investigator the minis the export could not. Best effort
 * and in place: an investigator whose face cannot be read is left with none,
 * which is where it started.
 *
 * Returns how many were given one, so the import can say so -- a portrait cut
 * from the card is a guess at the author's intent, not something to do silently.
 */
export async function attachInvestigatorPortraits(cards: CustomCard[]): Promise<number> {
  const investigators = cards.filter((c) => {
    const def = c.def as any
    return def.cardType === 'InvestigatorType' && !def.meta?.portrait && !!c.art
  })

  const done = await Promise.all(
    investigators.map(async (card) => {
      const def = card.def as any
      const [front, back] = await Promise.all([
        cropPortrait(card.art!),
        cropPortrait(card.art!, true),
      ])
      if (front) def.meta.portrait = front
      if (back) def.meta.portraitBack = back
      return front ? 1 : 0
    }),
  )

  return done.reduce((a: number, b: number) => a + b, 0)
}

// ---------------------------------------------------------- deck codes ---

/* Which ids get rewritten is `isArkhamBuildCardId`, which sits beside the
 * derivation itself so the two cannot drift apart. */
const translateCode = (code: string): string =>
  isArkhamBuildCardId(code) ? arkhamBuildCustomCardCode(code) : code

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
 * ArkhamDB codes are never arkham.build ids. */
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

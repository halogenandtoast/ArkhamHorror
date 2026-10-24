// Debug-authored cards: defs invented at runtime from the custom card editor.
//
// The backend stores them on the game and serves them from
// `games/:id/custom-cards` rather than folding them into the game payload,
// since a card's art can be an inlined data URI. This module is the client-side
// registry those defs land in; it deliberately imports nothing from
// `helpers.ts`, which reads from it.
import * as JsonDecoder from 'ts.data.json'
import { reactive } from 'vue'
import { cardDefDecoder, type CardDef } from '@/arkham/types/CardDef'
import { publishCardText } from '@/arkham/customText'

export type CustomCard = {
  def: CardDef
  art: string | null
}

export const customCardDecoder = JsonDecoder.object<CustomCard>(
  {
    def: cardDefDecoder,
    art: JsonDecoder.nullable(JsonDecoder.string()),
  },
  'CustomCard',
)

/* Card types that go in a deck or a hand. Only these can be earned for a
 * campaign or added to a hand; the rest are encounter-side. */
export const PLAYER_CARD_TYPES = [
  'AssetType',
  'EventType',
  'SkillType',
  'PlayerTreacheryType',
  'PlayerEnemyType',
]

export const CUSTOM_CARD_PREFIX = '*'

/* `ToJSON CardCode` prepends a 'c' so codes never serialize as bare numbers, but
 * `cdArt` is plain text and does not get one. A custom card is looked up by both
 * -- by code when it comes off the wire, by art when it is about to be drawn --
 * so every code is normalised to the bare form first. */
export function stripCardCodePrefix(code: string): string {
  return code.replace(/^c(?=\*)/, '')
}

/* `GET /arkham/card/:code` answers out of the engine's own card maps. A custom
 * card's def lives in the database and a homebrew one inside its box, so asking
 * for either by code is a guaranteed 404 -- don't ask. */
export function hasEngineCardDef(code: string): boolean {
  const bare = code.replace(/^c(?=[*:])/, '')
  return !bare.startsWith(CUSTOM_CARD_PREFIX) && !bare.startsWith(':')
}

/* The engine's own convention, mirrored from `FromJSON CardCode`: a code on the
 * wire carries a leading `c` that is not part of the code. Both sides of a
 * comparison have to go through this, because a code minted locally is bare and
 * one that came back from the server is prefixed. */
export const bareCardCode = (code: string) => code.replace(/^c+/, '')

export type SignatureSummary = {
  /* One entry per investigator that has signatures, for display. */
  owners: { name: string; count: number }[]
  /* Distinct signature cards spoken for by an investigator in this same group. */
  linked: number
  /* Cards restricted to an investigator that is not in this group. The
   * restriction still stands -- only they can take it -- but nothing here
   * brings it along, because the investigator that would is somewhere else. */
  orphans: number
}

/* Who brings what. Read off the investigators' `_signatures` -- the direction
 * this app treats as the link, and the one a deck follows to pick the cards up
 * -- and off the signatures' own `deckRestrictions` for the ones whose
 * investigator is not here to do it.
 *
 * Read-only: `linkSignatures` is what writes `_signatures` for an arkham.build
 * import, which records the link the other way round. */
export function summarizeSignatures(cards: CustomCard[]): SignatureSummary {
  const present = new Set(cards.map((c) => bareCardCode(c.def.cardCode)))
  const linked = new Set<string>()
  const owners: { name: string; count: number }[] = []

  for (const card of cards) {
    const def = card.def as any
    if (def.cardType !== 'InvestigatorType') continue
    const mine = ((def.meta?._signatures ?? []) as string[])
      .map(bareCardCode)
      .filter((code) => present.has(code))
    if (!mine.length) continue
    for (const code of mine) linked.add(code)
    owners.push({ name: def.name.title, count: mine.length })
  }

  const orphans = cards.filter((card) => {
    const def = card.def as any
    if (def.cardType === 'InvestigatorType') return false
    if (linked.has(bareCardCode(def.cardCode))) return false
    return ((def.deckRestrictions ?? []) as any[]).some(
      (r) => r?.tag === 'Signature' && typeof r.contents === 'string',
    )
  }).length

  owners.sort((a, b) => b.count - a.count || a.name.localeCompare(b.name))
  return { owners, linked: linked.size, orphans }
}

export function isCustomCardCode(code: string): boolean {
  return stripCardCodePrefix(code).startsWith(CUSTOM_CARD_PREFIX)
}

// Mint a code the engine's CardCode Eq is safe with: a trailing a/b/c/d there
// designates a card side, so two custom codes ending in complementary letters
// would compare equal. Always end in a digit.
export function mintCustomCardCode(): string {
  return `${CUSTOM_CARD_PREFIX}${crypto.randomUUID().replace(/-/g, '')}0`
}

/* The ids arkham.build names a custom card by, and not always a dashed UUID: a
 * pack's cards come through with a bare 32-hex or short 8-hex id instead.
 *
 * Lives here, beside the derivation it gates, because the two have to be the
 * same question asked once. The importer derives a code from whatever id it is
 * handed; the deck side rewrites a code only when it recognises the id as one
 * of these. If they ever disagree, a card imports under a code no deck will
 * ever name, and the deck fails validation as UnimplementedCard with the card
 * sitting right there in the library -- which is exactly what happened while
 * the deck side gated on the dashed form alone.
 *
 * ArkhamDB codes are five or six digits and match none of these. */
const ARKHAM_BUILD_ID_RE =
  /^(?:[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}|[0-9a-f]{32}|[0-9a-f]{8})$/i

export function isArkhamBuildCardId(id: string): boolean {
  return ARKHAM_BUILD_ID_RE.test(id)
}

/* Same shape as `mintCustomCardCode`, but derived from an arkham.build card's
 * own id rather than a fresh random one. Importing a card and later reading a
 * deck that names it by that id need to land on the same code, so this has to
 * be deterministic -- a random mint would never match up with itself. */
export function arkhamBuildCustomCardCode(uuid: string): string {
  return `${CUSTOM_CARD_PREFIX}${uuid.replace(/-/g, '').toLowerCase()}0`
}

/* arkham.build's "Investigator Placeholders" pack: Johnny Anybody and Jane Doe,
 * blank investigators the site ships so a deck can be built without one. A deck
 * built on one has no investigator of its own and plays under whichever one is
 * laid over it -- which is how a deck for an investigator built here gets built
 * on a site that cannot know them.
 *
 * Mirrors `arkhamBuildPlaceholderPack` / `arkhamBuildPlaceholderCodes` in
 * `Arkham.Card.CustomCard`; the server refuses to seat a deck still naming one,
 * so both sides have to recognise the same cards. */
export const ARKHAM_BUILD_PLACEHOLDER_PACK = 'd12a4d9c-8c65-4df0-be0c-c2e14af65a21'

const ARKHAM_BUILD_PLACEHOLDER_CODES = new Set(
  [
    '347ff5d0-8521-4d9b-a0fe-90c06114057d', // Johnny Anybody
    'c13c4114-0769-410c-864b-e43f5a596c0d', // Jane Doe
  ].map(arkhamBuildCustomCardCode),
)

export function isArkhamBuildPlaceholder(code: string): boolean {
  return ARKHAM_BUILD_PLACEHOLDER_CODES.has(stripCardCodePrefix(code))
}

/* The meta key a card records its arkham.build id under. Mirrors
 * `arkhamBuildIdKey` in `Arkham.Card.CustomCard`. */
export const ARKHAM_BUILD_ID_KEY = 'arkhamBuildId'

/* The arkham.build card this one stands in for, when its author said so.
 *
 * A card imported from an arkham.build pack needs none of this -- its code is
 * derived from the id, so a deck naming the id lands on it. A card built here
 * has a code of its own, and this is how it can still be named by a deck built
 * there: the same set published in both places is the same cards twice.
 *
 * Checked rather than trusted, so a mistyped id is a card that answers to one
 * code instead of one that claims a nonsense second one. */
export function arkhamBuildIdOf(def: any): string | null {
  const id = def?.meta?.[ARKHAM_BUILD_ID_KEY]
  return typeof id === 'string' && isArkhamBuildCardId(id) ? id : null
}

/* Every code a custom card answers to: its own first, then its aliases. */
export function customCardCodes(def: any): string[] {
  const id = arkhamBuildIdOf(def)
  return id ? [def.cardCode, arkhamBuildCustomCardCode(id)] : [def.cardCode]
}

/* A def written by the builder only carries the fields that card needed, and a
 * def that came off the wire went through `cardDefDecoder`, which fills in the
 * rest. Anything that reads a custom def as a `CardDef` -- the deck page, the
 * card views -- expects those fields to be there, so fill them in once here
 * rather than guarding at every use. */
export function normalizeCardDef(def: any): CardDef {
  return {
    ...def,
    classSymbols: def.classSymbols ?? [],
    cardTraits: def.cardTraits ?? [],
    skills: def.skills ?? [],
    customizations: def.customizations ?? [],
    options: def.options ?? [],
    tags: def.tags ?? [],
    meta: def.meta ?? {},
    level: def.level ?? null,
    cost: def.cost ?? null,
    otherSide: def.otherSide ?? null,
    errata: def.errata ?? null,
    doubleSided: def.doubleSided ?? false,
  } as CardDef
}

const registry = reactive(new Map<string, CustomCard>())

export function registerCustomCards(cards: CustomCard[]) {
  /* The text these cards carry, handed to vue-i18n as they arrive: a prompt
   * reads as words for every player in the game, not only the one whose
   * library the set is in. */
  publishCardText(cards.map((card) => card.def))
  for (const card of cards) {
    const cardCode = stripCardCodePrefix(card.def.cardCode)
    const existing = registry.get(cardCode)
    const entry = {
      def: normalizeCardDef({ ...card.def, cardCode, art: stripCardCodePrefix(card.def.art) }),
      // A def can come back from the server without its art (an older card, a
      // partial payload); never drop art already known for that card.
      art: card.art ?? existing?.art ?? null,
    }
    registry.set(cardCode, entry)
    /* The same entry under every code the card answers to, so a deck built on
     * arkham.build -- which names it by the code its id derives to -- resolves
     * to this def rather than to nothing. The entry keeps its own code, so
     * whatever reads the def back still sees the card's real identity.
     * Aliases never overwrite a real code: that card is the one a deck means. */
    for (const alias of customCardCodes(entry.def).slice(1)) {
      const bare = stripCardCodePrefix(alias)
      if (!cards.some((c) => stripCardCodePrefix(c.def.cardCode) === bare)) {
        registry.set(bare, entry)
      }
    }
  }
}

export function unregisterCustomCard(cardCode: string) {
  const bare = stripCardCodePrefix(cardCode)
  const entry = registry.get(bare)
  // Its aliases go with it, or a deleted card keeps answering under its claim.
  if (entry) {
    for (const [code, held] of [...registry.entries()]) {
      if (held === entry) registry.delete(code)
    }
  }
  registry.delete(bare)
}

export function customCards(): CustomCard[] {
  return [...registry.values()]
}

export function customCardDefs(): CardDef[] {
  return customCards().map((c) => c.def)
}

export function customCardDef(code: string): CardDef | undefined {
  return registry.get(stripCardCodePrefix(code))?.def
}

/* ------------------------------------------------------- set declarations ---

   What an investigator expansion brings besides cards: the tokens its cards
   place on themselves, and the keywords they print.

   Read off the cards rather than fetched: a set stamps a copy onto every card
   in it, which is the only form that reaches a running game -- the engine sends
   defs, not sets. Any card of the set answers for it, and the first one that
   says anything wins, so registering one card is enough to draw its tokens. */

export type CustomSetTokenDef = { slug: string; name: string; image?: string | null }
export type CustomSetKeywordDef = {
  name: string
  description: string
  token?: string | null
  shape: 'plain' | 'conditional'
}

const asArray = <T>(value: unknown): T[] => (Array.isArray(value) ? (value as T[]) : [])

/* Every token any registered card's set declares, by slug. Built on read rather
 * than maintained alongside the registry: the registry is reactive, cards
 * arrive in several batches, and a token looked up while drawing a card has to
 * see the ones that came in after it. */
export function customSetTokens(): Map<string, CustomSetTokenDef> {
  const found = new Map<string, CustomSetTokenDef>()
  for (const { def } of registry.values()) {
    for (const token of asArray<CustomSetTokenDef>(def.meta?.setTokens)) {
      if (token?.slug && !found.has(token.slug)) found.set(token.slug, token)
    }
  }
  return found
}

export const customSetToken = (slug: string): CustomSetTokenDef | undefined =>
  customSetTokens().get(slug)

/* The keywords a card's own set declares, so a card printing one can be shown
 * what it means. */
export function customSetKeywords(def: CardDef | undefined): CustomSetKeywordDef[] {
  return asArray<CustomSetKeywordDef>(def?.meta?.setKeywords).filter((k) => k?.name)
}

/* An art field may name a printed card instead of carrying an image of its own,
 * so a custom investigator can reuse an existing face, back, or portrait rather
 * than re-uploading one. It is stored as the bare card code, never as a URL: the
 * asset host differs between development and production, so only the code
 * survives an export. Whoever resolves it decides which image of that card it
 * means -- a portrait slot reads a portrait, an art slot reads the card. */
export function cardArtReference(art: string | null | undefined): string | null {
  const code = art?.trim() ?? ''
  return /^\d{5}[a-z]?$/.test(code) ? code : null
}

/* ----------------------------------------------------------- portraits ---

   A game row needs a custom investigator's portrait and nothing else, so it is
   served and held on its own rather than as a def: see `fetchCustomCardPortraits`.

   Four fields, against a def that can run to kilobytes of authoring script.

   Kept beside the registry instead of registered as a stub def, because a stub
   would overwrite the real def of a card the open game had already loaded, and
   a portrait is not a card. Every reader below consults the registry first and
   falls back here, so a full def always wins. */
export type CardPortrait = {
  cardCode: string
  art: string | null
  portrait: string | null
  portraitBack: string | null
}

const portraits = reactive(new Map<string, CardPortrait>())

export function registerCustomCardPortraits(entries: CardPortrait[]) {
  for (const entry of entries) {
    portraits.set(stripCardCodePrefix(entry.cardCode), entry)
  }
}

/* Whether anything at all is known about this card's face -- a def or a
 * portrait. A row waits on this before drawing, so it does not flash the
 * "definition missing" image while the portraits are still in flight. */
export function customCardFaceKnown(code: string): boolean {
  const bare = stripCardCodePrefix(code)
  return registry.has(bare) || portraits.has(bare)
}

/* The portrait a custom investigator carries, by side. `null` means the card
 * names no portrait of its own, which is a different answer from "not loaded
 * yet" -- `customCardFaceKnown` is that question. */
export function customCardPortrait(code: string, suffix: string = ''): string | null {
  const bare = stripCardCodePrefix(code)
  const def = registry.get(bare)?.def
  if (def) return (suffix === 'b' ? def.meta?.portraitBack : def.meta?.portrait) ?? null
  const entry = portraits.get(bare)
  if (!entry) return null
  return (suffix === 'b' ? entry.portraitBack : entry.portrait) ?? null
}

// `art` arrives with a card-side suffix appended by the art helpers (`…b` for a
// back). Custom cards are single-sided, so fall back to the bare code.
export function customCardArt(art: string): string | null {
  const code = stripCardCodePrefix(art)
  const exact = registry.get(code)
  if (exact) return exact.art

  // A portrait-only entry still knows where the card's own image lives, which
  // is what a row falls back to for an investigator carrying no portrait.
  const stub = portraits.get(code)
  if (stub) return stub.art

  // A back is asked for as "<code>b". An investigator draws its own back; other
  // custom cards fall back to their face.
  const base = registry.get(code.replace(/[ab]$/, ''))
  if (!base) return null
  if (code.endsWith('b') && base.def.meta?.backArt) return base.def.meta.backArt
  return base.art
}

const escapeXml = (t: string) =>
  t.replace(/[<>&"]/g, (c) => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;', '"': '&quot;' })[c] as string)

// Wrap on whole words so a long title does not run off the card.
function wrap(text: string, perLine: number): string[] {
  const lines: string[] = []
  let line = ''
  for (const word of text.split(/\s+/)) {
    if (line && (line + ' ' + word).length > perLine) {
      lines.push(line)
      line = word
    } else {
      line = line ? `${line} ${word}` : word
    }
  }
  if (line) lines.push(line)
  return lines.slice(0, 4)
}

/* A card with no art still has to be readable at the table, so draw one from
 * the def: title, type, and traits on a plain frame. */
export function renderCardPlaceholder(def: CardDef | undefined): string {
  const title = def ? def.name.title : 'Custom Card'
  const subtitle = def?.name.subtitle ?? null
  const traits = def?.cardTraits ?? []
  const kind = def?.cardType?.replace(/Type$/, '') ?? 'Card'

  const titleLines = wrap(title, 16)
  const titleSvg = titleLines
    .map((l, i) => `<text x="150" y="${150 + i * 26}" class="t">${escapeXml(l)}</text>`)
    .join('')

  const svg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 419">
    <style>
      text { font-family: Georgia, 'Times New Roman', serif; text-anchor: middle; fill: #f4ecd8 }
      .t { font-size: 22px; font-weight: bold }
      .s { font-size: 14px; font-style: italic; fill: #cdbfa4 }
      .k { font-size: 13px; letter-spacing: 2px; fill: #9c8a6b }
    </style>
    <rect width="300" height="419" rx="14" fill="#241c17"/>
    <rect x="10" y="10" width="280" height="399" rx="9" fill="none" stroke="#6b5a3e" stroke-width="2"/>
    <text x="150" y="58" class="k">${escapeXml(kind.toUpperCase())}</text>
    ${titleSvg}
    ${subtitle ? `<text x="150" y="${150 + titleLines.length * 26 + 4}" class="s">${escapeXml(subtitle)}</text>` : ''}
    <text x="150" y="376" class="s">${escapeXml(traits.join(' . '))}</text>
  </svg>`

  return `data:image/svg+xml;utf8,${encodeURIComponent(svg)}`
}

export function customCardPlaceholder(art: string): string {
  const code = stripCardCodePrefix(art)
  const def = customCardDef(code) ?? customCardDef(code.replace(/[ab]$/, ''))
  return def ? renderCardPlaceholder(def) : renderMissingCard(code)
}

/* Whether a card code names a custom card nothing here can describe.
 *
 * A custom def lives on the game and in the server's registry, so it can go
 * missing: deleted from the library it was built in, or never recorded on a
 * game that then outlived the process. The engine now builds an inert card
 * rather than failing, and this is how the table says so. */
export function isMissingCustomCard(code: string): boolean {
  const bare = stripCardCodePrefix(code)
  if (!isCustomCardCode(bare)) return false
  return !customCardDef(bare) && !customCardDef(bare.replace(/[ab]$/, ''))
}

/* Drawn in place of a card whose definition is gone: legible as broken at a
 * glance, and carrying the code so it can be looked up or rebuilt. */
export function renderMissingCard(code: string): string {
  const svg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 419">
    <style>
      text { font-family: Georgia, 'Times New Roman', serif; text-anchor: middle; fill: #fca5a5 }
      .t { font-size: 20px; font-weight: bold }
      .s { font-size: 11px; fill: #9c8a6b }
      .c { font-size: 9px; font-family: monospace; fill: #6b7280 }
    </style>
    <rect width="300" height="419" rx="14" fill="#1a1210"/>
    <rect x="10" y="10" width="280" height="399" rx="9" fill="none" stroke="#7f1d1d"
          stroke-width="2" stroke-dasharray="8 6"/>
    <g stroke="#7f1d1d" stroke-width="3" fill="none">
      <circle cx="150" cy="150" r="42"/>
      <path d="M150 130 v26 M150 168 v3"/>
    </g>
    <text x="150" y="240" class="t">Card definition</text>
    <text x="150" y="266" class="t">missing</text>
    <text x="150" y="300" class="s">This card does nothing until it is restored</text>
    <text x="150" y="376" class="c">${escapeXml(code)}</text>
  </svg>`
  return `data:image/svg+xml;utf8,${encodeURIComponent(svg)}`
}

/* Every kind of card the builder can make, in the order the type picker offers
 * them. The long label is what the picker asks with -- it has to say which back
 * a card has -- and the short one is what a chip or a filter says, where the
 * column it sits in is a few characters wide. */
export const CARD_TYPES = [
  { value: 'AssetType', label: 'Asset (player back)', short: 'Asset' },
  { value: 'EventType', label: 'Event', short: 'Event' },
  { value: 'SkillType', label: 'Skill', short: 'Skill' },
  { value: 'PlayerTreacheryType', label: 'Treachery (weakness)', short: 'Weakness' },
  { value: 'PlayerEnemyType', label: 'Enemy (weakness)', short: 'Enemy weakness' },
  { value: 'EnemyType', label: 'Enemy (encounter)', short: 'Enemy' },
  { value: 'TreacheryType', label: 'Treachery (encounter)', short: 'Treachery' },
  { value: 'EncounterAssetType', label: 'Asset (encounter back)', short: 'Encounter asset' },
  { value: 'LocationType', label: 'Location', short: 'Location' },
  { value: 'StoryType', label: 'Story', short: 'Story' },
  { value: 'InvestigatorType', label: 'Investigator', short: 'Investigator' },
] as const

/* What a card carries but does not own.
 *
 * The set stamps its name and its declarations onto every card it holds
 * (`stampSet`), and the server derives `art` from the card code as the card
 * comes in (`normalizeCard`). Written by hand they are not errors, they are
 * silently overwritten -- the set corrects its own copies on its next save, and
 * the code decides the art -- so an editor that shows the whole def should
 * leave these out of it and put them back unchanged.
 */
export const DERIVED_CARD_KEYS = ['art'] as const
export const SET_STAMPED_META_KEYS = ['set', 'setTokens', 'setKeywords'] as const

/* What kind of card this is, with the deck it comes from left out: a
 * PlayerTreachery and a TreacheryType are both treacheries, and a row that is
 * already inside one set is not where the difference between their backs gets
 * decided. `cardTypeLabel` is the one to reach for where it does -- a filter
 * offering both at once has to tell them apart. */
export const cardKind = (cardType: string): string =>
  cardType.replace(/Type$/, '').replace(/^Player/, '')

/* A card type as a chip says it. A type the list does not know -- one written
 * through the raw block -- keeps its own name without the suffix. */
export const cardTypeLabel = (cardType: string): string =>
  CARD_TYPES.find((t) => t.value === cardType)?.short ?? cardType.replace(/Type$/, '')

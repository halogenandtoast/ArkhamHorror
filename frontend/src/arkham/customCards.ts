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

export function isCustomCardCode(code: string): boolean {
  return stripCardCodePrefix(code).startsWith(CUSTOM_CARD_PREFIX)
}

// Mint a code the engine's CardCode Eq is safe with: a trailing a/b/c/d there
// designates a card side, so two custom codes ending in complementary letters
// would compare equal. Always end in a digit.
export function mintCustomCardCode(): string {
  return `${CUSTOM_CARD_PREFIX}${crypto.randomUUID().replace(/-/g, '')}0`
}

/* Same shape as `mintCustomCardCode`, but derived from an arkham.build card's
 * own UUID rather than a fresh random one. Importing a card and later reading
 * a deck that names it by that UUID need to land on the same code, so this has
 * to be deterministic -- a random mint would never match up with itself. */
export function arkhamBuildCustomCardCode(uuid: string): string {
  return `${CUSTOM_CARD_PREFIX}${uuid.replace(/-/g, '').toLowerCase()}0`
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
  for (const card of cards) {
    const cardCode = stripCardCodePrefix(card.def.cardCode)
    const existing = registry.get(cardCode)
    registry.set(cardCode, {
      def: normalizeCardDef({ ...card.def, cardCode, art: stripCardCodePrefix(card.def.art) }),
      // A def can come back from the server without its art (an older card, a
      // partial payload); never drop art already known for that card.
      art: card.art ?? existing?.art ?? null,
    })
  }
}

export function unregisterCustomCard(cardCode: string) {
  registry.delete(stripCardCodePrefix(cardCode))
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

// `art` arrives with a card-side suffix appended by the art helpers (`…b` for a
// back). Custom cards are single-sided, so fall back to the bare code.
export function customCardArt(art: string): string | null {
  const code = stripCardCodePrefix(art)
  const exact = registry.get(code)
  if (exact) return exact.art

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

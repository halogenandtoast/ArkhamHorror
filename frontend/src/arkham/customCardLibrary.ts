// Cards you have built, kept against your account so they outlive any one game
// and follow you between browsers.
//
// Distinct from `customCards.ts`, which is the registry of cards live in the
// *current* game — those come from that game and include cards other players
// made. A library card is added to a game by registering it there; the two stay
// separate on purpose.
import { computed, reactive, ref } from 'vue'
import * as Api from '@/arkham/api'
import { normalizeCardDef, registerCustomCards, type CustomCard } from '@/arkham/customCards'

export type LibraryCard = CustomCard & { id: string; updatedAt: string }

const LEGACY_STORAGE_KEY = 'arkham:custom-card-library'

const entries = reactive<LibraryCard[]>([])
export const libraryLoaded = ref(false)
let loading: Promise<void> | null = null

const toLibraryCard = (row: Api.StoredCustomCard): LibraryCard => ({
  id: row.id,
  def: normalizeCardDef(row.def),
  art: row.art,
  updatedAt: row.updatedAt,
})

function replaceAll(rows: Api.StoredCustomCard[]) {
  entries.splice(0, entries.length, ...rows.map(toLibraryCard))
  // Outside a game there is no game registry to resolve art and defs against,
  // so the deck page and the builder read your library through the same one.
  registerCustomCards(entries)
}

/* Cards made before the library moved server-side live in this browser only.
 * Push them up once, then drop the local copy so there is a single source of
 * truth. */
async function migrateLegacyCards() {
  let legacy: { def: any; art: string | null }[] = []
  try {
    const raw = localStorage.getItem(LEGACY_STORAGE_KEY)
    if (!raw) return
    const parsed = JSON.parse(raw)
    if (Array.isArray(parsed)) legacy = parsed
  } catch {
    return
  }

  if (!legacy.length) {
    localStorage.removeItem(LEGACY_STORAGE_KEY)
    return
  }

  try {
    await Api.importCustomCards(legacy.map((c) => ({ def: c.def, art: c.art ?? null })))
    localStorage.removeItem(LEGACY_STORAGE_KEY)
  } catch (error) {
    console.error(error)
  }
}

export async function loadLibrary(force = false) {
  if (libraryLoaded.value && !force) return
  loading ??= (async () => {
    await migrateLegacyCards()
    replaceAll(await Api.fetchCustomCardLibrary())
    libraryLoaded.value = true
  })()

  try {
    await loading
  } catch (error) {
    console.error(error)
  } finally {
    loading = null
  }
}

/* Whether there is anything to lay over a deck with. The overlay controls are
 * pointless without a card, so they stay hidden until you have built one. */
export const hasLibraryCards = computed(() => entries.length > 0)

export function libraryCards(): LibraryCard[] {
  return [...entries].sort((a, b) => b.updatedAt.localeCompare(a.updatedAt))
}

export function libraryCard(cardCode: string): LibraryCard | undefined {
  return entries.find((e) => e.def.cardCode === cardCode)
}

export async function saveToLibrary(card: CustomCard): Promise<LibraryCard> {
  const saved = toLibraryCard(await Api.saveCustomCard({ def: card.def, art: card.art }))
  const index = entries.findIndex((e) => e.def.cardCode === saved.def.cardCode)
  if (index === -1) entries.push(saved)
  else entries.splice(index, 1, saved)
  registerCustomCards([saved])
  return saved
}

export async function removeFromLibrary(cardCode: string) {
  const index = entries.findIndex((e) => e.def.cardCode === cardCode)
  if (index === -1) return
  const [removed] = entries.splice(index, 1)
  try {
    await Api.deleteCustomCard(removed.id)
  } catch (error) {
    console.error(error)
    entries.splice(index, 0, removed)
  }
}

export async function importLibraryCards(cards: CustomCard[]): Promise<LibraryCard[]> {
  const saved = (await Api.importCustomCards(cards.map((c) => ({ def: c.def, art: c.art })))).map(toLibraryCard)
  for (const card of saved) {
    const index = entries.findIndex((e) => e.def.cardCode === card.def.cardCode)
    if (index === -1) entries.push(card)
    else entries.splice(index, 1, card)
  }
  registerCustomCards(saved)
  return saved
}

export const EXPORT_VERSION = 1

export type CardExport = { version: number; cards: { def: any; art: string | null }[] }

/* An export carries the image itself, not a link to it.
 *
 * Art lives under the library it was uploaded to -- a different host in
 * development and production, and a different prefix per user -- so a bare URL
 * is worth nothing to whoever imports the file. Inlined as a data URI, the
 * import has bytes to store under its own account.
 *
 * Falls back to the URL when the image cannot be read: a production asset host
 * that sends no CORS headers refuses the fetch, and half an export beats none. */
export async function exportCards(cards: CustomCard[]): Promise<CardExport> {
  const inlined = await Promise.all(
    cards.map(async (c) => ({
      def: await inlineDefArt(c.def),
      art: (await inlineArt(c.art)) ?? c.art,
    })),
  )
  return { version: EXPORT_VERSION, cards: inlined }
}

/* An investigator has more images than its face — a card back and two portraits
 * — and they live in meta rather than on the card. */
const ART_META_KEYS = ['backArt', 'portrait', 'portraitBack']

async function inlineDefArt(def: any): Promise<any> {
  const meta = def?.meta
  if (!meta) return def
  const keys = ART_META_KEYS.filter((k) => typeof meta[k] === 'string')
  if (!keys.length) return def
  const entries = await Promise.all(
    keys.map(async (k) => [k, (await inlineArt(meta[k])) ?? meta[k]] as const),
  )
  return { ...def, meta: { ...meta, ...Object.fromEntries(entries) } }
}

async function inlineArt(art: string | null): Promise<string | null> {
  if (!art || art.startsWith('data:')) return art
  try {
    const response = await fetch(art)
    if (!response.ok) return null
    const blob = await response.blob()
    if (!blob.type.startsWith('image/')) return null
    return await new Promise<string | null>((resolve) => {
      const reader = new FileReader()
      reader.onload = () => resolve(typeof reader.result === 'string' ? reader.result : null)
      reader.onerror = () => resolve(null)
      reader.readAsDataURL(blob)
    })
  } catch {
    return null
  }
}

/* Accepts a whole export file or a single card, so a card pasted on its own
 * imports as readily as a file of many. */
export function parseCardExport(raw: string): CustomCard[] {
  const parsed = JSON.parse(raw)
  const cards = Array.isArray(parsed) ? parsed : (parsed.cards ?? [parsed])
  return cards
    .filter((c: any) => c?.def?.cardCode)
    .map((c: any) => ({ def: c.def, art: c.art ?? null }))
}

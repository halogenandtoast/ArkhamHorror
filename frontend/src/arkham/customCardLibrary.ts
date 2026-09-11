// Cards you have built, kept against your account so they outlive any one game
// and follow you between browsers.
//
// Distinct from `customCards.ts`, which is the registry of cards live in the
// *current* game — those come from that game and include cards other players
// made. A library card is added to a game by registering it there; the two stay
// separate on purpose.
import { computed, reactive, ref } from 'vue'
import * as Api from '@/arkham/api'
import {
  cardArtReference,
  normalizeCardDef,
  registerCustomCards,
  unregisterCustomCard,
  type CustomCard,
} from '@/arkham/customCards'

export type LibraryCard = CustomCard & { id: string; setId: string; updatedAt: string }
export type LibrarySet = Api.StoredCustomCardSet

const LEGACY_STORAGE_KEY = 'arkham:custom-card-library'

const entries = reactive<LibraryCard[]>([])
const sets = reactive<LibrarySet[]>([])
export const libraryLoaded = ref(false)
let loading: Promise<void> | null = null

const toLibraryCard = (row: Api.StoredCustomCard): LibraryCard => ({
  id: row.id,
  setId: row.setId,
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

/* A set's card count is maintained here rather than re-fetched: the server
 * sends it with the set, but every local add and removal has to keep it true
 * until the next load or the sidebar counts drift. */
function recountSets() {
  for (const set of sets) {
    set.cardCount = entries.filter((e) => e.setId === set.id).length
  }
}

function upsertSet(set: LibrarySet) {
  const index = sets.findIndex((s) => s.id === set.id)
  if (index === -1) sets.push(set)
  else sets.splice(index, 1, set)
}

/* Cards made before the library moved server-side live in this browser only.
 * Push them up once, then drop the local copy so there is a single source of
 * truth. They predate sets, so they arrive as one, named after whatever set
 * name they were carrying. */
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
    for (const [name, cards] of groupByDeclaredSet(legacy)) {
      await Api.importCustomCardSet({
        name,
        sourceCode: null,
        cards: cards.map((c) => ({ def: c.def, art: c.art ?? null })),
      })
    }
    localStorage.removeItem(LEGACY_STORAGE_KEY)
  } catch (error) {
    console.error(error)
  }
}

/* The set name a loose card carries in its own def -- what grouping used to be
 * before a set was a real thing. Read when cards arrive from outside (a legacy
 * browser library, an export file) and have to be put into one. */
function declaredSetName(card: { def: any }): string | null {
  const name = card.def?.meta?.set
  return typeof name === 'string' && name.trim() ? name.trim() : null
}

const UNNAMED_SET = 'Imported cards'

function groupByDeclaredSet(cards: { def: any; art: string | null }[]) {
  const groups = new Map<string, { def: any; art: string | null }[]>()
  for (const card of cards) {
    const name = declaredSetName(card) ?? UNNAMED_SET
    if (!groups.has(name)) groups.set(name, [])
    groups.get(name)!.push(card)
  }
  return groups
}

export async function loadLibrary(force = false) {
  if (libraryLoaded.value && !force) return
  loading ??= (async () => {
    await migrateLegacyCards()
    const [setRows, cardRows] = await Promise.all([
      Api.fetchCustomCardSets(),
      Api.fetchCustomCardLibrary(),
    ])
    sets.splice(0, sets.length, ...setRows)
    replaceAll(cardRows)
    recountSets()
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

// ------------------------------------------------------------------ sets ---

export function librarySets(): LibrarySet[] {
  return [...sets].sort((a, b) => a.name.localeCompare(b.name))
}

export const librarySet = (id: string | null): LibrarySet | undefined =>
  id ? sets.find((s) => s.id === id) : undefined

export const setCards = (setId: string): LibraryCard[] =>
  libraryCards().filter((c) => c.setId === setId)

export async function createSet(name: string): Promise<LibrarySet> {
  const set = await Api.createCustomCardSet(name)
  upsertSet(set)
  return set
}

export async function renameSet(id: string, name: string): Promise<LibrarySet> {
  const set = await Api.renameCustomCardSet(id, name)
  upsertSet(set)
  /* The name is stamped onto every card in the set so a card exported on its
   * own still says where it came from; the server rewrites them, and the copies
   * held here have to follow or the library would show the old name until a
   * reload. */
  for (const card of entries) {
    if (card.setId === id) card.def.meta = { ...card.def.meta, set: set.name }
  }
  registerCustomCards(entries.filter((c) => c.setId === id))
  return set
}

/* Deleting a set deletes what is in it -- the point of the set being the unit
 * you can change your mind about, rather than 150 cards you delete one by one. */
export async function removeSet(id: string) {
  await Api.deleteCustomCardSet(id)
  for (let i = entries.length - 1; i >= 0; i--) {
    if (entries[i].setId === id) {
      unregisterCustomCard(entries[i].def.cardCode)
      entries.splice(i, 1)
    }
  }
  const index = sets.findIndex((s) => s.id === id)
  if (index !== -1) sets.splice(index, 1)
}

/* Replaces the set's contents wholesale rather than merging into them: an
 * import is the whole set as it now stands, so a card the new file dropped has
 * to be gone here too. */
export async function importSet(payload: {
  name: string
  sourceCode: string | null
  cards: CustomCard[]
}): Promise<LibrarySet> {
  const { set, cards } = await Api.importCustomCardSet({
    name: payload.name,
    sourceCode: payload.sourceCode,
    cards: payload.cards.map((c) => ({ def: c.def, art: c.art })),
  })

  for (let i = entries.length - 1; i >= 0; i--) {
    if (entries[i].setId === set.id) {
      unregisterCustomCard(entries[i].def.cardCode)
      entries.splice(i, 1)
    }
  }
  const imported = cards.map(toLibraryCard)
  entries.push(...imported)
  registerCustomCards(imported)
  upsertSet(set)
  recountSets()
  return set
}

// ----------------------------------------------------------------- cards ---

export async function saveToLibrary(card: CustomCard, setId: string): Promise<LibraryCard> {
  const saved = toLibraryCard(await Api.saveCustomCard({ setId, def: card.def, art: card.art }))
  const index = entries.findIndex((e) => e.def.cardCode === saved.def.cardCode)
  if (index === -1) entries.push(saved)
  else entries.splice(index, 1, saved)
  registerCustomCards([saved])
  recountSets()
  return saved
}

export async function removeFromLibrary(cardCode: string) {
  const index = entries.findIndex((e) => e.def.cardCode === cardCode)
  if (index === -1) return
  const [removed] = entries.splice(index, 1)
  try {
    await Api.deleteCustomCard(removed.id)
    recountSets()
  } catch (error) {
    console.error(error)
    entries.splice(index, 0, removed)
  }
}

/* 2 carries the set the cards came from; 1 was cards alone. Both import -- a
 * version 1 file falls back to the set name each card names for itself. */
export const EXPORT_VERSION = 2

export type CardExport = {
  version: number
  set?: { name: string; sourceCode: string | null }
  cards: { def: any; art: string | null }[]
}

export type ParsedCardExport = { name: string; sourceCode: string | null; cards: CustomCard[] }

/* An export carries the image itself, not a link to it.
 *
 * Art lives under the library it was uploaded to -- a different host in
 * development and production, and a different prefix per user -- so a bare URL
 * is worth nothing to whoever imports the file. Inlined as a data URI, the
 * import has bytes to store under its own account.
 *
 * Falls back to the URL when the image cannot be read: a production asset host
 * that sends no CORS headers refuses the fetch, and half an export beats none. */
export async function exportCards(cards: CustomCard[], set?: LibrarySet): Promise<CardExport> {
  const inlined = await Promise.all(
    cards.map(async (c) => ({
      def: await inlineDefArt(c.def),
      art: (await inlineArt(c.art)) ?? c.art,
    })),
  )
  return {
    version: EXPORT_VERSION,
    ...(set ? { set: { name: set.name, sourceCode: set.sourceCode } } : {}),
    cards: inlined,
  }
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
  // A reference to a printed card's art is already portable, and means the same
  // card wherever it is imported; inlining it would only make the export bigger.
  if (cardArtReference(art)) return art
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
 * imports as readily as a file of many. Everything lands in a set: the one the
 * file names, else the one the cards name for themselves, else `fallbackName`
 * (the file's own name, which is all that is left to go on). */
export function parseCardExport(raw: string, fallbackName = 'Imported cards'): ParsedCardExport {
  const parsed = JSON.parse(raw)
  const rows = Array.isArray(parsed) ? parsed : (parsed.cards ?? [parsed])
  const cards: CustomCard[] = rows
    .filter((c: any) => c?.def?.cardCode)
    .map((c: any) => ({ def: c.def, art: c.art ?? null }))

  const declared = typeof parsed?.set?.name === 'string' ? parsed.set.name.trim() : ''
  return {
    name: declared || (cards.length ? declaredSetName(cards[0]) : null) || fallbackName,
    sourceCode: parsed?.set?.sourceCode ?? null,
    cards,
  }
}

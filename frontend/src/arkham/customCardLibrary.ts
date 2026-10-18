// Cards you have built, kept against your account so they outlive any one game
// and follow you between browsers.
//
// Distinct from `customCards.ts`, which is the registry of cards live in the
// *current* game — those come from that game and include cards other players
// made. A library card is added to a game by registering it there; the two stay
// separate on purpose.
import { computed, reactive, ref } from 'vue'
import * as Api from '@/arkham/api'
import { useCardStore } from '@/stores/cards'
import {
  bareCardCode,
  cardArtReference,
  customCardCodes,
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
 * truth. They predate sets, so they are filed under whatever set name they were
 * carrying.
 *
 * Card by card, never as a set import. A browser that has not been opened since
 * the library moved server-side is holding a snapshot of a library that has
 * since moved on -- possibly migrated already from another browser, and added to
 * there -- and an import replaces a set outright. Pushing each card as its own
 * upsert adds what this browser still has and leaves everything else alone,
 * which is what carrying old cards forward is supposed to mean. */
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
      // Names an existing set rather than making a second one: the server hands
      // back the set it already has under that name.
      const set = await Api.createCustomCardSet(name)
      for (const card of cards) {
        await Api.saveCustomCard({ setId: set.id, def: card.def, art: card.art ?? null })
      }
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

/* Its own code, or one it answers to: a card can name the arkham.build card it
 * stands in for, and a deck built there names that one. Own codes are checked
 * across the whole library before any alias, so a card cannot be shadowed by
 * another one's claim to be it.
 *
 * Compared bare on both sides: a stored def's code carries the wire `c`, an
 * alias is derived bare, and callers pass either. */
export function libraryCard(cardCode: string): LibraryCard | undefined {
  const want = bareCardCode(cardCode)
  const bare = (code: string) => bareCardCode(code) === want
  return (
    entries.find((e) => bare(e.def.cardCode))
    ?? entries.find((e) => customCardCodes(e.def).some(bare))
  )
}

// ------------------------------------------------------------------ sets ---

export function librarySets(): LibrarySet[] {
  return [...sets].sort((a, b) => a.name.localeCompare(b.name))
}

export const librarySet = (id: string | null): LibrarySet | undefined =>
  id ? sets.find((s) => s.id === id) : undefined

export const setCards = (setId: string): LibraryCard[] =>
  libraryCards().filter((c) => c.setId === setId)

/* Printed order -- the card number, which is the order the set's author put
 * them in. `libraryCards` and `setCards` hand cards back most-recently-edited
 * first, which is the wrong order anywhere a whole set is on show. */
export const byPrintedNumber = (a: LibraryCard, b: LibraryCard) =>
  (a.def.meta?.number ?? '').localeCompare(b.def.meta?.number ?? '', undefined, { numeric: true })

export async function createSet(name: string): Promise<LibrarySet> {
  const set = await Api.createCustomCardSet(name)
  upsertSet(set)
  return set
}

/* Rename, redescribe, relink, mark as an expansion, or declare its tokens and
 * keywords -- any of them. A field left out keeps whatever is stored, so
 * renaming cannot wipe a blurb, a link, or a set's tokens. */
export async function updateSet(
  id: string,
  changes: {
    name: string
    description?: string | null
    url?: string | null
    investigatorExpansion?: boolean
    icon?: string | null
    tokens?: Api.CustomSetToken[]
    keywords?: Api.CustomSetKeyword[]
  },
): Promise<LibrarySet> {
  const set = await Api.updateCustomCardSet(id, changes)
  upsertSet(set)
  /* The name, and what the set declares, are stamped onto every card in it: a
   * card exported on its own still says where it came from, and a card in a
   * running game still knows what its keyword means and what its token looks
   * like. The server rewrites them, and the copies held here have to follow or
   * an export taken before the next reload would carry the old ones. The
   * description, the link and the icon are not stamped on: they describe the
   * set, and a card on its own is not the set. */
  for (const card of entries) {
    if (card.setId === id) card.def.meta = { ...card.def.meta, ...setStamp(set) }
  }
  registerCustomCards(entries.filter((c) => c.setId === id))
  return set
}

/* What a set writes into each of its cards' defs, mirroring `stampSet` on the
 * server. An empty declaration is `undefined` rather than `[]`, so an ordinary
 * set's cards carry nothing but its name. */
function setStamp(set: LibrarySet): Record<string, unknown> {
  return {
    set: set.name,
    setTokens: set.tokens?.length ? set.tokens : undefined,
    setKeywords: set.keywords?.length ? set.keywords : undefined,
  }
}

export const renameSet = (id: string, name: string) => updateSet(id, { name })

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
  description?: string | null
  url?: string | null
  sourceCode: string | null
  investigatorExpansion?: boolean
  icon?: string | null
  tokens?: Api.CustomSetToken[]
  keywords?: Api.CustomSetKeyword[]
  cards: CustomCard[]
}): Promise<LibrarySet> {
  const { set, cards } = await Api.importCustomCardSet({
    name: payload.name,
    description: payload.description,
    url: payload.url,
    sourceCode: payload.sourceCode,
    investigatorExpansion: payload.investigatorExpansion,
    icon: payload.icon,
    tokens: payload.tokens,
    keywords: payload.keywords,
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

// ----------------------------------------------------------- marketplace ---

/* Publishing and following both change what the server holds for a set -- its
 * cards, on an update, and its subscription either way -- so the library is
 * reloaded rather than patched from the response. */

export async function publishSet(id: string, note: string | null, notify: boolean) {
  const published = await Api.publishCustomCardSet(id, note, notify)
  await loadLibrary(true)
  return published
}

export async function subscribeToSet(id: string, version?: number) {
  const published = await Api.subscribeToCardSet(id, version)
  await loadLibrary(true)
  return published
}

/* The other way to take a published set: your own copy, editable, frozen where
 * it was. Replaces a subscribed copy of the same listing, since nobody can hold
 * both. */
export async function importPublishedSet(id: string, version?: number) {
  const published = await Api.importCardSet(id, version)
  await loadLibrary(true)
  return published
}

export async function syncSet(id: string) {
  const published = await Api.syncCustomCardSet(id)
  await loadLibrary(true)
  return published
}

/* Take a set somebody shared the link to. Subscribing is the only way: the copy
 * is locked, so it stays what they shared, and subscribing again is how it
 * catches up with them.
 *
 * The server does the import, so the cards never round-trip through here; the
 * library is reloaded rather than patched, because taking a set again replaces
 * every card in it. */
export async function subscribeToSharedSet(token: string) {
  const shared = await Api.subscribeToSharedCardSet(token)
  await loadLibrary(true)
  return shared
}

/* Take a followed set into your editor. Nothing is fetched -- the cards are
 * already here -- so the set alone is replaced rather than the library
 * reloaded. */
export async function unlockSet(id: string) {
  const set = await Api.unlockCustomCardSet(id)
  upsertSet(set)
  return set
}

/* Whether this set is being handed to anyone by link, and to how many people.
 * Both read off the set the server sent, so a row can say so without asking. */
export const isShared = (set: LibrarySet) => set.shareToken !== null

/* Whether this set follows somebody else's and so cannot be edited: its cards,
 * its details, and a file imported over it are all refused by the server, and
 * the builder hides the controls that would try. */
export const isLocked = (set: LibrarySet) => set.locked

/* Whether this set was handed to you by a link rather than taken from the
 * marketplace, which decides what you may do with it beyond playing.
 *
 * A published set is public: a copy of one can be taken into your editor and
 * exported like any set of yours. A shared one is the author's unpublished work,
 * lent to a few people to try — so there is no editor copy and nothing to
 * export, and passing the cards on stays the author's to do. The only ways out
 * of one are to leave it or to delete it. */
export const isFromShare = (set: LibrarySet) => set.sharedToken !== null

/* Whether "Add to my editor" applies: a locked copy of something published. */
export const canUnlock = (set: LibrarySet) => set.locked && !isFromShare(set)

/* Whether the export controls are offered. Somebody else's unpublished work is
 * not yours to hand on; everything else in your library is. */
export const canExport = (set: LibrarySet) => !isFromShare(set)



/* Whether a set is still following what it was published as, and whether there
 * is a newer version than the one it is on. */
export const isSubscribed = (set: LibrarySet) => set.publishedCardSetId !== null

export const updateAvailable = (set: LibrarySet) =>
  isSubscribed(set) &&
  set.latestVersion !== null &&
  set.subscribedVersion !== null &&
  set.latestVersion > set.subscribedVersion

/* Where a set of your own stands with the marketplace. `null` for a set you have
 * never submitted, which is most of them. */
export const reviewStatus = (set: LibrarySet) => set.submissionStatus

export const isAwaitingReview = (set: LibrarySet) => set.submissionStatus === 'pending'

export const wasDenied = (set: LibrarySet) => set.submissionStatus === 'denied'

/* Whether a version of this set is in the marketplace. A later version may be
 * waiting or have been turned down without changing that. */
export const isListed = (set: LibrarySet) => set.approvedVersion !== null

export async function saveToLibrary(card: CustomCard, setId: string): Promise<LibraryCard> {
  const saved = toLibraryCard(await Api.saveCustomCard({ setId, def: card.def, art: card.art }))
  const index = entries.findIndex((e) => e.def.cardCode === saved.def.cardCode)
  if (index === -1) entries.push(saved)
  else entries.splice(index, 1, saved)
  registerCustomCards([saved])
  /* The registry is not the card pool the rest of the app reads: a game resolves
   * defs through the card store, which is loaded once per page. Without this a
   * save was invisible -- the old def kept answering for the card -- until a
   * reload. */
  useCardStore().syncCustomCards()
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
  set?: {
    name: string
    description?: string | null
    url?: string | null
    sourceCode: string | null
    /* What an investigator expansion brings besides cards. Written out rather
     * than left to the copies stamped on the cards so a file can be read and
     * edited by hand; the server falls back to the cards when it is absent. */
    investigatorExpansion?: boolean
    icon?: string | null
    tokens?: Api.CustomSetToken[]
    keywords?: Api.CustomSetKeyword[]
  }
  cards: { def: any; art: string | null }[]
}

export type ParsedCardExport = {
  name: string
  description: string | null
  url: string | null
  sourceCode: string | null
  investigatorExpansion?: boolean
  icon?: string | null
  tokens?: Api.CustomSetToken[]
  keywords?: Api.CustomSetKeyword[]
  cards: CustomCard[]
}

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
  const inline = artInliner()
  const inlined = await Promise.all(
    cards.map(async (c) => ({
      def: withoutSetStamp(await inlineDefArt(c.def, inline), set),
      art: (await inline(c.art)) ?? c.art,
    })),
  )
  const icon = set?.icon ? ((await inline(set.icon)) ?? set.icon) : null
  /* Token art is inlined for the same reason a card's is: it was uploaded under
   * the exporter's account, on a host the importer may not even be able to read,
   * so a bare URL is worth nothing to them. */
  const tokens = await Promise.all(
    (set?.tokens ?? []).map(async (t) => ({
      ...t,
      image: t.image ? ((await inline(t.image)) ?? t.image) : null,
    })),
  )
  return {
    version: EXPORT_VERSION,
    ...(set
      ? {
          set: {
            name: set.name,
            description: set.description,
            url: set.url,
            sourceCode: set.sourceCode,
            investigatorExpansion: set.investigatorExpansion,
            icon,
            tokens,
            keywords: set.keywords ?? [],
          },
        }
      : {}),
    cards: inlined,
  }
}

/* A set's declarations, said once rather than on every card in it.
 *
 * Each card carries a copy of the tokens and keywords its set declares, which
 * is what answers for the set where there is no set row to read. A file that
 * carries the set itself is not such a place: the declarations are at the top
 * of it, the import stamps them back onto every card as it writes them, and a
 * box of two hundred cards is otherwise half the same keyword two hundred
 * times. A loose card -- exported with no set — keeps its copy, which is the
 * whole reason it has one.
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
function withoutSetStamp(def: any, set?: LibrarySet): any {
  if (!set || !def?.meta) return def
  const { setTokens, setKeywords, ...meta } = def.meta
  return setTokens || setKeywords ? { ...def, meta } : def
}

/* An investigator has more images than its face — a card back and two portraits
 * — and they live in meta rather than on the card. */
const ART_META_KEYS = ['backArt', 'portrait', 'portraitBack']

async function inlineDefArt(def: any, inline: ArtInliner): Promise<any> {
  const meta = def?.meta
  if (!meta) return def
  const keys = ART_META_KEYS.filter((k) => typeof meta[k] === 'string')
  const tokens = await inlineSetTokenArt(meta, inline)
  if (!keys.length && !tokens) return def
  const entries = await Promise.all(
    keys.map(async (k) => [k, (await inline(meta[k])) ?? meta[k]] as const),
  )
  return {
    ...def,
    meta: {
      ...meta,
      ...Object.fromEntries(entries),
      ...(tokens ? { setTokens: tokens } : {}),
    },
  }
}

/* The tokens a set declares are stamped onto every card in it, and that copy is
 * what an import reads the set back off when the file names no set at all — a
 * card pasted on its own, or a selection exported out of the builder, neither of
 * which has a set to carry the declaration. Inlined for the same reason the
 * set's own copy is: a bare URL is a path into the exporter's library, and on
 * another host it is a broken image rather than a token.
 *
 * Falls back to the URL like everything else here: half a token beats none, and
 * the server drops an image it cannot serve rather than storing one. */
async function inlineSetTokenArt(meta: any, inline: ArtInliner) {
  if (!Array.isArray(meta.setTokens)) return null
  return await Promise.all(
    meta.setTokens.map(async (token: any) =>
      token && typeof token.image === 'string'
        ? { ...token, image: (await inline(token.image)) ?? token.image }
        : token,
    ),
  )
}

/* One export, one fetch per image. A set's token art is stamped onto every card
 * in it, so without this a selection of fifty cards would fetch the same marker
 * fifty times and embed fifty copies of it. */
type ArtInliner = (art: string | null) => Promise<string | null>

function artInliner(): ArtInliner {
  const seen = new Map<string, Promise<string | null>>()
  return (art) => {
    if (!art) return Promise.resolve(art)
    const held = seen.get(art)
    if (held) return held
    const inflight = inlineArt(art)
    seen.set(art, inflight)
    return inflight
  }
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
  const described =
    typeof parsed?.set?.description === 'string' ? parsed.set.description.trim() : ''
  const linked = typeof parsed?.set?.url === 'string' ? parsed.set.url.trim() : ''
  return {
    name: declared || (cards.length ? declaredSetName(cards[0]) : null) || fallbackName,
    // Null, not '', for a file written before descriptions existed: the server
    // reads "nothing said" as "leave the set's own alone".
    description: described || null,
    url: linked || null,
    sourceCode: parsed?.set?.sourceCode ?? null,
    /* Left undefined rather than defaulted when the file does not say: the
     * server then reads the copies stamped on the cards, which is the only
     * thing a file written before expansions existed has. */
    investigatorExpansion: parsed?.set?.investigatorExpansion,
    icon: parsed?.set?.icon,
    tokens: parsed?.set?.tokens,
    keywords: parsed?.set?.keywords,
    cards,
  }
}

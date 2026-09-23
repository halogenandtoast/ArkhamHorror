import { defineStore } from 'pinia'

export interface ArkhamDBCard {
  code: string
  name: string
  xp?: number
  subname?: string
  traits?: string
  text?: string
  back_name?: string
  back_traits?: string
  back_text?: string
  customization_text?: string
  flavor? :string
  back_flavor?: string
  faction_name: string
  faction2_name?: string
  faction3_name?: string
  faction_code?: string
  type_name: string
  pack_name: string
  real_name: string
  real_traits: string
  real_text: string
  type_code: string
  // "weakness" | "basicweakness"; absent on non-weakness cards
  subtype_code?: string
  is_unique: boolean
  double_sided: boolean
  encounter_code?: string
  // Investigator cards only: required signature cards keyed by code, each
  // mapping to its alternate versions (also keyed by code).
  deck_requirements?: {
    size?: number
    card?: Record<string, Record<string, string> | null>
    random?: unknown[]
  }
}

export interface DbCardsState {
  dbCards: ArkhamDBCard[]
  dbCardsIndex: Map<string, ArkhamDBCard>
  // Ambiguous names map to null; see buildRealNameIndex.
  dbCardsByRealName: Map<string, ArkhamDBCard | null>
  lang: string
  loadingLang: string | null
  // Languages whose fetch failed. Card lookups happen on every hover, so
  // without this a missing or malformed cards_<lang>.json is re-requested for
  // the rest of the session.
  failedLangs: Set<string>
}

/* A card's `b` face has no record of its own unless ArkhamDB stores one, so a record
 * lends the face its own entry -- the overlay then reads the `back_*` fields. Alias in
 * a second pass so that loan never wins over a record filed under the `b` code itself;
 * in one pass the winner was whichever of the two sat later in the file. */
function buildCardIndex(cards: ArkhamDBCard[]): Map<string, ArkhamDBCard> {
  const index = new Map<string, ArkhamDBCard>()
  for (const card of cards) index.set(card.code, card)
  for (const card of cards) {
    const back = `${card.code}b`
    if (!index.has(back)) index.set(back, card)
  }
  return index
}

/* Untranslated names to records, for the faces the engine codes separately from
 * ArkhamDB. A name two or more records answer to maps to null rather than to a guess. */
function buildRealNameIndex(cards: ArkhamDBCard[]): Map<string, ArkhamDBCard | null> {
  const index = new Map<string, ArkhamDBCard | null>()
  for (const card of cards) {
    if (!card.real_name) continue
    index.set(card.real_name, index.has(card.real_name) ? null : card)
  }
  return index
}

export const useDbCardStore = defineStore("dbCards", {
  state: (): DbCardsState => ({
    dbCards: [],
    dbCardsIndex: new Map(),
    dbCardsByRealName: new Map(),
    lang: 'en',
    loadingLang: null,
    failedLangs: new Set<string>()
  } as DbCardsState),

  actions: {
    getDbCard(code: string): ArkhamDBCard | null {
      if (this.dbCards.length < 1) {
        void this.initDbCards()
      }

      // ArkhamDB stores some split-card fronts with an "a" suffix, while the
      // game runtime refers to the same front using the unsuffixed code.
      return this.dbCardsIndex.get(code) ?? this.dbCardsIndex.get(`${code}a`) ?? null
    },

    /* The engine codes some faces ArkhamDB does not record separately -- the five
     * Masked Carnevale-Goers are 82017b-82021b, one per enemy they hide, where
     * ArkhamDB stores the shared printed card once. Their own name is the only
     * thing left to find them by, and a name more than one card answers to is no
     * answer at all. */
    getDbCardByRealName(realName: string): ArkhamDBCard | null {
      if (this.dbCards.length < 1) {
        void this.initDbCards()
      }

      return this.dbCardsByRealName.get(realName) ?? null
    },

    getCardName(cardTitle: string, typeCode: string = ""): string {
      if (this.dbCards.length < 1) {
        const language = localStorage.getItem('language') || 'en'
        if (language !== 'en') void this.initDbCards()
      }

      const i = typeCode
        ? this.dbCards.find((c: ArkhamDBCard) =>  c.type_code === typeCode && c.real_name == cardTitle)
        : this.dbCards.find((c: ArkhamDBCard) =>  c.real_name == cardTitle)

      return i ? i.name : cardTitle
    },

    async fetchDbCards(lang: string) {
      // Document-relative on purpose: routing is hash-based, so this resolves
      // against the app's own directory and keeps working when the offline
      // package serves it from a subdirectory.
      const path = `/cards/cards_${lang}.json`.replace(/^\//, '')
      const response = await fetch(path)

      if (!response.ok) {
        throw new Error(`${path}: ${response.status} ${response.statusText}`)
      }

      // The dev server and the SPA fallback answer a missing file with
      // index.html, which only fails once it hits JSON.parse. Say what is
      // actually wrong instead.
      const contentType = response.headers.get('content-type') ?? ''
      if (!contentType.includes('json')) {
        throw new Error(`${path}: expected JSON, got ${contentType || 'no content type'}`)
      }

      const data = await response.json() as ArkhamDBCard[]

      if (this.lang !== lang) return

      this.dbCards = data
      this.dbCardsIndex = buildCardIndex(data)
      this.dbCardsByRealName = buildRealNameIndex(data)
    },

    async initDbCards() {
      const language = localStorage.getItem('language') || 'en'

      if (this.lang === language && this.dbCards.length > 0) return
      if (this.loadingLang === language) return
      if (this.failedLangs.has(language)) return

      this.lang = language
      this.loadingLang = language

      try {
        await this.fetchDbCards(language)
      } catch (e) {
        // Callers fire this off without awaiting it, so swallow the rejection
        // rather than leaving an unhandled one behind on every card lookup.
        this.failedLangs.add(language)
        console.error('Failed to load card data', e)
      } finally {
        if (this.loadingLang === language) this.loadingLang = null
      }
    }
  }
})

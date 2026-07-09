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
  lang: string
  loadingLang: string | null
}

export const useDbCardStore = defineStore("dbCards", {
  state: (): DbCardsState => ({
    dbCards: [],
    dbCardsIndex: new Map(),
    lang: 'en',
    loadingLang: null
  } as DbCardsState),

  actions: {
    getDbCard(code: string): ArkhamDBCard | null {
      if (this.dbCards.length < 1) {
        void this.initDbCards()
      }

      return this.dbCardsIndex.get(code) ?? null
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
      const data = await fetch(`/cards/cards_${lang}.json`.replace(/^\//, '')).then(async (cardResponse) => {
        return await cardResponse.json()
      })

      if (this.lang !== lang) return

      this.dbCards = data
      const index = new Map<string, ArkhamDBCard>()
      for (const card of data as ArkhamDBCard[]) {
        index.set(card.code, card)
        index.set(`${card.code}b`, card)
      }
      this.dbCardsIndex = index
    },

    async initDbCards() {
      const language = localStorage.getItem('language') || 'en'

      if (this.lang === language && this.dbCards.length > 0) return
      if (this.loadingLang === language) return

      this.lang = language
      this.loadingLang = language

      try {
        await this.fetchDbCards(language)
      } finally {
        if (this.loadingLang === language) this.loadingLang = null
      }
    }
  }
})

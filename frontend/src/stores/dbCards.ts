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
  faction_name: string
  faction2_name?: string
  faction3_name?: string
  type_name: string
  pack_name: string
  real_name: string
  real_traits: string
  real_text: string
  type_code: string
}

export interface DbCardsState {
  dbCards: ArkhamDBCard[]
  lang: string
}

export const useDbCardStore = defineStore("dbCards", {
  state: (): DbCardsState => ({
    dbCards: [],
    lang: 'en'
  } as DbCardsState),
  
  actions: {
    getDbCard(code: string): ArkhamDBCard | null {
      if (this.dbCard.length < 1) {
        const language = localStorage.getItem('language') || 'en'
        if (language !== 'en') this.initDbCards()
      }
      
      return this.dbCards.find(c => c.code == code)
        || this.dbCards.find(c => `${c.code}b` == code)
        || null
    },
    
    getCardName(cardTitle: string, typeCode: string = ""): string {
      if (this.dbCard.length < 1) {
        const language = localStorage.getItem('language') || 'en'
        if (language !== 'en') this.initDbCards()
      }
      
      const i = typeCode
        ? this.dbCards.find(c =>  c.type_code === typeCode && c.real_name == cardTitle)
        : this.dbCards.find(c =>  c.real_name == cardTitle)
      
      return i ? i.name : cardTitle
    },
    
    async fetchDbCards() {
      const data = await fetch(`/cards_${this.lang}.json`.replace(/^\//, '')).then(async (cardResponse) => {
        return await cardResponse.json()
      })
      this.dbCards = data
    },
    
    async initDbCards() {
      const language = localStorage.getItem('language') || 'en'
      
      if (this.lang === language && this.dbCards.length < 1) {
        await this.fetchDbCards()
      }
      else {
        this.lang = language
        
        if (this.lang === 'en') this.dbCards = []
        else await this.fetchDbCards()
      }
    }
  }
})

import { defineStore } from 'pinia'
import * as Api from '@/arkham/api'
import type { CardDef } from '@/arkham/types/CardDef'

export interface CardsState {
  cards: CardDef[]
  loaded: boolean
}

export const useCardStore = defineStore("cards", {
  state: () => ({
    cards: [],
    loaded: false
  } as CardsState),
  getters: {
    getCards(state) {
      return state.cards
    }
  },
  actions: {
    async fetchCards() {
      if (!this.loaded) {
        try {
          const data = await Api.fetchCards(true)
          this.cards = data
          this.loaded = true
        } catch (error) {
          console.log(error)
        }
      }
    }
  }
})

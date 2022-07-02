import { defineStore } from 'pinia'
import * as Api from '@/arkham/api'
import type { CardDef } from '@/arkham/types/CardDef'

export interface CardsState {
  cards: CardDef[]
}

export const useCardStore = defineStore("cards", {
  state: () => ({
    cards: []
  } as CardsState),
  getters: {
    getCards(state) {
      return state.cards
    }
  },
  actions: {
    async fetchCards() {
      try {
        const data = await Api.fetchCards(true)
        this.cards = data
      } catch (error) {
        console.log(error)
      }
    }
  }
})

import { defineStore } from 'pinia'
import * as Api from '@/arkham/api'
import type { CardDef } from '@/arkham/types/CardDef'

export interface CardsState {
  cards: CardDef[]
  loaded: boolean
}

let fetchCardsPromise: Promise<CardDef[]> | null = null

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
      if (this.loaded) return this.cards

      fetchCardsPromise ??= Api.fetchCards(true)

      try {
        const data = await fetchCardsPromise
        this.cards = data
        this.loaded = true
        return data
      } catch (error) {
        fetchCardsPromise = null
        console.log(error)
      }
    }
  }
})

import { defineStore } from 'pinia'
import * as Api from '@/arkham/api'
import { customCardDefs, registerCustomCards } from '@/arkham/customCards'
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
    },

    /* Debug-authored cards live on their game, not in the global card pool, so
     * they are fetched per game and folded in afterwards -- `fetchCards`
     * replaces `cards` wholesale, so this has to run after it settles. */
    async fetchCustomCards(gameId: string) {
      await this.fetchCards()

      try {
        const custom = await Api.fetchCustomCards(gameId)
        registerCustomCards(custom)
        // Read the defs back out of the registry rather than using the payload
        // directly: that is where card codes get normalised.
        const defs = customCardDefs()
        const codes = new Set(defs.map((c) => c.cardCode))
        this.cards = [...this.cards.filter((c) => !codes.has(c.cardCode)), ...defs]
        return custom
      } catch (error) {
        console.log(error)
        return []
      }
    }
  }
})

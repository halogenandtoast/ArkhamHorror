import { defineStore } from 'pinia'
import * as Api from '@/arkham/api'
import {
  customCardDefs,
  customCardFaceKnown,
  isCustomCardCode,
  registerCustomCardPortraits,
  registerCustomCards,
  stripCardCodePrefix,
} from '@/arkham/customCards'
import type { CustomCard } from '@/arkham/customCards'
import type { CardDef } from '@/arkham/types/CardDef'

export interface CardsState {
  cards: CardDef[]
  loaded: boolean
}

let fetchCardsPromise: Promise<CardDef[]> | null = null

/* Per game, so two components opening the same game share one request, and
 * leaving and coming back does not buy the cards again. */
const customCardRequests: Record<string, Promise<CustomCard[]>> = {}

/* Codes already asked about, whether or not the server had anything to say.
 * Without this a row whose card has no portrait would be re-requested by every
 * page that lists it. */
const requestedPortraits = new Set<string>()

/* Codes gathered but not yet sent, and the flush that will send them.
 *
 * Rows mount one after another inside a single tick, so without this a page of
 * twelve games asks twelve times. Collecting through a microtask means every
 * row that mounts together -- and the page's own preload, if it has one --
 * leaves in one request. */
let pendingPortraitCodes = new Set<string>()
let pendingPortraitFlush: Promise<void> | null = null

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
     * replaces `cards` wholesale, so this has to run after it settles.
     *
     * One request in flight per game, and the result is remembered for the
     * session: opening a game, leaving it and coming back used to buy the
     * table's whole card set again each time. The server sends an ETag, so even
     * a reload revalidates rather than re-downloading -- but the cheapest
     * request is the one not made. `reload` is for after a builder save, which
     * is the one time the answer is known to have changed. */
    async fetchCustomCards(gameId: string, reload = false) {
      await this.fetchCards()

      if (reload) delete customCardRequests[gameId]

      customCardRequests[gameId] ??= Api.fetchCustomCards(gameId)
        .then((custom) => {
          registerCustomCards(custom)
          this.syncCustomCards()
          return custom
        })
        .catch((error) => {
          // Not remembered, so the next caller tries again.
          delete customCardRequests[gameId]
          console.log(error)
          return [] as CustomCard[]
        })

      return customCardRequests[gameId]
    },

    /* Portraits for custom investigators named in a list of game rows.
     *
     * A row draws a face and nothing else, so it has no use for the defs --
     * which is just as well, because asking for them is the single most
     * expensive read on the home page. Only codes nothing is known about are
     * asked for, and the whole page asks once. */
    async ensureCustomCardPortraits(codes: string[]) {
      /* Claimed synchronously, before anything is awaited, so the next row in
       * this same tick adds its own codes to the batch rather than asking for
       * these again. */
      for (const code of codes) {
        if (!isCustomCardCode(code)) continue
        if (customCardFaceKnown(code) || requestedPortraits.has(code)) continue
        requestedPortraits.add(code)
        pendingPortraitCodes.add(code)
      }
      if (pendingPortraitCodes.size === 0) return pendingPortraitFlush ?? Promise.resolve()

      pendingPortraitFlush ??= Promise.resolve().then(async () => {
        const wanted = [...pendingPortraitCodes]
        pendingPortraitCodes = new Set()
        pendingPortraitFlush = null

        try {
          const found = await Api.fetchCustomCardPortraits(wanted)
          const byCode = new Map(found.map((entry) => [stripCardCodePrefix(entry.cardCode), entry]))
          /* Every code asked about gets an entry, including the ones the server
           * had nothing for: "we asked and there is no such card" has to be
           * tellable from "not loaded yet", or a row whose def has been deleted
           * waits forever instead of drawing the placeholder that says so. */
          registerCustomCardPortraits(
            wanted.map(
              (code) =>
                byCode.get(stripCardCodePrefix(code)) ?? {
                  cardCode: code,
                  art: null,
                  portrait: null,
                  portraitBack: null,
                },
            ),
          )
        } catch (error) {
          // Let a later page try again rather than leaving faces permanently blank.
          for (const code of wanted) requestedPortraits.delete(code)
          console.log(error)
        }
      })

      return pendingPortraitFlush
    },

    /* Fold the custom-card registry into `cards`, replacing any older copy of
     * the same code. The registry is read rather than a payload because that is
     * where card codes get normalised. Saving in the card builder calls this, so
     * an open game sees the edited def without being reloaded. */
    syncCustomCards() {
      const defs = customCardDefs()
      const codes = new Set(defs.map((c) => c.cardCode))
      this.cards = [...this.cards.filter((c) => !codes.has(c.cardCode)), ...defs]
    }
  }
})

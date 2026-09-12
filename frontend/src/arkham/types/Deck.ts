import * as JsonDecoder from 'ts.data.json';
import { investigatorClass } from '@/arkham/helpers';
import { v2Optional } from '@/arkham/parser';

export type DeckMeta = string | Record<string, unknown>

interface Meta {
  alternate_front?: string
  card_pool?: string
  [key: string]: unknown
}

export interface ArkhamDbDecklist {
  id: string
  url: string | null
  meta?: DeckMeta
  name: string
  investigator_code: string
  investigator_name: string
  slots: {
    [key: string]: number
  }
  sideSlots?: {
    [key: string]: number
  }
  taboo_id?: number | null
}


/* The deck as it will be played -- the stored list with any overlay applied.
 * The backend computes it so the two never disagree; `list` stays the deck the
 * user actually built, which is what the overlay editor edits against. */
export function deckPlayList(deck: Deck): DeckList {
  return deck.playList ?? deck.list
}

export function deckInvestigator(deck: Deck) {
  const list = deckPlayList(deck)
  if (list.meta) {
    try {
      const result = JSON.parse(list.meta)
      if (result && result.alternate_front) {
        return result.alternate_front
      }
    } catch (_e) { console.log("No parse") }
  }
  return list.investigator_code.replace(/^c/, '')
}

export function deckClass(deck: Deck) {
  const investigator = deckInvestigator(deck)
  if (investigator) {
    return investigatorClass(investigator)
  }

  return {}
}

export type DeckSort = 'name' | 'class' | 'recent'

const CLASS_ORDER = ["guardian", "seeker", "rogue", "mystic", "survivor", "neutral"] as const

/* Shared by the decks page and the in-game picker so the two agree.
 * `recent` puts never-played decks after every played one rather than mixing
 * them in at the bottom of an arbitrary order, and falls back to name so the
 * tail stays stable. */
export function sortDecks(decks: Deck[], sortBy: DeckSort): Deck[] {
  const byName = (a: Deck, b: Deck) => a.name.localeCompare(b.name)

  switch (sortBy) {
    case 'class':
      return [...decks].sort((a, b) => {
        const rank = (d: Deck) => {
          const cls = deckClass(d)
          const idx = CLASS_ORDER.findIndex(k => cls[k])
          return idx === -1 ? CLASS_ORDER.length - 1 : idx
        }
        return rank(a) - rank(b) || byName(a, b)
      })
    case 'recent':
      return [...decks].sort((a, b) => {
        const used = (d: Deck) => d.lastUsedAt ?? ''
        return used(b).localeCompare(used(a)) || byName(a, b)
      })
    default:
      return [...decks].sort(byName)
  }
}

export type DeckList = {
  investigator_code: string;
  slots: Record<string, number>;
  sideSlots?: Record<string, number>;
  meta?: string
  taboo_id?: number
}

export type DeckOverlay = {
  investigator: string | null;
  swaps: Record<string, string>;
  add: Record<string, number>;
  remove: Record<string, number>;
}

export type Deck = {
  id: string;
  name: string;
  url : string | null;
  list: DeckList;
  playList?: DeckList;
  overlay?: DeckOverlay | null;
  /* When the deck was last taken into a game. Null for a deck never played. */
  lastUsedAt?: string | null;
}

export const deckOverlayDecoder = JsonDecoder.object<DeckOverlay>(
  {
    investigator: JsonDecoder.nullable(JsonDecoder.string()),
    swaps: JsonDecoder.record<string>(JsonDecoder.string(), 'Dict<cardcode, cardcode>'),
    add: JsonDecoder.record<number>(JsonDecoder.number(), 'Dict<cardcode, number>'),
    remove: JsonDecoder.record<number>(JsonDecoder.number(), 'Dict<cardcode, number>'),
  },
  'DeckOverlay',
);

export const deckListDecoder = JsonDecoder.object<DeckList>(
  {
    investigator_code: JsonDecoder.string(),
    slots: JsonDecoder.record<number>(JsonDecoder.number(), 'Dict<cardcode, number'),
    sideSlots: v2Optional(JsonDecoder.record<number>(JsonDecoder.number(), 'Dict<cardcode, number')),
    meta: v2Optional(JsonDecoder.string()),
    taboo_id: v2Optional(JsonDecoder.number()),
  },
  'DeckList',
);

export const deckDecoder = JsonDecoder.object<Deck>(
  {
    id: JsonDecoder.string(),
    name: JsonDecoder.string(),
    url: JsonDecoder.nullable(JsonDecoder.string()),
    list: deckListDecoder,
    playList: v2Optional(deckListDecoder),
    overlay: v2Optional(JsonDecoder.nullable(deckOverlayDecoder)),
    lastUsedAt: v2Optional(JsonDecoder.nullable(JsonDecoder.string())),
  },
  'Deck',
);

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
  },
  'Deck',
);

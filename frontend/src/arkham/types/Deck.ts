import * as JsonDecoder from 'ts.data.json';
import { investigatorClass } from '@/arkham/helpers';
import { v2Optional } from '@/arkham/parser';

interface Meta {
  alternate_front: string
}

export interface ArkhamDbDecklist {
  id: string
  url: string | null
  meta?: Meta
  name: string
  investigator_code: string
  investigator_name: string
  slots: {
    [key: string]: number
  }
}


export function deckInvestigator(deck: Deck) {
  if (deck.list.meta) {
    try {
      const result = JSON.parse(deck.list.meta)
      if (result && result.alternate_front) {
        return result.alternate_front
      }
    } catch (_e) { console.log("No parse") }
  }
  return deck.list.investigator_code.replace('c', '')
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
  meta?: string
  taboo_id?: number
}

export type Deck = {
  id: string;
  name: string;
  url : string | null;
  list: DeckList;
}

export const deckListDecoder = JsonDecoder.object<DeckList>(
  {
    investigator_code: JsonDecoder.string(),
    slots: JsonDecoder.record<number>(JsonDecoder.number(), 'Dict<cardcode, number'),
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
  },
  'Deck',
);

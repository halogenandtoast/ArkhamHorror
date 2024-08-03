import { JsonDecoder } from 'ts.data.json';

export type DeckList = {
  investigator_code: string;
  slots: Record<string, number>;
  meta?: string
}

export type Deck = {
  id: string;
  name: string;
  url : string | null;
  list: DeckList;
}

export const deckListDecoder = JsonDecoder.object<DeckList>(
  {
    investigator_code: JsonDecoder.string,
    slots: JsonDecoder.dictionary<number>(JsonDecoder.number, 'Dict<cardcode, number'),
    meta: JsonDecoder.optional(JsonDecoder.string),
  },
  'DeckList',
);

export const deckDecoder = JsonDecoder.object<Deck>(
  {
    id: JsonDecoder.string,
    name: JsonDecoder.string,
    url: JsonDecoder.nullable(JsonDecoder.string),
    list: deckListDecoder,
  },
  'Deck',
);

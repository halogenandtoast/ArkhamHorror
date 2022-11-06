import { JsonDecoder } from 'ts.data.json';

export interface DeckList {
  investigator_code: string;
}

export interface Deck {
  id: string;
  name: string;
  url : string;
  list: DeckList;
}

export const deckListDecoder = JsonDecoder.object<DeckList>(
  {
    investigator_code: JsonDecoder.string,
  },
  'DeckList',
);

export const deckDecoder = JsonDecoder.object<Deck>(
  {
    id: JsonDecoder.string,
    name: JsonDecoder.string,
    url: JsonDecoder.string,
    list: deckListDecoder,
  },
  'Deck',
);

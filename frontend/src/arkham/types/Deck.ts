import { JsonDecoder } from 'ts.data.json';

export interface Deck {
  id: string;
  name: string;
  url : string;
}

export const deckDecoder = JsonDecoder.object<Deck>(
  {
    id: JsonDecoder.string,
    name: JsonDecoder.string,
    url: JsonDecoder.string,
  },
  'Deck',
);

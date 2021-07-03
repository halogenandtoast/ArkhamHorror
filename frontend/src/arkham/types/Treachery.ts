import { JsonDecoder } from 'ts.data.json';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';

export interface TreacheryContents {
  id: string;
  cardDef: CardDef;
  clues?: number;
  resources?: number;
}


export const treacheryContentsDecoder = JsonDecoder.object<TreacheryContents>({
  id: JsonDecoder.string,
  cardDef: cardDefDecoder,
  clues: JsonDecoder.optional(JsonDecoder.number),
  resources: JsonDecoder.optional(JsonDecoder.number),
}, 'TreacheryContents');

export interface Treachery {
  tag: string;
  contents: TreacheryContents;
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  tag: JsonDecoder.string,
  contents: treacheryContentsDecoder,
}, 'Treachery');

import { JsonDecoder } from 'ts.data.json';

export interface TreacheryContents {
  id: string;
  cardCode: string;
  clues?: number;
  resources?: number;
}


export const treacheryContentsDecoder = JsonDecoder.object<TreacheryContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
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

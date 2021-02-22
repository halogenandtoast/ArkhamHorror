import { JsonDecoder } from 'ts.data.json';

export interface TreacheryName {
  title: string;
  subtitle: string | null;
}

export const treacheryNameDecoder = JsonDecoder.object<TreacheryName>(
  {
    title: JsonDecoder.string,
    subtitle: JsonDecoder.nullable(JsonDecoder.string),
  },
  'TreacheryName'
);

export interface TreacheryContents {
  id: string;
  cardCode: string;
  name: TreacheryName;
  clues?: number;
  resources?: number;
}


export const treacheryContentsDecoder = JsonDecoder.object<TreacheryContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  name: treacheryNameDecoder,
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

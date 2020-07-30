import { JsonDecoder } from 'ts.data.json';

export interface TreacheryContents {
  id: string;
  cardCode: string;
  name: string;
}

export const treacheryContentsDecoder = JsonDecoder.object<TreacheryContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  name: JsonDecoder.string,
}, 'TreacheryContents');

export interface Treachery {
  tag: string;
  contents: TreacheryContents;
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  tag: JsonDecoder.string,
  contents: treacheryContentsDecoder,
}, 'Treachery');

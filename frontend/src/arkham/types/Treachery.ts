import { JsonDecoder } from 'ts.data.json';

export interface Treachery {
  id: string;
  cardCode: string;
  clues?: number;
  resources?: number;
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  clues: JsonDecoder.optional(JsonDecoder.number),
  resources: JsonDecoder.optional(JsonDecoder.number),
}, 'Treachery');

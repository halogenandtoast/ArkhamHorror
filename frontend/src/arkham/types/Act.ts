import { JsonDecoder } from 'ts.data.json';

export interface ActSequence {
  number: number;
  side: string;
}

export interface Act {
  id: string;
  clues: number | null;
  deckId: number;
  sequence: ActSequence;
  treacheries: string[];
}

export const actSequenceDecoder = JsonDecoder.
  tuple([JsonDecoder.number, JsonDecoder.string], '[number, string]').
  map(([number, side]) => { return { number, side } })

export const actDecoder = JsonDecoder.object<Act>({
  id: JsonDecoder.string,
  clues: JsonDecoder.nullable(JsonDecoder.number),
  deckId: JsonDecoder.number,
  sequence: actSequenceDecoder,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
}, 'Act');

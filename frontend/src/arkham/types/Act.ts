import { JsonDecoder } from 'ts.data.json';

export interface Act {
  tag: string;
  contents: ActContents;
}

export interface ActSequence {
  number: number;
  side: string;
}

export interface ActContents {
  id: string;
  name: string;
  clues: number | null;
  sequence: ActSequence;
  treacheries: string[];
}

export const actSequenceDecoder = JsonDecoder.
  tuple([JsonDecoder.number, JsonDecoder.string], '[number, string]').
  map(([number, side]) => { return { number, side } })

export const actContentsDecoder = JsonDecoder.object<ActContents>({
  id: JsonDecoder.string,
  name: JsonDecoder.string,
  clues: JsonDecoder.nullable(JsonDecoder.number),
  sequence: actSequenceDecoder,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
}, 'ActContents');

export const actDecoder = JsonDecoder.object<Act>({
  tag: JsonDecoder.string,
  contents: actContentsDecoder,
}, 'Act');

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
  sequence: ActSequence;
}

export const actSequenceDecoder = JsonDecoder.
  tuple([JsonDecoder.number, JsonDecoder.string], '[number, string]').
  map(([number, side]) => { return { number, side } })

export const actContentsDecoder = JsonDecoder.object<ActContents>({
  id: JsonDecoder.string,
  name: JsonDecoder.string,
  sequence: actSequenceDecoder,
}, 'ActContents');

export const actDecoder = JsonDecoder.object<Act>({
  tag: JsonDecoder.string,
  contents: actContentsDecoder,
}, 'Act');

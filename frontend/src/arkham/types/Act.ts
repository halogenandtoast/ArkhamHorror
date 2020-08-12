import { JsonDecoder } from 'ts.data.json';

export interface Act {
  tag: string;
  contents: ActContents;
}

export interface ActContents {
  canAdvance: boolean;
  flipped: boolean;
  id: string;
  name: string;
  sequence: string;
}

export const actContentsDecoder = JsonDecoder.object<ActContents>({
  canAdvance: JsonDecoder.boolean,
  flipped: JsonDecoder.boolean,
  id: JsonDecoder.string,
  name: JsonDecoder.string,
  sequence: JsonDecoder.string,
}, 'Attrs');

export const actDecoder = JsonDecoder.object<Act>({
  tag: JsonDecoder.string,
  contents: actContentsDecoder,
}, 'Act');

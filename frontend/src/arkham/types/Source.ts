import { JsonDecoder } from 'ts.data.json';

export interface Source {
  tag: string;
  contents?: string;
}

export const sourceDecoder = JsonDecoder.object<Source>(
  {
    tag: JsonDecoder.string,
    contents: JsonDecoder.optional(JsonDecoder.string),
  },
  'Source',
);

import { JsonDecoder } from 'ts.data.json';

export interface Source {
  tag: string;
  contents: string | null;
}

export const sourceDecoder = JsonDecoder.object<Source>(
  {
    tag: JsonDecoder.string,
    contents: JsonDecoder.nullable(JsonDecoder.string),
  },
  'Source',
);

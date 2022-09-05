import { JsonDecoder } from 'ts.data.json'

export interface Target {
  tag: string
  contents?: string
}

export const targetDecoder = JsonDecoder.object<Target>(
  {
    tag: JsonDecoder.string,
    contents: JsonDecoder.optional(JsonDecoder.string),
  },
  'Target',
);

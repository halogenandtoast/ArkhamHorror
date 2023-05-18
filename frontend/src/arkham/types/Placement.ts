import { JsonDecoder } from 'ts.data.json';

export type Placement = { tag: "InThreatArea", contents: string } | { tag: string }

export const placementDecoder = JsonDecoder.oneOf<Placement>([
  JsonDecoder.object<Placement>({ tag: JsonDecoder.constant("InThreatArea"), contents: JsonDecoder.string }, 'InThreatArea')
  , JsonDecoder.object<Placement>({ tag: JsonDecoder.string }, 'Placement'),
], 'Placement')

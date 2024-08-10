import { JsonDecoder } from 'ts.data.json';

export type Placement
  = { tag: "InThreatArea", contents: string }
  | { tag: "StillInHand", contents: string }
  | { tag: "HiddenInHand", contents: string }
  | { tag: "OnTopOfDeck", contents: string }
  | { tag: "AsSwarm", swarmHost: string }
  | { tag: "VoidZone" }
  | { tag: "NextToAgenda" }
  | { tag: "OtherPlacement", contents: string }

export const placementDecoder = JsonDecoder.oneOf<Placement>([
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("AsSwarm"), swarmHost: JsonDecoder.string }, 'AsSwarm'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("NextToAgenda")}, 'NextToAgenda'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("InThreatArea"), contents: JsonDecoder.string }, 'InThreatArea'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("StillInHand"), contents: JsonDecoder.string }, 'StillInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("HiddenInHand"), contents: JsonDecoder.string }, 'HiddenInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("OnTopOfDeck"), contents: JsonDecoder.string }, 'OnTopOfDeck'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("OutOfPlay"), contents: JsonDecoder.isExactly("VoidZone")}, 'VoidZone').map(() => ({ tag: "VoidZone" })),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.constant("OtherPlacement"), contents: JsonDecoder.string }, 'Placement', { contents: 'tag' }),
], 'Placement')

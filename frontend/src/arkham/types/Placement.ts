import { JsonDecoder } from 'ts.data.json';
import { cardDecoder, Card } from '@/arkham/types/Card';

export type Placement
  = { tag: "InThreatArea", contents: string }
  | { tag: "StillInHand", contents: string }
  | { tag: "HiddenInHand", contents: string }
  | { tag: "OnTopOfDeck", contents: string }
  | { tag: "OutOfPlay", contents: string }
  | { tag: "AtLocation", contents: number }
  | { tag: "InVehicle", contents: number }
  | { tag: "AttachedToLocation", contents: number }
  | { tag: "AsSwarm", swarmHost: number, swarmCard: Card }
  | { tag: "Limbo" }
  | { tag: "NextToAgenda" }
  | { tag: "OtherPlacement", contents: string | number }

export const placementDecoder = JsonDecoder.oneOf<Placement>([
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("AsSwarm"), swarmHost: JsonDecoder.number, swarmCard: cardDecoder }, 'AsSwarm'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("NextToAgenda")}, 'NextToAgenda'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("Limbo")}, 'Limbo'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("AtLocation"), contents: JsonDecoder.number }, 'AtLocation'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("InVehicle"), contents: JsonDecoder.number }, 'InVehicle'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("AttachedToLocation"), contents: JsonDecoder.number }, 'AttachedToLocation'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("InThreatArea"), contents: JsonDecoder.string }, 'InThreatArea'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("StillInHand"), contents: JsonDecoder.string }, 'StillInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("HiddenInHand"), contents: JsonDecoder.string }, 'HiddenInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("OnTopOfDeck"), contents: JsonDecoder.string }, 'OnTopOfDeck'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("OutOfPlay"), contents: JsonDecoder.string}, 'OutOfPlay'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.constant("OtherPlacement"), contents: JsonDecoder.oneOf([JsonDecoder.string, JsonDecoder.number], 'OtherPlacement.contents') }, 'Placement', { contents: 'tag' }),
], 'Placement')

import { JsonDecoder } from 'ts.data.json';
import { cardDecoder, Card } from '@/arkham/types/Card';

export type Placement
  = { tag: "InThreatArea", contents: string }
  | { tag: "StillInHand", contents: string }
  | { tag: "HiddenInHand", contents: string }
  | { tag: "OnTopOfDeck", contents: string }
  | { tag: "OutOfPlay", contents: string }
  | { tag: "AtLocation", contents: string }
  | { tag: "InVehicle", contents: string }
  | { tag: "AttachedToLocation", contents: string }
  | { tag: "AsSwarm", swarmHost: string, swarmCard: Card }
  | { tag: "Limbo" }
  | { tag: "NextToAgenda" }
  | { tag: "OtherPlacement", contents: string }

export const placementDecoder = JsonDecoder.oneOf<Placement>([
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("AsSwarm"), swarmHost: JsonDecoder.string, swarmCard: cardDecoder }, 'AsSwarm'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("NextToAgenda")}, 'NextToAgenda'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("Limbo")}, 'Limbo'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("AtLocation"), contents: JsonDecoder.string }, 'AtLocation'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("InVehicle"), contents: JsonDecoder.string }, 'InVehicle'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("AttachedToLocation"), contents: JsonDecoder.string }, 'AttachedToLocation'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("InThreatArea"), contents: JsonDecoder.string }, 'InThreatArea'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("StillInHand"), contents: JsonDecoder.string }, 'StillInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("HiddenInHand"), contents: JsonDecoder.string }, 'HiddenInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.isExactly("OnTopOfDeck"), contents: JsonDecoder.string }, 'OnTopOfDeck'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("OutOfPlay"), contents: JsonDecoder.string}, 'OutOfPlay'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.constant("OtherPlacement"), contents: JsonDecoder.string }, 'Placement', { contents: 'tag' }),
], 'Placement')

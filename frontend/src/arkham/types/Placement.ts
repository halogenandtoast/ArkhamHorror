import * as JsonDecoder from 'ts.data.json';
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
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("AsSwarm"), swarmHost: JsonDecoder.string(), swarmCard: cardDecoder }, 'AsSwarm'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("NextToAgenda")}, 'NextToAgenda'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("Limbo")}, 'Limbo'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("AtLocation"), contents: JsonDecoder.string() }, 'AtLocation'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("InVehicle"), contents: JsonDecoder.string() }, 'InVehicle'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("AttachedToLocation"), contents: JsonDecoder.string() }, 'AttachedToLocation'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("InThreatArea"), contents: JsonDecoder.string() }, 'InThreatArea'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("StillInHand"), contents: JsonDecoder.string() }, 'StillInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("HiddenInHand"), contents: JsonDecoder.string() }, 'HiddenInHand'),
  JsonDecoder.object<Placement>({ tag: JsonDecoder.literal("OnTopOfDeck"), contents: JsonDecoder.string() }, 'OnTopOfDeck'),
  JsonDecoder.object({ tag: JsonDecoder.literal("OutOfPlay"), contents: JsonDecoder.string()}, 'OutOfPlay'),
  JsonDecoder.object({ tag: JsonDecoder.string() }, 'OtherPlacement').map(({tag}) => ({ tag: "OtherPlacement", contents: tag }))
], 'Placement')

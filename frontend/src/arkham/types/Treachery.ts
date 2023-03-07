import { JsonDecoder } from 'ts.data.json';
import { Target, targetDecoder } from '@/arkham/types/Target';

export type TreacheryPlacement =
  { tag: "TreacheryAttachedTo", contents: Target }
  | { tag: "TreacheryInHandOf", contents: string }
  | { tag: "TreacheryNextToAct" }
  | { tag: "TreacheryLimbo" }

export const treacheryPlacementDecoder = JsonDecoder.oneOf<TreacheryPlacement>(
  [ JsonDecoder.object({ tag: JsonDecoder.isExactly('TreacheryAttachedTo'), contents: targetDecoder }, 'TreacheryAttachedTo')
  , JsonDecoder.object({ tag: JsonDecoder.isExactly('TreacheryInHandOf'), contents: JsonDecoder.string }, 'TreacheryInHandOf')
  , JsonDecoder.object({ tag: JsonDecoder.isExactly('TreacheryNextToAct') }, 'TreacheryNextToAct')
  , JsonDecoder.object({ tag: JsonDecoder.isExactly('TreacheryLimbo') }, 'TreacheryLimbo')
  ], 'TreacheryPlacement')

export interface Treachery {
  id: string;
  cardCode: string;
  clues?: number;
  horror?: number;
  resources?: number;
  doom?: number;
  placement: TreacheryPlacement;
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  clues: JsonDecoder.optional(JsonDecoder.number),
  horror: JsonDecoder.optional(JsonDecoder.number),
  resources: JsonDecoder.optional(JsonDecoder.number),
  doom: JsonDecoder.optional(JsonDecoder.number),
  placement: treacheryPlacementDecoder,
}, 'Treachery');

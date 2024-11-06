import { JsonDecoder } from 'ts.data.json';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';

export type ActSequence = {
  number: number
  side: string
}

export type Act = {
  id: string
  clues: number | null
  deckId: number
  sequence: ActSequence
  treacheries: string[]
  breaches: number | null;
  keys: ArkhamKey[];
}

export const actSequenceDecoder = JsonDecoder.
  tuple([JsonDecoder.number, JsonDecoder.string], '[number, string]').
  map(([number, side]) => { return { number, side } })

export const actDecoder = JsonDecoder.object<Act>({
  id: JsonDecoder.string,
  clues: JsonDecoder.nullable(JsonDecoder.number),
  deckId: JsonDecoder.number,
  sequence: actSequenceDecoder,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  breaches: JsonDecoder.nullable(JsonDecoder.number),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
}, 'Act');

import * as JsonDecoder from 'ts.data.json';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';

export type ActSequence = {
  number: number
  side: string
}

export type Act = {
  id: string
  tokens: Tokens
  deckId: number
  sequence: ActSequence
  treacheries: string[]
  breaches: number | null;
  keys: ArkhamKey[];
}

export const actSequenceDecoder = JsonDecoder.
  tuple([JsonDecoder.number(), JsonDecoder.string()], '[number, string]').
  map(([number, side]) => { return { number, side } })

export const actDecoder = JsonDecoder.object<Act>({
  id: JsonDecoder.string(),
  tokens: tokensDecoder,
  deckId: JsonDecoder.number(),
  sequence: actSequenceDecoder,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string(), 'TreacheryId[]'),
  breaches: JsonDecoder.nullable(JsonDecoder.number()),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
}, 'Act');

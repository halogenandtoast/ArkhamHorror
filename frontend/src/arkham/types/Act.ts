import * as JsonDecoder from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { v2Optional } from '@/arkham/parser';

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
  cardsUnderneath: Card[];
  breaches: number | null;
  keys: ArkhamKey[];
  // Presence only; the value is never read.
  advanceCost?: unknown;
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
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'UnderneathCard[]'),
  breaches: JsonDecoder.nullable(JsonDecoder.number()),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  advanceCost: v2Optional(JsonDecoder.succeed()),
}, 'Act');

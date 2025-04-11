import * as JsonDecoder from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Customization, customizationsDecoder } from '@/arkham/types/Customization';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { v2Optional } from '@/arkham/parser';
import {
  Card,
  cardDecoder,
} from '@/arkham/types/Card';

export type Event = {
  id: string;
  cardCode: string;
  cardId: string;
  doom: number;
  exhausted: boolean;
  sealedChaosTokens: ChaosToken[];
  cardsUnderneath: Card[];
  tokens: Tokens;
  customizations: Customization[];
  mutated?: string;
  placement: Placement;
}

export const eventDecoder = JsonDecoder.object<Event>({
  id: JsonDecoder.string(),
  cardCode: JsonDecoder.string(),
  cardId: JsonDecoder.string(),
  doom: JsonDecoder.number(),
  exhausted: JsonDecoder.boolean(),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  tokens: tokensDecoder,
  customizations: customizationsDecoder,
  mutated: v2Optional(JsonDecoder.string()),
  placement: placementDecoder,
}, 'Event');

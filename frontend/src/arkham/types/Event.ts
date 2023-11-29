import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import {
  Card,
  cardDecoder,
} from '@/arkham/types/Card';

export type Event = {
  id: string;
  cardCode: string;
  cardId: string;
  doom: number;
  sealedChaosTokens: ChaosToken[];
  cardsUnderneath: Card[];
}

export const eventDecoder = JsonDecoder.object<Event>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  cardId: JsonDecoder.string,
  doom: JsonDecoder.number,
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
}, 'Event');

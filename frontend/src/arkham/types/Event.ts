import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export interface Event {
  id: string;
  cardCode: string;
  cardId: string;
  doom: number;
  sealedChaosTokens: ChaosToken[];
}

export const eventDecoder = JsonDecoder.object<Event>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  cardId: JsonDecoder.string,
  doom: JsonDecoder.number,
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
}, 'Event');

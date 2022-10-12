import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';

export interface Event {
  id: string;
  cardCode: string;
  doom: number;
  sealedTokens: ChaosToken[];
}

export const eventDecoder = JsonDecoder.object<Event>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  doom: JsonDecoder.number,
  sealedTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
}, 'Event');

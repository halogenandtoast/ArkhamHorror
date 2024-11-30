import { JsonDecoder } from 'ts.data.json';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Placement, placementDecoder } from '@/arkham/types/Placement';

export type Treachery = {
  id: number;
  cardId: number;
  cardCode: string;
  tokens: Tokens;
  drawnBy: string
  peril: boolean
  placement: Placement;
  sealedChaosTokens: ChaosToken[];
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  id: JsonDecoder.number,
  cardId: JsonDecoder.number,
  cardCode: JsonDecoder.string,
  drawnBy: JsonDecoder.string,
  tokens: tokensDecoder,
  placement: placementDecoder,
  peril: JsonDecoder.boolean,
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
}, 'Treachery');

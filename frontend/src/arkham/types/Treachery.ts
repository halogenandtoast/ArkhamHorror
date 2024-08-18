import { JsonDecoder } from 'ts.data.json';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Placement, placementDecoder } from '@/arkham/types/Placement';

export type Treachery = {
  id: string;
  cardId: string;
  cardCode: string;
  tokens: Tokens;
  drawnBy: string
  peril: boolean
  placement: Placement;
  sealedChaosTokens: ChaosToken[];
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  id: JsonDecoder.string,
  cardId: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  drawnBy: JsonDecoder.string,
  tokens: tokensDecoder,
  placement: placementDecoder,
  peril: JsonDecoder.boolean,
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
}, 'Treachery');

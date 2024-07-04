import { JsonDecoder } from 'ts.data.json';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Placement, placementDecoder } from '@/arkham/types/Placement';

export type Treachery = {
  id: string;
  cardId: string;
  cardCode: string;
  tokens: Tokens;
  placement: Placement;
  sealedChaosTokens: ChaosToken[];
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  id: JsonDecoder.string,
  cardId: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  tokens: tokensDecoder,
  placement: placementDecoder,
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
}, 'Treachery');

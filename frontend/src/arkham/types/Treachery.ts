import * as JsonDecoder from 'ts.data.json';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

export type Treachery = {
  id: string;
  cardId: string;
  cardCode: string;
  tokens: Tokens;
  drawnBy: string
  peril: boolean
  placement: Placement;
  sealedChaosTokens: ChaosToken[];
  exhausted: boolean;
  /* Treacheries attached to this one. Rare, but Unleashed Chaos (Mutation)
   * attaches to whatever encounter card its revelation turned up. */
  treacheries: string[];
  modifiers: Modifier[];
}

export const treacheryDecoder = JsonDecoder.object<Treachery>({
  id: JsonDecoder.string(),
  cardId: JsonDecoder.string(),
  cardCode: JsonDecoder.string(),
  drawnBy: JsonDecoder.string(),
  tokens: tokensDecoder,
  placement: placementDecoder,
  peril: JsonDecoder.boolean(),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  exhausted: JsonDecoder.boolean(),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string(), 'TreacheryId[]'),
  modifiers: JsonDecoder.failover([], JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
}, 'Treachery');

import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { ChaosToken, TokenFace, tokenFaceDecoder } from '@/arkham/types/ChaosToken';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';

export type Stability = 'Stable' | 'Unstable'

export type ScarletKey = {
  id: string
  cardId: string
  placement: Placement
  stability: Stability
}


export const scarletKeyDecoder = JsonDecoder.object<ScarletKey>({
  id: JsonDecoder.string(),
  cardId: JsonDecoder.string(),
  placement: placementDecoder,
  stability: JsonDecoder.oneOf<Stability>([
    JsonDecoder.literal('Stable'),
    JsonDecoder.literal('Unstable'),
  ], 'Stability'),
}, 'ScarletKey');

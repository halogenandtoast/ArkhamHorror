import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { ChaosToken, chaosTokenDecoder, tokenFaceDecoder } from '@/arkham/types/ChaosToken';
import { type TokenBagMeta, bagTokenDecoder } from '@/arkham/types/TokenBag';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

type StoryMeta = TokenBagMeta & {
  infestationTokens?: ChaosToken[]
  infestationSetAside?: ChaosToken[]
  infestationCurrentToken?: ChaosToken | null
  predationTokens?: ChaosToken[]
  predationSetAside?: ChaosToken[]
  predationCurrentToken?: ChaosToken | null
  predationCancelNext?: boolean
  crossedOff?: string[]
}

const bagTokensDecoder = v2Optional(JsonDecoder.array<ChaosToken>(bagTokenDecoder, 'BagToken[]'));
const currentBagTokenDecoder = v2Optional(JsonDecoder.nullable(bagTokenDecoder));

export const storyMetaDecoder = JsonDecoder.object<StoryMeta>({
  bagTokens: bagTokensDecoder,
  bagSetAside: bagTokensDecoder,
  bagCurrentToken: currentBagTokenDecoder,
  bagCancelNext: v2Optional(JsonDecoder.boolean()),
  bagDebugNext: v2Optional(JsonDecoder.nullable(tokenFaceDecoder)),
  infestationTokens: bagTokensDecoder,
  infestationSetAside: bagTokensDecoder,
  infestationCurrentToken: currentBagTokenDecoder,
  predationTokens: bagTokensDecoder,
  predationSetAside: bagTokensDecoder,
  predationCurrentToken: currentBagTokenDecoder,
  predationCancelNext: v2Optional(JsonDecoder.boolean()),
  crossedOff: v2Optional(JsonDecoder.array<string>(JsonDecoder.string(), 'string[]'))
}, 'StoryMeta');

const optionalStoryMetaDecoder: JsonDecoder.Decoder<StoryMeta | undefined> = JsonDecoder.succeed().flatMap((value: unknown) => {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    return storyMetaDecoder
  }

  return JsonDecoder.constant(undefined)
})

export type Story = {
  id: string
  art: string
  flippedArt: string
  cardId: string
  placement: Placement
  otherSide: Target | null
  flipped: boolean
  meta?: StoryMeta
  tokens: Tokens;
  sealedChaosTokens: ChaosToken[];
  modifiers: Modifier[];
}

export const storyDecoder = JsonDecoder.object<Story>({
  id: JsonDecoder.string(),
  art: JsonDecoder.string(),
  flippedArt: JsonDecoder.string(),
  cardId: JsonDecoder.string(),
  placement: placementDecoder,
  otherSide: JsonDecoder.nullable(targetDecoder),
  flipped: JsonDecoder.boolean(),
  meta: optionalStoryMetaDecoder,
  tokens: tokensDecoder,
  sealedChaosTokens: JsonDecoder.fallback([], JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]')),
  modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
}, 'Story');

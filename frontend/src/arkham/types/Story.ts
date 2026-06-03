import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { ChaosToken, TokenFace, tokenFaceDecoder } from '@/arkham/types/ChaosToken';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

export type InfestationToken = {
  infestationTokenId: string
  infestationTokenFace: TokenFace
}

export const infestationAsChaosToken = (infestationToken: InfestationToken): ChaosToken => {
  return {
    id: infestationToken.infestationTokenId,
    face: infestationToken.infestationTokenFace
  }
}

export type PredationToken = {
  predationTokenId: string
  predationTokenFace: TokenFace
}

export const predationAsChaosToken = (predationToken: PredationToken): ChaosToken => {
  return {
    id: predationToken.predationTokenId,
    face: predationToken.predationTokenFace
  }
}

type StoryMeta = {
  infestationTokens?: InfestationToken[]
  infestationSetAside?: InfestationToken[]
  infestationCurrentToken?: InfestationToken | null
  predationTokens?: PredationToken[]
  predationSetAside?: PredationToken[]
  predationCurrentToken?: PredationToken | null
  crossedOff?: string[]
}

export const infestationTokenDecoder = JsonDecoder.object<InfestationToken>({
  infestationTokenFace: tokenFaceDecoder,
  infestationTokenId: JsonDecoder.string()
}, 'InfestationToken');

export const predationTokenDecoder = JsonDecoder.object<PredationToken>({
  predationTokenFace: tokenFaceDecoder,
  predationTokenId: JsonDecoder.string()
}, 'PredationToken');

export const storyMetaDecoder = JsonDecoder.object<StoryMeta>({
  infestationTokens: v2Optional(JsonDecoder.array<InfestationToken>(infestationTokenDecoder, 'InfestationToken[]')),
  infestationSetAside: v2Optional(JsonDecoder.array<InfestationToken>(infestationTokenDecoder, 'InfestationToken[]')),
  infestationCurrentToken: v2Optional(JsonDecoder.nullable(infestationTokenDecoder)),
  predationTokens: v2Optional(JsonDecoder.array<PredationToken>(predationTokenDecoder, 'PredationToken[]')),
  predationSetAside: v2Optional(JsonDecoder.array<PredationToken>(predationTokenDecoder, 'PredationToken[]')),
  predationCurrentToken: v2Optional(JsonDecoder.nullable(predationTokenDecoder)),
  crossedOff: v2Optional(JsonDecoder.array<string>(JsonDecoder.string(), 'string[]'))
}, 'StoryMeta');

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
  meta: v2Optional(storyMetaDecoder),
  tokens: tokensDecoder,
  modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
}, 'Story');

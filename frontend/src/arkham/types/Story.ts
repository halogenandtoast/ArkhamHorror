import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { ChaosToken, TokenFace, tokenFaceDecoder } from '@/arkham/types/ChaosToken';

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

type StoryMeta = {
  infestationSetAside?: InfestationToken[]
  crossedOff?: string[]
}

export const infestationTokenDecoder = JsonDecoder.object<InfestationToken>({
  infestationTokenFace: tokenFaceDecoder,
  infestationTokenId: JsonDecoder.string()
}, 'InfestationToken');

export const storyMetaDecoder = JsonDecoder.object<StoryMeta>({
  infestationSetAside: v2Optional(JsonDecoder.array<InfestationToken>(infestationTokenDecoder, 'InfestationToken[]')),
  crossedOff: v2Optional(JsonDecoder.array<string>(JsonDecoder.string(), 'string[]'))
}, 'StoryMeta');

export type Story = {
  id: string
  cardId: string
  placement: Placement
  otherSide: Target | null
  flipped: boolean
  meta?: StoryMeta
}

export const storyDecoder = JsonDecoder.object<Story>({
  id: JsonDecoder.string(),
  cardId: JsonDecoder.string(),
  placement: placementDecoder,
  otherSide: JsonDecoder.nullable(targetDecoder),
  flipped: JsonDecoder.boolean(),
  meta: v2Optional(storyMetaDecoder)
}, 'Story');

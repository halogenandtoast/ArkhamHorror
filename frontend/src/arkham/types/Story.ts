import { JsonDecoder } from 'ts.data.json';
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
}

export const infestationTokenDecoder = JsonDecoder.object<InfestationToken>({
  infestationTokenFace: tokenFaceDecoder,
  infestationTokenId: JsonDecoder.string
}, 'InfestationToken');

export const storyMetaDecoder = JsonDecoder.object<StoryMeta>({
  infestationSetAside: JsonDecoder.optional(JsonDecoder.array<InfestationToken>(infestationTokenDecoder, 'InfestationToken[]'))
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
  id: JsonDecoder.string,
  cardId: JsonDecoder.string,
  placement: placementDecoder,
  otherSide: JsonDecoder.nullable(targetDecoder),
  flipped: JsonDecoder.boolean,
  meta: JsonDecoder.optional(storyMetaDecoder)
}, 'Story');

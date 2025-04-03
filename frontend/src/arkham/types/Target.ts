import * as JsonDecoder from 'ts.data.json'
import { Ability, abilityDecoder } from '@/arkham/types/Ability';
import { v2Optional } from '@/arkham/parser'

type TargetContents = string | { face: string, id: string } | { ability: Ability }

export type Target = {
  tag: string
  contents?: TargetContents
}

export const targetDecoder = JsonDecoder.object<Target>(
  {
    tag: JsonDecoder.string(),
    contents: v2Optional(
      JsonDecoder.oneOf<TargetContents>(
        [ JsonDecoder.string(),
          JsonDecoder.object({ chaosTokenFace: JsonDecoder.string(), chaosTokenId: JsonDecoder.string() }, 'Token').
            map(({chaosTokenFace, chaosTokenId}) => ({ face: chaosTokenFace, id: chaosTokenId })),
          JsonDecoder.tuple([JsonDecoder.string(), abilityDecoder], 'Ability').map(([, ability]) => ({ ability })),
        ], 'TargetContents')
    ),
  },
  'Target',
);

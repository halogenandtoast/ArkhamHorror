import * as JsonDecoder from 'ts.data.json'
import { AbilityRef } from '@/arkham/types/Ability';
import { Source, sourceDecoder } from '@/arkham/types/Source';
import { v2Optional } from '@/arkham/parser'

type TargetContents = string | { face: string, id: string } | { ability: AbilityRef }

export type Target = {
  tag: string
  contents?: TargetContents
}

export const targetDecoder = JsonDecoder.object<Target>(
  {
    tag: JsonDecoder.string(),
    contents: v2Optional(
      JsonDecoder.oneOf<TargetContents>(
        [ JsonDecoder.string()
        , JsonDecoder.object({ chaosTokenFace: JsonDecoder.string(), chaosTokenId: JsonDecoder.string() }, 'Token').map(({chaosTokenFace, chaosTokenId}) => ({ face: chaosTokenFace, id: chaosTokenId }))
        , JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.tuple([sourceDecoder, JsonDecoder.number()], 'inner')], 'Target').map(([,[source, index]]) => ({ ability: { source, index } }))
        ], 'TargetContents')
    ),
  },
  'Target',
);

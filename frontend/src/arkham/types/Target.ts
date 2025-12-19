import * as JsonDecoder from 'ts.data.json'
import { AbilityRef } from '@/arkham/types/Ability';
import { Source, sourceDecoder } from '@/arkham/types/Source';
import { v2Optional } from '@/arkham/parser'

type TargetContents = string | { face: string, id: string } | { ability: AbilityRef }

export type Target = {
  tag: string
  contents?: TargetContents
}

const  targetContentsDecoder: JsonDecoder.Decoder<TargetContents> = JsonDecoder.oneOf<TargetContents>(
  [ JsonDecoder.string()
  , JsonDecoder.object({ chaosTokenFace: JsonDecoder.string(), chaosTokenId: JsonDecoder.string() }, 'Token').map(({chaosTokenFace, chaosTokenId}) => ({ face: chaosTokenFace, id: chaosTokenId }))
  , JsonDecoder.tuple([
      JsonDecoder.string(),
      JsonDecoder.tuple([sourceDecoder, JsonDecoder.number()], 'inner')
    ], 'Target').map(([,[source, index]]) => ({ ability: { source, index } }))
  ], 'TargetContents'
);

export const targetDecoder: JsonDecoder.Decoder<Target> = JsonDecoder.lazy(() =>
  JsonDecoder.oneOf<Target>([
    JsonDecoder.object<Target>(
      {
        tag: JsonDecoder.literal('ProxyTarget'),
        contents: JsonDecoder.array<Target>(targetDecoder, 'Target[]').map(arr => arr.length > 0 ? arr[0].contents : undefined),
      },
      'ProxyTarget'
    ),
    JsonDecoder.object<Target>(
      {
        tag: JsonDecoder.string(),
        contents: v2Optional(targetContentsDecoder),
      },
      'Target'
    ),
  ],
  'Target')
);

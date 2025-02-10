import { JsonDecoder } from 'ts.data.json'
import { Ability, abilityDecoder } from '@/arkham/types/Ability';

type TargetContents = string | { face: string, id: string } | { ability: Ability }

export type Target = {
  tag: string
  contents?: TargetContents
}


export const targetDecoder = JsonDecoder.object<Target>(
  {
    tag: JsonDecoder.string,
    contents: JsonDecoder.optional(
      JsonDecoder.oneOf<TargetContents>(
        [ JsonDecoder.string,
          JsonDecoder.object<TargetContents>({ face: JsonDecoder.string, id: JsonDecoder.string }, 'Token', { face: 'chaosTokenFace', id: 'chaosTokenId' }),
          JsonDecoder.tuple([JsonDecoder.string, abilityDecoder], 'Ability').map(([, ability]) => ({ ability })),
        ], 'TargetContents')
    ),
  },
  'Target',
);

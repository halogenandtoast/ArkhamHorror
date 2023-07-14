import { JsonDecoder } from 'ts.data.json'

type TargetContents = string | { face: string, id: string }

export interface Target {
  tag: string
  contents?: TargetContents
}


export const targetDecoder = JsonDecoder.object<Target>(
  {
    tag: JsonDecoder.string,
    contents: JsonDecoder.optional(
      JsonDecoder.oneOf<TargetContents>(
        [ JsonDecoder.string,
          JsonDecoder.object<TargetContents>({ face: JsonDecoder.string, id: JsonDecoder.string }, 'Token', { face: 'chaosTokenFace', id: 'chaosTokenId' })
        ], 'TargetContents')
    ),
  },
  'Target',
);

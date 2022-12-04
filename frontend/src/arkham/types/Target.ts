import { JsonDecoder } from 'ts.data.json'

type TargetContents = string | { tokenFace: string, tokenId: string }

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
          JsonDecoder.object({ tokenFace: JsonDecoder.string, tokenId: JsonDecoder.string }, 'Token')
        ], 'TargetContents')
    ),
  },
  'Target',
);

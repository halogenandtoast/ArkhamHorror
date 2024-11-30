import { JsonDecoder } from 'ts.data.json'

type TargetContents = string | number | { face: string, id: number }

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
          JsonDecoder.number,
          JsonDecoder.object<TargetContents>({ face: JsonDecoder.string, id: JsonDecoder.number }, 'Token', { face: 'chaosTokenFace', id: 'chaosTokenId' }),

          JsonDecoder.succeed.chain((f) => {
            console.log('Failed to decode message', f)
            return JsonDecoder.fail(f)
          })
        ], 'TargetContents')
    ),
  },
  'Target',
);

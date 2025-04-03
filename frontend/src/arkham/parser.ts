import * as JsonDecoder from 'ts.data.json';

export function v2Optional<T>(decoder: JsonDecoder.Decoder<T>) {
  return JsonDecoder.oneOf([decoder, JsonDecoder.null().map(() => undefined), JsonDecoder.undefined()], 'optional');
}

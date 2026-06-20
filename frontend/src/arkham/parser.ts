import * as JsonDecoder from 'ts.data.json';

export function v2Optional<T>(decoder: JsonDecoder.Decoder<T>) {
  return JsonDecoder.oneOf([decoder, JsonDecoder.null().map(() => undefined), JsonDecoder.undefined()], 'optional');
}

// Like a decoder, but yields `def` when the key is absent or null. Used so a
// slimmed payload (omitted empty/default fields) still decodes to full objects.
export function withDefault<T, D = T>(def: D, decoder: JsonDecoder.Decoder<T>) {
  return JsonDecoder.oneOf<T | D>([
    decoder,
    JsonDecoder.null().map(() => def),
    JsonDecoder.undefined().map(() => def),
  ], 'withDefault');
}

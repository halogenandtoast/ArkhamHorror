import { JsonDecoder } from 'ts.data.json'

export interface ProxySource {
  sourceTag: "ProxySource"
  tag: "ProxySource"
  source: Source
  originalSource: Source
}

export interface OtherSource {
  sourceTag: "OtherSource"
  tag: string
  contents?: string
}

export type Source = ProxySource | OtherSource

export const proxySourceDecoder: JsonDecoder.Decoder<ProxySource> = JsonDecoder.object<ProxySource>({
  tag: JsonDecoder.isExactly("ProxySource"),
  sourceTag: JsonDecoder.constant("ProxySource"),
  source: JsonDecoder.lazy<Source>(() => sourceDecoder),
  originalSource: JsonDecoder.lazy<Source>(() => sourceDecoder),
}, 'ProxySource')

export const otherSourceDecoder = JsonDecoder.object<OtherSource>(
  {
    sourceTag: JsonDecoder.constant("OtherSource"),
    tag: JsonDecoder.string,
    contents: JsonDecoder.optional(JsonDecoder.string),
  },
  'OtherSource'
)

export const sourceDecoder = JsonDecoder.oneOf<Source>([
  proxySourceDecoder,
  otherSourceDecoder
], 'Source')

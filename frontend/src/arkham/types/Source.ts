import { JsonDecoder } from 'ts.data.json'
import { TarotCard, tarotCardDecoder } from '@/arkham/types/TarotCard'

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

export interface TarotSource {
  sourceTag: "TarotSource"
  tag: "TarotSource"
  contents: TarotCard
}

export type Source = ProxySource | TarotSource | OtherSource

export const proxySourceDecoder: JsonDecoder.Decoder<ProxySource> = JsonDecoder.object<ProxySource>({
  tag: JsonDecoder.isExactly("ProxySource"),
  sourceTag: JsonDecoder.constant("ProxySource"),
  source: JsonDecoder.lazy<Source>(() => sourceDecoder),
  originalSource: JsonDecoder.lazy<Source>(() => sourceDecoder),
}, 'ProxySource')

export const tarotSourceDecoder: JsonDecoder.Decoder<TarotSource> = JsonDecoder.object<TarotSource>({
  tag: JsonDecoder.isExactly("TarotSource"),
  sourceTag: JsonDecoder.constant("TarotSource"),
  contents: tarotCardDecoder
}, 'TarotSource')

export const otherSourceDecoder: JsonDecoder.Decoder<OtherSource> = JsonDecoder.object<OtherSource>(
  {
    sourceTag: JsonDecoder.constant("OtherSource"),
    tag: JsonDecoder.string,
    contents: JsonDecoder.lazy<string>(() => JsonDecoder.oneOf(
      [ JsonDecoder.
          tuple([sourceDecoder, JsonDecoder.number], 'proxySource').
          chain(([source,]) => "contents" in source ? JsonDecoder.succeed : JsonDecoder.fail("missing contents"))
      , JsonDecoder.optional(JsonDecoder.string)
      ], 'OtherSource.contents'))
  },
  'OtherSource'
)

export const sourceDecoder = JsonDecoder.oneOf<Source>([
  proxySourceDecoder,
  tarotSourceDecoder,
  otherSourceDecoder
], 'Source')

import * as JsonDecoder from 'ts.data.json'
import { TarotCard, tarotCardDecoder } from '@/arkham/types/TarotCard'
import { v2Optional } from '@/arkham/parser'

export function sameSource (a: Source | undefined, b: Source | undefined): boolean {
  if (a === undefined || b === undefined) return false
  return sourceKey(a) === sourceKey(b)
}

export function sourceKey (source: Source): string | undefined {
  switch (source.sourceTag) {
    case "ProxySource":
      return sourceKey(source.source)
    case "TarotSource":
      return source.contents.arcana
    case "OtherSource":
      return source.contents
  }
}

export type ProxySource = {
  sourceTag: "ProxySource"
  tag: "ProxySource"
  source: Source
  originalSource: Source
}

export type OtherSource = {
  sourceTag: "OtherSource"
  tag: string
  contents?: string
}

export type TarotSource = {
  sourceTag: "TarotSource"
  tag: "TarotSource"
  contents: TarotCard
}

export type Source = ProxySource | TarotSource | OtherSource

export const proxySourceDecoder: JsonDecoder.Decoder<ProxySource> = JsonDecoder.object<ProxySource>({
  tag: JsonDecoder.literal("ProxySource"),
  sourceTag: JsonDecoder.constant("ProxySource"),
  source: JsonDecoder.lazy<Source>(() => sourceDecoder),
  originalSource: JsonDecoder.lazy<Source>(() => sourceDecoder),
}, 'ProxySource')

export const tarotSourceDecoder: JsonDecoder.Decoder<TarotSource> = JsonDecoder.object<TarotSource>({
  tag: JsonDecoder.literal("TarotSource"),
  sourceTag: JsonDecoder.constant("TarotSource"),
  contents: tarotCardDecoder
}, 'TarotSource')

export const otherSourceDecoder: JsonDecoder.Decoder<OtherSource> = JsonDecoder.object<OtherSource>(
  {
    sourceTag: JsonDecoder.constant("OtherSource"),
    tag: JsonDecoder.string(),
    contents: JsonDecoder.lazy<string>(() => JsonDecoder.oneOf(
      [ JsonDecoder.
          tuple([sourceDecoder, JsonDecoder.number()], 'proxySource').
          flatMap(([source,]) => "contents" in source ? JsonDecoder.succeed() : JsonDecoder.fail("missing contents"))
      , v2Optional(JsonDecoder.string())
      ], 'OtherSource.contents'))
  },
  'OtherSource'
)

export const sourceDecoder = JsonDecoder.oneOf<Source>([
  proxySourceDecoder,
  tarotSourceDecoder,
  otherSourceDecoder
], 'Source')

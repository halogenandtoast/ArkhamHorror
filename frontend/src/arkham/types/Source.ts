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
    case "IndexedSource":
      return source.contents ? sourceKey(source.contents[1]) : undefined
    case "AbilitySource":
      return sourceKey(source.contents[0])
    case "UseAbilitySource":
      return sourceKey(source.contents[1])
    case "PaymentSource":
      return sourceKey(source.contents)
    case "BothSource":
      return sourceKey(source.contents[0]) ?? sourceKey(source.contents[1])
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

export type IndexedSource = {
  sourceTag: "IndexedSource"
  tag: "IndexedSource"
  contents?: [number, Source]
}

export type AbilitySource = {
  sourceTag: "AbilitySource"
  tag: "AbilitySource"
  contents: [Source, number]
}

export type UseAbilitySource = {
  sourceTag: "UseAbilitySource"
  tag: "UseAbilitySource"
  contents: [string, Source, number]
}

export type PaymentSource = {
  sourceTag: "PaymentSource"
  tag: "PaymentSource"
  contents: Source
}

export type BothSource = {
  sourceTag: "BothSource"
  tag: "BothSource"
  contents: [Source, Source]
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

export type Source = ProxySource | TarotSource | IndexedSource | AbilitySource | UseAbilitySource | PaymentSource | BothSource | OtherSource

export const proxySourceDecoder: JsonDecoder.Decoder<ProxySource> = JsonDecoder.object<ProxySource>({
  tag: JsonDecoder.literal("ProxySource"),
  sourceTag: JsonDecoder.constant("ProxySource"),
  source: JsonDecoder.lazy<Source>(() => sourceDecoder),
  originalSource: JsonDecoder.lazy<Source>(() => sourceDecoder),
}, 'ProxySource')

export const indexedSourceDecoder: JsonDecoder.Decoder<IndexedSource> = JsonDecoder.object<IndexedSource>({
  tag: JsonDecoder.literal("IndexedSource"),
  sourceTag: JsonDecoder.constant("IndexedSource"),
  contents: v2Optional(JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.lazy<Source>(() => sourceDecoder)], 'IndexedSource.contents')),
}, 'IndexedSource')

export const abilitySourceDecoder: JsonDecoder.Decoder<AbilitySource> = JsonDecoder.object<AbilitySource>({
  tag: JsonDecoder.literal("AbilitySource"),
  sourceTag: JsonDecoder.constant("AbilitySource"),
  contents: JsonDecoder.tuple([JsonDecoder.lazy<Source>(() => sourceDecoder), JsonDecoder.number()], 'AbilitySource.contents'),
}, 'AbilitySource')

export const useAbilitySourceDecoder: JsonDecoder.Decoder<UseAbilitySource> = JsonDecoder.object<UseAbilitySource>({
  tag: JsonDecoder.literal("UseAbilitySource"),
  sourceTag: JsonDecoder.constant("UseAbilitySource"),
  contents: JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.lazy<Source>(() => sourceDecoder), JsonDecoder.number()], 'UseAbilitySource.contents'),
}, 'UseAbilitySource')

export const paymentSourceDecoder: JsonDecoder.Decoder<PaymentSource> = JsonDecoder.object<PaymentSource>({
  tag: JsonDecoder.literal("PaymentSource"),
  sourceTag: JsonDecoder.constant("PaymentSource"),
  contents: JsonDecoder.lazy<Source>(() => sourceDecoder),
}, 'PaymentSource')

export const bothSourceDecoder: JsonDecoder.Decoder<BothSource> = JsonDecoder.object<BothSource>({
  tag: JsonDecoder.literal("BothSource"),
  sourceTag: JsonDecoder.constant("BothSource"),
  contents: JsonDecoder.tuple([JsonDecoder.lazy<Source>(() => sourceDecoder), JsonDecoder.lazy<Source>(() => sourceDecoder)], 'BothSource.contents'),
}, 'BothSource')

export const tarotSourceDecoder: JsonDecoder.Decoder<TarotSource> = JsonDecoder.object<TarotSource>({
  tag: JsonDecoder.literal("TarotSource"),
  sourceTag: JsonDecoder.constant("TarotSource"),
  contents: tarotCardDecoder
}, 'TarotSource')

export const otherSourceDecoder: JsonDecoder.Decoder<OtherSource> = JsonDecoder.object<OtherSource>(
  {
    sourceTag: JsonDecoder.constant("OtherSource"),
    tag: JsonDecoder.string(),
    contents: JsonDecoder.succeed().map((contents: unknown) =>
      typeof contents === 'string' ? contents : undefined
    )
  },
  'OtherSource'
)

export const sourceDecoder = JsonDecoder.oneOf<Source>([
  proxySourceDecoder,
  tarotSourceDecoder,
  indexedSourceDecoder,
  abilitySourceDecoder,
  useAbilitySourceDecoder,
  paymentSourceDecoder,
  bothSourceDecoder,
  otherSourceDecoder
], 'Source')

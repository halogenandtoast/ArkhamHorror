import * as JsonDecoder from 'ts.data.json';

// The Haskell `Cost` type has dozens of variants. Rather than mirror every
// one, we keep `Cost` open: every value has a `tag` and arbitrary extra
// fields. Rendering is dispatched on `tag` in `formatCost` (see
// `@/arkham/cost.ts`); unknown tags fall back to a generic representation so
// a missing case never crashes the UI. The named variants below are kept for
// callers that still type-narrow on them (e.g. ability-action accounting).
export type Costs = {
  tag: "Costs"
  contents: Cost[]
}

export type ActionCost = {
  tag: "ActionCost"
  contents: number
}

export type OtherCost = {
  tag: "OtherCost"
}

export type GenericCost = {
  tag: string
  [key: string]: unknown
}

export type Cost = Costs | ActionCost | OtherCost | GenericCost

export const costDecoder: JsonDecoder.Decoder<Cost> = JsonDecoder.succeed<Cost>().flatMap((value) => {
  if (value && typeof value === 'object' && typeof (value as { tag?: unknown }).tag === 'string') {
    return JsonDecoder.constant(value as Cost)
  }
  return JsonDecoder.fail<Cost>(`Expected Cost object with a tag, got ${JSON.stringify(value)}`)
})

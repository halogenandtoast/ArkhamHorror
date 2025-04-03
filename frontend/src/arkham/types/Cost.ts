import * as JsonDecoder from 'ts.data.json';

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

export type Cost = Costs | ActionCost | OtherCost

export const costsDecoder: JsonDecoder.Decoder<Costs> = JsonDecoder.object<Costs>({
  tag: JsonDecoder.literal("Costs"),
  contents: JsonDecoder.lazy<Cost[]>(() => JsonDecoder.array<Cost>(costDecoder, 'Costs[]')),
}, 'Costs')

export const actionCostDecoder = JsonDecoder.object<ActionCost>({
  tag: JsonDecoder.literal("ActionCost"),
  contents: JsonDecoder.number(),
}, 'ActionCost')

export const costDecoder = JsonDecoder.oneOf<Cost>([
  costsDecoder,
  actionCostDecoder,
  JsonDecoder.constant<OtherCost>({ tag: "OtherCost" })
], 'Cost')

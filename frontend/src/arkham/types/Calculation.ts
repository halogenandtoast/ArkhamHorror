import * as JsonDecoder from 'ts.data.json'

export type Calculation
  = { tag: "Fixed", contents: number }
  | { tag: "OtherCalculation" }

export const calculationDecoder: JsonDecoder.Decoder<Calculation> = JsonDecoder.oneOf<Calculation>([
  JsonDecoder.object<Calculation>({ tag: JsonDecoder.literal("Fixed"), contents: JsonDecoder.number() }, 'Fixed'),
  JsonDecoder.succeed().map(() => ({ tag: "OtherCalculation" }))
], 'Calculation');

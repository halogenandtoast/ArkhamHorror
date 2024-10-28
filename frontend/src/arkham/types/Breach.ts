import { JsonDecoder } from 'ts.data.json';

export type BreachStatus = { tag: "Breaches", contents: number } | { tag: "Incursion", contents: number }

export const breachStatusDecoder: JsonDecoder.Decoder<BreachStatus> = JsonDecoder.oneOf<BreachStatus>([
  JsonDecoder.object<BreachStatus>({ tag: JsonDecoder.isExactly("Breaches"), contents: JsonDecoder.number }, 'Breaches'),
  JsonDecoder.object<BreachStatus>({ tag: JsonDecoder.isExactly("Incursion"), contents: JsonDecoder.number }, 'Incursion'),
], 'BreachStatus');


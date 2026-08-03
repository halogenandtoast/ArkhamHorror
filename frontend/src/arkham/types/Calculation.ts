import * as JsonDecoder from 'ts.data.json'

/* An enemy's printed health/fight/evade arrives as a GameValueCalculation wrapping
 * a GameValue, so the inner tag is the only way to tell a real value from a printed
 * dash (ValueStar) or an X. Everything else still collapses to OtherCalculation. */
export type GameValueTag =
  | "Static"
  | "PerPlayer"
  | "StaticWithPerPlayer"
  | "ByPlayerCount"
  | "ValueX"
  | "ValueStar"
  | "ValueUnknown"

export type Calculation
  = { tag: "Fixed", contents: number }
  | { tag: "GameValueCalculation", contents: { tag: GameValueTag } }
  | { tag: "OtherCalculation" }

export const calculationDecoder: JsonDecoder.Decoder<Calculation> = JsonDecoder.oneOf<Calculation>([
  JsonDecoder.object<Calculation>({ tag: JsonDecoder.literal("Fixed"), contents: JsonDecoder.number() }, 'Fixed'),
  JsonDecoder.object<Calculation>(
    {
      tag: JsonDecoder.literal("GameValueCalculation"),
      contents: JsonDecoder.object<{ tag: GameValueTag }>(
        { tag: JsonDecoder.string().map((t) => t as GameValueTag) },
        'GameValue',
      ),
    },
    'GameValueCalculation',
  ),
  JsonDecoder.succeed().map(() => ({ tag: "OtherCalculation" }))
], 'Calculation');

/* True when a value is printed as a dash or an X rather than a number — the enemy
 * has no such stat at all, as opposed to one that happens to be zero. */
export const isUnvaluedCalculation = (c: Calculation | null): boolean => {
  if (c === null) return true
  if (c.tag !== "GameValueCalculation") return false
  return c.contents.tag === "ValueStar" || c.contents.tag === "ValueX" || c.contents.tag === "ValueUnknown"
}

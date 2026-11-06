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
  | { tag: "GameValueCalculation", contents: { tag: GameValueTag, contents?: unknown } }
  | { tag: "OtherCalculation" }

export const calculationDecoder: JsonDecoder.Decoder<Calculation> = JsonDecoder.oneOf<Calculation>([
  JsonDecoder.object<Calculation>({ tag: JsonDecoder.literal("Fixed"), contents: JsonDecoder.number() }, 'Fixed'),
  JsonDecoder.object<Calculation>(
    {
      tag: JsonDecoder.literal("GameValueCalculation"),
      contents: JsonDecoder.object<{ tag: GameValueTag; contents?: unknown }>(
        { tag: JsonDecoder.string().map((t) => t as GameValueTag), contents: JsonDecoder.optional(JsonDecoder.succeed()) },
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

/* The number a printed stat resolves to for this many investigators, or null when it is a dash, an X or
 * something the server did not spell out. Per-player values scale with the investigators the game started with. */
export const resolveCalculation = (c: Calculation | null, players: number): number | null => {
  if (c === null) return null
  if (c.tag === "Fixed") return c.contents
  if (c.tag !== "GameValueCalculation") return null
  const { tag, contents } = c.contents
  const numbers = Array.isArray(contents) ? contents.filter((n): n is number => typeof n === "number") : []
  if (tag === "Static" && typeof contents === "number") return contents
  if (tag === "PerPlayer" && typeof contents === "number") return contents * players
  if (tag === "StaticWithPerPlayer" && numbers.length === 2) return numbers[0] + numbers[1] * players
  if (tag === "ByPlayerCount" && numbers.length > 0) return numbers[Math.min(players, numbers.length) - 1]
  return null
}

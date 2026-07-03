import * as JsonDecoder from 'ts.data.json';

export const TOKENS = [
  "Aether",
  "AlarmLevel",
  "Ammo",
  "Antiquity",
  "Bounty",
  "Brilliance",
  "Charge",
  "Civilian",
  "Clue",
  "Corruption",
  "Damage",
  "DarknessLevel",
  "Depletion",
  "Depth",
  "Doom",
  "Durability",
  "Eclipse",
  "Empowerment",
  "Evidence",
  "Growth",
  "Horror",
  "Inspiration",
  "Key",
  "Kindling",
  "Lead",
  "Leyline",
  "Lock",
  "LostSoul",
  "Memory",
  "Mutation",
  "Newspaper",
  "Offering",
  "Overgrowth",
  "Pillar",
  "Portent",
  "Redemption",
  "Renown",
  "Resource",
  "Rumor",
  "ScoutingReport",
  "Scrap",
  "Seal",
  "Secret",
  "Seed",
  "Shard",
  "Shell",
  "Shipment",
  "Study",
  "Supply",
  "Suspicion",
  "Switch",
  "Target",
  "Ticket",
  "Time",
  "TimeCapsule",
  "Truth",
  "Try",
  "Ward",
  "Warning",
  "Whistle",
  "Wish",
] as const;

function literalUnionDecoder<const T extends readonly string[]>(
  xs: T,
  name: string
): JsonDecoder.Decoder<T[number]> {
  return JsonDecoder.oneOf(
    xs.map((x) => JsonDecoder.literal(x)) as JsonDecoder.Decoder<T[number]>[],
    name
  );
}

export const tokenDecoder = literalUnionDecoder(TOKENS, "Token");
export type KnownToken = JsonDecoder.FromDecoder<typeof tokenDecoder>;
export type Token = KnownToken | (string & {});
export const TokenType = Object.fromEntries(TOKENS.map((t) => [t, t])) as {
  readonly [K in KnownToken]: K;
};
export type Tokens = Partial<Record<Token, number>>;
export function isUse(t: Token): boolean {
  return t !== 'Damage' && t !== 'Horror' && t !== 'Clue' && t !== 'Doom';
}
export const tokensDecoder =
  JsonDecoder.array<[Token, number]>(
    JsonDecoder.tuple([tokenDecoder, JsonDecoder.number()], 'Token[]'),
    'Token[]'
  ).map<{ [key in Token]?: number}>(pairs => pairs.reduce((acc, v) => ({ ...acc, [v[0]]: v[1] }), {}))

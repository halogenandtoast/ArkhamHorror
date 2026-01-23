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
  "Lead",
  "Leyline",
  "Lock",
  "LostSoul",
  "Offering",
  "Pillar",
  "Portent",
  "Resource",
  "ScoutingReport",
  "Scrap",
  "Seal",
  "Secret",
  "Shard",
  "Shell",
  "Study",
  "Supply",
  "Suspicion",
  "Switch",
  "Target",
  "Ticket",
  "Time",
  "Truth",
  "Try",
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
export type Token = JsonDecoder.FromDecoder<typeof tokenDecoder>;
export const TokenType = Object.fromEntries(TOKENS.map((t) => [t, t])) as {
  readonly [K in Token]: K;
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

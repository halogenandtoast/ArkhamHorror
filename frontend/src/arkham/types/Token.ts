import * as JsonDecoder from 'ts.data.json';
import { withDefault } from '@/arkham/parser';

export const TOKENS = [
  "Aether",
  "AlarmLevel",
  "Ammo",
  "Antiquity",
  "Bounty",
  "Brilliance",
  "Chance",
  "Charge",
  "Civilian",
  "Clue",
  "Corruption",
  "Damage",
  "DarknessLevel",
  "Depletion",
  "Depth",
  "Discovery",
  "Doom",
  "Dose",
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
  "Obligation",
  "Obsession",
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
  "Sign",
  "Snare",
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

const knownTokenDecoder = literalUnionDecoder(TOKENS, "Token");
export type KnownToken = JsonDecoder.FromDecoder<typeof knownTokenDecoder>;
export type Token = KnownToken | (string & {});

/* How the engine spells a token that belongs to a custom card set rather than
 * one it prints itself (`Arkham.Token.customTokenPrefix`). */
export const CUSTOM_TOKEN_PREFIX = 'custom:';

export const customTokenSlug = (token: Token): string | null =>
  token.startsWith(CUSTOM_TOKEN_PREFIX) ? token.slice(CUSTOM_TOKEN_PREFIX.length) : null;

/* A set's own token is not a literal: the slug is the author's to choose, so
 * anything past the prefix is one. Without this a card holding one decoded to
 * nothing and the whole entity's tokens came back empty. */
const customTokenDecoder: JsonDecoder.Decoder<Token> = JsonDecoder.string().chain((t) =>
  customTokenSlug(t)
    ? JsonDecoder.constant<Token>(t)
    : JsonDecoder.fail<Token>(`not a custom token: ${t}`),
);

export const tokenDecoder: JsonDecoder.Decoder<Token> = JsonDecoder.oneOf<Token>(
  [knownTokenDecoder, customTokenDecoder],
  "Token",
);
export const TokenType = Object.fromEntries(TOKENS.map((t) => [t, t])) as {
  readonly [K in KnownToken]: K;
};
export type Tokens = Partial<Record<Token, number>>;
export function isUse(t: Token): boolean {
  return t !== 'Damage' && t !== 'Horror' && t !== 'Clue' && t !== 'Doom';
}
const tokenPairsDecoder =
  JsonDecoder.array<[Token, number]>(
    JsonDecoder.tuple([tokenDecoder, JsonDecoder.number()], 'Token[]'),
    'Token[]'
  ).map<{ [key in Token]?: number}>(pairs => pairs.reduce((acc, v) => ({ ...acc, [v[0]]: v[1] }), {}))

export const tokensDecoder = withDefault({}, tokenPairsDecoder)

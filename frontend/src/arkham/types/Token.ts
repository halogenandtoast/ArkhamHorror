import { JsonDecoder } from 'ts.data.json';

// data Token = Resource | Damage | Horror | Clue | Doom | TokenAs Text Token

export type Token = 'Doom' | 'Clue' | 'Resource' | 'Damage' | 'Horror' | 'LostSoul' | 'Bounty' | 'Offering' | 'AlarmLevel';

export const TokenType = {
  Doom: 'Doom',
  Clue: 'Clue',
  Resource: 'Resource',
  Damage: 'Damage',
  Horror: 'Horror',
  LostSoul: 'LostSoul',
  Bounty: 'Bounty',
  Offering: 'Offering',
  AlarmLevel: 'AlarmLevel',
} as const;

export const tokenDecoder: JsonDecoder.Decoder<Token> = JsonDecoder.oneOf<Token>([
  JsonDecoder.isExactly('Doom'),
  JsonDecoder.isExactly('Clue'),
  JsonDecoder.isExactly('Resource'),
  JsonDecoder.isExactly('Damage'),
  JsonDecoder.isExactly('Horror'),
  JsonDecoder.isExactly('LostSoul'),
  JsonDecoder.isExactly('Bounty'),
  JsonDecoder.isExactly('Offering'),
  JsonDecoder.isExactly('AlarmLevel'),
], 'Token');

export type Tokens = { [key in Token]?: number };

export const tokensDecoder =
  JsonDecoder.array<[Token, number]>(
    JsonDecoder.tuple([tokenDecoder, JsonDecoder.number], 'Token[]'),
    'Token[]'
  ).map<{ [key in Token]?: number}>(pairs => pairs.reduce((acc, v) => ({ ...acc, [v[0]]: v[1] }), {}))

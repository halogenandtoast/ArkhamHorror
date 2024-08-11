import { JsonDecoder } from 'ts.data.json';

export type Token
  = 'Aether'
  | 'AlarmLevel'
  | 'Ammo'
  | 'Bounty'
  | 'Charge'
  | 'Clue'
  | 'Corruption'
  | 'Damage'
  | 'Depth'
  | 'Doom'
  | 'Durability'
  | 'Evidence'
  | 'Growth'
  | 'Horror'
  | 'Key'
  | 'Lead'
  | 'Leyline'
  | 'Lock'
  | 'LostSoul'
  | 'Offering'
  | 'Resource'
  | 'Secret'
  | 'Supply'
  | 'Suspicion'
  | 'Time'
  | 'Try'
  | 'Whistle'
  | 'Wish'

export const TokenType = {
  Aether: 'Aether',
  AlarmLevel: 'AlarmLevel',
  Ammo: 'Ammo',
  Bounty: 'Bounty',
  Charge: 'Charge',
  Clue: 'Clue',
  Corruption: 'Corruption',
  Damage: 'Damage',
  Depth: 'Depth',
  Doom: 'Doom',
  Durability: 'Durability',
  Evidence: 'Evidence',
  Growth: 'Growth',
  Horror: 'Horror',
  Key: 'Key',
  Lead: 'Lead',
  Leyline: 'Leyline',
  Lock: 'Lock',
  LostSoul: 'LostSoul',
  Offering: 'Offering',
  Resource: 'Resource',
  Secret: 'Secret',
  Supply: 'Supply',
  Suspicion: 'Suspicion',
  Try: 'Try',
  Whistle: 'Whistle',
  Wish: 'Wish',
} as const;

export const tokenDecoder: JsonDecoder.Decoder<Token> = JsonDecoder.oneOf<Token>([
  JsonDecoder.isExactly('Aether'),
  JsonDecoder.isExactly('AlarmLevel'),
  JsonDecoder.isExactly('Ammo'),
  JsonDecoder.isExactly('Bounty'),
  JsonDecoder.isExactly('Charge'),
  JsonDecoder.isExactly('Clue'),
  JsonDecoder.isExactly('Corruption'),
  JsonDecoder.isExactly('Damage'),
  JsonDecoder.isExactly('Depth'),
  JsonDecoder.isExactly('Doom'),
  JsonDecoder.isExactly('Durability'),
  JsonDecoder.isExactly('Evidence'),
  JsonDecoder.isExactly('Growth'),
  JsonDecoder.isExactly('Horror'),
  JsonDecoder.isExactly('Key'),
  JsonDecoder.isExactly('Lead'),
  JsonDecoder.isExactly('Leyline'),
  JsonDecoder.isExactly('Lock'),
  JsonDecoder.isExactly('LostSoul'),
  JsonDecoder.isExactly('Offering'),
  JsonDecoder.isExactly('Resource'),
  JsonDecoder.isExactly('Secret'),
  JsonDecoder.isExactly('Supply'),
  JsonDecoder.isExactly('Suspicion'),
  JsonDecoder.isExactly('Time'),
  JsonDecoder.isExactly('Try'),
  JsonDecoder.isExactly('Whistle'),
  JsonDecoder.isExactly('Wish'),
], 'Token');

export type Tokens = Partial<Record<Token, number>>;

export function isUse(t: Token): boolean {
  return t !== 'Damage' && t !== 'Horror' && t !== 'Clue' && t !== 'Doom';
}

export const tokensDecoder =
  JsonDecoder.array<[Token, number]>(
    JsonDecoder.tuple([tokenDecoder, JsonDecoder.number], 'Token[]'),
    'Token[]'
  ).map<{ [key in Token]?: number}>(pairs => pairs.reduce((acc, v) => ({ ...acc, [v[0]]: v[1] }), {}))

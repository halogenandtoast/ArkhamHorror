import * as JsonDecoder from 'ts.data.json';

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
  | 'Inspiration'
  | 'Key'
  | 'Lead'
  | 'Leyline'
  | 'Lock'
  | 'LostSoul'
  | 'Offering'
  | 'Resource'
  | 'Secret'
  | 'Shell'
  | 'Supply'
  | 'Suspicion'
  | 'Ticket'
  | 'Time'
  | 'Try'
  | 'Warning'
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
  Inspiration: 'Inspiration',
  Key: 'Key',
  Lead: 'Lead',
  Leyline: 'Leyline',
  Lock: 'Lock',
  LostSoul: 'LostSoul',
  Offering: 'Offering',
  Resource: 'Resource',
  Secret: 'Secret',
  Shell: 'Shell',
  Supply: 'Supply',
  Suspicion: 'Suspicion',
  Ticket: 'Ticket',
  Time: 'Time',
  Try: 'Try',
  Warning: 'Warning',
  Whistle: 'Whistle',
  Wish: 'Wish',
} as const;

export const tokenDecoder: JsonDecoder.Decoder<Token> = JsonDecoder.oneOf<Token>([
  JsonDecoder.literal('Aether'),
  JsonDecoder.literal('AlarmLevel'),
  JsonDecoder.literal('Ammo'),
  JsonDecoder.literal('Bounty'),
  JsonDecoder.literal('Charge'),
  JsonDecoder.literal('Clue'),
  JsonDecoder.literal('Corruption'),
  JsonDecoder.literal('Damage'),
  JsonDecoder.literal('Depth'),
  JsonDecoder.literal('Doom'),
  JsonDecoder.literal('Durability'),
  JsonDecoder.literal('Evidence'),
  JsonDecoder.literal('Growth'),
  JsonDecoder.literal('Horror'),
  JsonDecoder.literal('Inspiration'),
  JsonDecoder.literal('Key'),
  JsonDecoder.literal('Lead'),
  JsonDecoder.literal('Leyline'),
  JsonDecoder.literal('Lock'),
  JsonDecoder.literal('LostSoul'),
  JsonDecoder.literal('Offering'),
  JsonDecoder.literal('Resource'),
  JsonDecoder.literal('Secret'),
  JsonDecoder.literal('Shell'),
  JsonDecoder.literal('Supply'),
  JsonDecoder.literal('Suspicion'),
  JsonDecoder.literal('Ticket'),
  JsonDecoder.literal('Time'),
  JsonDecoder.literal('Try'),
  JsonDecoder.literal('Warning'),
  JsonDecoder.literal('Whistle'),
  JsonDecoder.literal('Wish'),
], 'Token');

export type Tokens = Partial<Record<Token, number>>;

export function isUse(t: Token): boolean {
  return t !== 'Damage' && t !== 'Horror' && t !== 'Clue' && t !== 'Doom';
}

export const tokensDecoder =
  JsonDecoder.array<[Token, number]>(
    JsonDecoder.tuple([tokenDecoder, JsonDecoder.number()], 'Token[]'),
    'Token[]'
  ).map<{ [key in Token]?: number}>(pairs => pairs.reduce((acc, v) => ({ ...acc, [v[0]]: v[1] }), {}))

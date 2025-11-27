import * as JsonDecoder from 'ts.data.json';

export type Token
  = 'Aether'
  | 'AlarmLevel'
  | 'Ammo'
  | 'Antiquity'
  | 'Bounty'
  | 'Charge'
  | 'Civilian'
  | 'Clue'
  | 'Corruption'
  | 'Damage'
  | 'Depletion'
  | 'Depth'
  | 'Doom'
  | 'Durability'
  | 'Eclipse'
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
  | 'Pillar'
  | 'Portent'
  | 'Resource'
  | 'ScoutingReport'
  | 'Seal'
  | 'Secret'
  | 'Shard'
  | 'Shell'
  | 'Study'
  | 'Supply'
  | 'Suspicion'
  | 'Target'
  | 'Ticket'
  | 'Time'
  | 'Truth'
  | 'Try'
  | 'Warning'
  | 'Whistle'
  | 'Wish'

export const TokenType = {
  Aether: 'Aether',
  AlarmLevel: 'AlarmLevel',
  Ammo: 'Ammo',
  Antiquity: 'Antiquity',
  Portent: 'Portent',
  Bounty: 'Bounty',
  Charge: 'Charge',
  Civilian: 'Civilian',
  Clue: 'Clue',
  Corruption: 'Corruption',
  Damage: 'Damage',
  Depth: 'Depth',
  Depletion: 'Depletion',
  Doom: 'Doom',
  Durability: 'Durability',
  Eclipse: 'Eclipse',
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
  Pillar: 'Pillar',
  Resource: 'Resource',
  ScoutingReport: 'ScoutingReport',
  Seal: 'Seal',
  Secret: 'Secret',
  Shard: 'Shard',
  Shell: 'Shell',
  Study: 'Study',
  Supply: 'Supply',
  Suspicion: 'Suspicion',
  Target: 'Target',
  Ticket: 'Ticket',
  Time: 'Time',
  Try: 'Try',
  Warning: 'Warning',
  Whistle: 'Whistle',
  Wish: 'Wish',
  Truth: 'Truth',
} as const;

export const tokenDecoder: JsonDecoder.Decoder<Token> = JsonDecoder.oneOf<Token>([
  JsonDecoder.literal('Aether'),
  JsonDecoder.literal('AlarmLevel'),
  JsonDecoder.literal('Ammo'),
  JsonDecoder.literal('Antiquity'),
  JsonDecoder.literal('Bounty'),
  JsonDecoder.literal('Charge'),
  JsonDecoder.literal('Civilian'),
  JsonDecoder.literal('Clue'),
  JsonDecoder.literal('Corruption'),
  JsonDecoder.literal('Damage'),
  JsonDecoder.literal('Depletion'),
  JsonDecoder.literal('Depth'),
  JsonDecoder.literal('Doom'),
  JsonDecoder.literal('Durability'),
  JsonDecoder.literal('Eclipse'),
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
  JsonDecoder.literal('Pillar'),
  JsonDecoder.literal('Portent'),
  JsonDecoder.literal('Resource'),
  JsonDecoder.literal('ScoutingReport'),
  JsonDecoder.literal('Seal'),
  JsonDecoder.literal('Secret'),
  JsonDecoder.literal('Shard'),
  JsonDecoder.literal('Shell'),
  JsonDecoder.literal('Study'),
  JsonDecoder.literal('Supply'),
  JsonDecoder.literal('Suspicion'),
  JsonDecoder.literal('Target'),
  JsonDecoder.literal('Ticket'),
  JsonDecoder.literal('Time'),
  JsonDecoder.literal('Truth'),
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

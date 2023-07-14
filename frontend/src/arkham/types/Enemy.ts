import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';

// data Token = Resource | Damage | Horror | Clue | Doom | TokenAs Text Token

export type Token = 'Doom' | 'Clue' | 'Resource' | 'Damage' | 'Horror' | 'LostSoul'

export const TokenType = {
  Doom: 'Doom',
  Clue: 'Clue',
  Resource: 'Resource',
  Damage: 'Damage',
  Horror: 'Horror',
  LostSoul: 'LostSoul',
} as const;

export const tokenDecoder: JsonDecoder.Decoder<Token> = JsonDecoder.oneOf<Token>([
  JsonDecoder.isExactly('Doom'),
  JsonDecoder.isExactly('Clue'),
  JsonDecoder.isExactly('Resource'),
  JsonDecoder.isExactly('Damage'),
  JsonDecoder.isExactly('Horror'),
  JsonDecoder.isExactly('LostSoul'),
], 'Token');



export interface Enemy {
  id: string;
  cardCode: string;
  assignedDamage: number;
  tokens: { [key in Token]?: number };
  exhausted: boolean;
  engagedInvestigators: string[];
  treacheries: string[];
  assets: string[];
  asSelfLocation: string | null;
  sealedChaosTokens: ChaosToken[];
  placement: Placement;
  keys: ArkhamKey[];
}

type DamageAssignment = { damageAssignmentAmount: number }

export const enemyDecoder = JsonDecoder.object<Enemy>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  tokens: JsonDecoder.array<[Token, number]>(
    JsonDecoder.tuple([tokenDecoder, JsonDecoder.number], 'Token[]'),
    'Token[]'
  ).map<{ [key in Token]?: number}>(pairs => pairs.reduce((acc, v) => ({ ...acc, [v[0]]: v[1] }), {})),
  assignedDamage: JsonDecoder.array<[boolean, number]>(
    JsonDecoder.tuple(
      [ JsonDecoder.constant(true),
        JsonDecoder.object<DamageAssignment>(
          { damageAssignmentAmount: JsonDecoder.number },
          'DamageAssignment'
        ).map(o => o.damageAssignmentAmount)
      ], '[bool, number]'),
    '[bool, number][]'
  ).map(a => a.reduce((acc, v) => acc + v[1], 0)),
  exhausted: JsonDecoder.boolean,
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorIds[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  asSelfLocation: JsonDecoder.nullable(JsonDecoder.string),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  placement: placementDecoder,
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]')
}, 'Enemy');

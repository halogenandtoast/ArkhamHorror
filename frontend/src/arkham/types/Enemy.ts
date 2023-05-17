import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Placement, placementDecoder } from '@/arkham/types/Placement';

export interface Enemy {
  id: string;
  cardCode: string;
  damage: number;
  assignedDamage: number;
  doom: number;
  clues: number;
  resources: number;
  exhausted: boolean;
  engagedInvestigators: string[];
  treacheries: string[];
  assets: string[];
  asSelfLocation: string | null;
  sealedTokens: ChaosToken[];
  placement: Placement;
}

type DamageAssignment = { damageAssignmentAmount: number }

export const enemyDecoder = JsonDecoder.object<Enemy>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  damage: JsonDecoder.number,
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
  doom: JsonDecoder.number,
  clues: JsonDecoder.number,
  resources: JsonDecoder.number,
  exhausted: JsonDecoder.boolean,
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string, 'InvestigatorIds[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  asSelfLocation: JsonDecoder.nullable(JsonDecoder.string),
  sealedTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  placement: placementDecoder
}, 'Enemy');

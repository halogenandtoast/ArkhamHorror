import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

export type Enemy = {
  id: number;
  cardId: number;
  cardCode: string;
  assignedDamage: number;
  tokens: Tokens;
  exhausted: boolean;
  engagedInvestigators: string[];
  treacheries: number[];
  assets: number[];
  skills: number[];
  events: number[];
  asSelfLocation: string | null;
  sealedChaosTokens: ChaosToken[];
  placement: Placement;
  keys: ArkhamKey[];
  modifiers: Modifier[];
  fight: number | null;
  evade: number | null;
  healthDamage: number;
  sanityDamage: number;
  health: GameValue | null;
  meta: any;
  flipped: boolean;
}

type DamageAssignment = { damageAssignmentAmount: number }

type GameValue = { tag: "Static", contents: number } | { tag: "PerPlayer", contents: number }

export const gameValueDecoder = JsonDecoder.oneOf<GameValue>([
  JsonDecoder.object({ tag: JsonDecoder.isExactly("Static"), contents: JsonDecoder.number }, 'Static'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("PerPlayer"), contents: JsonDecoder.number }, 'PerPlayer')
], 'GameValue')

export const enemyDecoder = JsonDecoder.object<Enemy>({
  id: JsonDecoder.number,
  cardId: JsonDecoder.number,
  cardCode: JsonDecoder.string,
  fight: JsonDecoder.nullable(JsonDecoder.number),
  evade: JsonDecoder.nullable(JsonDecoder.number),
  healthDamage: JsonDecoder.number,
  sanityDamage: JsonDecoder.number,
  health: JsonDecoder.failover(null, gameValueDecoder),
  tokens: tokensDecoder,
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
  treacheries: JsonDecoder.array<number>(JsonDecoder.number, 'TreacheryId[]'),
  assets: JsonDecoder.array<number>(JsonDecoder.number, 'AssetId[]'),
  skills: JsonDecoder.array<number>(JsonDecoder.number, 'SkillId[]'),
  events: JsonDecoder.array<number>(JsonDecoder.number, 'EventId[]'),
  asSelfLocation: JsonDecoder.nullable(JsonDecoder.string),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  placement: placementDecoder,
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
  meta: JsonDecoder.succeed,
  flipped: JsonDecoder.boolean
}, 'Enemy');

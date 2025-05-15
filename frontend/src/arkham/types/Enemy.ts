import * as JsonDecoder from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Calculation, calculationDecoder } from '@/arkham/types/Calculation';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { Card, cardDecoder } from '@/arkham/types/Card';

export type Enemy = {
  id: string;
  cardId: string;
  cardCode: string;
  assignedDamage: number;
  tokens: Tokens;
  exhausted: boolean;
  engagedInvestigators: string[];
  treacheries: string[];
  assets: string[];
  skills: string[];
  events: string[];
  asSelfLocation: string | null;
  sealedChaosTokens: ChaosToken[];
  placement: Placement;
  keys: ArkhamKey[];
  modifiers: Modifier[];
  fight: Calculation | null;
  evade: Calculation | null;
  healthDamage: number;
  sanityDamage: number;
  health: Calculation | null;
  meta: any;
  flipped: boolean;
  cardsUnderneath: Card[];
  referenceCards: string[]
}

type DamageAssignment = { damageAssignmentAmount: number }

type GameValue = { tag: "Static", contents: number } | { tag: "PerPlayer", contents: number }

export const gameValueDecoder = JsonDecoder.oneOf<GameValue>([
  JsonDecoder.object({ tag: JsonDecoder.literal("Static"), contents: JsonDecoder.number() }, 'Static'),
  JsonDecoder.object({ tag: JsonDecoder.literal("PerPlayer"), contents: JsonDecoder.number() }, 'PerPlayer')
], 'GameValue')

export const enemyDecoder = JsonDecoder.object<Enemy>({
  id: JsonDecoder.string(),
  cardId: JsonDecoder.string(),
  cardCode: JsonDecoder.string(),
  fight: JsonDecoder.nullable(calculationDecoder),
  evade: JsonDecoder.nullable(calculationDecoder),
  healthDamage: JsonDecoder.number(),
  sanityDamage: JsonDecoder.number(),
  health: JsonDecoder.fallback(null, calculationDecoder),
  tokens: tokensDecoder,
  assignedDamage: JsonDecoder.array<[boolean, number]>(
    JsonDecoder.tuple(
      [ JsonDecoder.constant(true),
        JsonDecoder.object<DamageAssignment>(
          { damageAssignmentAmount: JsonDecoder.number() },
          'DamageAssignment'
        ).map(o => o.damageAssignmentAmount)
      ], '[bool, number]'),
    '[bool, number][]'
  ).map(a => a.reduce((acc, v) => acc + v[1], 0)),
  exhausted: JsonDecoder.boolean(),
  engagedInvestigators: JsonDecoder.array<string>(JsonDecoder.string(), 'InvestigatorIds[]'),
  treacheries: JsonDecoder.array<string>(JsonDecoder.string(), 'TreacheryId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string(), 'AssetId[]'),
  skills: JsonDecoder.array<string>(JsonDecoder.string(), 'SkillId[]'),
  events: JsonDecoder.array<string>(JsonDecoder.string(), 'EventId[]'),
  asSelfLocation: JsonDecoder.nullable(JsonDecoder.string()),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  placement: placementDecoder,
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  modifiers: JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]'),
  meta: JsonDecoder.succeed(),
  flipped: JsonDecoder.boolean(),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'UnderneathCard[]'),
  referenceCards: JsonDecoder.array<string>(JsonDecoder.string(), 'CardCode[]'),
}, 'Enemy');

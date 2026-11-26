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
  stories: string[];
  scarletKeys: string[];
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
  /* The stats the engine will actually use, and the printed ones, both already
   * resolved to numbers by the server (EnemyMetadata in Arkham/ModifierData.hs).
   * `fight`/`health`/`evade` above are the printed CALCULATION, whose number
   * this decoder throws away, so these are the only numbers the UI ever sees. */
  modifiedFight: number | null;
  modifiedHealth: number | null;
  modifiedEvade: number | null;
  printedFight: number | null;
  printedHealth: number | null;
  printedEvade: number | null;
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
  /* nullable, not fallback: calculationDecoder ends in a catch-all that succeeds on
     anything, including null, so a fallback never fires and an enemy printing no
     health at all decoded as OtherCalculation rather than null. Matches fight/evade. */
  health: JsonDecoder.fallback(null, JsonDecoder.nullable(calculationDecoder)),
  /* failover, not nullable: a server older than these fields sends no key. */
  modifiedFight: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
  modifiedHealth: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
  modifiedEvade: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
  printedFight: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
  printedHealth: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
  printedEvade: JsonDecoder.failover(null, JsonDecoder.nullable(JsonDecoder.number())),
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
  stories: JsonDecoder.array<string>(JsonDecoder.string(), 'StoryId[]'),
  scarletKeys: JsonDecoder.array<string>(JsonDecoder.string(), 'ScarletKey[]'),
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

import * as JsonDecoder from 'ts.data.json';
import { Card, cardDecoder } from '@/arkham/types/Card';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';

export type SkillTestResultKind = 'Automatic' | 'NonAutomatic';

export type SkillTestResult =
  | { tag: 'Unrun' }
  | { tag: 'SucceededBy', contents: [SkillTestResultKind, number] }
  | { tag: 'FailedBy', contents: [SkillTestResultKind, number] };

const skillTestResultKindDecoder = JsonDecoder.oneOf<SkillTestResultKind>([
  JsonDecoder.literal('Automatic'),
  JsonDecoder.literal('NonAutomatic'),
], 'SkillTestResultKind');

export const skillTestResultDecoder = JsonDecoder.oneOf<SkillTestResult>([
  JsonDecoder.object({ tag: JsonDecoder.literal('Unrun') }, 'Unrun'),
  JsonDecoder.object({
    tag: JsonDecoder.literal('SucceededBy'),
    contents: JsonDecoder.tuple([skillTestResultKindDecoder, JsonDecoder.number()], '[SkillTestResultKind, number]'),
  }, 'SucceededBy'),
  JsonDecoder.object({
    tag: JsonDecoder.literal('FailedBy'),
    contents: JsonDecoder.tuple([skillTestResultKindDecoder, JsonDecoder.number()], '[SkillTestResultKind, number]'),
  }, 'FailedBy'),
], 'SkillTestResult');

export type DefeatedEnemyAttrs = {
  id: string;
  cardId: string;
  cardCode: string;
  [key: string]: unknown;
}

export type DefeatedEnemy = {
  defeatedEnemyAttrs: DefeatedEnemyAttrs;
  defeatedEnemyHealth: number;
}

const defeatedEnemyAttrsDecoder = JsonDecoder.object<DefeatedEnemyAttrs>({
  id: JsonDecoder.string(),
  cardId: JsonDecoder.string(),
  cardCode: JsonDecoder.string(),
}, 'DefeatedEnemyAttrs');

export const defeatedEnemyDecoder = JsonDecoder.object<DefeatedEnemy>({
  defeatedEnemyAttrs: defeatedEnemyAttrsDecoder,
  defeatedEnemyHealth: JsonDecoder.number(),
}, 'DefeatedEnemy');

export type SkillTestPerformed = [SkillType[], SkillTestResult];

export const skillTestPerformedDecoder: JsonDecoder.Decoder<SkillTestPerformed> = JsonDecoder.tuple(
  [JsonDecoder.array(skillTypeDecoder, 'SkillType[]'), skillTestResultDecoder],
  '[SkillType[], SkillTestResult]',
);

export type History = {
  historyTreacheriesDrawn: string[];
  historyEnemiesDrawn: string[];
  historyDealtDamageTo: Target[];
  historyEnemiesDefeated: DefeatedEnemy[];
  historyEnemiesAttackedBy: string[];
  historyMoved: number;
  historyLocationsSuccessfullyInvestigated: string[];
  historySuccessfulExplore: boolean;
  historyActionsCompleted: number;
  historyActionsSpent: number;
  historySkillTestsPerformed: SkillTestPerformed[];
  historyPlayedCards: Card[];
  historyCluesDiscovered: Record<string, number>;
  historyAttacksOfOpportunity: number;
  historySuccessfulAttacks: number;
  historySuccessfulEvasions: number;
  historySuccessfulInvestigations: number;
  historyResourcesGained: number;
  historyCardsDrawn: number;
}

export const historyDecoder: JsonDecoder.Decoder<History> = JsonDecoder.object<History>({
  historyTreacheriesDrawn: JsonDecoder.array(JsonDecoder.string(), 'CardCode[]'),
  historyEnemiesDrawn: JsonDecoder.array(JsonDecoder.string(), 'CardCode[]'),
  historyDealtDamageTo: JsonDecoder.array(targetDecoder, 'Target[]'),
  historyEnemiesDefeated: JsonDecoder.array(defeatedEnemyDecoder, 'DefeatedEnemy[]'),
  historyEnemiesAttackedBy: JsonDecoder.array(JsonDecoder.string(), 'EnemyId[]'),
  historyMoved: JsonDecoder.number(),
  historyLocationsSuccessfullyInvestigated: JsonDecoder.array(JsonDecoder.string(), 'LocationId[]'),
  historySuccessfulExplore: JsonDecoder.boolean(),
  historyActionsCompleted: JsonDecoder.number(),
  historyActionsSpent: JsonDecoder.number(),
  historySkillTestsPerformed: JsonDecoder.array(skillTestPerformedDecoder, 'SkillTestPerformed[]'),
  historyPlayedCards: JsonDecoder.array(cardDecoder, 'Card[]'),
  historyCluesDiscovered: JsonDecoder.record(JsonDecoder.number(), 'Record<LocationId, number>'),
  historyAttacksOfOpportunity: JsonDecoder.number(),
  historySuccessfulAttacks: JsonDecoder.number(),
  historySuccessfulEvasions: JsonDecoder.number(),
  historySuccessfulInvestigations: JsonDecoder.number(),
  historyResourcesGained: JsonDecoder.number(),
  historyCardsDrawn: JsonDecoder.number(),
}, 'History');

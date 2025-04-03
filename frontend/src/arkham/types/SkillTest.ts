import * as JsonDecoder from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Card, cardDecoder} from '@/arkham/types/Card';
import { SkillType, skillTypeDecoder} from '@/arkham/types/SkillType';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';

export type SkillTestStep
  = "DetermineSkillOfTestStep"
  | "SkillTestFastWindow1"
  | "CommitCardsFromHandToSkillTestStep"
  | "SkillTestFastWindow2"
  | "RevealChaosTokenStep"
  | "ResolveChaosSymbolEffectsStep"
  | "DetermineInvestigatorsModifiedSkillValueStep"
  | "DetermineSuccessOrFailureOfSkillTestStep"
  | "ApplySkillTestResultsStep"
  | "SkillTestEndsStep"

export type Source = {
  tag: string;
  contents: any; // eslint-disable-line
}

export const sourceDecoder = JsonDecoder.object<Source>({
  tag: JsonDecoder.string(),
  contents: JsonDecoder.succeed(),
}, 'Source');

type SkillTestBaseValue
  = { tag: 'SkillBaseValue' }
  | { tag: 'AndSkillBaseValue' }
  | { tag: 'HalfResourcesOf' }
  | { tag: 'FixedBaseValue' }

const baseValueDecoder = JsonDecoder.oneOf<SkillTestBaseValue>(
  [
    JsonDecoder.object({ tag: JsonDecoder.literal('SkillBaseValue') }, 'SkillBaseValue'),
    JsonDecoder.object({ tag: JsonDecoder.literal('AndSkillBaseValue') }, 'AndSkillBaseValue'),
    JsonDecoder.object({ tag: JsonDecoder.literal('HalfResourcesOf') }, 'HalfResourcesOf'),
    JsonDecoder.object({ tag: JsonDecoder.literal('FixedBaseValue') }, 'FixedBaseValue'),
  ],
  'SkillTestBaseValue',
);

export type SkillTest = {
  investigator: string;
  setAsideChaosTokens: ChaosToken[];
  revealedChaosTokens: ChaosToken[];
  resolvedChaosTokens: ChaosToken[];
  // result: SkillTestResult;
  committedCards: Card[]
  source: Source;
  action: string | null;
  targetCard?: string | null;
  sourceCard?: string | null;
  modifiedSkillValue: number;
  modifiedDifficulty: number;
  skills: SkillType[];
  step: SkillTestStep;
  baseValue: SkillTestBaseValue;
  result: null | { tag: string };
  modifiers?: Modifier[];
}

export type SkillTestResults = {
  skillTestResultsSkillValue: number;
  skillTestResultsIconValue: number;
  skillTestResultsChaosTokensValue: number;
  skillTestResultsDifficulty: number;
  skillTestResultsResultModifiers: number | null;
  skillTestResultsSuccess: boolean;
}

const skillTestStepDecoder = JsonDecoder.oneOf<SkillTestStep>(
  [
    JsonDecoder.literal('DetermineSkillOfTestStep'),
    JsonDecoder.literal('SkillTestFastWindow1'),
    JsonDecoder.literal('CommitCardsFromHandToSkillTestStep'),
    JsonDecoder.literal('SkillTestFastWindow2'),
    JsonDecoder.literal('RevealChaosTokenStep'),
    JsonDecoder.literal('ResolveChaosSymbolEffectsStep'),
    JsonDecoder.literal('DetermineInvestigatorsModifiedSkillValueStep'),
    JsonDecoder.literal('DetermineSuccessOrFailureOfSkillTestStep'),
    JsonDecoder.literal('ApplySkillTestResultsStep'),
    JsonDecoder.literal('SkillTestEndsStep'),
  ],
  'SkillTestStep',
);

export const skillTestDecoder = JsonDecoder.object<SkillTest>(
  {
    investigator: JsonDecoder.string(),
    action: JsonDecoder.nullable(JsonDecoder.string()),
    modifiedDifficulty: JsonDecoder.number(),
    setAsideChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    revealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    resolvedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    // result: skillTestResultDecoder,
    committedCards: JsonDecoder.dictionary(JsonDecoder.array(cardDecoder, 'Card[]'), 'Record<string, Card[]>').map((record) => Object.values(record).flat()),
    source: sourceDecoder,
    targetCard: JsonDecoder.optional(JsonDecoder.string()),
    sourceCard: JsonDecoder.optional(JsonDecoder.string()),
    modifiedSkillValue: JsonDecoder.number(),
    skills: JsonDecoder.array(skillTypeDecoder, 'SkillType[]'),
    step: JsonDecoder.fallback("DetermineSkillOfTestStep", skillTestStepDecoder),
    baseValue: baseValueDecoder,
    result: JsonDecoder.nullable(JsonDecoder.object({ tag: JsonDecoder.string() }, 'SkillTestResult')),
    modifiers: JsonDecoder.optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
  },
  'SkillTest',
);

export const skillTestResultsDecoder = JsonDecoder.object<SkillTestResults>(
  {
    skillTestResultsSkillValue: JsonDecoder.number(),
    skillTestResultsIconValue: JsonDecoder.number(),
    skillTestResultsChaosTokensValue: JsonDecoder.number(),
    skillTestResultsDifficulty: JsonDecoder.number(),
    skillTestResultsResultModifiers: JsonDecoder.nullable(JsonDecoder.number()),
    skillTestResultsSuccess: JsonDecoder.boolean(),
  },
  'SkillTestResults',
);

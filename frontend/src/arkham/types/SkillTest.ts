import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Card, cardDecoder} from '@/arkham/types/Card';
import { SkillType, skillTypeDecoder} from '@/arkham/types/SkillType';

export type SkillTestStep
  = "DetermineSkillOfTestStep"
  | "CommitCardsFromHandToSkillTestStep"
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
  tag: JsonDecoder.string,
  contents: JsonDecoder.succeed,
}, 'Source');

export type SkillTest = {
  investigator: string;
  setAsideChaosTokens: ChaosToken[];
  revealedChaosTokens: ChaosToken[];
  // result: SkillTestResult;
  committedCards: Card[]
  source: Source;
  action: string | null;
  card: string | null;
  modifiedSkillValue: number;
  modifiedDifficulty: number;
  skills: SkillType[];
  step: SkillTestStep;
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
    JsonDecoder.isExactly('DetermineSkillOfTestStep'),
    JsonDecoder.isExactly('CommitCardsFromHandToSkillTestStep'),
    JsonDecoder.isExactly('RevealChaosTokenStep'),
    JsonDecoder.isExactly('ResolveChaosSymbolEffectsStep'),
    JsonDecoder.isExactly('DetermineInvestigatorsModifiedSkillValueStep'),
    JsonDecoder.isExactly('DetermineSuccessOrFailureOfSkillTestStep'),
    JsonDecoder.isExactly('ApplySkillTestResultsStep'),
    JsonDecoder.isExactly('SkillTestEndsStep'),
  ],
  'SkillTestStep',
);

export const skillTestDecoder = JsonDecoder.object<SkillTest>(
  {
    investigator: JsonDecoder.string,
    action: JsonDecoder.nullable(JsonDecoder.string),
    modifiedDifficulty: JsonDecoder.number,
    setAsideChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    revealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    // result: skillTestResultDecoder,
    committedCards: JsonDecoder.dictionary(JsonDecoder.array(cardDecoder, 'Card[]'), 'Record<string, Card[]>').map((record) => Object.values(record).flat()),
    source: sourceDecoder,
    card: JsonDecoder.nullable(JsonDecoder.string),
    modifiedSkillValue: JsonDecoder.number,
    skills: JsonDecoder.array(skillTypeDecoder, 'SkillType[]'),
    step: JsonDecoder.failover({ tag: "DetermineSkillOfTestStep" }, skillTestStepDecoder),
  },
  'SkillTest',
);

export const skillTestResultsDecoder = JsonDecoder.object<SkillTestResults>(
  {
    skillTestResultsSkillValue: JsonDecoder.number,
    skillTestResultsIconValue: JsonDecoder.number,
    skillTestResultsChaosTokensValue: JsonDecoder.number,
    skillTestResultsDifficulty: JsonDecoder.number,
    skillTestResultsResultModifiers: JsonDecoder.nullable(JsonDecoder.number),
    skillTestResultsSuccess: JsonDecoder.boolean,
  },
  'SkillTestResults',
);

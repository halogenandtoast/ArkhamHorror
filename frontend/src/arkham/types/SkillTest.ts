import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Card, cardDecoder} from '@/arkham/types/Card';
import { SkillType, skillTypeDecoder} from '@/arkham/types/SkillType';

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
  // result: SkillTestResult;
  committedCards: Card[]
  source: Source;
  action: string | null;
  card: string | null;
  modifiedSkillValue: number;
  modifiedDifficulty: number;
  skills: SkillType[];
}

export type SkillTestResults = {
  skillTestResultsSkillValue: number;
  skillTestResultsIconValue: number;
  skillTestResultsChaosTokensValue: number;
  skillTestResultsDifficulty: number;
  skillTestResultsResultModifiers: number | null;
  skillTestResultsSuccess: boolean;
}

export const skillTestDecoder = JsonDecoder.object<SkillTest>(
  {
    investigator: JsonDecoder.string,
    action: JsonDecoder.nullable(JsonDecoder.string),
    modifiedDifficulty: JsonDecoder.number,
    setAsideChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    // result: skillTestResultDecoder,
    committedCards: JsonDecoder.dictionary(JsonDecoder.array(cardDecoder, 'Card[]'), 'Record<string, Card[]>').map((record) => Object.values(record).flat()),
    source: sourceDecoder,
    card: JsonDecoder.nullable(JsonDecoder.string),
    modifiedSkillValue: JsonDecoder.number,
    skills: JsonDecoder.array(skillTypeDecoder, 'SkillType[]')
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

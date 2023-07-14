import { JsonDecoder } from 'ts.data.json';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Card, cardDecoder} from '@/arkham/types/Card';

export interface Source {
  tag: string;
  contents: any; // eslint-disable-line
}

export const sourceDecoder = JsonDecoder.object<Source>({
  tag: JsonDecoder.string,
  contents: JsonDecoder.succeed,
}, 'Source');

export interface SkillTest {
  investigator: string;
  difficulty: number;
  setAsideChaosTokens: ChaosToken[];
  // result: SkillTestResult;
  committedCards: Card[]
  source: Source;
  action: string | null;
}

export interface SkillTestResults {
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
    difficulty: JsonDecoder.number,
    setAsideChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    // result: skillTestResultDecoder,
    committedCards: JsonDecoder.dictionary(JsonDecoder.array(cardDecoder, 'Card[]'), 'Record<string, Card[]>').map((record) => Object.values(record).flat()),
    source: sourceDecoder,
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

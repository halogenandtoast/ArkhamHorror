import { JsonDecoder } from 'ts.data.json';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';
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
  setAsideTokens: ChaosToken[];
  // result: SkillTestResult;
  committedCards: Card[]
  source: Source;
  action: string | null;
}

export interface SkillTestResults {
  skillTestResultsSkillValue: number;
  skillTestResultsIconValue: number;
  skillTestResultsTokensValue: number;
  skillTestResultsDifficulty: number;
  skillTestResultsResultModifiers: number | null;
}

export const skillTestDecoder = JsonDecoder.object<SkillTest>(
  {
    investigator: JsonDecoder.string,
    action: JsonDecoder.nullable(JsonDecoder.string),
    difficulty: JsonDecoder.number,
    setAsideTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    // result: skillTestResultDecoder,
    committedCards: JsonDecoder.dictionary(JsonDecoder.tuple([JsonDecoder.string, cardDecoder], '[string, Card]'), 'Record<string, [string, Card]>').map((record) => Object.values(record).map(([,card]) => card)),
    source: sourceDecoder,
  },
  'SkillTest',
);

export const skillTestResultsDecoder = JsonDecoder.object<SkillTestResults>(
  {
    skillTestResultsSkillValue: JsonDecoder.number,
    skillTestResultsIconValue: JsonDecoder.number,
    skillTestResultsTokensValue: JsonDecoder.number,
    skillTestResultsDifficulty: JsonDecoder.number,
    skillTestResultsResultModifiers: JsonDecoder.nullable(JsonDecoder.number),
  },
  'SkillTestResults',
);

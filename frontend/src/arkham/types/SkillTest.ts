import { JsonDecoder } from 'ts.data.json';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
// import { SkillTestResult, skillTestResultDecoder } from '@/arkham/types/SkillTestResult';
// import { Card, cardDecoder } from '@/arkham/types/Card';

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
  skillType: SkillType;
  difficulty: number;
  setAsideTokens: ChaosToken[];
  committedCards: Record<string, string[]>;
  source: Source;
}

export const skillTestDecoder = JsonDecoder.object<SkillTest>(
  {
    investigator: JsonDecoder.string,
    skillType: skillTypeDecoder,
    difficulty: JsonDecoder.number,
    setAsideTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    committedCards: JsonDecoder.dictionary<string[]>(JsonDecoder.array(JsonDecoder.string, 'string[]'), 'Dict<string, string[]>'),
    source: sourceDecoder,
  },
  'SkillTest',
);

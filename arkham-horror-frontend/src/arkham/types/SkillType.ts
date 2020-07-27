import { JsonDecoder } from 'ts.data.json';

export type SkillType = 'SkillWillpower' | 'SkillIntellect' | 'SkillCombat' | 'SkillAgility' | 'SkillWild'

export const skillTypeDecoder = JsonDecoder.oneOf<SkillType>([
  JsonDecoder.isExactly('SkillWillpower'),
  JsonDecoder.isExactly('SkillIntellect'),
  JsonDecoder.isExactly('SkillCombat'),
  JsonDecoder.isExactly('SkillAgility'),
  JsonDecoder.isExactly('SkillWild'),
], 'SkillType');

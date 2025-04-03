import * as JsonDecoder from 'ts.data.json';

export type SkillType = 'SkillWillpower' | 'SkillIntellect' | 'SkillCombat' | 'SkillAgility' | 'SkillWild';

export const skillTypeDecoder = JsonDecoder.oneOf<SkillType>([
  JsonDecoder.literal('SkillWillpower'),
  JsonDecoder.literal('SkillIntellect'),
  JsonDecoder.literal('SkillCombat'),
  JsonDecoder.literal('SkillAgility'),
  JsonDecoder.literal('SkillWild'),
], 'SkillType');

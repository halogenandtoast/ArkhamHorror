import { JsonDecoder } from 'ts.data.json';

export interface SkillContents {
  id: string;
  cardCode: string;
}

export const skillContentsDecoder = JsonDecoder.object<SkillContents>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
}, 'SkillContents');

export interface Skill {
  tag: string;
  contents: SkillContents;
}

export const skillDecoder = JsonDecoder.object<Skill>({
  tag: JsonDecoder.string,
  contents: skillContentsDecoder,
}, 'Skill');

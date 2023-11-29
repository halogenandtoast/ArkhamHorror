import { JsonDecoder } from 'ts.data.json';

export type Skill = {
  id: string;
  cardCode: string;
}

export const skillDecoder = JsonDecoder.object<Skill>({
  id: JsonDecoder.string,
  cardCode: JsonDecoder.string,
}, 'Skill');

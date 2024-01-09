import { JsonDecoder } from 'ts.data.json';

export type Skill = {
  id: string;
  cardId: string;
  cardCode: string;
}

export const skillDecoder = JsonDecoder.object<Skill>({
  id: JsonDecoder.string,
  cardId: JsonDecoder.string,
  cardCode: JsonDecoder.string,
}, 'Skill');

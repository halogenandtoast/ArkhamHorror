import { JsonDecoder } from 'ts.data.json';
import { Customization, customizationsDecoder } from '@/arkham/types/Customization';

export type Skill = {
  id: string;
  cardId: string;
  cardCode: string;
  customizations: Customization[];
}

export const skillDecoder = JsonDecoder.object<Skill>({
  id: JsonDecoder.string,
  cardId: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  customizations: customizationsDecoder,
}, 'Skill');

import { JsonDecoder } from 'ts.data.json';
import { Name, nameDecoder } from '@/arkham/types/Name';

export interface CardDef {
  cardCode: string;
  art: string;
  name: Name;
}

export const cardDefDecoder = JsonDecoder.object<CardDef>(
  {
    art: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    name: nameDecoder,
  },
  'CardDef',
);

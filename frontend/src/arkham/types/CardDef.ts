import { JsonDecoder } from 'ts.data.json';
import { Name, nameDecoder } from '@/arkham/types/Name';

export interface CardDef {
  cardCode: string;
  name: Name;
  victoryPoints: number | null;
}

export const cardDefDecoder = JsonDecoder.object<CardDef>({
  cardCode: JsonDecoder.string,
  name: nameDecoder,
  victoryPoints: JsonDecoder.nullable(JsonDecoder.number),
}, 'CardDef');

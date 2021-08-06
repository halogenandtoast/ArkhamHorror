import { JsonDecoder } from 'ts.data.json';

export interface CardDef {
  cardCode: string;
}

export const cardDefDecoder = JsonDecoder.object<CardDef>(
  {
    cardCode: JsonDecoder.string,
  },
  'CardDef',
);

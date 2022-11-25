import { JsonDecoder } from 'ts.data.json';
import { Name, nameDecoder } from '@/arkham/types/Name';

type CardCost = { contents: number, tag: "StaticCost" } | { tag: "DynamicCost" }


export interface CardDef {
  cardCode: string;
  classSymbols: string[];
  cardType: string;
  art: string;
  level: number;
  name: Name;
  cardTraits: string[];
  skills: string[];
  cost: CardCost | null;
}

const cardCostDecoder = JsonDecoder.oneOf<CardCost>([
  JsonDecoder.object({ contents: JsonDecoder.number, tag: JsonDecoder.isExactly("StaticCost") }, 'StaticCost'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("DynamicCost") }, 'DynamicCost')
], 'CardCost')

export const cardDefDecoder = JsonDecoder.object<CardDef>(
  {
    art: JsonDecoder.string,
    level: JsonDecoder.number,
    cardType: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    classSymbols: JsonDecoder.array<string>(JsonDecoder.string, 'string[]'),
    cardTraits: JsonDecoder.array<string>(JsonDecoder.string, 'string[]'),
    skills: JsonDecoder.array<string>(JsonDecoder.string, 'string[]'),
    name: nameDecoder,
    cost: JsonDecoder.nullable(cardCostDecoder),
  },
  'CardDef',
);

import * as JsonDecoder from 'ts.data.json';
import { Name, nameDecoder } from '@/arkham/types/Name';

type CardCost = { contents: number, tag: "StaticCost" } | { tag: "DynamicCost" } | { tag: "DiscardAmountCost" }

type SkillIcon = { contents: string, tag: "SkillIcon" } | { tag: "WildIcon" } | { tag: "WildMinusIcon" }


export type CardDef = {
  cardCode: string;
  doubleSided: boolean;
  classSymbols: string[];
  cardType: string;
  art: string;
  level: number | null;
  name: Name;
  cardTraits: string[];
  skills: SkillIcon[];
  cost: CardCost | null;
  otherSide: string | null;
}

const cardCostDecoder = JsonDecoder.oneOf<CardCost>([
  JsonDecoder.object({ contents: JsonDecoder.number(), tag: JsonDecoder.literal("StaticCost") }, 'StaticCost'),
  JsonDecoder.object({ tag: JsonDecoder.literal("DynamicCost") }, 'DynamicCost'),
  JsonDecoder.object({ tag: JsonDecoder.literal("MaxDynamicCost") }, 'MaxDynamicCost').map(() => ({ tag: "DynamicCost"})),
  JsonDecoder.object({ tag: JsonDecoder.literal("DiscardAmountCost") }, 'DiscardAmountCost')
], 'CardCost')

const skillIconDecoder = JsonDecoder.oneOf<SkillIcon>([
  JsonDecoder.object({ contents: JsonDecoder.string(), tag: JsonDecoder.literal("SkillIcon") }, 'SkillIcon'),
  JsonDecoder.object({ tag: JsonDecoder.literal("WildIcon") }, 'WildIcon'),
  JsonDecoder.object({ tag: JsonDecoder.literal("WildMinusIcon") }, 'WildMinusIcon')
], 'SkillIcon')

export const cardDefDecoder = JsonDecoder.object<CardDef>(
  {
    art: JsonDecoder.string(),
    level: JsonDecoder.nullable(JsonDecoder.number()),
    otherSide: JsonDecoder.nullable(JsonDecoder.string()),
    cardType: JsonDecoder.string(),
    cardCode: JsonDecoder.string(),
    doubleSided: JsonDecoder.boolean(),
    classSymbols: JsonDecoder.array<string>(JsonDecoder.string(), 'string[]'),
    cardTraits: JsonDecoder.array<string>(JsonDecoder.string(), 'string[]'),
    skills: JsonDecoder.array<SkillIcon>(skillIconDecoder, 'SkillIcon[]'),
    name: nameDecoder,
    cost: JsonDecoder.nullable(cardCostDecoder),
  },
  'CardDef',
);

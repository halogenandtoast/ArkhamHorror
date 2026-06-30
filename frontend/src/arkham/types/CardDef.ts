import * as JsonDecoder from 'ts.data.json';
import { v2Optional, withDefault } from '@/arkham/parser';
import { Name, nameDecoder } from '@/arkham/types/Name';

type CardCost
  = { contents: number, tag: "StaticCost" }
  | { tag: "DynamicCost" }
  | { tag: "DeferredCost" }
  | { tag: "DiscardAmountCost" }
  | { tag: "AnyMatchingCardCost" }
  | { tag: "MatchingEnemyFieldCost" }

type SkillIcon
  = { contents: string, tag: "SkillIcon" }
  | { tag: "WildIcon" }
  | { tag: "WildMinusIcon" }

export type CustomizationDef = [string, number]

export type CardDef = {
  cardCode: string;
  doubleSided: boolean;
  classSymbols: string[];
  cardType: string;
  art: string;
  level: number | null;
  stage?: number | null;
  name: Name;
  cardTraits: string[];
  skills: SkillIcon[];
  cost: CardCost | null;
  otherSide: string | null;
  meta: Record<string, any>;
  encounterSet?: any;
  customizations?: CustomizationDef[];
}

const cardCostDecoder = JsonDecoder.oneOf<CardCost>([
  JsonDecoder.object({ contents: JsonDecoder.number(), tag: JsonDecoder.literal("StaticCost") }, 'StaticCost'),
  JsonDecoder.object({ tag: JsonDecoder.literal("DynamicCost") }, 'DynamicCost'),
  JsonDecoder.object({ tag: JsonDecoder.literal("DeferredCost") }, 'DeferredCost'),
  JsonDecoder.object({ tag: JsonDecoder.literal("MaxDynamicCost") }, 'MaxDynamicCost').map(() => ({ tag: "DynamicCost"})),
  JsonDecoder.object({ tag: JsonDecoder.literal("DiscardAmountCost") }, 'DiscardAmountCost'),
  JsonDecoder.object({ tag: JsonDecoder.literal("AnyMatchingCardCost") }, 'AnyMatchingCardCost'),
  JsonDecoder.object({ tag: JsonDecoder.literal("MatchingEnemyFieldCost") }, 'MatchingEnemyFieldCost')
], 'CardCost')

const skillIconDecoder = JsonDecoder.oneOf<SkillIcon>([
  JsonDecoder.object({ contents: JsonDecoder.string(), tag: JsonDecoder.literal("SkillIcon") }, 'SkillIcon'),
  JsonDecoder.object({ tag: JsonDecoder.literal("WildIcon") }, 'WildIcon'),
  JsonDecoder.object({ tag: JsonDecoder.literal("WildMinusIcon") }, 'WildMinusIcon')
], 'SkillIcon')

export const cardDefDecoder = JsonDecoder.object<CardDef>(
  {
    art: JsonDecoder.string(),
    level: withDefault(null, JsonDecoder.number()),
    stage: JsonDecoder.oneOf([
      JsonDecoder.number(),
      JsonDecoder.null().map(() => undefined),
      JsonDecoder.undefined(),
    ], 'optional stage'),
    otherSide: withDefault(null, JsonDecoder.string()),
    cardType: JsonDecoder.string(),
    cardCode: JsonDecoder.string(),
    doubleSided: withDefault(false, JsonDecoder.boolean()),
    classSymbols: withDefault([], JsonDecoder.array<string>(JsonDecoder.string(), 'string[]')),
    cardTraits: withDefault([], JsonDecoder.array<string>(JsonDecoder.string(), 'string[]')),
    skills: withDefault([], JsonDecoder.array<SkillIcon>(skillIconDecoder, 'SkillIcon[]')),
    name: nameDecoder,
    cost: withDefault(null, cardCostDecoder),
    meta: JsonDecoder.succeed().map((v: any) => v ?? {}),
    encounterSet: v2Optional(JsonDecoder.succeed()),
    customizations: withDefault<CustomizationDef[]>([], JsonDecoder.array(JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.number()], 'CustomizationDef'), 'CustomizationDef[]')),
  },
  'CardDef',
);

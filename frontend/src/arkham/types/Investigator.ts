import { JsonDecoder } from 'ts.data.json';
import { LogContents, logContentsDecoder } from '@/arkham/types/Log';
import { Name, nameDecoder } from '@/arkham/types/Name';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import {
  Card,
  CardContents,
  cardContentsDecoder,
  cardDecoder,
} from '@/arkham/types/Card';

type ClassSymbol = 'Guardian' | 'Seeker' | 'Rogue' | 'Mystic' | 'Survivor' | 'Neutral';

export const classSymbolDecoder = JsonDecoder.oneOf<ClassSymbol>([
  JsonDecoder.isExactly('Guardian'),
  JsonDecoder.isExactly('Seeker'),
  JsonDecoder.isExactly('Rogue'),
  JsonDecoder.isExactly('Mystic'),
  JsonDecoder.isExactly('Survivor'),
  JsonDecoder.isExactly('Neutral'),
], 'ClassSymbol');

type AdditionalActionType
  = { tag: "ActionRestrictedAdditionalAction" }
  | { tag: "EffectAction", contents: [string, string] }
  | { tag: "TraitRestrictedAdditionalAction" }
  | { tag: "AnyAdditionalAction" }
  | { tag: "BountyAction" }
  | { tag: "BobJenkinsAction" }

type AdditionalAction = {
  kind: AdditionalActionType
}

export const additionalActionContentsDecoder = JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.string], 'AdditionalActionContents')

export const additionalActionTypeDecoder = JsonDecoder.oneOf<AdditionalActionType>(
  [ JsonDecoder.object({ tag: JsonDecoder.isExactly("EffectAction"), contents: additionalActionContentsDecoder}, 'EffectAction')
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("ActionRestrictedAdditionalAction") }, "ActionRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("TraitRestrictedAdditionalAction") }, "TraitRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("AnyAdditionalAction") }, "AnyAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("BountyAction") }, "BountyAction")
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("BobJenkinsAction") }, "BobJenkinsAction")
  ], "AdditionalActionType")

export const additionalActionDecoder = JsonDecoder.object<AdditionalAction>(
  { kind: additionalActionTypeDecoder }, 'AdditionalAction')

type InvestigatorSearch = {
  searchingFoundCards: Record<string, Card[]>;
}

export const investigatorSearchDecoder = JsonDecoder.object<InvestigatorSearch>({
  searchingFoundCards: JsonDecoder.dictionary<Card[]>(JsonDecoder.array(cardDecoder, 'Card[]'), 'Dict<string, Card[]>'),
}, 'InvestigatorSearch');

export type Investigator = {
  deckSize?: number;
  connectedLocations: string[];
  modifiers?: Modifier[];
  name: Name;
  id: string;
  playerId: string;
  cardCode: string;
  class: ClassSymbol;
  health: number;
  sanity: number;
  willpower: number;
  intellect: number;
  combat: number;
  agility: number;
  tokens: Tokens;
  assignedHealthDamage: number;
  assignedSanityDamage: number;
  location: string;
  remainingActions: number;
  endedTurn: boolean;
  engagedEnemies: string[];
  assets: string[];
  events: string[];
  skills: string[];
  discard: CardContents[];
  hand: Card[];
  deck: CardContents[];
  decks: [string, Card[]][];
  treacheries: string[];
  defeated: boolean;
  resigned: boolean;
  additionalActions: AdditionalActionType[];
  cardsUnderneath: Card[];
  foundCards: Record<string, Card[]>;
  xp: number;
  supplies: string[];
  keys: ArkhamKey[];
  hunchDeck?: CardContents[];
  revealedHunchCard?: string | null;
  isYithian: boolean;
  slots: Slot[];
  log: LogContents;
}

type SlotType = 'HandSlot' | 'BodySlot' | 'AccessorySlot' | 'ArcaneSlot' | 'TarotSlot' | 'AllySlot'

export const slotTypeDecoder = JsonDecoder.oneOf<SlotType>([
  JsonDecoder.isExactly('HandSlot'),
  JsonDecoder.isExactly('BodySlot'),
  JsonDecoder.isExactly('AccessorySlot'),
  JsonDecoder.isExactly('ArcaneSlot'),
  JsonDecoder.isExactly('TarotSlot'),
  JsonDecoder.isExactly('AllySlot'),
], 'SlotType')

export type Slot = {
  tag: SlotType
  empty: boolean
}

export const slotDecoder = JsonDecoder.object<Slot>({
  tag: slotTypeDecoder,
  empty: JsonDecoder.boolean,
}, 'Slot')

type SlotContents = {
  assets: string[]
}

const slotContentsDecoder = JsonDecoder.object<SlotContents>({
  assets: JsonDecoder.array(JsonDecoder.string, 'AssetId[]'),
}, 'SlotContents')

export const slotsDecoder = JsonDecoder.
  array<[SlotType, SlotContents[]]>(
    JsonDecoder.tuple([
      slotTypeDecoder,
      JsonDecoder.array(slotContentsDecoder, 'contents')
    ], 'tup')
  , 'Slot[]').
  map((arr) =>
      arr.flatMap(([key, value]) =>
        value.map((contents) => ({ tag: key , empty: contents.assets.length === 0})
  )))

export const investigatorDecoder = JsonDecoder.object<Investigator>({
  name: nameDecoder,
  id: JsonDecoder.string,
  playerId: JsonDecoder.string,
  cardCode: JsonDecoder.string,
  class: classSymbolDecoder,
  health: JsonDecoder.number,
  sanity: JsonDecoder.number,
  willpower: JsonDecoder.number,
  intellect: JsonDecoder.number,
  combat: JsonDecoder.number,
  agility: JsonDecoder.number,
  tokens: tokensDecoder,
  assignedHealthDamage: JsonDecoder.number,
  assignedSanityDamage: JsonDecoder.number,
  location: placementDecoder.map((placement) => placement.tag === "AtLocation" ? placement.contents : "00000000-0000-0000-0000-000000000000"),
  remainingActions: JsonDecoder.number,
  endedTurn: JsonDecoder.boolean,
  engagedEnemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  events: JsonDecoder.array<string>(JsonDecoder.string, 'EventId[]'),
  skills: JsonDecoder.array<string>(JsonDecoder.string, 'SkillId[]'),
  // deck: Deck PlayerCard,
  discard: JsonDecoder.array<CardContents>(cardContentsDecoder, 'PlayerCardContents[]'),
  hand: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  deck: JsonDecoder.array<CardContents>(cardContentsDecoder, 'PlayerCardContents[]'),
  decks: JsonDecoder.array<[string, Card[]]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.array<Card>(cardDecoder, 'Card[]')], '[string, Card[]]'), '[string, Card[]][]'),
  hunchDeck: JsonDecoder.optional(JsonDecoder.array<CardContents>(cardContentsDecoder, 'PlayerCardContents[]')),
  revealedHunchCard: JsonDecoder.optional(JsonDecoder.nullable(JsonDecoder.string)),
  // traits: HashSet Trait,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  defeated: JsonDecoder.boolean,
  resigned: JsonDecoder.boolean,
  additionalActions: JsonDecoder.array<AdditionalAction>(additionalActionDecoder, 'AdditionalAction').map((arr) => arr.map((action) => action.kind)),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  foundCards: JsonDecoder.nullable(investigatorSearchDecoder).map((search) => search?.searchingFoundCards || {}),
  xp: JsonDecoder.number,
  supplies: JsonDecoder.array<string>(JsonDecoder.string, 'supplies'),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  deckSize: JsonDecoder.optional(JsonDecoder.number),
  connectedLocations: JsonDecoder.array<string>(JsonDecoder.string, 'LocationId[]'),
  modifiers: JsonDecoder.optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
  isYithian: JsonDecoder.boolean,
  slots: slotsDecoder,
  log: logContentsDecoder,
}, 'Investigator', { foundCards: 'search', location: 'placement' });

import { JsonDecoder } from 'ts.data.json';
import { Name, nameDecoder } from '@/arkham/types/Name';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
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

type AdditionalAction
  = { tag: "ActionRestrictedAdditionalAction" }
  | { tag: "EffectAction", contents: [string, string] }
  | { tag: "TraitRestrictedAdditionalAction" }
  | { tag: "AnyAdditionalAction" }

export const additionalActionContentsDecoder = JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.string], 'AdditionalActionContents')

export const additionalActionDecoder = JsonDecoder.oneOf<AdditionalAction>(
  [ JsonDecoder.object({ tag: JsonDecoder.isExactly("EffectAction"), contents: additionalActionContentsDecoder}, 'EffectAction')
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("ActionRestrictedAdditionalAction") }, "ActionRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("TraitRestrictedAdditionalAction") }, "TraitRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.isExactly("AnyAdditionalAction") }, "AnyAdditionalAction")
  ], "AdditionalAction")

export interface Investigator {
  deckSize?: number;
  connectedLocations: string[];
  modifiers?: Modifier[];
  name: Name;
  id: string;
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
  discard: CardContents[];
  hand: Card[];
  deck: CardContents[];
  decks: [string, Card[]][];
  treacheries: string[];
  defeated: boolean;
  resigned: boolean;
  additionalActions: AdditionalAction[];
  cardsUnderneath: Card[];
  foundCards: Record<string, Card[]>;
  xp: number;
  supplies: string[];
  keys: ArkhamKey[];
  hunchDeck?: CardContents[];
  revealedHunchCard?: string | null;
  isYithian: boolean;
  slots: Slot[];
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

interface Slot {
  tag: SlotType
  empty: boolean
}

export const slotDecoder = JsonDecoder.object<Slot>({
  tag: slotTypeDecoder,
  empty: JsonDecoder.boolean,
}, 'Slot')

interface SlotContents {
  asset: string | null
}

const slotContentsDecoder = JsonDecoder.object<SlotContents>({
  asset: JsonDecoder.nullable(JsonDecoder.string),
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
        value.map((contents) => ({ tag: key , empty: contents.asset === null})
  )))

export const investigatorDecoder = JsonDecoder.object<Investigator>({
  name: nameDecoder,
  id: JsonDecoder.string,
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
  location: JsonDecoder.string,
  remainingActions: JsonDecoder.number,
  endedTurn: JsonDecoder.boolean,
  engagedEnemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  events: JsonDecoder.array<string>(JsonDecoder.string, 'EventId[]'),
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
  additionalActions: JsonDecoder.array<AdditionalAction>(additionalActionDecoder, 'AdditionalAction'),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  foundCards: JsonDecoder.dictionary<Card[]>(JsonDecoder.array(cardDecoder, 'Card[]'), 'Dict<string, Card[]>'),
  xp: JsonDecoder.number,
  supplies: JsonDecoder.array<string>(JsonDecoder.string, 'supplies'),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  deckSize: JsonDecoder.optional(JsonDecoder.number),
  connectedLocations: JsonDecoder.array<string>(JsonDecoder.string, 'LocationId[]'),
  modifiers: JsonDecoder.optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
  isYithian: JsonDecoder.boolean,
  slots: slotsDecoder,
}, 'Investigator');

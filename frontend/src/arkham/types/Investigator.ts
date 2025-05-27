import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { LogContents, logContentsDecoder } from '@/arkham/types/Log';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Name, nameDecoder } from '@/arkham/types/Name';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import { Seal, sealDecoder } from '@/arkham/types/Seal';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import {
  Card,
  CardContents,
  cardContentsDecoder,
  cardDecoder,
} from '@/arkham/types/Card';

type ClassSymbol = 'Guardian' | 'Seeker' | 'Rogue' | 'Mystic' | 'Survivor' | 'Neutral';
type Form
  = { tag: 'RegularForm' }
  | { tag: 'YithianForm' }
  | { tag: 'HomunculusForm' }
  | { tag: 'TransfiguredForm', contents: string }

export const formDecoder = JsonDecoder.oneOf<Form>([
  JsonDecoder.object({ tag: JsonDecoder.literal('RegularForm') }, 'RegularForm'),
  JsonDecoder.object({ tag: JsonDecoder.literal('YithianForm') }, 'YithianForm'),
  JsonDecoder.object({ tag: JsonDecoder.literal('HomunculusForm') }, 'HomunculusForm'),
  JsonDecoder.object({ tag: JsonDecoder.literal('TransfiguredForm'), contents: JsonDecoder.string() }, 'TransfiguredForm'),
], 'Form');

export const classSymbolDecoder = JsonDecoder.oneOf<ClassSymbol>([
  JsonDecoder.literal('Guardian'),
  JsonDecoder.literal('Seeker'),
  JsonDecoder.literal('Rogue'),
  JsonDecoder.literal('Mystic'),
  JsonDecoder.literal('Survivor'),
  JsonDecoder.literal('Neutral'),
], 'ClassSymbol');

type AdditionalActionType
  = { tag: "ActionRestrictedAdditionalAction" }
  | { tag: "AbilityRestrictedAdditionalAction" }
  | { tag: "EffectAction", contents: [string, string] }
  | { tag: "TraitRestrictedAdditionalAction" }
  | { tag: "PlayCardRestrictedAdditionalAction" }
  | { tag: "AnyAdditionalAction" }
  | { tag: "BountyAction" }
  | { tag: "BobJenkinsAction" }

type AdditionalAction = {
  kind: AdditionalActionType
}

export const additionalActionContentsDecoder = JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.string()], 'AdditionalActionContents')

export const additionalActionTypeDecoder = JsonDecoder.oneOf<AdditionalActionType>(
  [ JsonDecoder.object({ tag: JsonDecoder.literal("EffectAction"), contents: additionalActionContentsDecoder}, 'EffectAction')
  , JsonDecoder.object({ tag: JsonDecoder.literal("ActionRestrictedAdditionalAction") }, "ActionRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.literal("AbilityRestrictedAdditionalAction") }, "AbilityRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.literal("TraitRestrictedAdditionalAction") }, "TraitRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.literal("PlayCardRestrictedAdditionalAction") }, "PlayCardRestrictedAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.literal("AnyAdditionalAction") }, "AnyAdditionalAction")
  , JsonDecoder.object({ tag: JsonDecoder.literal("BountyAction") }, "BountyAction")
  , JsonDecoder.object({ tag: JsonDecoder.literal("BobJenkinsAction") }, "BobJenkinsAction")
  ], "AdditionalActionType")

export const additionalActionDecoder = JsonDecoder.object<AdditionalAction>(
  { kind: additionalActionTypeDecoder }, 'AdditionalAction')

type Search = {
  searchFoundCards: Record<string, Card[]>;
}

export const searchDecoder = JsonDecoder.object<Search>({
  searchFoundCards: JsonDecoder.dictionary<Card[]>(JsonDecoder.array(cardDecoder, 'Card[]'), 'Dict<string, Card[]>'),
}, 'Search');

export type InvestigatorDetails = {
  id: string;
  classSymbol: ClassSymbol;
}

// data CardSettings = CardSettings
//   { globalSettings :: GlobalSettings
//   , perCardSettings :: Map CardCode PerCardSettings
//   }
//   deriving stock (Show, Eq, Generic, Data)
//   deriving anyclass (ToJSON, FromJSON)
//
// data GlobalSettings = GlobalSettings
//   { ignoreUnrelatedSkillTestTriggers :: Bool
//   }
//   deriving stock (Show, Eq, Generic, Data)
//   deriving anyclass (ToJSON, FromJSON)
//
// data PerCardSettings = PerCardSettings
//   { cardIgnoreUnrelatedSkillTestTriggers :: Bool
//   }
//   deriving stock (Show, Eq, Generic, Data)
//   deriving anyclass (ToJSON, FromJSON)

type CardSettings = {
  globalSettings: {
    ignoreUnrelatedSkillTestTriggers: boolean;
  };
  perCardSettings: Record<string, {
    cardIgnoreUnrelatedSkillTestTriggers: boolean;
  }>;
}

export const cardSettingsDecoder = JsonDecoder.object<CardSettings>({
  globalSettings: JsonDecoder.object({
    ignoreUnrelatedSkillTestTriggers: JsonDecoder.boolean(),
  }, 'GlobalSettings'),
  perCardSettings: JsonDecoder.dictionary(JsonDecoder.object({
    cardIgnoreUnrelatedSkillTestTriggers: JsonDecoder.boolean(),
  }, 'PerCardSettings'), 'Dict<string, PerCardSettings>'),
}, 'CardSettings');

export type Investigator = {
  deckSize?: number;
  connectedLocations: string[];
  modifiers?: Modifier[];
  form: Form;
  name: Name;
  id: string;
  playerId: string;
  cardCode: string;
  art: string;
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
  placement: Placement;
  remainingActions: number;
  endedTurn: boolean;
  engagedEnemies: string[];
  assets: string[];
  events: string[];
  skills: string[];
  discard: CardContents[];
  hand: Card[];
  bondedCards: Card[];
  deck: CardContents[];
  decks: [string, Card[]][];
  treacheries: string[];
  defeated: boolean;
  resigned: boolean;
  additionalActions: AdditionalActionType[];
  cardsUnderneath: Card[];
  sealedChaosTokens: ChaosToken[];
  foundCards: Record<string, Card[]>;
  xp: number;
  physicalTrauma: number;
  mentalTrauma: number;
  supplies: string[];
  keys: ArkhamKey[];
  seals: Seal[];
  hunchDeck?: CardContents[];
  revealedHunchCard?: string | null;
  devoured?: Card[]
  mutated?: string;
  taboo?: string;
  deckUrl?: string;
  slots: Slot[];
  log: LogContents;
  meta: any;
  settings: CardSettings;
  handSize?: number | null;
}

type SlotType = 'HandSlot' | 'BodySlot' | 'AccessorySlot' | 'ArcaneSlot' | 'TarotSlot' | 'AllySlot'

export const slotTypeDecoder = JsonDecoder.oneOf<SlotType>([
  JsonDecoder.literal('HandSlot'),
  JsonDecoder.literal('BodySlot'),
  JsonDecoder.literal('AccessorySlot'),
  JsonDecoder.literal('ArcaneSlot'),
  JsonDecoder.literal('TarotSlot'),
  JsonDecoder.literal('AllySlot'),
], 'SlotType')

export type Slot = {
  tag: SlotType
  empty: boolean
}

export const slotDecoder = JsonDecoder.object<Slot>({
  tag: slotTypeDecoder,
  empty: JsonDecoder.boolean(),
}, 'Slot')

type SlotContents = {
  assets: string[]
}

const slotContentsDecoder = JsonDecoder.object<SlotContents>({
  assets: JsonDecoder.array(JsonDecoder.string(), 'AssetId[]'),
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

export const investigatorDetailsDecoder = JsonDecoder.object<InvestigatorDetails>({
  id: JsonDecoder.string(),
  classSymbol: classSymbolDecoder,
}, 'InvestigatorDetails');

export const investigatorDecoder = JsonDecoder.object({
  name: nameDecoder,
  id: JsonDecoder.string(),
  playerId: JsonDecoder.string(),
  cardCode: JsonDecoder.string(),
  art: JsonDecoder.string(),
  class: classSymbolDecoder,
  health: JsonDecoder.number(),
  sanity: JsonDecoder.number(),
  willpower: JsonDecoder.number(),
  intellect: JsonDecoder.number(),
  combat: JsonDecoder.number(),
  agility: JsonDecoder.number(),
  tokens: tokensDecoder,
  assignedHealthDamage: JsonDecoder.number(),
  assignedSanityDamage: JsonDecoder.number(),
  placement: placementDecoder,
  remainingActions: JsonDecoder.number(),
  endedTurn: JsonDecoder.boolean(),
  engagedEnemies: JsonDecoder.array<string>(JsonDecoder.string(), 'EnemyId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string(), 'AssetId[]'),
  events: JsonDecoder.array<string>(JsonDecoder.string(), 'EventId[]'),
  skills: JsonDecoder.array<string>(JsonDecoder.string(), 'SkillId[]'),
  // deck: Deck PlayerCard,
  discard: JsonDecoder.array<CardContents>(cardContentsDecoder, 'PlayerCardContents[]'),
  hand: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  bondedCards: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  deck: JsonDecoder.array<CardContents>(cardContentsDecoder, 'PlayerCardContents[]'),
  decks: JsonDecoder.array<[string, Card[]]>(JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.array<Card>(cardDecoder, 'Card[]')], '[string, Card[]]'), '[string, Card[]][]'),
  hunchDeck: v2Optional(JsonDecoder.array<CardContents>(cardContentsDecoder, 'PlayerCardContents[]')),
  revealedHunchCard: v2Optional(JsonDecoder.nullable(JsonDecoder.string())),
  devoured: v2Optional(JsonDecoder.array<Card>(cardDecoder, 'Card[]')),
  // traits: HashSet Trait,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string(), 'TreacheryId[]'),
  defeated: JsonDecoder.boolean(),
  resigned: JsonDecoder.boolean(),
  additionalActions: JsonDecoder.array<AdditionalAction>(additionalActionDecoder, 'AdditionalAction').map((arr) => arr.map((action) => action.kind)),
  cardsUnderneath: JsonDecoder.array<Card>(cardDecoder, 'CardUnderneath'),
  sealedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
  search: JsonDecoder.nullable(searchDecoder).map((search) => search?.searchFoundCards || {}),
  xp: JsonDecoder.number(),
  physicalTrauma: JsonDecoder.number(),
  mentalTrauma: JsonDecoder.number(),
  supplies: JsonDecoder.array<string>(JsonDecoder.string(), 'supplies'),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  seals: JsonDecoder.array<Seal>(sealDecoder, 'Seal[]'),
  deckSize: v2Optional(JsonDecoder.number()),
  connectedLocations: JsonDecoder.array<string>(JsonDecoder.string(), 'LocationId[]'),
  modifiers: v2Optional(JsonDecoder.array<Modifier>(modifierDecoder, 'Modifier[]')),
  form: formDecoder,
  mutated: v2Optional(JsonDecoder.string()),
  taboo: v2Optional(JsonDecoder.string()),
  deckUrl: v2Optional(JsonDecoder.string()),
  slots: slotsDecoder,
  log: logContentsDecoder,
  meta: JsonDecoder.succeed(),
  settings: cardSettingsDecoder,
  handSize: v2Optional(JsonDecoder.number()),
}, 'Investigator').map(({search, placement, ...rest}) => ({
  foundCards: search,
  location: placement.tag === "AtLocation" ? placement.contents : "00000000-0000-0000-0000-000000000000",
  placement,
  ...rest
}))

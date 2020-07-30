import { JsonDecoder } from 'ts.data.json';
import {
  Card,
  PlayerCardContents,
  playerCardContentsDecoder,
  cardDecoder,
} from '@/arkham/types/Card';

export interface Investigator {
  tag: string;
  contents: InvestigatorContents;
}

export interface InvestigatorContents {
  name: string;
  id: string;
  health: number;
  sanity: number;
  willpower: number;
  intellect: number;
  combat: number;
  agility: number;
  healthDamage: number;
  sanityDamage: number;
  clues: number;
  resources: number;
  location: string;
  remainingActions: number;
  endedTurn: boolean;
  engagedEnemies: string[];
  assets: string[];
  discard: PlayerCardContents[];
  hand: Card[];
  connectedLocations: string[];
  treacheries: string[];
  defeated: boolean;
  resigned: boolean;
}

export const investigatorContentsDecoder = JsonDecoder.object<InvestigatorContents>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  health: JsonDecoder.number,
  sanity: JsonDecoder.number,
  willpower: JsonDecoder.number,
  intellect: JsonDecoder.number,
  combat: JsonDecoder.number,
  agility: JsonDecoder.number,
  healthDamage: JsonDecoder.number,
  sanityDamage: JsonDecoder.number,
  clues: JsonDecoder.number,
  resources: JsonDecoder.number,
  location: JsonDecoder.string,
  remainingActions: JsonDecoder.number,
  endedTurn: JsonDecoder.boolean,
  engagedEnemies: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyId[]'),
  assets: JsonDecoder.array<string>(JsonDecoder.string, 'AssetId[]'),
  // deck: Deck PlayerCard,
  discard: JsonDecoder.array<PlayerCardContents>(playerCardContentsDecoder, 'PlayerCardContents[]'),
  hand: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  connectedLocations: JsonDecoder.array<string>(JsonDecoder.string, 'LocationId[]'),
  // traits: HashSet Trait,
  treacheries: JsonDecoder.array<string>(JsonDecoder.string, 'TreacheryId[]'),
  defeated: JsonDecoder.boolean,
  resigned: JsonDecoder.boolean,
}, 'Attrs');

export const investigatorDecoder = JsonDecoder.object<Investigator>({
  tag: JsonDecoder.string,
  contents: investigatorContentsDecoder,
}, 'Investigator');

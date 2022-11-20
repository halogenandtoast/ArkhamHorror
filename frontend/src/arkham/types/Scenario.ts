import { JsonDecoder } from 'ts.data.json';
import {
  Card,
  cardDecoder,
  EncounterCardContents,
  encounterCardContentsDecoder,
} from '@/arkham/types/Card';
import { ChaosBag, chaosBagDecoder } from '@/arkham/types/ChaosBag';
import { logContentsDecoder } from '@/arkham/types/Campaign';
import type { LogContents } from '@/arkham/types/Campaign';

export interface ScenarioName {
  title: string;
  subtitle: string | null;
}

export const scenarioNameDecoder = JsonDecoder.object<ScenarioName>(
  {
    title: JsonDecoder.string,
    subtitle: JsonDecoder.nullable(JsonDecoder.string),
  },
  'ScenarioName'
);

export interface ScenarioDeck {
  tag: string;
  deckSize: number;
}

type ScenarioMeta = { currentDepth: number }

export const scenarioMetaDecoder = JsonDecoder.oneOf<ScenarioMeta>([
  JsonDecoder.object<ScenarioMeta>({ currentDepth: JsonDecoder.number }, 'ScenarioMeta')], 'ScenarioMeta')

export interface Scenario {
  name: ScenarioName;
  id: string;
  difficulty: string;
  locationLayout: string[] | null;
  decksLayout: string[];
  decks: [string, Card[]][];
  cardsUnderAgendaDeck: Card[];
  cardsUnderActDeck: Card[];
  cardsNextToActDeck: Card[];
  setAsideCards: Card[];
  chaosBag: ChaosBag;
  discard: EncounterCardContents[];
  victoryDisplay: Card[];
  standaloneCampaignLog: LogContents | null;
  meta: ScenarioMeta | null;
}

export const scenarioDeckDecoder = JsonDecoder.object<ScenarioDeck>({
  tag: JsonDecoder.string,
  deckSize: JsonDecoder.array<Card>(cardDecoder, 'Card[]').map(cards => cards.length),
}, 'ScenarioDeck', { deckSize: 'contents' });

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  name: scenarioNameDecoder,
  id: JsonDecoder.string,
  difficulty: JsonDecoder.string,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]')),
  decksLayout: JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]'),
  decks: JsonDecoder.array<[string, Card[]]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.array<Card>(cardDecoder, 'Card[]')], '[string, Card[]]'), '[string, Card[]][]'),
  cardsUnderAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderActDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathActCards'),
  cardsNextToActDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToActDeck'),
  setAsideCards: JsonDecoder.array<Card>(cardDecoder, 'SetAsideCards'),
  chaosBag: chaosBagDecoder,
  discard: JsonDecoder.array<EncounterCardContents>(encounterCardContentsDecoder, 'EncounterCardContents[]'),
  victoryDisplay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  standaloneCampaignLog: logContentsDecoder,
  meta: JsonDecoder.nullable(scenarioMetaDecoder),
}, 'Scenario');

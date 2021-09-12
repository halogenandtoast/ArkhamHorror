import { JsonDecoder } from 'ts.data.json';
import { Card, cardDecoder, encounterCardContentsDecoder, EncounterCardContents } from '@/arkham/types/Card';

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

export interface Scenario {
  tag: string;
  contents: ScenarioContents;
}

export interface ScenarioDeck {
  tag: string;
  deckSize: number;
}

export interface ScenarioContents {
  name: ScenarioName;
  id: string;
  difficulty: string;
  locationLayout: string[] | null;
  deck: ScenarioDeck | null;
  cardsUnderAgendaDeck: Card[];
  cardsUnderActDeck: Card[];
  cardsNextToActDeck: Card[];
  setAsideCards: Card[];
}

export const scenarioDeckDecoder = JsonDecoder.object<ScenarioDeck>({
  tag: JsonDecoder.string,
  deckSize: JsonDecoder.array<EncounterCardContents>(encounterCardContentsDecoder, 'Card[]').map(cards => cards.length),
}, 'ScenarioDeck', { deckSize: 'contents' });

export const scenarioContentsDecoder = JsonDecoder.object<ScenarioContents>({
  name: scenarioNameDecoder,
  id: JsonDecoder.string,
  difficulty: JsonDecoder.string,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]')),
  deck: JsonDecoder.nullable(scenarioDeckDecoder),
  cardsUnderAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderActDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathActCards'),
  cardsNextToActDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToActDeck'),
  setAsideCards: JsonDecoder.array<Card>(cardDecoder, 'SetAsideCards'),
}, 'ScenarioContents');

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  tag: JsonDecoder.string,
  contents: scenarioContentsDecoder,
}, 'Scenario');

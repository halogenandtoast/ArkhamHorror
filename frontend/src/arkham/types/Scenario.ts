import { JsonDecoder } from 'ts.data.json';
import {
  Card,
  cardDecoder,
  EncounterCardContents,
  encounterCardContentsDecoder,
} from '@/arkham/types/Card';
import { ChaosBag, chaosBagDecoder } from '@/arkham/types/ChaosBag';
import { logContentsDecoder } from '@/arkham/types/Campaign';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
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

export interface Scenario {
  name: ScenarioName;
  id: string;
  reference: string;
  difficulty: string;
  locationLayout: string[] | null;
  decksLayout: string[];
  decks: [string, Card[]][];
  cardsUnderAgendaDeck: Card[];
  cardsUnderActDeck: Card[];
  cardsNextToActDeck: Card[];
  setAsideCards: Card[];
  setAsideKeys: ArkhamKey[];
  chaosBag: ChaosBag;
  discard: EncounterCardContents[];
  victoryDisplay: Card[];
  standaloneCampaignLog: LogContents | null;
  counts: Record<string, number>; // eslint-disable-line
  encounterDecks: Record<string, [EncounterCardContents[], EncounterCardContents[]]>;
}

export const scenarioDeckDecoder = JsonDecoder.object<ScenarioDeck>({
  tag: JsonDecoder.string,
  deckSize: JsonDecoder.array<Card>(cardDecoder, 'Card[]').map(cards => cards.length),
}, 'ScenarioDeck', { deckSize: 'contents' });

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  name: scenarioNameDecoder,
  id: JsonDecoder.string,
  reference: JsonDecoder.string,
  difficulty: JsonDecoder.string,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]')),
  decksLayout: JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]'),
  decks: JsonDecoder.array<[string, Card[]]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.array<Card>(cardDecoder, 'Card[]')], '[string, Card[]]'), '[string, Card[]][]'),
  cardsUnderAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderActDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathActCards'),
  cardsNextToActDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToActDeck'),
  setAsideKeys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  setAsideCards: JsonDecoder.array<Card>(cardDecoder, 'SetAsideCards'),
  chaosBag: chaosBagDecoder,
  discard: JsonDecoder.array<EncounterCardContents>(encounterCardContentsDecoder, 'EncounterCardContents[]'),
  victoryDisplay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  standaloneCampaignLog: logContentsDecoder,
  counts: JsonDecoder.array<[string, number]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.number], '[string, number]'), '[string, number][]').map<Record<string, number>>(res => {
    return res.reduce<Record<string, number>>((acc, [k, v]) => {
      acc[k] = v
      return acc
    }, {})
  }),
  encounterDecks: JsonDecoder.array<[string, [EncounterCardContents[], EncounterCardContents[]]]>(
    JsonDecoder.tuple([
      JsonDecoder.string,
      JsonDecoder.tuple([
        JsonDecoder.array<EncounterCardContents>(encounterCardContentsDecoder, 'EncounterCardContents[]'),
        JsonDecoder.array<EncounterCardContents>(encounterCardContentsDecoder, 'EncounterCardContents[]')
      ], '[[EncounterCardContents[], EncounterCardContents[]]'),
    ], '[string, [EncounterCardContents[], EncounterCardContents[]]'),
    '[string, [EncounterCardContents[], EncounterCardContents[]]][]').map<Record<string, [EncounterCardContents[], EncounterCardContents[]]>>(res => {
      return res.reduce<Record<string, [EncounterCardContents[], EncounterCardContents[]]>>((acc, [k, v]) => {
        acc[k] = v
        return acc
      }, {})
    }),

}, 'Scenario');

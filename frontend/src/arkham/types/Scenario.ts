import { JsonDecoder } from 'ts.data.json';
import {
  Card,
  cardDecoder,
  CardContents,
  cardContentsDecoder,
} from '@/arkham/types/Card';
import { ChaosBag, chaosBagDecoder } from '@/arkham/types/ChaosBag';
import { logContentsDecoder } from '@/arkham/types/Campaign';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import type { LogContents } from '@/arkham/types/Campaign';
import { Difficulty, difficultyDecoder } from '@/arkham/types/Difficulty';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { TarotScope, TarotCard, tarotCardDecoder, tarotScopeDecoder } from '@/arkham/types/TarotCard';

export type ScenarioName = {
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

export type ScenarioDeck = {
  tag: string;
  deckSize: number;
}

export type Scenario = {
  name: ScenarioName;
  id: string;
  reference: string;
  difficulty: Difficulty;
  locationLayout: string[] | null;
  usesGrid: boolean;
  decksLayout: string[];
  decks: [string, Card[]][];
  cardsUnderScenarioReference: Card[];
  cardsUnderAgendaDeck: Card[];
  cardsUnderActDeck: Card[];
  cardsNextToActDeck: Card[];
  cardsNextToAgendaDeck: Card[];
  setAsideCards: Card[];
  setAsideKeys: ArkhamKey[];
  chaosBag: ChaosBag;
  discard: CardContents[];
  victoryDisplay: Card[];
  standaloneCampaignLog: LogContents | null;
  tokens: Tokens;
  counts: Record<string, number>; // eslint-disable-line
  encounterDecks: Record<string, [CardContents[], CardContents[]]>;
  hasEncounterDeck: boolean;
  tarotCards: TarotCard[];
}

export const scenarioDeckDecoder = JsonDecoder.object<ScenarioDeck>({
  tag: JsonDecoder.string,
  deckSize: JsonDecoder.array<Card>(cardDecoder, 'Card[]').map(cards => cards.length),
}, 'ScenarioDeck', { deckSize: 'contents' });

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  name: scenarioNameDecoder,
  id: JsonDecoder.string,
  reference: JsonDecoder.string,
  difficulty: difficultyDecoder,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]')),
  usesGrid: JsonDecoder.boolean,
  decksLayout: JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]'),
  decks: JsonDecoder.array<[string, Card[]]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.array<Card>(cardDecoder, 'Card[]')], '[string, Card[]]'), '[string, Card[]][]'),
  cardsUnderScenarioReference: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderActDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathActCards'),
  cardsNextToActDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToActDeck'),
  cardsNextToAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToAgendaDeck'),
  setAsideKeys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  setAsideCards: JsonDecoder.array<Card>(cardDecoder, 'SetAsideCards'),
  chaosBag: chaosBagDecoder,
  discard: JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]'),
  victoryDisplay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  standaloneCampaignLog: logContentsDecoder,
  tokens: tokensDecoder,
  hasEncounterDeck: JsonDecoder.boolean,
  // tarotCards: JsonDecoder.array<TarotCard>(tarotCardDecoder, 'TarotCard[]'),
  tarotCards: JsonDecoder.
    array(
      JsonDecoder.tuple([tarotScopeDecoder, JsonDecoder.array(tarotCardDecoder, 'TarotCard[]')], '[TarotScope, TarotCard[]]'),
      '[TarotScope, TarotCard[]][]'
    ).map(res => res.reduce<TarotCard[]>((acc, [k, vs]) => [...acc, ...vs.map(v => ({ ...v, scope: k }))], [])),
  counts: JsonDecoder.array<[string, number]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.number], '[string, number]'), '[string, number][]').map<Record<string, number>>(res => {
    return res.reduce<Record<string, number>>((acc, [k, v]) => {
      acc[k] = v
      return acc
    }, {})
  }),
  encounterDecks: JsonDecoder.array<[string, [CardContents[], CardContents[]]]>(
    JsonDecoder.tuple([
      JsonDecoder.string,
      JsonDecoder.tuple([
        JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]'),
        JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]')
      ], '[[EncounterCardContents[], EncounterCardContents[]]'),
    ], '[string, [EncounterCardContents[], EncounterCardContents[]]'),
    '[string, [EncounterCardContents[], EncounterCardContents[]]][]').map<Record<string, [CardContents[], CardContents[]]>>(res => {
      return res.reduce<Record<string, [CardContents[], CardContents[]]>>((acc, [k, v]) => {
        acc[k] = v
        return acc
      }, {})
    }),

}, 'Scenario');

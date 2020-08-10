import { JsonDecoder } from 'ts.data.json';
import { Investigator, investigatorDecoder } from '@/arkham/types/Investigator';
import { Enemy, enemyDecoder } from '@/arkham/types/Enemy';
import { Location, locationDecoder } from '@/arkham/types/Location';
import { Scenario, scenarioDecoder } from '@/arkham/types/Scenario';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Act, actDecoder } from '@/arkham/types/Act';
import { Agenda, agendaDecoder } from '@/arkham/types/Agenda';
import { Phase, phaseDecoder } from '@/arkham/types/Phase';
import { Asset, assetDecoder } from '@/arkham/types/Asset';
import { Question, questionDecoder } from '@/arkham/types/Question';
import { Treachery, treacheryDecoder } from '@/arkham/types/Treachery';
import { SkillTest, skillTestDecoder } from '@/arkham/types/SkillTest';
import {
  Card,
  cardDecoder,
  EncounterCardContents,
  encounterCardContentsDecoder,
} from '@/arkham/types/Card';

export interface Game {
  id: number;
  currentData: GameState;
}

export function choices(game: Game) {
  if (!game.currentData.question) {
    return [];
  }

  switch (game.currentData.question.tag) {
    case 'ChooseOne':
      return game.currentData.question.contents;
    case 'ChooseOneAtATime':
      return game.currentData.question.contents;
    case 'ChooseOneFromSource':
      return game.currentData.question.contents.choices;
    default:
      return [];
  }
}

export function choicesSource(game: Game) {
  if (!game.currentData.question) {
    return null;
  }

  switch (game.currentData.question.tag) {
    case 'ChooseOne':
      return null;
    case 'ChooseOneAtATime':
      return null;
    case 'ChooseOneFromSource':
      return game.currentData.question.contents.source;
    default:
      return null;
  }
}

export interface GameState {
  activeInvestigatorId: string;
  acts: Record<string, Act>;
  agendas: Record<string, Agenda>;
  assets: Record<string, Asset>;
  chaosBag: ChaosToken[];
  discard: EncounterCardContents[];
  enemies: Record<string, Enemy>;
  gameOver: boolean;
  investigators: Record<string, Investigator>;
  leadInvestigatorId: string;
  locations: Record<string, Location>;
  phase: Phase;
  question: Question | null;
  scenario: Scenario;
  skillTest: SkillTest | null;
  treacheries: Record<string, Treachery>;
  focusedCards: Card[];
  victory: Card[];
}

export const gameStateDecoder = JsonDecoder.object<GameState>(
  {
    activeInvestigatorId: JsonDecoder.string,
    acts: JsonDecoder.dictionary<Act>(actDecoder, 'Dict<UUID, Act>'),
    agendas: JsonDecoder.dictionary<Agenda>(agendaDecoder, 'Dict<UUID, Agenda>'),
    assets: JsonDecoder.dictionary<Asset>(assetDecoder, 'Dict<UUID, Asset>'),
    chaosBag: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'ChaosToken[]'),
    discard: JsonDecoder.array<EncounterCardContents>(encounterCardContentsDecoder, 'EncounterCardContents[]'),
    enemies: JsonDecoder.dictionary<Enemy>(enemyDecoder, 'Dict<UUID, Enemy>'),
    gameOver: JsonDecoder.boolean,
    investigators: JsonDecoder.dictionary<Investigator>(investigatorDecoder, 'Dict<UUID, Investigator>'),
    leadInvestigatorId: JsonDecoder.string,
    locations: JsonDecoder.dictionary<Location>(locationDecoder, 'Dict<UUID, Location>'),
    phase: phaseDecoder,
    question: JsonDecoder.nullable(questionDecoder),
    scenario: scenarioDecoder,
    skillTest: JsonDecoder.nullable(skillTestDecoder),
    treacheries: JsonDecoder.dictionary<Treachery>(treacheryDecoder, 'Dict<UUID, Treachery>'),
    focusedCards: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
    victory: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  },
  'GameState',
);

export const gameDecoder = JsonDecoder.object<Game>(
  {
    id: JsonDecoder.number,
    currentData: gameStateDecoder,
  },
  'Game',
);

import { JsonDecoder } from 'ts.data.json';
import { Investigator, investigatorDecoder } from '@/arkham/types/Investigator';
import { Enemy, enemyDecoder } from '@/arkham/types/Enemy';
import { Story, storyDecoder } from '@/arkham/types/Story';
import { Location, locationDecoder } from '@/arkham/types/Location';
import { Message } from '@/arkham/types/Message';
import { Source } from '@/arkham/types/Source';
import { Scenario, scenarioDecoder } from '@/arkham/types/Scenario';
import { Campaign, campaignDecoder } from '@/arkham/types/Campaign';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Act, actDecoder } from '@/arkham/types/Act';
import { Agenda, agendaDecoder } from '@/arkham/types/Agenda';
import { Phase, phaseDecoder } from '@/arkham/types/Phase';
import { Asset, assetDecoder } from '@/arkham/types/Asset';
import { Event, eventDecoder } from '@/arkham/types/Event';
import { Question, questionDecoder } from '@/arkham/types/Question';
import { Skill, skillDecoder } from '@/arkham/types/Skill';
import { Treachery, treacheryDecoder } from '@/arkham/types/Treachery';
import { SkillTest, skillTestDecoder, SkillTestResults, skillTestResultsDecoder } from '@/arkham/types/SkillTest';
import {
  Card,
  cardDecoder,
} from '@/arkham/types/Card';

type GameState = { tag: 'IsPending' } | { tag: 'IsActive' } | { tag: 'IsOver' };

export const gameStateDecoder = JsonDecoder.oneOf<GameState>(
  [
    JsonDecoder.object({ tag: JsonDecoder.isExactly('IsPending') }, 'IsPending'),
    JsonDecoder.object({ tag: JsonDecoder.isExactly('IsActive') }, 'IsActive'),
    JsonDecoder.object({ tag: JsonDecoder.isExactly('IsOver') }, 'IsOver'),
  ],
  'GameState'
);

export interface Game {
  id: string;
  name: string;
  log: string[];

  activeInvestigatorId: string;
  acts: Record<string, Act>;
  agendas: Record<string, Agenda>;
  assets: Record<string, Asset>;
  events: Record<string, Event>;
  enemies: Record<string, Enemy>;
  stories: Record<string, Story>;
  outOfPlayEnemies: Record<string, Enemy>;
  enemiesInVoid: Record<string, Enemy>;
  gameState: GameState;
  investigators: Record<string, Investigator>;
  leadInvestigatorId: string;
  locations: Record<string, Location>;
  phase: Phase;
  playerOrder: string[];
  question: Record<string, Question>;
  scenario: Scenario | null;
  campaign: Campaign | null;
  skills: Record<string, Skill>;
  skillTest: SkillTest | null;
  skillTestResults: SkillTestResults | null;
  treacheries: Record<string, Treachery>;
  focusedCards: Card[];
  foundCards: Record<string, Card[]>;
  focusedTokens: ChaosToken[];
  skillTestTokens: ChaosToken[];
  activeCard: Card | null;
  removedFromPlay: Card[];
  encounterDeckSize: number;
}

export function choices(game: Game, investigatorId: string): Message[] {
  if (!game.question[investigatorId]) {
    return [];
  }

  const question: Question = game.question[investigatorId];

  const toContents = (q: Question): Message[] => {
    switch (q.tag) {
      case 'ChooseOne':
        return q.choices;
      case 'ChooseN':
        return q.choices;
      case 'ChooseUpToN':
        return q.choices;
      case 'ChooseSome':
        return q.contents;
      case 'ChooseOneAtATime':
        return q.choices;
      case 'QuestionLabel':
        return toContents(q.question);
      case 'Read':
        return q.readChoices;
      case 'PickSupplies':
        return q.choices;
      default:
        return [];
    }
  }

  return toContents(question)
}

export function choicesSource(game: Game, investigatorId: string): Source | null {
  if (!game.question[investigatorId]) {
    return null;
  }

  const question = game.question[investigatorId];

  switch (question.tag) {
    case 'ChooseOne':
      return null;
    case 'ChooseOneAtATime':
      return null;
    default:
      return null;
  }
}

interface Mode {
  This?: Campaign;
  That?: Scenario;
}

export const modeDecoder = JsonDecoder.object<Mode>(
  {
    This: JsonDecoder.optional(campaignDecoder),
    That: JsonDecoder.optional(scenarioDecoder)
  },
  'Mode'
);

export const gameDecoder = JsonDecoder.object<Game>(
  {
    id: JsonDecoder.string,
    name: JsonDecoder.string,
    log: JsonDecoder.array(JsonDecoder.string, 'LogEntry[]'),

    activeInvestigatorId: JsonDecoder.string,
    acts: JsonDecoder.dictionary<Act>(actDecoder, 'Dict<UUID, Act>'),
    agendas: JsonDecoder.dictionary<Agenda>(agendaDecoder, 'Dict<UUID, Agenda>'),
    assets: JsonDecoder.dictionary<Asset>(assetDecoder, 'Dict<UUID, Asset>'),
    events: JsonDecoder.dictionary<Event>(eventDecoder, 'Dict<UUID, Event>'),
    enemies: JsonDecoder.dictionary<Enemy>(enemyDecoder, 'Dict<UUID, Enemy>'),
    stories: JsonDecoder.dictionary<Story>(storyDecoder, 'Dict<UUID, Story>'),
    outOfPlayEnemies: JsonDecoder.dictionary<Enemy>(enemyDecoder, 'Dict<UUID, Enemy>'),
    enemiesInVoid: JsonDecoder.dictionary<Enemy>(enemyDecoder, 'Dict<UUID, Enemy>'),
    gameState: gameStateDecoder,
    investigators: JsonDecoder.dictionary<Investigator>(investigatorDecoder, 'Dict<UUID, Investigator>'),
    leadInvestigatorId: JsonDecoder.string,
    locations: JsonDecoder.dictionary<Location>(locationDecoder, 'Dict<UUID, Location>'),
    phase: phaseDecoder,
    playerOrder: JsonDecoder.array(JsonDecoder.string, 'PlayerOrder[]'),
    question: JsonDecoder.dictionary<Question>(questionDecoder, 'Dict<InvestigatorId, Question>'),
    scenario: modeDecoder.map(mode => mode.That || null),
    campaign: modeDecoder.map(mode => mode.This || null),
    skills: JsonDecoder.dictionary<Skill>(skillDecoder, 'Dict<UUID, Skill>'),
    skillTest: JsonDecoder.nullable(skillTestDecoder),
    skillTestResults: JsonDecoder.nullable(skillTestResultsDecoder),
    treacheries: JsonDecoder.dictionary<Treachery>(treacheryDecoder, 'Dict<UUID, Treachery>'),
    focusedCards: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
    foundCards: JsonDecoder.dictionary<Card[]>(JsonDecoder.array(cardDecoder, 'Card[]'), 'Dict<string, Card[]>'),
    focusedTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'Token[]'),
    skillTestTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'Token[]'),
    activeCard: JsonDecoder.nullable(cardDecoder),
    removedFromPlay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
    encounterDeckSize: JsonDecoder.number,
  },
  'Game',
  {
    scenario: 'mode',
    campaign: 'mode'
  }
);

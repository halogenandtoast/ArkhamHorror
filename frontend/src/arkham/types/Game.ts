import * as JsonDecoder from 'ts.data.json';
import { v2Optional } from '@/arkham/parser';
import { Investigator, InvestigatorDetails, investigatorDecoder, investigatorDetailsDecoder } from '@/arkham/types/Investigator';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ConcealedCard, concealedCardDecoder } from '@/arkham/types/ConcealedCard';
import { Enemy, enemyDecoder } from '@/arkham/types/Enemy';
import { Story, storyDecoder } from '@/arkham/types/Story';
import { Location, locationDecoder } from '@/arkham/types/Location';
import { Message, MessageType } from '@/arkham/types/Message';
import { Source } from '@/arkham/types/Source';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { Scenario, ScenarioDetails, scenarioDecoder, scenarioDetailsDecoder } from '@/arkham/types/Scenario';
import { Campaign, CampaignDetails, campaignDecoder, campaignDetailsDecoder } from '@/arkham/types/Campaign';
import { ChaosToken, chaosTokenDecoder } from '@/arkham/types/ChaosToken';
import { Act, actDecoder } from '@/arkham/types/Act';
import { Agenda, agendaDecoder } from '@/arkham/types/Agenda';
import { Phase, phaseDecoder, PhaseStep, phaseStepDecoder } from '@/arkham/types/Phase';
import { Asset, assetDecoder } from '@/arkham/types/Asset';
import { Event, eventDecoder } from '@/arkham/types/Event';
import { Question, questionDecoder } from '@/arkham/types/Question';
import { Skill, skillDecoder } from '@/arkham/types/Skill';
import { Treachery, treacheryDecoder } from '@/arkham/types/Treachery';
import { SkillTest, skillTestDecoder, SkillTestResults, skillTestResultsDecoder } from '@/arkham/types/SkillTest';
import { Card, cardDecoder, } from '@/arkham/types/Card';
import { TarotCard, tarotCardDecoder, } from '@/arkham/types/TarotCard';

type GameState = { tag: 'IsPending' } | { tag: 'IsActive' } | { tag: 'IsOver' } | { tag: 'IsChooseDecks', contents: string[] };

export const gameStateDecoder = JsonDecoder.oneOf<GameState>(
  [
    JsonDecoder.object({ tag: JsonDecoder.literal('IsPending') }, 'IsPending'),
    JsonDecoder.object({ tag: JsonDecoder.literal('IsActive') }, 'IsActive'),
    JsonDecoder.object({ tag: JsonDecoder.literal('IsOver') }, 'IsOver'),
    JsonDecoder.object({ tag: JsonDecoder.literal('IsChooseDecks'), contents: JsonDecoder.array(JsonDecoder.string(), 'string[]') }, 'IsChooseDecks'),
  ],
  'GameState'
);

export type GameDetails = {
  id: string;
  scenario: ScenarioDetails | null;
  campaign: CampaignDetails | null;
  gameState: GameState;
  name: string;
  investigators: InvestigatorDetails[];
  otherInvestigators: InvestigatorDetails[];
  multiplayerVariant: MultiplayerVariant;
}

export type MultiplayerVariant = 'WithFriends' | 'Solo'

const multiplayerVariantDecoder = JsonDecoder.oneOf<MultiplayerVariant>(
  [
    JsonDecoder.literal('WithFriends'),
    JsonDecoder.literal('Solo'),
  ],
  'MultiplayerVariant'
);

export type GameDetailsEntry = GameDetails & { tag: "game" }| { error: string, tag: "error" }

export type Game = {
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
  gameState: GameState;
  investigators: Record<string, Investigator>;
  otherInvestigators: Record<string, Investigator>;
  leadInvestigatorId: string;
  activePlayerId: string;
  locations: Record<string, Location>;
  concealed: Record<string, ConcealedCard>;
  phase: Phase;
  phaseStep: PhaseStep | null;
  playerOrder: string[];
  playerCount: number;
  question: Record<string, Question>;
  scenario: Scenario | null;
  campaign: Campaign | null;
  skills: Record<string, Skill>;
  skillTest: SkillTest | null;
  skillTestResults: SkillTestResults | null;
  treacheries: Record<string, Treachery>;
  focusedCards: Card[];
  focusedTarotCards: TarotCard[];
  foundCards: Record<string, Card[]>;
  focusedChaosTokens: ChaosToken[];
  skillTestChaosTokens: ChaosToken[];
  activeCard: Card | null;
  removedFromPlay: Card[];
  encounterDeckSize: number;
  cards: Record<string, Card>;
  modifiers: [Target, Modifier[]][];
  totalDoom: number;
  totalClues: number;
  scenarioSteps: number;
}

export function choices(game: Game, playerId: string): Message[] {
  if (!game.question[playerId]) {
    return [];
  }

  const question: Question = game.question[playerId];

  const toContents = (q: Question): Message[] => {
    switch (q.tag) {
      case 'ChooseOne':
        return q.choices;
      case 'ChooseN':
        return q.choices;
      case 'ChooseUpToN':
        return q.choices;
      case 'ChooseSome':
        return q.choices;
      case 'ChooseSome1':
        return q.choices;
      case 'ChooseOneAtATime':
        return q.choices;
      case 'ChooseOneAtATimeWithAuto':
        return [{tag: MessageType.LABEL, label: q.label }, ...q.choices];
      case 'QuestionLabel':
        return toContents(q.question);
      case 'Read':
        return q.readChoices.contents;
      case 'PickSupplies':
        return q.choices;
      case 'PickDestiny':
        return [];
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
    case 'ChooseOneAtATimeWithAuto':
      return null;
    default:
      return null;
  }
}

type Mode = {
  This?: Campaign;
  That?: Scenario;
}

export const modeDecoder = JsonDecoder.object<Mode>(
  {
    This: v2Optional(campaignDecoder),
    That: v2Optional(scenarioDecoder)
  },
  'Mode'
);

export const gameDetailsDecoder = JsonDecoder.object<GameDetails>(
  {
    id: JsonDecoder.string(),
    scenario: JsonDecoder.nullable(scenarioDetailsDecoder),
    campaign: JsonDecoder.nullable(campaignDetailsDecoder),
    gameState: gameStateDecoder,
    name: JsonDecoder.string(),
    investigators: JsonDecoder.array(investigatorDetailsDecoder, 'InvestigatorDetails[]'),
    otherInvestigators: JsonDecoder.array(investigatorDetailsDecoder, 'InvestigatorDetails[]'),
    multiplayerVariant: multiplayerVariantDecoder,
  },
  'GameDetails',
);

export const gameDetailsEntryDecoder = JsonDecoder.oneOf<GameDetailsEntry>(
  [
    gameDetailsDecoder.map(details => ({ ...details, tag: 'game' })),
    JsonDecoder.object({ error: JsonDecoder.string() }, 'Error').map(error => ({ ...error, tag: 'error' }))
  ],
  'GameDetailsEntry'
);

export const gameDecoder: JsonDecoder.Decoder<Game> = JsonDecoder.object(
  {
    id: JsonDecoder.string(),
    name: JsonDecoder.string(),
    log: JsonDecoder.array(JsonDecoder.string(), 'LogEntry[]'),

    activeInvestigatorId: JsonDecoder.string(),
    acts: JsonDecoder.record<Act>(actDecoder, 'Dict<UUID, Act>'),
    agendas: JsonDecoder.record<Agenda>(agendaDecoder, 'Dict<UUID, Agenda>'),
    assets: JsonDecoder.record<Asset>(assetDecoder, 'Dict<UUID, Asset>'),
    events: JsonDecoder.record<Event>(eventDecoder, 'Dict<UUID, Event>'),
    enemies: JsonDecoder.record<Enemy>(enemyDecoder, 'Dict<UUID, Enemy>'),
    stories: JsonDecoder.record<Story>(storyDecoder, 'Dict<UUID, Story>'),
    gameState: gameStateDecoder,
    investigators: JsonDecoder.record<Investigator>(investigatorDecoder, 'Dict<UUID, Investigator>'),
    otherInvestigators: JsonDecoder.record<Investigator>(investigatorDecoder, 'Dict<UUID, Investigator>'),
    leadInvestigatorId: JsonDecoder.string(),
    activePlayerId: JsonDecoder.string(),
    locations: JsonDecoder.record<Location>(locationDecoder, 'Dict<UUID, Location>'),
    concealed: JsonDecoder.record<ConcealedCard>(concealedCardDecoder, 'Dict<UUID, ConcealedCard>'),
    phase: phaseDecoder,
    phaseStep: JsonDecoder.nullable(phaseStepDecoder),
    playerOrder: JsonDecoder.array(JsonDecoder.string(), 'PlayerOrder[]'),
    playerCount: JsonDecoder.number(),
    question: JsonDecoder.record<Question>(questionDecoder, 'Dict<InvestigatorId, Question>'),
    mode: modeDecoder,
    skills: JsonDecoder.record<Skill>(skillDecoder, 'Dict<UUID, Skill>'),
    skillTest: JsonDecoder.nullable(skillTestDecoder),
    skillTestResults: JsonDecoder.nullable(skillTestResultsDecoder),
    treacheries: JsonDecoder.record<Treachery>(treacheryDecoder, 'Dict<UUID, Treachery>'),
    focusedCards: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
    focusedTarotCards: JsonDecoder.array<TarotCard>(tarotCardDecoder, 'TarotCard[]'),
    foundCards: JsonDecoder.record<Card[]>(JsonDecoder.array(cardDecoder, 'Card[]'), 'Dict<string, Card[]>'),
    focusedChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'Token[]'),
    skillTestChaosTokens: JsonDecoder.array<ChaosToken>(chaosTokenDecoder, 'Token[]'),
    activeCard: JsonDecoder.nullable(cardDecoder),
    removedFromPlay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
    encounterDeckSize: JsonDecoder.number(),
    cards: JsonDecoder.record<Card>(cardDecoder, 'Dict<string, Card>'),
    modifiers: JsonDecoder.array(JsonDecoder.tuple([targetDecoder, JsonDecoder.array(modifierDecoder, 'Modifier[]')], 'Target, Modifier[]'), 'Modifier[]'),
    totalDoom: JsonDecoder.number(),
    totalClues: JsonDecoder.number(),
    scenarioSteps: JsonDecoder.number()
  },
  'Game',
).map(({mode, ...game}) => ({
  scenario: mode?.That ?? null,
  campaign: mode?.This ?? null,
  ...game
}))

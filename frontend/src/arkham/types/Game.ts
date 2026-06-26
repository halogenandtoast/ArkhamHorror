import * as JsonDecoder from 'ts.data.json';
import { v2Optional, withDefault } from '@/arkham/parser';
import { AiFocus } from '@/arkham/types/NewGame';
import { Investigator, InvestigatorDetails, investigatorDecoder, investigatorDetailsDecoder } from '@/arkham/types/Investigator';
import { Modifier, modifierDecoder } from '@/arkham/types/Modifier';
import { ConcealedCard, concealedCardDecoder } from '@/arkham/types/ConcealedCard';
import { Enemy, enemyDecoder } from '@/arkham/types/Enemy';
import { Story, storyDecoder } from '@/arkham/types/Story';
import { ScarletKey, scarletKeyDecoder } from '@/arkham/types/ScarletKey';
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
export type { TarotCard } from '@/arkham/types/TarotCard';
import { History, historyDecoder } from '@/arkham/types/History';

type GameState = { tag: 'IsPending', contents: string[] } | { tag: 'IsActive' } | { tag: 'IsOver' } | { tag: 'IsChooseDecks', contents: string[] };

type AsIfRuling = 'chapter1' | 'chapter2'

// Per-seat AI state serialized into the game blob (Arkham.Ai.State.AiPlayerState),
// surfaced under `settings.aiPlayers` keyed by playerId so the UI can read which
// seats are AI, their enable flag, focus override, response delay, and priorities.
export type AiPlayerState = {
  aiEnabled: boolean
  aiInvestigatorCode: string
  aiFocusOverride: AiFocus | null
  aiPriorities: Target[]
  aiResponseDelayMs: number
}

type GameSettings = {
  settingsAbilitiesCannotReactToThemselves: boolean
  settingsAsIfRuling: AsIfRuling
  settingsStrictAsIfAt: boolean
  aiPlayers: Record<string, AiPlayerState>
}

const aiFocusDecoder = JsonDecoder.oneOf<AiFocus>([
  JsonDecoder.literal('combat'),
  JsonDecoder.literal('investigate'),
  JsonDecoder.literal('evade'),
  JsonDecoder.literal('support'),
  JsonDecoder.literal('survival'),
  JsonDecoder.literal('mobility'),
], 'AiFocus')

const aiPlayerStateDecoder = JsonDecoder.object<AiPlayerState>({
  aiEnabled: withDefault(true, JsonDecoder.boolean()),
  aiInvestigatorCode: JsonDecoder.string(),
  aiFocusOverride: withDefault<AiFocus | null>(null, aiFocusDecoder),
  aiPriorities: withDefault<Target[]>([], JsonDecoder.array(targetDecoder, 'Target[]')),
  aiResponseDelayMs: withDefault(1500, JsonDecoder.number()),
}, 'AiPlayerState')

const gameSettingsDecoder = JsonDecoder.object<GameSettings>({
  settingsAbilitiesCannotReactToThemselves: JsonDecoder.boolean(),
  settingsAsIfRuling: JsonDecoder.oneOf<AsIfRuling>([
    JsonDecoder.literal('chapter1'),
    JsonDecoder.literal('chapter2'),
  ], 'AsIfRuling'),
  settingsStrictAsIfAt: JsonDecoder.boolean(),
  aiPlayers: withDefault<Record<string, AiPlayerState>>({}, JsonDecoder.record<AiPlayerState>(aiPlayerStateDecoder, 'Dict<PlayerId, AiPlayerState>')),
}, 'GameSettings')

export const gameStateDecoder = JsonDecoder.oneOf<GameState>(
  [
    JsonDecoder.object({ tag: JsonDecoder.literal('IsPending'), contents: JsonDecoder.array(JsonDecoder.string(), 'string[]') }, 'IsPending'),
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
  hasOpenSeats: boolean;
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
  settings: GameSettings;

  activeInvestigatorId: string;
  acts: Record<string, Act>;
  agendas: Record<string, Agenda>;
  assets: Record<string, Asset>;
  events: Record<string, Event>;
  scarletKeys: Record<string, ScarletKey>;
  enemies: Record<string, Enemy>;
  stories: Record<string, Story>;
  gameState: GameState;
  investigators: Record<string, Investigator>;
  otherInvestigators: Record<string, Investigator>;
  killedInvestigators: Record<string, Investigator>;
  leadInvestigatorId: string;
  activePlayerId: string;
  locations: Record<string, Location>;
  concealed: Record<string, ConcealedCard>;
  phase: Phase;
  phaseStep: PhaseStep | null;
  inAction: boolean;
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
  undoActionStep: number | null;
  undoTurnStep: number | null;
  undoPhaseStep: number | null;
  undoRoundStep: number | null;
  roundHistory: Record<string, History>;
  phaseHistory: Record<string, History>;
  turnHistory: Record<string, History>;
  enemyAttackTargets: EnemyAttackTarget[];
}

export type EnemyAttackTarget = {
  enemy: string;
  target: Target;
};

const choicesCache = new WeakMap<Game, Map<string, Message[]>>();
const choicesSourceCache = new WeakMap<Game, Map<string, Source | null>>();
const choicesTooltipCache = new WeakMap<Game, Map<string, string | null>>();

function cachedByPlayer<T>(cache: WeakMap<Game, Map<string, T>>, game: Game, playerId: string, build: () => T): T {
  let gameCache = cache.get(game);
  if (!gameCache) {
    gameCache = new Map();
    cache.set(game, gameCache);
  }

  if (gameCache.has(playerId)) return gameCache.get(playerId) as T;
  const value = build();
  gameCache.set(playerId, value);
  return value;
}

function questionChoices(question: Question): Message[] {
  switch (question.tag) {
    case 'ChooseOne':
      return question.choices;
    case 'ChooseN':
      return question.choices;
    case 'ChooseUpToN':
      return question.choices;
    case 'ChooseSome':
      return question.choices;
    case 'ChooseSome1':
      return question.choices;
    case 'ChooseOneAtATime':
      return question.choices;
    case 'ChooseOneAtATimeWithAuto':
      return [{ tag: MessageType.LABEL, label: question.label }, ...question.choices];
    case 'QuestionLabel':
      return questionChoices(question.question);
    case 'QuestionWithSource':
      return questionChoices(question.question);
    case 'Read':
      return question.readChoices.contents;
    case 'PickSupplies':
      return question.choices;
    case 'PickDestiny':
      return [];
    default:
      return [];
  }
}

export function choices(game: Game, playerId: string): Message[] {
  return cachedByPlayer(choicesCache, game, playerId, () => {
    const question = game.question[playerId];
    return question ? questionChoices(question) : [];
  });
}

// True when the player's active question is a fast/action player window (the
// backend's `PlayerWindowChooseOne`, normalized to `ChooseOne` with `isPlayerWindow`).
// Used to distinguish a genuine "play this card" choice from an unrelated prompt that
// merely happens to offer the same card as a target (e.g. a search popup).
export function activeQuestionIsPlayerWindow(game: Game, playerId: string): boolean {
  let question: Question | undefined = game.question[playerId];

  while (question) {
    if (question.tag === 'ChooseOne') return question.isPlayerWindow === true;
    question = 'question' in question ? question.question : undefined;
  }

  return false;
}

// Returns the Source that prompted the player's active question, if any. The
// engine wraps such questions in `QuestionWithSource` so the frontend can
// highlight the source entity on the board while the question is pending.
export function choicesSource(game: Game, playerId: string): Source | null {
  return cachedByPlayer(choicesSourceCache, game, playerId, () => {
    let question: Question | undefined = game.question[playerId];

    while (question) {
      if (question.tag === 'QuestionWithSource') return question.source;
      question = 'question' in question ? question.question : undefined;
    }

    return null;
  });
}

// Returns the optional tooltip carried by the active `QuestionWithSource`, shown
// on the highlighted source entity.
export function choicesTooltip(game: Game, playerId: string): string | null {
  return cachedByPlayer(choicesTooltipCache, game, playerId, () => {
    let question: Question | undefined = game.question[playerId];

    while (question) {
      if (question.tag === 'QuestionWithSource') return question.tooltip;
      question = 'question' in question ? question.question : undefined;
    }

    return null;
  });
}

// When the player is assigning damage/horror, the engine wraps the assignment
// `ChooseOne` in a `QuestionLabel` whose label states the totals still to apply
// (e.g. "Assign 2 damage and 1 horror"). Returns the remaining token counts so
// the UI can show the tokens that still need placing (and suppress the modal).
export function damageAssignmentTokens(
  game: Game,
  playerId: string,
): { damage: number; horror: number } | null {
  // The assignment ChooseOne may be wrapped in a QuestionLabel (totals) and a
  // QuestionWithSource (damage source highlight). Walk down to the ChooseOne,
  // remembering the label that carries the remaining counts.
  let question: Question | undefined = game.question[playerId];
  let label: string | null = null;
  while (question) {
    if (question.tag === 'QuestionLabel') label = question.label;
    if (question.tag === 'ChooseOne') break;
    question = 'question' in question ? question.question : undefined;
  }
  if (!question || question.tag !== 'ChooseOne' || label === null) return null;
  const assigningDamage = question.choices.some(
    (c) =>
      c.tag === MessageType.COMPONENT_LABEL &&
      'tokenType' in c.component &&
      (c.component.tokenType === 'DamageToken' || c.component.tokenType === 'HorrorToken'),
  );
  if (!assigningDamage) return null;
  const damageMatch = label.match(/(\d+)\s+damage/);
  const horrorMatch = label.match(/(\d+)\s+horror/);
  return {
    damage: damageMatch ? parseInt(damageMatch[1], 10) : 0,
    horror: horrorMatch ? parseInt(horrorMatch[1], 10) : 0,
  };
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
    hasOpenSeats: JsonDecoder.boolean(),
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
    settings: v2Optional(gameSettingsDecoder),
    gameSettings: v2Optional(gameSettingsDecoder),

    activeInvestigatorId: JsonDecoder.string(),
    acts: JsonDecoder.record<Act>(actDecoder, 'Dict<UUID, Act>'),
    agendas: JsonDecoder.record<Agenda>(agendaDecoder, 'Dict<UUID, Agenda>'),
    assets: JsonDecoder.record<Asset>(assetDecoder, 'Dict<UUID, Asset>'),
    events: JsonDecoder.record<Event>(eventDecoder, 'Dict<UUID, Event>'),
    scarletKeys: JsonDecoder.record<ScarletKey>(scarletKeyDecoder, 'Dict<CardCode, ScarletKey>'),
    enemies: JsonDecoder.record<Enemy>(enemyDecoder, 'Dict<UUID, Enemy>'),
    stories: JsonDecoder.record<Story>(storyDecoder, 'Dict<UUID, Story>'),
    gameState: gameStateDecoder,
    investigators: JsonDecoder.record<Investigator>(investigatorDecoder, 'Dict<UUID, Investigator>'),
    otherInvestigators: JsonDecoder.record<Investigator>(investigatorDecoder, 'Dict<UUID, Investigator>'),
    killedInvestigators: JsonDecoder.optional(JsonDecoder.record<Investigator>(investigatorDecoder, 'Dict<UUID, Investigator>')),
    leadInvestigatorId: JsonDecoder.string(),
    activePlayerId: JsonDecoder.string(),
    locations: JsonDecoder.record<Location>(locationDecoder, 'Dict<UUID, Location>'),
    concealed: JsonDecoder.record<ConcealedCard>(concealedCardDecoder, 'Dict<UUID, ConcealedCard>'),
    phase: phaseDecoder,
    phaseStep: JsonDecoder.nullable(phaseStepDecoder),
    inAction: v2Optional(JsonDecoder.boolean()),
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
    scenarioSteps: JsonDecoder.number(),
    undoActionStep: v2Optional(JsonDecoder.number()),
    undoTurnStep: v2Optional(JsonDecoder.number()),
    undoPhaseStep: v2Optional(JsonDecoder.number()),
    undoRoundStep: v2Optional(JsonDecoder.number()),
    roundHistory: v2Optional(JsonDecoder.record<History>(historyDecoder, 'Dict<InvestigatorId, History>')),
    phaseHistory: v2Optional(JsonDecoder.record<History>(historyDecoder, 'Dict<InvestigatorId, History>')),
    turnHistory: v2Optional(JsonDecoder.record<History>(historyDecoder, 'Dict<InvestigatorId, History>')),
    enemyAttackTargets: JsonDecoder.fallback([], JsonDecoder.array(JsonDecoder.object<EnemyAttackTarget>({ enemy: JsonDecoder.string(), target: targetDecoder }, 'EnemyAttackTarget'), 'EnemyAttackTarget[]')),
  },
  'Game',
).map(({mode, killedInvestigators, settings, gameSettings, inAction, undoActionStep, undoTurnStep, undoPhaseStep, undoRoundStep, roundHistory, phaseHistory, turnHistory, ...game}) => ({
  scenario: mode?.That ?? null,
  campaign: mode?.This ?? null,
  killedInvestigators: killedInvestigators ?? {},
  inAction: inAction ?? false,
  settings: settings ?? gameSettings ?? {
    settingsAbilitiesCannotReactToThemselves: true,
    settingsAsIfRuling: 'chapter1',
    settingsStrictAsIfAt: false,
    aiPlayers: {},
  },
  undoActionStep: undoActionStep ?? null,
  undoTurnStep: undoTurnStep ?? null,
  undoPhaseStep: undoPhaseStep ?? null,
  undoRoundStep: undoRoundStep ?? null,
  roundHistory: roundHistory ?? {},
  phaseHistory: phaseHistory ?? {},
  turnHistory: turnHistory ?? {},
  ...game
}))

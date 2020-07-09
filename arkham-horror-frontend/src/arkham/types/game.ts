import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamCycle,
  ArkhamScenario,
  ArkhamPlayer,
  arkhamCycleDecoder,
  arkhamScenarioDecoder,
  arkhamPlayerDecoder,
  ArkhamEnemy,
  arkhamEnemyDecoder,
} from '@/arkham/types';
import {
  ArkhamLocation,
  arkhamLocationDecoder,
} from '@/arkham/types/location';
import {
  ArkhamChaosToken,
  arkhamChaosTokenDecoder,
} from '@/arkham/types/chaostoken';
import {
  ArkhamCard,
  arkhamCardDecoder,
  ArkhamEncounterCardContents,
  arkhamEncounterCardContentsDecoder,
} from '@/arkham/types/card';

export type ArkhamPhase = 'Mythos' | 'Investigation' | 'Enemy' | 'Upkeep';

export interface ArkhamGame {
  id: number;
  cycle: ArkhamCycle;
  scenario: ArkhamScenario;
  gameState: ArkhamGameState;
}

type ArkhamAgenda = {
  image: string;
  imageBack: string;
  cardCode: string;
  doom: number;
}

const arkhamAgendaDecoder = JsonDecoder.object<ArkhamAgenda>(
  {
    image: JsonDecoder.string,
    imageBack: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    doom: JsonDecoder.number,
  },
  'ArkhamAgenda',
);

type ActStackContents = {
  image: string;
  cardCode: string;
  canProgress: boolean;
}

export type AgendaStack = {
  tag: 'AgendaStack';
  contents: ArkhamAgenda[];
};

export type ActStack = {
  tag: 'ActStack';
  contents: ActStackContents[];
};

export type ArkhamStack = AgendaStack | ActStack;

export enum ArkhamStepTypes {
  INVESTIGATOR_ACTION = 'ArkhamGameStateStepInvestigatorActionStep',
  SKILL_CHECK = 'ArkhamGameStateStepSkillCheckStep',
  REVEAL_TOKEN = 'ArkhamGameStateStepRevealTokenStep',
  RESOLVE_ENEMIES = 'ArkhamGameStateStepResolveEnemiesStep',
  RESOLVE_ATTACKS_OF_OPPORTUNITY = 'ArkhamGameStateStepAttackOfOpportunityStep',
  CHOOSE_ONE = 'ArkhamGameStateStepChooseOneStep',
}

interface ArkhamInvestigatorActionStep {
  tag: ArkhamStepTypes.INVESTIGATOR_ACTION;
}

interface ArkhamSkillCheckStep {
  tag: ArkhamStepTypes.SKILL_CHECK;
  contents: ArkhamSkillCheckStepContents;
}

interface ArkhamRevealTokenStep {
  tag: ArkhamStepTypes.REVEAL_TOKEN;
  contents: ArkhamRevealTokenStepContents;
}

interface ArkhamResolveEnemiesStep {
  tag: ArkhamStepTypes.RESOLVE_ENEMIES;
  contents: ArkhamResolveEnemiesStepContents;
}

interface ArkhamAttackOfOpportunityStep {
  tag: ArkhamStepTypes.RESOLVE_ATTACKS_OF_OPPORTUNITY;
  contents: ArkhamResolveAttacksOfOpportunityStepContents;
}

interface ArkhamChooseOneStep {
  tag: ArkhamStepTypes.CHOOSE_ONE;
  contents: ArkhamChooseOneStepContents;
}

interface ArkhamLocationTarget {
  tag: 'LocationTarget';
  contents: ArkhamLocation;
}

interface ArkhamEnemyTarget {
  tag: 'EnemyTarget';
  contents: string;
}

interface ArkhamAgendaTarget {
  tag: 'AgendaTarget';
  contents: ArkhamAgenda;
}

type ArkhamTarget = ArkhamLocationTarget | ArkhamEnemyTarget | ArkhamAgendaTarget;

export const arkhamLocationTargetDecoder = JsonDecoder.object<ArkhamLocationTarget>({
  tag: JsonDecoder.isExactly('LocationTarget'),
  contents: arkhamLocationDecoder,
}, 'ArkhamLocationTarget');

export const arkhamEnemyTargetDecoder = JsonDecoder.object<ArkhamEnemyTarget>({
  tag: JsonDecoder.isExactly('EnemyTarget'),
  contents: JsonDecoder.string,
}, 'ArkhamEnemyTarget');

export const arkhamAgendaTargetDecoder = JsonDecoder.object<ArkhamAgendaTarget>({
  tag: JsonDecoder.isExactly('AgendaTarget'),
  contents: arkhamAgendaDecoder,
}, 'ArkhamAgendaTarget');

export const arkhamTargetDecoder = JsonDecoder.oneOf<ArkhamTarget>([
  arkhamLocationTargetDecoder,
  arkhamEnemyTargetDecoder,
  arkhamAgendaTargetDecoder,
], 'ArkhamTarget');

type ArkhamSkillType = string;

interface ArkhamInvestigateAction {
  tag: 'InvestigateAction';
  contents: string;
}

interface ArkhamFightEnemyAction {
  tag: 'FightEnemyAction';
  contents: string;
}

interface ArkhamEvadeEnemyAction {
  tag: 'EvadeEnemyAction';
  contents: string;
}

type ArkhamAction = ArkhamInvestigateAction | ArkhamFightEnemyAction | ArkhamEvadeEnemyAction;

interface ArkhamSkillCheckStepContents {
  action: ArkhamAction;
  type: ArkhamSkillType;
}

interface ArkhamRevealTokenStepContents {
  action: ArkhamAction;
  type: ArkhamSkillType;
  difficulty: number;
  modifiedSkillValue: number;
  cards: ArkhamCard[];
  token: ArkhamChaosToken;
}

interface ArkhamResolveEnemiesStepContents {
  enemyIds: string[];
}

interface ArkhamResolveAttacksOfOpportunityStepContents {
  enemyIds: string[];
  playerId: string;
}

interface ArkhamChooseOneStepContents {
  playerId: string;
  choices: string[];
  choiceTarget: ArkhamTarget;
}

export const arkhamActionInvestigateActionDecoder = JsonDecoder.object<ArkhamInvestigateAction>({
  tag: JsonDecoder.isExactly('InvestigateAction'),
  contents: JsonDecoder.string,
}, 'ArkhamInvestigateAction');

export const arkhamActionFightEnemyActionDecoder = JsonDecoder.object<ArkhamFightEnemyAction>({
  tag: JsonDecoder.isExactly('FightEnemyAction'),
  contents: JsonDecoder.string,
}, 'ArkhamFightEnemyAction');

export const arkhamActionEvadeEnemyActionDecoder = JsonDecoder.object<ArkhamEvadeEnemyAction>({
  tag: JsonDecoder.isExactly('EvadeEnemyAction'),
  contents: JsonDecoder.string,
}, 'ArkhamEvadeEnemyAction');

export const arkhamActionDecoder = JsonDecoder.oneOf<ArkhamAction>([
  arkhamActionInvestigateActionDecoder,
  arkhamActionFightEnemyActionDecoder,
  arkhamActionEvadeEnemyActionDecoder,
], 'ArkhamAction');

export const arkhamStepSkillCheckStepContentsDecoder = JsonDecoder.object<
    ArkhamSkillCheckStepContents
  >({
    action: arkhamActionDecoder,
    type: JsonDecoder.string,
  }, 'ArkhamSkillCheckStepContents');

export const arkhamStepRevealTokenStepContentsDecoder = JsonDecoder.object<
    ArkhamRevealTokenStepContents
  >({
    action: arkhamActionDecoder,
    type: JsonDecoder.string,
    difficulty: JsonDecoder.number,
    modifiedSkillValue: JsonDecoder.number,
    cards: JsonDecoder.array<ArkhamCard>(arkhamCardDecoder, 'ArkhamCard[]'),
    token: arkhamChaosTokenDecoder,
  }, 'ArkhamSkillCheckStepContents');

export const arkhamStepResolveEnemiesStepContentsDecoder = JsonDecoder.object<
    ArkhamResolveEnemiesStepContents
  >({
    enemyIds: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyID[]'),
  }, 'ArkhamResolveEnemiesStepContents');

export const arkhamStepResolveAttacksOfOpportunityContentsDecoder = JsonDecoder.object<
    ArkhamResolveAttacksOfOpportunityStepContents
  >({
    enemyIds: JsonDecoder.array<string>(JsonDecoder.string, 'EnemyID[]'),
    playerId: JsonDecoder.string,
  }, 'ArkhamResolveAttacksOfOpportunityStepContents');

export const arkhamStepChooseOneContentsDecoder = JsonDecoder.object<
    ArkhamChooseOneStepContents
  >({
    choices: JsonDecoder.array<string>(JsonDecoder.string, 'choices[]'),
    playerId: JsonDecoder.string,
    choiceTarget: arkhamTargetDecoder,
  }, 'ArkhamChooseOneStepContents');

type ArkhamStep = ArkhamInvestigatorActionStep
  | ArkhamSkillCheckStep
  | ArkhamRevealTokenStep
  | ArkhamResolveEnemiesStep
  | ArkhamAttackOfOpportunityStep
  | ArkhamChooseOneStep;

export const arkhamStepInvestigatorActionStepDecoder = JsonDecoder.object<
    ArkhamInvestigatorActionStep
  >({
    tag: JsonDecoder.isExactly(ArkhamStepTypes.INVESTIGATOR_ACTION),
  }, 'ArkhamInvestigateStep');

export const arkhamStepSkillCheckStepDecoder = JsonDecoder.object<
    ArkhamSkillCheckStep
  >({
    tag: JsonDecoder.isExactly(ArkhamStepTypes.SKILL_CHECK),
    contents: arkhamStepSkillCheckStepContentsDecoder,
  }, 'ArkhamSkillCheckStep');

export const arkhamStepRevealTokenStepDecoder = JsonDecoder.object<
    ArkhamRevealTokenStep
  >({
    tag: JsonDecoder.isExactly(ArkhamStepTypes.REVEAL_TOKEN),
    contents: arkhamStepRevealTokenStepContentsDecoder,
  }, 'ArkhamRevealTokenStep');

export const arkhamStepResolveEnemiesStepDecoder = JsonDecoder.object<
    ArkhamResolveEnemiesStep
  >({
    tag: JsonDecoder.isExactly(ArkhamStepTypes.RESOLVE_ENEMIES),
    contents: arkhamStepResolveEnemiesStepContentsDecoder,
  }, 'ArkhamResolveEnemiesStep');

export const arkhamStepAttackOfOpportunityStepDecoder = JsonDecoder.object<
    ArkhamAttackOfOpportunityStep
  >({
    tag: JsonDecoder.isExactly(ArkhamStepTypes.RESOLVE_ATTACKS_OF_OPPORTUNITY),
    contents: arkhamStepResolveAttacksOfOpportunityContentsDecoder,
  }, 'ArkhamResolveAttacksOfOpportunityStep');

export const arkhamStepChooseOneStepDecoder = JsonDecoder.object<
    ArkhamChooseOneStep
  >({
    tag: JsonDecoder.isExactly(ArkhamStepTypes.CHOOSE_ONE),
    contents: arkhamStepChooseOneContentsDecoder,
  }, 'ArkhamChooseOneStep');

export const arkhamStepDecoder = JsonDecoder.oneOf<ArkhamStep>([
  arkhamStepInvestigatorActionStepDecoder,
  arkhamStepSkillCheckStepDecoder,
  arkhamStepRevealTokenStepDecoder,
  arkhamStepResolveEnemiesStepDecoder,
  arkhamStepAttackOfOpportunityStepDecoder,
  arkhamStepChooseOneStepDecoder,
], 'ArkhamStep');

export interface ArkhamGameState {
  users: Record<string, string>;
  players: Record<string, ArkhamPlayer>;
  phase: ArkhamPhase;
  locations: Record<string, ArkhamLocation>;
  enemies: Record<string, ArkhamEnemy>;
  stacks: Record<string, ArkhamStack>;
  encounterDiscard: ArkhamEncounterCardContents[];
  step: ArkhamStep;
  chaosBag: ArkhamChaosToken[];
  activeUser: number;
}

export const arkhamPhaseDecoder = JsonDecoder.oneOf<ArkhamPhase>([
  JsonDecoder.isExactly('Mythos'),
  JsonDecoder.isExactly('Investigation'), JsonDecoder.isExactly('Enemy'),
  JsonDecoder.isExactly('Upkeep'),
], 'ArkhamPhase');

const arkhamActContentsDecoder = JsonDecoder.object<ActStackContents>(
  {
    image: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    canProgress: JsonDecoder.boolean,
  },
  'AgendaStackContents',
);

export const arkhamStackAgendaStackDecoder = JsonDecoder.object<AgendaStack>(
  {
    tag: JsonDecoder.isExactly('AgendaStack'),
    contents: JsonDecoder.array<ArkhamAgenda>(arkhamAgendaDecoder, 'Agenda[]'),
  },
  'AgendaStack',
);

export const arkhamStackActStackDecoder = JsonDecoder.object<ActStack>(
  {
    tag: JsonDecoder.isExactly('ActStack'),
    contents: JsonDecoder.array<ActStackContents>(arkhamActContentsDecoder, 'ActStack[]'),
  },
  'ActStack',
);

export const arkhamStackDecoder = JsonDecoder.oneOf<ArkhamStack>(
  [
    arkhamStackAgendaStackDecoder,
    arkhamStackActStackDecoder,
  ],
  'ArkhamStack',
);

export const arkhamGameStateDecoder = JsonDecoder.object<ArkhamGameState>(
  {
    users: JsonDecoder.dictionary(JsonDecoder.string, 'Dict<UserId, UUID>'),
    players: JsonDecoder.dictionary(arkhamPlayerDecoder, 'Dict<UUID, ArkhamPlayer>'),
    phase: arkhamPhaseDecoder,
    enemies: JsonDecoder.dictionary(
      arkhamEnemyDecoder,
      'Dict<UUID, ArkhamEnemy>',
    ),
    locations: JsonDecoder.dictionary(
      arkhamLocationDecoder,
      'Dict<LocationId, ArkhamLocation>',
    ),
    encounterDiscard: JsonDecoder.array<ArkhamEncounterCardContents>(arkhamEncounterCardContentsDecoder, 'ArkhamEncounterCardContents[]'),
    stacks: JsonDecoder.dictionary(arkhamStackDecoder, 'Dict<Text, ArkhamStack>'),
    step: arkhamStepDecoder,
    chaosBag: JsonDecoder.array<ArkhamChaosToken>(arkhamChaosTokenDecoder, 'ArkhamChaosToken[]'),
    activeUser: JsonDecoder.number,
  },
  'ArkhamGameState',
);

export const arkhamGameDecoder = JsonDecoder.object<ArkhamGame>(
  {
    id: JsonDecoder.number,
    cycle: arkhamCycleDecoder,
    scenario: arkhamScenarioDecoder,
    gameState: arkhamGameStateDecoder,
  },
  'ArkhamGame',
);

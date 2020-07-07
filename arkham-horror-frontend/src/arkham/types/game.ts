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

type AgendaStackContents = {
  image: string;
  cardCode: string;
  doom: number;
}

type ActStackContents = {
  image: string;
  cardCode: string;
  canProgress: boolean;
}

export type AgendaStack = {
  tag: 'AgendaStack';
  contents: AgendaStackContents[];
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

interface ArkhamLocationTarget {
  tag: 'LocationTarget';
  contents: ArkhamLocation;
}

interface ArkhamEnemyTarget {
  tag: 'EnemyTarget';
  contents: string;
}

type ArkhamTarget = ArkhamLocationTarget | ArkhamEnemyTarget;

export const arkhamLocationTargetDecoder = JsonDecoder.object<ArkhamLocationTarget>({
  tag: JsonDecoder.isExactly('LocationTarget'),
  contents: arkhamLocationDecoder,
}, 'ArkhamLocationTarget');

export const arkhamEnemyTargetDecoder = JsonDecoder.object<ArkhamEnemyTarget>({
  tag: JsonDecoder.isExactly('EnemyTarget'),
  contents: JsonDecoder.string,
}, 'ArkhamEnemyTarget');

export const arkhamTargetDecoder = JsonDecoder.oneOf<ArkhamTarget>([
  arkhamLocationTargetDecoder,
  arkhamEnemyTargetDecoder,
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

type ArkhamStep = ArkhamInvestigatorActionStep
  | ArkhamSkillCheckStep
  | ArkhamRevealTokenStep
  | ArkhamResolveEnemiesStep;

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

export const arkhamStepDecoder = JsonDecoder.oneOf<ArkhamStep>([
  arkhamStepInvestigatorActionStepDecoder,
  arkhamStepSkillCheckStepDecoder,
  arkhamStepRevealTokenStepDecoder,
  arkhamStepResolveEnemiesStepDecoder,
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

const arkhamAgendaContentsDecoder = JsonDecoder.object<AgendaStackContents>(
  {
    image: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    doom: JsonDecoder.number,
  },
  'AgendaStackContents',
);

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
    contents: JsonDecoder.array<AgendaStackContents>(arkhamAgendaContentsDecoder, 'AgendaStack[]'),
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

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
}

type AgendaStack = {
  tag: 'AgendaStack';
  contents: AgendaStackContents[];
};

type ActStack = {
  tag: 'ActStack';
  contents: ActStackContents[];
};

type ArkhamStack = AgendaStack | ActStack;

export enum ArkhamStepTypes {
  INVESTIGATOR_ACTION = 'ArkhamGameStateStepInvestigatorActionStep',
  SKILL_CHECK = 'ArkhamGameStateStepSkillCheckStep',
  REVEAL_TOKEN = 'ArkhamGameStateStepRevealTokenStep',
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

interface ArkhamLocationTarget {
  tag: 'LocationTarget';
  contents: ArkhamLocation;
}

type ArkhamTarget = ArkhamLocationTarget;

export const arkhamLocationTargetDecoder = JsonDecoder.object<ArkhamLocationTarget>({
  tag: JsonDecoder.isExactly('LocationTarget'),
  contents: arkhamLocationDecoder,
}, 'ArkhamLocationTarget');

export const arkhamTargetDecoder = JsonDecoder.oneOf<ArkhamTarget>([
  arkhamLocationTargetDecoder,
], 'ArkhamTarget');

type ArkhamSkillType = string;

interface ArkhamInvestigateAction {
  tag: 'InvestigateAction';
  contents: string;
}

type ArkhamAction = ArkhamInvestigateAction

interface ArkhamSkillCheckStepContents {
  action: ArkhamAction;
  target: ArkhamTarget;
  type: ArkhamSkillType;
}

interface ArkhamRevealTokenStepContents {
  action: ArkhamAction;
  target: ArkhamTarget;
  type: ArkhamSkillType;
  difficulty: number;
  modifiedSkillValue: number;
  token: ArkhamChaosToken;
}

export const arkhamActionInvestigateActionDecoder = JsonDecoder.object<ArkhamInvestigateAction>({
  tag: JsonDecoder.isExactly('InvestigateAction'),
  contents: JsonDecoder.string,
}, 'ArkhamInvestigateAction');

export const arkhamActionDecoder = JsonDecoder.oneOf<ArkhamAction>([
  arkhamActionInvestigateActionDecoder,
], 'ArkhamAction');

export const arkhamStepSkillCheckStepContentsDecoder = JsonDecoder.object<
    ArkhamSkillCheckStepContents
  >({
    action: arkhamActionDecoder,
    target: arkhamTargetDecoder,
    type: JsonDecoder.string,
  }, 'ArkhamSkillCheckStepContents');

export const arkhamStepRevealTokenStepContentsDecoder = JsonDecoder.object<
    ArkhamRevealTokenStepContents
  >({
    action: arkhamActionDecoder,
    target: arkhamTargetDecoder,
    type: JsonDecoder.string,
    difficulty: JsonDecoder.number,
    modifiedSkillValue: JsonDecoder.number,
    token: arkhamChaosTokenDecoder,
  }, 'ArkhamSkillCheckStepContents');

type ArkhamStep = ArkhamInvestigatorActionStep | ArkhamSkillCheckStep | ArkhamRevealTokenStep;

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

export const arkhamStepDecoder = JsonDecoder.oneOf<ArkhamStep>([
  arkhamStepInvestigatorActionStepDecoder,
  arkhamStepSkillCheckStepDecoder,
  arkhamStepRevealTokenStepDecoder,
], 'ArkhamStep');

export interface ArkhamGameState {
  player: ArkhamPlayer;
  phase: ArkhamPhase;
  locations: Record<string, ArkhamLocation>;
  enemies: Record<string, ArkhamEnemy>;
  stacks: Record<string, ArkhamStack>;
  step: ArkhamStep;
  chaosBag: ArkhamChaosToken[];
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
    player: arkhamPlayerDecoder,
    phase: arkhamPhaseDecoder,
    enemies: JsonDecoder.dictionary(
      arkhamEnemyDecoder,
      'Dict<UUID, ArkhamEnemy>',
    ),
    locations: JsonDecoder.dictionary(
      arkhamLocationDecoder,
      'Dict<LocationId, ArkhamLocation>',
    ),
    stacks: JsonDecoder.dictionary(arkhamStackDecoder, 'Dict<Text, ArkhamStack>'),
    step: arkhamStepDecoder,
    chaosBag: JsonDecoder.array<ArkhamChaosToken>(arkhamChaosTokenDecoder, 'ArkhamChaosToken[]'),
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

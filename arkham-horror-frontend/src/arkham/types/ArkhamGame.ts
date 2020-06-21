import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamCycle,
  ArkhamScenario,
  ArkhamPlayer,
  ArkhamInvestigator,
  arkhamCycleDecoder,
  arkhamScenarioDecoder,
  arkhamPlayerDecoder,
  arkhamInvestigatorDecoder,
} from '@/arkham/types';
import {
  ArkhamRevealedLocation,
  ArkhamUnrevealedLocation,
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

type AgendaStack = {
  tag: 'AgendaStack';
  contents: string;
};

type ActStack = {
  tag: 'ActStack';
  contents: string;
};

type ArkhamStack = AgendaStack | ActStack;

type LocationClues = {
  tag: 'LocationClues';
  contents: number;
}

type LocationInvestigator = {
  tag: 'LocationInvestigator';
  contents: ArkhamInvestigator;
}

type LocationContent = LocationClues | LocationInvestigator;

interface ArkhamInvestigatorActionStep {
  tag: 'ArkhamGameStateStepInvestigatorActionStep';
}

interface ArkhamSkillCheckStep {
  tag: 'ArkhamGameStateStepSkillCheckStep';
}

type ArkhamStep = ArkhamInvestigatorActionStep | ArkhamSkillCheckStep;

export const arkhamStepInvestigatorActionStepDecoder = JsonDecoder.object<
    ArkhamInvestigatorActionStep
  >({
    tag: JsonDecoder.isExactly('ArkhamGameStateStepInvestigatorActionStep'),
  }, 'ArkhamInvestigateStep');

export const arkhamStepSkillCheckStepDecoder = JsonDecoder.object<
    ArkhamSkillCheckStep
  >({
    tag: JsonDecoder.isExactly('ArkhamGameStateStepSkillCheckStep'),
  }, 'ArkhamSkillCheckStep');

export const arkhamStepDecoder = JsonDecoder.oneOf<ArkhamStep>([
  arkhamStepInvestigatorActionStepDecoder,
  arkhamStepSkillCheckStepDecoder,
], 'ArkhamStep');

export const arkhamLocationContentLocationCluesDecoder = JsonDecoder.object<LocationContent>({
  tag: JsonDecoder.isExactly('LocationClues'),
  contents: JsonDecoder.number,
}, 'LocationClues');

export const arkhamLocationContentLocationInvestigatorDecoder = JsonDecoder.object<
    LocationContent
  >({
    tag: JsonDecoder.isExactly('LocationInvestigator'),
    contents: arkhamInvestigatorDecoder,
  }, 'LocationInvestigator');

export const arkhamLocationContentDecoder = JsonDecoder.oneOf<LocationContent>([
  arkhamLocationContentLocationCluesDecoder,
  arkhamLocationContentLocationInvestigatorDecoder,
], 'Record<string, LocationContent[]>');

export interface ArkhamGameState {
  player: ArkhamPlayer;
  phase: ArkhamPhase;
  locations: (ArkhamRevealedLocation | ArkhamUnrevealedLocation)[];
  locationContents: Record<string, LocationContent[]>;
  stacks: ArkhamStack[];
  step: ArkhamStep;
  chaosBag: ArkhamChaosToken[];
}

export const arkhamPhaseDecoder = JsonDecoder.oneOf<ArkhamPhase>([
  JsonDecoder.isExactly('Mythos'),
  JsonDecoder.isExactly('Investigation'), JsonDecoder.isExactly('Enemy'),
  JsonDecoder.isExactly('Upkeep'),
], 'ArkhamPhase');

export const arkhamStackAgendaStackDecoder = JsonDecoder.object<AgendaStack>(
  {
    tag: JsonDecoder.isExactly('AgendaStack'),
    contents: JsonDecoder.string,
  },
  'AgendaStack',
);


export const arkhamStackActStackDecoder = JsonDecoder.object<ActStack>(
  {
    tag: JsonDecoder.isExactly('ActStack'),
    contents: JsonDecoder.string,
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
    locations: JsonDecoder.array<ArkhamUnrevealedLocation | ArkhamRevealedLocation>(arkhamLocationDecoder, 'ArkhamLocation[]'),
    locationContents: JsonDecoder.dictionary(
      JsonDecoder.array<LocationContent>(arkhamLocationContentDecoder, 'LocationContent[]'),
      'Dict<LocationContent[]>',
    ),
    stacks: JsonDecoder.array<ArkhamStack>(arkhamStackDecoder, 'ArkhamStack[]'),
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

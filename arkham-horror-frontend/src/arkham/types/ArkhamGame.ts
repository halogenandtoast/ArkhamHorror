import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamCycle,
  ArkhamScenario,
  ArkhamPlayer,
  arkhamCycleDecoder,
  arkhamScenarioDecoder,
  arkhamPlayerDecoder,
} from '@/arkham/types';
import {
  ArkhamRevealedLocation,
  ArkhamUnrevealedLocation,
  arkhamLocationDecoder,
} from '@/arkham/types/location';

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

export interface ArkhamGameState {
  player: ArkhamPlayer;
  phase: ArkhamPhase;
  locations: (ArkhamRevealedLocation | ArkhamUnrevealedLocation)[];
  stacks: ArkhamStack[];
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
    stacks: JsonDecoder.array<ArkhamStack>(arkhamStackDecoder, 'ArkhamStack[]'),
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

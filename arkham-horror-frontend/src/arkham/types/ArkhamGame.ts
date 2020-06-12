import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamCycle,
  ArkhamScenario,
  ArkhamPlayer,
  arkhamCycleDecoder,
  arkhamScenarioDecoder,
  arkhamPlayerDecoder,
} from '@/arkham/types';

export type ArkhamPhase = 'Mythos' | 'Investigation' | 'Enemy' | 'Upkeep';

export interface ArkhamGame {
  cycle: ArkhamCycle;
  scenario: ArkhamScenario;
  gameState: ArkhamGameState;
}

export interface ArkhamGameState {
  player: ArkhamPlayer;
  phase: ArkhamPhase;
}

export const arkhamPhaseDecoder = JsonDecoder.oneOf<ArkhamPhase>([
  JsonDecoder.isExactly('Mythos'),
  JsonDecoder.isExactly('Investigation'),
  JsonDecoder.isExactly('Enemy'),
  JsonDecoder.isExactly('Upkeep'),
], 'ArkhamPhase');

export const arkhamGameStateDecoder = JsonDecoder.object<ArkhamGameState>(
  {
    player: arkhamPlayerDecoder,
    phase: arkhamPhaseDecoder,
  },
  'ArkhamGameState',
);

export const arkhamGameDecoder = JsonDecoder.object<ArkhamGame>(
  {
    cycle: arkhamCycleDecoder,
    scenario: arkhamScenarioDecoder,
    gameState: arkhamGameStateDecoder,
  },
  'ArkhamGame',
);

import { JsonDecoder } from 'ts.data.json';
import {
  ArkhamCycle,
  ArkhamScenario,
  ArkhamPlayer,
  arkhamCycleDecoder,
  arkhamScenarioDecoder,
  arkhamPlayerDecoder,
} from '@/arkham/types';

export interface ArkhamGame {
  cycle: ArkhamCycle;
  scenario: ArkhamScenario;
  player: ArkhamPlayer;
}

export const arkhamGameDecoder = JsonDecoder.object<ArkhamGame>(
  {
    cycle: arkhamCycleDecoder,
    scenario: arkhamScenarioDecoder,
    player: arkhamPlayerDecoder,
  },
  'ArkhamGame',
);

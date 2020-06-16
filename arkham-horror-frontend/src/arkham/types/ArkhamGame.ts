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
  id: number;
  cycle: ArkhamCycle;
  scenario: ArkhamScenario;
  gameState: ArkhamGameState;
}

export interface ArkhamGameState {
  player: ArkhamPlayer;
  phase: ArkhamPhase;
  locations: (ArkhamRevealedLocation | ArkhamUnrevealedLocation)[];
}

export const arkhamPhaseDecoder = JsonDecoder.oneOf<ArkhamPhase>([
  JsonDecoder.isExactly('Mythos'),
  JsonDecoder.isExactly('Investigation'), JsonDecoder.isExactly('Enemy'),
  JsonDecoder.isExactly('Upkeep'),
], 'ArkhamPhase');

export type ArkhamLocationSymbol = 'Circle' | 'Heart';

export const arkhamLocationSymbolDecoder = JsonDecoder.oneOf<ArkhamLocationSymbol>([
  JsonDecoder.isExactly('Circle'),
  JsonDecoder.isExactly('Heart'),
], 'ArkhamLocationSymbol');

export type ArkhamLocation<T> = {
  tag: string;
  contents: T;
}

export interface ArkhamUnrevealedLocation {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  image: string;
}

export const arkhamUnrevealedLocationDecoder = JsonDecoder.object<ArkhamUnrevealedLocation>(
  {
    name: JsonDecoder.string,
    locationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    image: JsonDecoder.string,
  },
  'ArkhamUnrevealedLocation',
);

export interface ArkhamRevealedLocation {
  name: string;
  locationSymbols: ArkhamLocationSymbol[];
  shroud: number;
  image: string;
}

export const arkhamRevealedLocationDecoder = JsonDecoder.object<ArkhamRevealedLocation>(
  {
    name: JsonDecoder.string,
    locationSymbols: JsonDecoder.array<ArkhamLocationSymbol>(arkhamLocationSymbolDecoder, 'ArkhamLocationSymbol[]'),
    shroud: JsonDecoder.number,
    image: JsonDecoder.string,
  },
  'ArkhamUnrevealedLocation',
);

export const arkhamLocationRevealedLocationDecoder = JsonDecoder.object<
    ArkhamLocation<ArkhamRevealedLocation>
  >(
    {
      tag: JsonDecoder.isExactly('RevealedLocation'),
      contents: arkhamRevealedLocationDecoder,
    },
    'ArkhamLocation<ArkhamRevealedLocation>',
  );

export const arkhamLocationUnrevealedLocationDecoder = JsonDecoder.object<
    ArkhamLocation<ArkhamUnrevealedLocation>
  >(
    {
      tag: JsonDecoder.isExactly('UnrevealedLocation'),
      contents: arkhamUnrevealedLocationDecoder,
    },
    'ArkhamLocation<ArkhamUnrevealedLocation>',
  );

export const arkhamLocationDecoder = JsonDecoder.object<
    ArkhamLocation<ArkhamUnrevealedLocation | ArkhamRevealedLocation>
  >(
    {
      tag: JsonDecoder.string,
      contents: JsonDecoder.succeed,
    },
    'ArkhamLocation',
  ).then((value) => {
    switch (value.tag) {
      case 'RevealedLocation':
        return arkhamLocationRevealedLocationDecoder;
      case 'UnrevealedLocation':
        return arkhamLocationUnrevealedLocationDecoder;
      default:
        return JsonDecoder.fail<ArkhamLocation<ArkhamUnrevealedLocation | ArkhamRevealedLocation>>(
          `<ArkhamLocation> does not support tag ${value.tag}`,
        );
    }
  }).map((value) => value.contents);

export const arkhamGameStateDecoder = JsonDecoder.object<ArkhamGameState>(
  {
    player: arkhamPlayerDecoder,
    phase: arkhamPhaseDecoder,
    locations: JsonDecoder.array<ArkhamUnrevealedLocation | ArkhamRevealedLocation>(arkhamLocationDecoder, 'ArkhamLocation[]'),
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

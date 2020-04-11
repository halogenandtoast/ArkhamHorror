export interface Cycle {
  id: number;
  name: string;
}

export interface Scenario {
  id: number;
  name: string;
}

export interface Game {
  cycle: Cycle;
  scenario: Scenario;
}

export interface GameState {
  cycles: Cycle[];
  scenarios: Record<number, Scenario[]>;
  game: Game | null;
}

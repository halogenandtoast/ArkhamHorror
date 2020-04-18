export type ArkhamHorrorDifficulty = 'Easy' | 'Standard' | 'Hard' | 'Expert';

export interface ArkhamHorrorCycle {
  id: number;
  name: string;
}

export interface ArkhamHorrorCampaign {
  cycle: ArkhamHorrorCycle;
  difficulty: ArkhamHorrorDifficulty;
}

export interface ArkhamHorrorSettings {
  cycle: ArkhamHorrorCycle;
  difficulty: ArkhamHorrorDifficulty;
  deckUrl: string;
}

export type ArkhamHorrorStack = ArkhamHorrorAgendaStack | ArkhamHorrorActStack

export interface ArkhamHorrorAgendaStack {
  tag: string;
  currentCard: ArkhamHorrorCard;
}

export interface ArkhamHorrorCard {
  front: ArkhamHorrorCardFront;
  back: ArkhamHorrorCardBack;
}

export interface ArkhamHorrorCardFront {
  url: string;
}

export interface ArkhamHorrorCardBack {
  url: string;
}

export interface ArkhamHorrorActStack {
  tag: string;
}

export interface ArkhamHorrorScenario {
  name: string;
  stacks: ArkhamHorrorStack[];
}

export interface ArkhamHorrorGame {
  cycle: ArkhamHorrorCycle;
  scenario: ArkhamHorrorScenario;
  difficulty: ArkhamHorrorDifficulty;
}

export interface ArkhamHorrorGameState {
  cycles: ArkhamHorrorCycle[];
  scenarios: Record<number, ArkhamHorrorScenario[]>;
  game: ArkhamHorrorGame | null;
}

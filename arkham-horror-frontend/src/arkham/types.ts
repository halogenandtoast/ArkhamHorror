export type ArkhamDifficulty = 'Easy' | 'Standard' | 'Hard' | 'Expert';

export interface ArkhamCycle {
  id: number;
  name: string;
}

export interface ArkhamCampaign {
  cycle: ArkhamCycle;
  difficulty: ArkhamDifficulty;
}

export interface ArkhamSettings {
  cycle: ArkhamCycle;
  difficulty: ArkhamDifficulty;
  deckUrl: string;
}

export type ArkhamStack = ArkhamAgendaStack | ArkhamActStack

export interface ArkhamAgendaStack {
  tag: string;
  currentCard: ArkhamCard;
}

export interface ArkhamCard {
  front: ArkhamCardFront;
  back: ArkhamCardBack;
}

export interface ArkhamCardFront {
  url: string;
}

export interface ArkhamCardBack {
  url: string;
}

export interface ArkhamActStack {
  tag: string;
}

export interface ArkhamScenario {
  name: string;
  stacks: ArkhamStack[];
  locations: ArkhamLocation[];
}

export interface ArkhamRevealLocation {
  index: number;
}

export type ArkhamAction = ArkhamRevealLocation

export type ArkhamLocation = ArkhamLocationUnrevealed | ArkhamLocationRevealed

export type ArkhamSkillCheckResultType = 'success' | 'failure'

export type ArkhamSkill = 'Willpower' | 'Intellect' | 'Combat' | 'Agility'

export interface ArkhamSkillCheckResult {
  skill: ArkhamSkill;
  skillValue: number;
  difficulty: number;
  type: ArkhamSkillCheckResultType;
}

export interface ArkhamLocationUnrevealed {
  id: string;
  name: string;
  type: string;
}

export interface ArkhamLocationRevealed {
  id: string;
  name: string;
  type: string;
}

export interface ArkhamGame {
  id: string;
  cycle: ArkhamCycle;
  scenario: ArkhamScenario;
  difficulty: ArkhamDifficulty;
  actions: ArkhamAction[];
}

export interface ArkhamGameState {
  cycles: ArkhamCycle[];
  scenarios: Record<number, ArkhamScenario[]>;
  game: ArkhamGame | null;
}

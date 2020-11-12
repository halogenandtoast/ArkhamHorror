export enum ArkhamActionTypes {
  DRAW_CARD = 'DrawCardAction',
  TAKE_RESOURCE = 'TakeResourceAction',
  PLAY_CARD = 'InitiatePlayCard',
  // Activate a card ability
  MOVE = 'MoveAction',
  INVESTIGATE = 'InvestigateAction',
  FIGHT_ENEMY = 'FightEnemyAction',
  ENGAGE_ENEMY = 'EngageEnemyAction',
  EVADE_ENEMY = 'EvadeEnemyAction',
}

export interface ArkhamInvestigateAction {
  tag: ArkhamActionTypes.INVESTIGATE;
  contents: string;
}

export interface ArkhamTakeResourceAction {
  tag: ArkhamActionTypes.TAKE_RESOURCE;
  contents: [];
}

export interface ArkhamDrawCardAction {
  tag: ArkhamActionTypes.DRAW_CARD;
  contents: [];
}

export interface ArkhamPlayCardAction {
  tag: ArkhamActionTypes.PLAY_CARD;
  contents: number;
}

export interface ArkhamMoveAction {
  tag: ArkhamActionTypes.MOVE;
  contents: string;
}

export interface ArkhamFightEnemyAction {
  tag: ArkhamActionTypes.FIGHT_ENEMY;
  contents: string;
}

export interface ArkhamEvadeEnemyAction {
  tag: ArkhamActionTypes.EVADE_ENEMY;
  contents: string;
}

export interface ArkhamEngageEnemyAction {
  tag: ArkhamActionTypes.ENGAGE_ENEMY;
  contents: string;
}

export type ArkhamAction
  = ArkhamInvestigateAction
  | ArkhamTakeResourceAction
  | ArkhamDrawCardAction
  | ArkhamPlayCardAction
  | ArkhamMoveAction
  | ArkhamFightEnemyAction
  | ArkhamEvadeEnemyAction
  | ArkhamEngageEnemyAction;

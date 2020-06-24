export enum ArkhamActionTypes {
  INVESTIGATE = 'InvestigateAction',
  TAKE_RESOURCE = 'TakeResourceAction',
  DRAW_CARD = 'DrawCardAction',
  PLAY_CARD = 'PlayCardAction',
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

export type ArkhamAction
  = ArkhamInvestigateAction
  | ArkhamTakeResourceAction
  | ArkhamDrawCardAction
  | ArkhamPlayCardAction;

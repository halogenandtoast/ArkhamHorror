export enum ArkhamActionTypes {
  INVESTIGATE = 'InvestigateAction',
  TAKE_RESOURCE = 'TakeResourceAction',
}

export interface ArkhamInvestigateAction {
  tag: ArkhamActionTypes.INVESTIGATE;
  contents: string;
}

export interface ArkhamTakeResourceAction {
  tag: ArkhamActionTypes.TAKE_RESOURCE;
  contents: [];
}

export type ArkhamAction = ArkhamInvestigateAction | ArkhamTakeResourceAction

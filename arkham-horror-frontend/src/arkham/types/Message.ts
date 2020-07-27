import { JsonDecoder } from 'ts.data.json';

export type Message
  = ChooseTakeResourceAction
  | ChooseDrawCardAction
  | ChoosePlayCardAction
  | ChooseInvestigateAction
  | ChooseEndTurn;

export interface ChooseTakeResourceAction {
  tag: 'ChooseTakeResourceAction';
}

export const chooseTakeResourceActionDecoder = JsonDecoder.object<ChooseTakeResourceAction>(
  {
    tag: JsonDecoder.isExactly('ChooseTakeResourceAction'),
  },
  'ChooseTakeResourceAction',
);

export interface ChooseDrawCardAction {
  tag: 'ChooseDrawCardAction';
}

export const chooseDrawCardActionDecoder = JsonDecoder.object<ChooseDrawCardAction>(
  {
    tag: JsonDecoder.isExactly('ChooseDrawCardAction'),
  },
  'ChooseDrawCardAction',
);

export interface ChoosePlayCardAction {
  tag: 'ChoosePlayCardAction';
}

export const choosePlayCardActionDecoder = JsonDecoder.object<ChoosePlayCardAction>(
  {
    tag: JsonDecoder.isExactly('ChoosePlayCardAction'),
  },
  'ChoosePlayCardAction',
);

export interface ChooseInvestigateAction {
  tag: 'ChooseInvestigateAction';
}

export const chooseInvestigateActionDecoder = JsonDecoder.object<ChooseInvestigateAction>(
  {
    tag: JsonDecoder.isExactly('ChooseInvestigateAction'),
  },
  'ChooseInvestigateAction',
);

export interface ChooseEndTurn {
  tag: 'ChooseEndTurn';
}

export const chooseEndTurnDecoder = JsonDecoder.object<ChooseEndTurn>(
  {
    tag: JsonDecoder.isExactly('ChooseEndTurn'),
  },
  'ChooseEndTurn',
);

export const messageDecoder = JsonDecoder.oneOf<Message>([
  chooseTakeResourceActionDecoder,
  chooseDrawCardActionDecoder,
  choosePlayCardActionDecoder,
  chooseInvestigateActionDecoder,
  chooseEndTurnDecoder,
], 'Message');

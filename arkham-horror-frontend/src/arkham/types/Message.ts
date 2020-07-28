import { JsonDecoder } from 'ts.data.json';

export type Message
  = ChooseTakeResourceAction
  | ChooseDrawCardAction
  | ChoosePlayCardAction
  | ChooseInvestigateAction
  | SkillTestCommitCard
  | StartSkillTest
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

export interface SkillTestCommitCard {
  tag: 'SkillTestCommitCard';
}

export const skillTestCommitCardDecoder = JsonDecoder.object<SkillTestCommitCard>(
  {
    tag: JsonDecoder.isExactly('SkillTestCommitCard'),
  },
  'SkillTestCommitCard',
);

export interface StartSkillTest {
  tag: 'StartSkillTest';
}

export const startSkillTestDecoder = JsonDecoder.object<StartSkillTest>(
  {
    tag: JsonDecoder.isExactly('StartSkillTest'),
  },
  'StartSkillTest',
);

export const messageDecoder = JsonDecoder.oneOf<Message>([
  chooseTakeResourceActionDecoder,
  chooseDrawCardActionDecoder,
  choosePlayCardActionDecoder,
  chooseInvestigateActionDecoder,
  chooseEndTurnDecoder,
  skillTestCommitCardDecoder,
  startSkillTestDecoder,
], 'Message');

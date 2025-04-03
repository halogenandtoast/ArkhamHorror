import * as JsonDecoder from 'ts.data.json';

export type CampaignStep = PrologueStep | ScenarioStep | InterludeStep | UpgradeDeckStep | EpilogueStep | ResupplyPoint | CheckpointStep

export type PrologueStep = {
  tag: 'PrologueStep';
}

export type ResupplyPoint = {
  tag: 'ResupplyPoint';
}

export type EpilogueStep = {
  tag: 'EpilogueStep';
}

export const prologueStepDecoder = JsonDecoder.object<PrologueStep>(
  {
    tag: JsonDecoder.literal('PrologueStep'),
  },
  'PrologueStep',
);

export const resupplyPointStepDecoder = JsonDecoder.object<ResupplyPoint>(
  {
    tag: JsonDecoder.literal('ResupplyPoint'),
  },
  'ResupplyPoint',
);

export const epilogueStepDecoder = JsonDecoder.object<EpilogueStep>(
  {
    tag: JsonDecoder.literal('EpilogueStep'),
  },
  'EpilogueStep',
);

export type ScenarioStep = {
  tag: 'ScenarioStep';
  contents: string;
}

export const scenarioStepDecoder = JsonDecoder.object<ScenarioStep>(
  {
    tag: JsonDecoder.literal('ScenarioStep'),
    contents: JsonDecoder.string()
  },
  'ScenarioStep',
);

export type InterludeStep = {
  tag: 'InterludeStep';
  contents: number;
}

export const interludeStepDecoder = JsonDecoder.object<InterludeStep>(
  {
    tag: JsonDecoder.literal('InterludeStep'),
    contents: JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.succeed()], 'contents').map(([contents]) => contents),
  },
  'InterludeStep',
);

export type CheckpointStep = {
  tag: 'CheckpointStep';
  contents: number;
}

export const checkpointStepDecoder = JsonDecoder.object<CheckpointStep>(
  {
    tag: JsonDecoder.literal('CheckpointStep'),
    contents: JsonDecoder.number()
  },
  'CheckpointStep',
);

export type UpgradeDeckStep = {
  tag: 'UpgradeDeckStep';
}

export const upgradeStepDecoder = JsonDecoder.object<UpgradeDeckStep>(
  {
    tag: JsonDecoder.literal('UpgradeDeckStep'),
  },
  'UpgradeDeckStep',
);

export const campaignStepDecoder = JsonDecoder.oneOf<CampaignStep>(
  [
    prologueStepDecoder,
    resupplyPointStepDecoder,
    scenarioStepDecoder,
    interludeStepDecoder,
    checkpointStepDecoder,
    upgradeStepDecoder,
    epilogueStepDecoder
  ],
  'Question',
);


import * as JsonDecoder from 'ts.data.json';

export type CampaignStep = PrologueStep | ScenarioStep | InterludeStep | InterludeStepPart | UpgradeDeckStep | EpilogueStep | ResupplyPoint | CheckpointStep | CampaignSpecificStep;

export type PrologueStep = {
  tag: 'PrologueStep';
}

export type ResupplyPoint = {
  tag: 'ResupplyPoint';
}

export type CampaignSpecificStep = {
  tag: 'CampaignSpecificStep';
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

export const campaignSpecificStepDecoder = JsonDecoder.object<CampaignSpecificStep>(
  {
    tag: JsonDecoder.literal('CampaignSpecificStep'),
  },
  'CampaignSpecificStep',
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

export type InterludeStepPart = {
  tag: 'InterludeStepPart';
  contents: number;
}

export const interludeStepPartDecoder = JsonDecoder.object<InterludeStepPart>(
  {
    tag: JsonDecoder.literal('InterludeStepPart'),
    contents: JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.succeed(), JsonDecoder.number()], 'contents').map(([contents]) => contents),
  },
  'InterludeStepPart',
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
    campaignSpecificStepDecoder,
    scenarioStepDecoder,
    interludeStepDecoder,
    interludeStepPartDecoder,
    checkpointStepDecoder,
    upgradeStepDecoder,
    epilogueStepDecoder
  ],
  'Question',
);


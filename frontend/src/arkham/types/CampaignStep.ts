import { JsonDecoder } from 'ts.data.json';

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
    tag: JsonDecoder.isExactly('PrologueStep'),
  },
  'PrologueStep',
);

export const resupplyPointStepDecoder = JsonDecoder.object<ResupplyPoint>(
  {
    tag: JsonDecoder.isExactly('ResupplyPoint'),
  },
  'ResupplyPoint',
);

export const epilogueStepDecoder = JsonDecoder.object<EpilogueStep>(
  {
    tag: JsonDecoder.isExactly('EpilogueStep'),
  },
  'EpilogueStep',
);

export type ScenarioStep = {
  tag: 'ScenarioStep';
  contents: string;
}

export const scenarioStepDecoder = JsonDecoder.object<ScenarioStep>(
  {
    tag: JsonDecoder.isExactly('ScenarioStep'),
    contents: JsonDecoder.string
  },
  'ScenarioStep',
);

export type InterludeStep = {
  tag: 'InterludeStep';
  contents: number;
}

export const interludeStepDecoder = JsonDecoder.object<InterludeStep>(
  {
    tag: JsonDecoder.isExactly('InterludeStep'),
    contents: JsonDecoder.tuple([JsonDecoder.number, JsonDecoder.succeed]).map(([contents]) => contents),
  },
  'InterludeStep',
);

export type CheckpointStep = {
  tag: 'CheckpointStep';
  contents: number;
}

export const checkpointStepDecoder = JsonDecoder.object<CheckpointStep>(
  {
    tag: JsonDecoder.isExactly('CheckpointStep'),
    contents: JsonDecoder.number
  },
  'CheckpointStep',
);

export type UpgradeDeckStep = {
  tag: 'UpgradeDeckStep';
}

export const upgradeStepDecoder = JsonDecoder.object<UpgradeDeckStep>(
  {
    tag: JsonDecoder.isExactly('UpgradeDeckStep'),
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


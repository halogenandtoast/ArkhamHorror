import { JsonDecoder } from 'ts.data.json';

export interface Campaign {
  tag: string;
  contents: CampaignContents;
}

export type CampaignStep = PrologueStep | ScenarioStep | InterludeStep | UpgradeDeckStep

export interface PrologueStep {
  tag: 'PrologueStep';
}

export const prologueStepDecoder = JsonDecoder.object<PrologueStep>(
  {
    tag: JsonDecoder.isExactly('PrologueStep'),
  },
  'PrologueStep',
);

export interface ScenarioStep {
  tag: 'ScenarioStep';
}

export const scenarioStepDecoder = JsonDecoder.object<ScenarioStep>(
  {
    tag: JsonDecoder.isExactly('ScenarioStep'),
  },
  'ScenarioStep',
);

export interface InterludeStep {
  tag: 'InterludeStep';
}

export const interludeStepDecoder = JsonDecoder.object<InterludeStep>(
  {
    tag: JsonDecoder.isExactly('InterludeStep'),
  },
  'InterludeStep',
);

export interface UpgradeDeckStep {
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
    scenarioStepDecoder,
    interludeStepDecoder,
    upgradeStepDecoder
  ],
  'Question',
);

export interface LogContents {
  recorded: string[];
  recordedSets: any[]; // eslint-disable-line
}

export interface CampaignContents {
  name: string;
  id: string;
  log: LogContents;
  step: CampaignStep | null;
}

export const logContentsDecoder = JsonDecoder.object<LogContents>({
  recorded: JsonDecoder.array<string>(JsonDecoder.string, 'recorded[]'),
  recordedSets: JsonDecoder.array<any>(JsonDecoder.succeed, 'recordedSets[]'), // eslint-disable-line
}, 'LogContents');

export const campaignContentsDecoder = JsonDecoder.object<CampaignContents>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  log: logContentsDecoder,
  step: JsonDecoder.nullable(campaignStepDecoder)
}, 'CampaignContents');

export const campaignDecoder = JsonDecoder.object<Campaign>({
  tag: JsonDecoder.string,
  contents: campaignContentsDecoder,
}, 'Campaign');

